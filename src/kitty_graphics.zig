/// Kitty Graphics Protocol support.
///
/// Intercepts APC_G escape sequences from the terminal data stream,
/// decodes image data, and calls into Elisp to record images and
/// placements for rendering.
///
/// The protocol uses APC (Application Program Command) sequences:
///   ESC _ G <key=value,...> ; <base64-payload> ST
/// where ST is ESC \ or 0x9C.
const std = @import("std");
const emacs = @import("emacs.zig");
const Terminal = @import("terminal.zig");
const gt = @import("ghostty.zig");

const alloc = std.heap.c_allocator;

// ---- Public types ----

pub const StoredImage = struct {
    id: u32,
    width: u32,
    height: u32,
    data: []const u8, // PNG or PPM bytes, owned
    is_png: bool,

    fn deinit(self: *StoredImage) void {
        if (self.data.len > 0) alloc.free(@constCast(self.data));
    }
};

pub const ImageStore = struct {
    images: std.AutoHashMap(u32, StoredImage),
    loading: ?LoadingState = null,
    total_bytes: usize = 0,
    next_id: u32 = 2147483647, // start high like ghostty

    /// Buffer for APC sequences split across processData calls.
    /// When an unterminated ESC_G is found, we save it here and
    /// prepend it to the next call's data.
    partial_apc: std.ArrayList(u8) = .empty,

    /// Cursor position captured when the partial APC started.
    partial_cursor_y: u16 = 0,
    partial_cursor_x: u16 = 0,

    pub fn init() ImageStore {
        return .{
            .images = std.AutoHashMap(u32, StoredImage).init(alloc),
        };
    }

    pub fn deinit(self: *ImageStore) void {
        var it = self.images.iterator();
        while (it.next()) |entry| entry.value_ptr.deinit();
        self.images.deinit();
        if (self.loading) |*ld| ld.deinit();
        self.partial_apc.deinit(alloc);
    }

    fn storeImage(self: *ImageStore, img: StoredImage) void {
        if (self.images.getPtr(img.id)) |old| {
            self.total_bytes -|= old.data.len;
            old.deinit();
            old.* = img;
        } else {
            self.images.put(img.id, img) catch return;
        }
        self.total_bytes += img.data.len;
    }

    fn removeImage(self: *ImageStore, image_id: u32) void {
        if (self.images.fetchRemove(image_id)) |kv| {
            self.total_bytes -|= kv.value.data.len;
            var v = kv.value;
            v.deinit();
        }
    }

    fn deleteAll(self: *ImageStore) void {
        var it = self.images.iterator();
        while (it.next()) |entry| entry.value_ptr.deinit();
        self.images.clearRetainingCapacity();
        self.total_bytes = 0;
    }

    fn allocateId(self: *ImageStore) u32 {
        defer self.next_id +%= 1;
        return self.next_id;
    }
};

// ---- Chunked transfer state ----

const LoadingState = struct {
    image_id: u32,
    width: u32,
    height: u32,
    format: Format,
    compression: Compression,
    display_after: bool,
    placement_id: u32,
    columns: u32,
    rows: u32,
    z_index: i32,
    chunks: std.ArrayList(u8) = .empty,

    fn init(cmd: *const Params) LoadingState {
        return .{
            .image_id = cmd.image_id,
            .width = cmd.width,
            .height = cmd.height,
            .format = cmd.format,
            .compression = cmd.compression,
            .display_after = cmd.action == .transmit_and_display,
            .placement_id = cmd.placement_id,
            .columns = cmd.columns,
            .rows = cmd.rows,
            .z_index = cmd.z_index,
        };
    }

    fn addData(self: *LoadingState, data: []const u8) void {
        self.chunks.appendSlice(alloc, data) catch {};
    }

    fn deinit(self: *LoadingState) void {
        self.chunks.deinit(alloc);
    }
};

// ---- Command types ----

const Action = enum { transmit, transmit_and_display, display, delete, query };
const Format = enum { rgb, rgba, png };
const Compression = enum { none, zlib };

const Params = struct {
    action: Action = .transmit,
    image_id: u32 = 0,
    placement_id: u32 = 0,
    format: Format = .rgba,
    compression: Compression = .none,
    more_chunks: bool = false,
    width: u32 = 0,
    height: u32 = 0,
    columns: u32 = 0,
    rows: u32 = 0,
    z_index: i32 = 0,
    delete_what: u8 = 'a',
};

// ---- Main entry point ----

/// Process terminal data, intercepting Kitty graphics APC sequences.
/// Feeds non-APC data to vtWrite in chunks and handles graphics commands.
/// Handles APC sequences that are split across multiple calls by buffering
/// partial sequences in the ImageStore.
pub fn processData(env: emacs.Env, term: *Terminal, data: []const u8) void {
    const store = &term.image_store;

    // If we have a partial APC from a previous call, try to complete it.
    if (store.partial_apc.items.len > 0) {
        // Look for the terminator in the new data.
        const term_pos = findApcTerminator(data, 0);
        const term_len = terminatorLen(data, term_pos);

        if (term_pos >= data.len) {
            // Still unterminated — accumulate and wait for more.
            store.partial_apc.appendSlice(alloc, data) catch {};
            // Feed to vtWrite so libghostty sees the data progressively.
            term.vtWrite(data);
            return;
        }

        // Found the terminator — complete the APC.
        store.partial_apc.appendSlice(alloc, data[0..term_pos]) catch {};

        // Feed the terminator portion to vtWrite.
        term.vtWrite(data[0 .. term_pos + term_len]);

        // Parse the completed payload (skip the ESC_G prefix we stored).
        const full = store.partial_apc.items;
        if (full.len > 3) {
            handlePayload(env, term, full[3..], store.partial_cursor_y, store.partial_cursor_x);
        }
        store.partial_apc.clearRetainingCapacity();

        // Continue processing the rest of the data.
        processDataInner(env, term, data[term_pos + term_len ..]);
        return;
    }

    processDataInner(env, term, data);
}

fn processDataInner(env: emacs.Env, term: *Terminal, data: []const u8) void {
    const store = &term.image_store;
    var pos: usize = 0;

    // Detect screen-clear sequences (ESC[2J or ESC[3J) that should
    // remove image placements.  Scan the raw data before splitting.
    if (containsEraseDisplay(data)) {
        _ = env.call0(emacs.sym.@"ghostel--kitty-clear-all");
    }

    while (pos < data.len) {
        // Find next ESC _ G (0x1b 0x5f 0x47)
        const apc_start = findApcG(data, pos) orelse {
            term.vtWrite(data[pos..]);
            break;
        };

        // Feed data before the APC to vtWrite
        if (apc_start > pos) {
            term.vtWrite(data[pos..apc_start]);
        }

        // Find the APC terminator
        const payload_start = apc_start + 3; // skip ESC _ G
        const term_pos = findApcTerminator(data, payload_start);
        const term_len = terminatorLen(data, term_pos);

        if (term_pos >= data.len) {
            // Unterminated APC — buffer it for the next call.
            store.partial_cursor_y = term.getCursorY();
            store.partial_cursor_x = term.getCursorX();
            store.partial_apc.clearRetainingCapacity();
            store.partial_apc.appendSlice(alloc, data[apc_start..]) catch {};
            // Feed to vtWrite so libghostty sees it progressively.
            term.vtWrite(data[apc_start..]);
            break;
        }

        // Capture cursor position BEFORE libghostty processes this APC
        const cursor_y = term.getCursorY();
        const cursor_x = term.getCursorX();

        // Feed the APC to vtWrite so libghostty handles cursor movement
        // and protocol responses (written back via write_pty callback).
        term.vtWrite(data[apc_start .. term_pos + term_len]);

        // Parse and process our copy of the payload
        const payload = data[payload_start..term_pos];
        handlePayload(env, term, payload, cursor_y, cursor_x);

        pos = term_pos + term_len;
    }
}

// ---- APC scanner ----

/// Check if data contains an erase-display CSI sequence.
/// Matches ESC [ 2 J (erase screen) and ESC [ 3 J (erase scrollback).
fn containsEraseDisplay(data: []const u8) bool {
    if (data.len < 4) return false;
    var i: usize = 0;
    while (i + 3 < data.len) {
        if (data[i] == 0x1b and data[i + 1] == '[' and
            (data[i + 2] == '2' or data[i + 2] == '3') and data[i + 3] == 'J')
        {
            return true;
        }
        i += 1;
    }
    return false;
}

/// Find the next ESC _ G sequence starting at `from`.
fn findApcG(data: []const u8, from: usize) ?usize {
    if (data.len < from + 3) return null;
    var i = from;
    while (i + 2 < data.len) {
        if (data[i] == 0x1b and data[i + 1] == '_' and data[i + 2] == 'G') {
            return i;
        }
        i += 1;
    }
    return null;
}

/// Find the APC terminator: ST (ESC \) or single-byte ST (0x9C).
fn findApcTerminator(data: []const u8, start: usize) usize {
    var pos = start;
    while (pos < data.len) {
        if (data[pos] == 0x9c) return pos;
        if (data[pos] == 0x1b and pos + 1 < data.len and data[pos + 1] == '\\') return pos;
        pos += 1;
    }
    return data.len;
}

fn terminatorLen(data: []const u8, pos: usize) usize {
    if (pos >= data.len) return 0;
    if (data[pos] == 0x9c) return 1;
    if (data[pos] == 0x1b and pos + 1 < data.len and data[pos + 1] == '\\') return 2;
    return 0;
}

// ---- Payload processing ----

fn handlePayload(env: emacs.Env, term: *Terminal, payload: []const u8, cursor_y: u16, cursor_x: u16) void {
    // Split at ';' — control params ; base64-data
    const semi = std.mem.indexOfScalar(u8, payload, ';');
    const ctrl = if (semi) |s| payload[0..s] else payload;
    const b64 = if (semi) |s| payload[s + 1 ..] else &[_]u8{};

    var params = parseControlParams(ctrl);

    // Decode base64 payload
    const decoded = decodeBase64(b64);
    defer if (decoded) |d| alloc.free(d);
    const data = decoded orelse &[_]u8{};

    const store = &term.image_store;

    switch (params.action) {
        .transmit => _ = handleTransmit(store, &params, data),
        .transmit_and_display => {
            const image_id = handleTransmit(store, &params, data);
            if (!params.more_chunks and image_id != 0) {
                emitPlacement(env, store, image_id, &params, cursor_y, cursor_x, term);
            }
        },
        .display => {
            emitPlacement(env, store, params.image_id, &params, cursor_y, cursor_x, term);
        },
        .delete => handleDelete(env, store, &params),
        .query => {}, // libghostty handles the response
    }
}

/// Process a transmit command. Returns the assigned image ID (0 on failure).
fn handleTransmit(store: *ImageStore, params: *const Params, data: []const u8) u32 {
    if (params.more_chunks) {
        // Start or continue chunked transfer
        if (store.loading == null) {
            var ld = LoadingState.init(params);
            if (ld.image_id == 0) ld.image_id = store.allocateId();
            store.loading = ld;
        }
        if (store.loading) |*ld| ld.addData(data);
        return if (store.loading) |ld| ld.image_id else 0;
    }

    // Final or single-chunk image
    var final_data = data;
    var format = params.format;
    var width = params.width;
    var height = params.height;
    var image_id = params.image_id;

    if (store.loading) |*ld| {
        // Append final chunk
        ld.addData(data);
        final_data = ld.chunks.items;
        format = ld.format;
        width = ld.width;
        height = ld.height;
        image_id = ld.image_id;
    }

    if (image_id == 0) image_id = store.allocateId();

    // Skip zlib-compressed data for now
    if (params.compression == .zlib or
        (store.loading != null and store.loading.?.compression == .zlib))
    {
        if (store.loading) |*ld| ld.deinit();
        store.loading = null;
        return 0;
    }

    // For PNG, extract dimensions from IHDR if not specified
    if (format == .png and (width == 0 or height == 0)) {
        extractPngDimensions(final_data, &width, &height);
    }

    // Convert to Emacs-compatible image format
    const result = toEmacsFormat(final_data, format, width, height) orelse {
        if (store.loading) |*ld| ld.deinit();
        store.loading = null;
        return 0;
    };

    store.storeImage(.{
        .id = image_id,
        .width = width,
        .height = height,
        .data = result.data,
        .is_png = result.is_png,
    });

    if (store.loading) |*ld| ld.deinit();
    store.loading = null;
    return image_id;
}

fn emitPlacement(
    env: emacs.Env,
    store: *const ImageStore,
    image_id: u32,
    params: *const Params,
    cursor_y: u16,
    cursor_x: u16,
    term: *const Terminal,
) void {
    const img = store.images.get(image_id) orelse return;

    // Compute display cell dimensions
    var cols = params.columns;
    var rows_count = params.rows;
    if (cols == 0 or rows_count == 0) {
        const cw: u32 = if (term.cell_width_px > 1) term.cell_width_px else 8;
        const ch: u32 = if (term.cell_height_px > 1) term.cell_height_px else 16;
        if (cols == 0) cols = (img.width + cw - 1) / cw;
        if (rows_count == 0) rows_count = (img.height + ch - 1) / ch;
    }
    if (cols == 0) cols = 1;
    if (rows_count == 0) rows_count = 1;

    // Pass absolute row (scrollback + viewport row) so placements
    // survive viewport scrolling.  Elisp converts back to viewport
    // coordinates during apply-placements.
    const abs_row: i64 = @as(i64, @intCast(term.getScrollbackRows())) +
        @as(i64, cursor_y);

    const data_val = env.makeUnibyteString(img.data) orelse return;
    var args = [_]emacs.Value{
        env.makeInteger(image_id),
        env.makeInteger(abs_row),
        env.makeInteger(cursor_x),
        env.makeInteger(cols),
        env.makeInteger(rows_count),
        data_val,
        if (img.is_png) env.t() else env.nil(),
    };
    _ = env.funcall(emacs.sym.@"ghostel--kitty-graphics-place", &args);
}

fn handleDelete(env: emacs.Env, store: *ImageStore, params: *const Params) void {
    switch (params.delete_what) {
        'a', 'A' => {
            store.deleteAll();
            _ = env.call1(
                emacs.sym.@"ghostel--kitty-graphics-delete",
                env.makeString("a"),
            );
        },
        'i', 'I' => {
            store.removeImage(params.image_id);
            _ = env.call2(
                emacs.sym.@"ghostel--kitty-graphics-delete",
                env.makeString("i"),
                env.makeInteger(params.image_id),
            );
        },
        else => {
            // Other delete types (by position, etc.) — treat as delete-all for now
            store.deleteAll();
            _ = env.call1(
                emacs.sym.@"ghostel--kitty-graphics-delete",
                env.makeString("a"),
            );
        },
    }
}

// ---- Control param parsing ----

fn parseControlParams(ctrl: []const u8) Params {
    var params = Params{};
    var start: usize = 0;

    for (ctrl, 0..) |byte, i| {
        if (byte == ',' or i == ctrl.len - 1) {
            const end = if (byte == ',') i else i + 1;
            const kv = ctrl[start..end];
            if (kv.len >= 3 and kv[1] == '=') {
                applyParam(&params, kv[0], kv[2..]);
            }
            start = i + 1;
        }
    }

    return params;
}

fn applyParam(p: *Params, key: u8, val: []const u8) void {
    switch (key) {
        'a' => if (val.len == 1) {
            p.action = switch (val[0]) {
                't' => .transmit,
                'T' => .transmit_and_display,
                'p' => .display,
                'd' => .delete,
                'q' => .query,
                else => .transmit,
            };
        },
        'i' => p.image_id = u32p(val),
        'p' => p.placement_id = u32p(val),
        'f' => p.format = switch (u32p(val)) {
            24 => .rgb,
            32 => .rgba,
            100 => .png,
            else => .rgba,
        },
        'o' => if (val.len == 1 and val[0] == 'z') {
            p.compression = .zlib;
        },
        'm' => p.more_chunks = u32p(val) == 1,
        's' => p.width = u32p(val),
        'v' => p.height = u32p(val),
        'c' => p.columns = u32p(val),
        'r' => p.rows = u32p(val),
        'z' => p.z_index = std.fmt.parseInt(i32, val, 10) catch 0,
        'd' => if (val.len > 0) {
            p.delete_what = val[0];
        },
        else => {},
    }
}

fn u32p(val: []const u8) u32 {
    return std.fmt.parseInt(u32, val, 10) catch 0;
}

// ---- Base64 decoding ----

fn decodeBase64(encoded: []const u8) ?[]u8 {
    if (encoded.len == 0) return null;

    // Strip whitespace (protocol allows newlines in base64)
    const clean = alloc.alloc(u8, encoded.len) catch return null;
    var clean_len: usize = 0;
    for (encoded) |byte| {
        if (byte != '\n' and byte != '\r' and byte != ' ' and byte != '\t') {
            clean[clean_len] = byte;
            clean_len += 1;
        }
    }
    defer alloc.free(clean);

    if (clean_len == 0) return null;

    const decoder = std.base64.standard.Decoder;
    const decoded_len = decoder.calcSizeForSlice(clean[0..clean_len]) catch return null;
    const buf = alloc.alloc(u8, decoded_len) catch return null;
    decoder.decode(buf, clean[0..clean_len]) catch {
        alloc.free(buf);
        return null;
    };
    return buf;
}

// ---- Image format conversion ----

const FormatResult = struct { data: []u8, is_png: bool };

fn toEmacsFormat(data: []const u8, format: Format, width: u32, height: u32) ?FormatResult {
    if (data.len == 0) return null;

    switch (format) {
        .png => {
            // PNG — copy and pass through
            const copy = alloc.alloc(u8, data.len) catch return null;
            @memcpy(copy, data);
            return .{ .data = copy, .is_png = true };
        },
        .rgb => {
            return createPpm(data, width, height, 3);
        },
        .rgba => {
            return createPpm(data, width, height, 4);
        },
    }
}

/// Extract width and height from a PNG file's IHDR chunk.
/// PNG layout: 8-byte signature, then IHDR chunk with width at offset 16
/// and height at offset 20, both as big-endian u32.
fn extractPngDimensions(data: []const u8, width: *u32, height: *u32) void {
    if (data.len < 24) return;
    // Verify PNG signature
    const png_sig = [_]u8{ 0x89, 'P', 'N', 'G', 0x0d, 0x0a, 0x1a, 0x0a };
    if (!std.mem.eql(u8, data[0..8], &png_sig)) return;
    width.* = std.mem.readInt(u32, data[16..20], .big);
    height.* = std.mem.readInt(u32, data[20..24], .big);
}

/// Create a PPM (P6) image from raw pixel data.
/// For RGBA (channels=4), alpha is stripped.
fn createPpm(data: []const u8, width: u32, height: u32, channels: u32) ?FormatResult {
    if (width == 0 or height == 0) return null;

    const pixel_count = width * height;
    const expected_bytes = pixel_count * channels;
    if (data.len < expected_bytes) return null;

    // PPM header: "P6\n<w> <h>\n255\n"
    var hdr_buf: [64]u8 = undefined;
    const header = std.fmt.bufPrint(&hdr_buf, "P6\n{d} {d}\n255\n", .{ width, height }) catch return null;

    const rgb_bytes = pixel_count * 3;
    const total = header.len + rgb_bytes;
    const buf = alloc.alloc(u8, total) catch return null;

    @memcpy(buf[0..header.len], header);

    if (channels == 3) {
        @memcpy(buf[header.len..][0..rgb_bytes], data[0..rgb_bytes]);
    } else {
        // Strip alpha channel
        var out: usize = header.len;
        var in: usize = 0;
        for (0..pixel_count) |_| {
            buf[out] = data[in];
            buf[out + 1] = data[in + 1];
            buf[out + 2] = data[in + 2];
            out += 3;
            in += 4;
        }
    }

    return .{ .data = buf, .is_png = false };
}
