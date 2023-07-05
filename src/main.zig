const std = @import("std");
const print = std.debug.print;
const io = std.io;
const mem = std.mem;
const os = std.os;
const SingleArrayList = std.ArrayList;
const Map = std.AutoHashMap;
const builtin = @import("builtin");

const native_endian = builtin.target.cpu.arch.endian();

pub const TrueTypeHeader = extern struct {
    version: u32,
    FontRevision: u32,
    CheckSumAdjustment: u32,
    MagicNumber: u32,
    Flags: u16,
    UnitsPerEm: u16,
    Created: u64,
    Modified: u64,
    Xmin: i16,
    Ymin: i16,
    Xmax: i16,
    Ymax: i16,
    MacStyle: u16,
    LowestRecPPEM: u16,
    FontDirectionHint: i16,
    IndexToLocFormat: i16,
    GlyphDataFromat: i16,
};

pub const OffsetTable = extern struct {
    ScalerType: u32,
    NumTables: u16,
    SearchRange: u16,
    EntrySelector: u16,
    RangeShift: u16,
};

pub const TableEntry = extern struct {
    id: u32,
    CheckSum: u32,
    Offset: u32,
    Length: u32,

    pub fn getId(self: *@This()) [4]u8 {
        var d = (self.id & 0xFF000000) >> 24;
        var c = (self.id & 0x00FF0000) >> 16;
        var b = (self.id & 0x0000FF00) >> 8;
        var a = (self.id & 0x000000FF);

        if (native_endian == .Big) {
            return [_]u8{ @truncate(a), @truncate(b), @truncate(c), @truncate(d) };
        } else {
            return [_]u8{ @truncate(d), @truncate(c), @truncate(b), @truncate(a) };
        }
    }

    pub fn isId(self: *@This(), name: []const u8) bool {
        const id = self.getId();
        for (0..4) |index| {
            if (id[index] != name[index]) {
                return false;
            }
        }
        return true;
    }
};

pub const VerticalHeaderTable = extern struct {
    version: u32,
    vertTypoAscender: i16,
    vertTypoDescender: i16,
    vertTypoLineGap: i16,
    advanceHeightMax: i16,
    minTopSideBearing: i16,
    minBottomSideBearing: i16,
    yMaxExtent: i16,
    caretSlopeRise: i16,
    caretSlopeRun: i16,
    caretOffset: i16,
    reserved: i16,
    reserved1: i16,
    reserved2: i16,
    reserved4: i16,
    metricDataFormat: u16,
    numOfLongVerMetrics: u16,
};

pub const HorizontalHeaderTable = extern struct {
    version: u32,
    ascent: i16,
    descent: i16,
    lineGap: i16,
    advanceWidthMax: u16,
    minLeftSideBearing: i16,
    minRightSideBearing: i16,
    xMaxExtent: i16,
    caretSlopeRise: i16,
    caretSlopeRun: i16,
    caretOffset: i16,
    reserved: i16,
    reserved1: i16,
    reserved2: i16,
    reserved4: i16,
    metricDataFormat: i16,
    numOfLongHorMetrics: u16,
};

pub const longHorMetric = extern struct {
    advanceWidth: u16,
    leftSideBearing: i16,
};

pub const MaxP = extern struct {
    version: u32,
    numGlyphs: u16,
    maxPoints: u16,
    maxContours: u16,
    maxComponentPoints: u16,
    maxComponentContours: u16,
    maxZones: u16,
    maxTwilightPoints: u16,
    maxStorage: u16,
    maxFunctionDefs: u16,
    maxInstructionDefs: u16,
    maxStackElements: u16,
    maxSizeOfInstructions: u16,
    maxComponentElements: u16,
    maxComponentDepth: u16,
};

pub const Glyph = extern struct {
    numberOfContours: i16,
    xMin: i16,
    yMin: i16,
    xMax: i16,
    yMax: i16,
};

pub const TrueTypeFontFile = struct {
    const Self = @This();

    header: *TrueTypeHeader = undefined,
    horizentalHeaderTable: *HorizontalHeaderTable = undefined,
    longHorMetrics: *SingleArrayList(longHorMetric) = undefined,
    maxP: *MaxP = undefined,
    cMapIndexes: Map(u32, u32) = undefined,
    glyphs: Map(u32, Glyph) = undefined,

    allocator: std.mem.Allocator = undefined,

    pub fn init(self: *Self, alloc: std.mem.Allocator) *Self {
        self.cMapIndexes = Map(u32, u32).init(alloc);
        self.glyphs = Map(u32, Glyph).init(alloc);
        self.allocator = alloc;

        return self;
    }

    pub fn load_file(self: *Self, file_path: []const u8) !void {
        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        const reader = file.seekableStream();
        try self.load_bytes(file.reader(), reader);
    }

    pub fn dump_offset(comptime S: type, ptr: *S, abs_offset: u32) void {
        if (@typeInfo(S) != .Struct) @compileError("dump_ofsets expects a struct as the first argument");
        inline for (std.meta.fields(S)) |f| {
            const offset = @intFromPtr(&@field(ptr, f.name)) - @intFromPtr(ptr);
            const abs = offset + abs_offset;
            print("{s} = relative: {}, abs: 0x{x}\n", .{ f.name, offset, abs });
        }
    }

    pub fn load_bytes(self: *Self, reader: std.fs.File.Reader, seeker: std.fs.File.SeekableStream) !void {
        var off: OffsetTable = try reader.readStructBig(OffsetTable);

        const glyfOffsets = SingleArrayList(u32).init(self.allocator);
        const glyfOffset = 0;
        _ = glyfOffset;
        _ = glyfOffsets;

        var i: u32 = 0;

        print("{d}\n", .{off.NumTables});
        while (i < off.NumTables) {
            var te = try reader.readStructBig(TableEntry);
            print("Reading table at offset 0x{x}\n", .{te.Offset});
            const pos = try seeker.getPos();
            try seeker.seekTo(te.Offset - 4);
            if (te.isId("head")) {
                print("Found Header\n", .{});
                print("Header offset: 0x{x}\n", .{te.Offset});
                var header = try reader.readStructBig(TrueTypeHeader);
                dump_offset(TrueTypeHeader, &header, te.Offset);
                print(
                    \\{{
                    \\  "Version": {d},
                    \\  "FontRevision": {d},
                    \\  "CheckSumAdjustment": {d},
                    \\  "MagicNumber": {d},
                    \\  "Flags": {d},
                    \\  "UnitsPerEm": {d},
                    \\  "Created": {d},
                    \\  "Modified": {d},
                    \\  "Xmin": {d},
                    \\  "Ymin": {d},
                    \\  "Xmax": {d},
                    \\  "Ymax": {d},
                    \\  "MacStyle": {d},
                    \\  "LowestRecPPEM": {d},
                    \\  "FontDirectionHint": {d},
                    \\  "IndexToLocFormat": {d},
                    \\  "GlyphDataFormat": {d}
                    \\}}
                , .{
                    header.version,
                    header.FontRevision,
                    header.CheckSumAdjustment,
                    header.MagicNumber,
                    header.Flags,
                    header.UnitsPerEm,
                    header.Created,
                    header.Modified,
                    header.Xmin,
                    header.Ymin,
                    header.Xmax,
                    header.Ymax,
                    header.MacStyle,
                    header.LowestRecPPEM,
                    header.FontDirectionHint,
                    header.IndexToLocFormat,
                    header.GlyphDataFromat,
                });
                std.os.exit(0);
            } else if (te.isId("vhea")) {
                print("Found Vertical Header\n", .{});
                var hheader = try reader.readStruct(VerticalHeaderTable);
                print("{}\n", .{hheader});
            } else if (te.isId("hmtx")) {
                print("Found Horizontal Metric\n", .{});
                var hheader = try reader.readStructBig(longHorMetric);
                print("{}\n", .{hheader});
            } else if (te.isId("maxp")) {
                print("Found Horizontal Header\n", .{});
                var hheader = try reader.readStructBig(HorizontalHeaderTable);
                print("{}\n", .{hheader});
            } else if (te.isId("cmap")) {
                print("Found Maximum point\n", .{});
                var maxp = try reader.readStructBig(MaxP);
                _ = maxp;
            }
            try seeker.seekTo(pos);
            i += 1;
        }
    }

    pub fn deinit(self: *Self) !void {
        _ = self;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var fontFile = TrueTypeFontFile{};
    _ = fontFile.init(alloc);
    try fontFile.load_file("Hack-Regular.ttf");
}
