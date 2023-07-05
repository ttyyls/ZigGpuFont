const std = @import("std");
const print = std.debug.print;
const io = std.io;
const mem = std.mem;
const os = std.os;
const SingleArrayList = std.ArrayList;
const Map = std.AutoHashMap;
const builtin = @import("builtin");

const native_endian = builtin.target.cpu.arch.endian();
pub fn dump_offset(comptime S: type, ptr: *S, abs_offset: u32) void {
    if (@typeInfo(S) != .Struct) @compileError("dump_ofsets expects a struct as the first argument");
    inline for (std.meta.fields(S)) |f| {
        const offset = @intFromPtr(&@field(ptr, f.name)) - @intFromPtr(ptr);
        const abs = offset + abs_offset;
        print("{s} = relative: {}, abs: 0x{x}\n", .{ f.name, offset, abs });
    }
}

pub const OutlineFlags = enum(u5) {
    OnCurve = 1,
    XIsByte = 2,
    YIsByte = 4,
    Repeat = 8,
    XDelta = 16,
    YDelta = 32,
};

pub const ComponentFlags = enum(u12) {
    Arg1And2AreWords = 0x0001,
    ArgsAreXyValues = 0x0002,
    RoundXyToGrid = 0x0004,
    WeHaveAScale = 0x0008,

    Reserved = 0xE010,
    MoreComponents = 0x0020,
    WeHaveAnXAndYScale = 0x0040,
    WeHaveATwoByTwo = 0x0080,
    WeHaveInstructions = 0x0100,
    UseMyMetrics = 0x0200,
    OverlapComponent = 0x0400,
    ScaledComponentOffset = 0x0800,
    UnscaledComponentOffset = 0x1000,
};

pub const Cmap = extern struct {
    format: u16 align(1),
    length: u16 align(1),
    language: u16 align(1),
    segCountX2: u16 align(1),
    searchRange: u16 align(1),
    entrySelector: u16 align(1),
    rangeShift: u16 align(1),
};

pub const CmapIndex = extern struct {
    Version: u16 align(1),
    NumberSubtables: u16 align(1),
};

pub const CmapEncoding = extern struct {
    platformID: u16 align(1),
    plaformSpecificID: u16 align(1),
    offset: u32 align(1),
};

pub const TrueTypeHeader = extern struct {
    version: u32 align(1),
    FontRevision: u32 align(1),
    CheckSumAdjustment: u32 align(1),
    MagicNumber: u32 align(1),
    Flags: u16 align(1),
    UnitsPerEm: u16 align(1),
    Created: u64 align(1),
    Modified: u64 align(1),
    Xmin: i16 align(1),
    Ymin: i16 align(1),
    Xmax: i16 align(1),
    Ymax: i16 align(1),
    MacStyle: u16 align(1),
    LowestRecPPEM: u16 align(1),
    FontDirectionHint: i16 align(1),
    IndexToLocFormat: i16 align(1),
    GlyphDataFromat: i16 align(1),
};

pub const OffsetTable = extern struct {
    ScalerType: u32 align(1),
    NumTables: u16 align(1),
    SearchRange: u16 align(1),
    EntrySelector: u16 align(1),
    RangeShift: u16 align(1),
};

pub const TableEntry = extern struct {
    id: u32 align(1),
    CheckSum: u32 align(1),
    Offset: u32 align(1),
    Length: u32 align(1),

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
    version: u32 align(1),
    vertTypoAscender: i16 align(1),
    vertTypoDescender: i16 align(1),
    vertTypoLineGap: i16 align(1),
    advanceHeightMax: i16 align(1),
    minTopSideBearing: i16 align(1),
    minBottomSideBearing: i16 align(1),
    yMaxExtent: i16 align(1),
    caretSlopeRise: i16 align(1),
    caretSlopeRun: i16 align(1),
    caretOffset: i16 align(1),
    reserved: i16 align(1),
    reserved1: i16 align(1),
    reserved2: i16 align(1),
    reserved4: i16 align(1),
    metricDataFormat: u16 align(1),
    numOfLongVerMetrics: u16 align(1),
};

pub const HorizontalHeaderTable = extern struct {
    version: u32 align(1),
    ascent: i16 align(1),
    descent: i16 align(1),
    lineGap: i16 align(1),
    advanceWidthMax: u16 align(1),
    minLeftSideBearing: i16 align(1),
    minRightSideBearing: i16 align(1),
    xMaxExtent: i16 align(1),
    caretSlopeRise: i16 align(1),
    caretSlopeRun: i16 align(1),
    caretOffset: i16 align(1),
    reserved: i16 align(1),
    reserved1: i16 align(1),
    reserved2: i16 align(1),
    reserved4: i16 align(1),
    metricDataFormat: i16 align(1),
    numOfLongHorMetrics: u16 align(1),
};

pub const longHorMetric = extern struct {
    advanceWidth: u16 align(1),
    leftSideBearing: i16 align(1),
};

pub const MaxP = extern struct {
    version: u32 align(1),
    numGlyphs: u16 align(1),
    maxPoints: u16 align(1),
    maxContours: u16 align(1),
    maxComponentPoints: u16 align(1),
    maxComponentContours: u16 align(1),
    maxZones: u16 align(1),
    maxTwilightPoints: u16 align(1),
    maxStorage: u16 align(1),
    maxFunctionDefs: u16 align(1),
    maxInstructionDefs: u16 align(1),
    maxStackElements: u16 align(1),
    maxSizeOfInstructions: u16 align(1),
    maxComponentElements: u16 align(1),
    maxComponentDepth: u16 align(1),
};

pub const GlyphDescription = extern struct {
    numberOfContours: i16 align(1),
    xMin: i16 align(1),
    yMin: i16 align(1),
    xMax: i16 align(1),
    yMax: i16 align(1),
};

pub const GlyfPoint = struct {
    X: f32,
    Y: f32,
    isMidpoint: bool,
    isOnCurve: bool,
};

pub const ComponentGlyph = struct { Flags: ComponentFlags };

pub const ComponentTriangle = struct {
    A: GlyfPoint,
    B: GlyfPoint,
    C: GlyfPoint,
};

pub const Glyf = struct {
    numberOfCountours: i16,
    xMin: i16,
    yMin: i16,
    xMax: i16,
    yMax: i16,

    Shapes: SingleArrayList(GlyfPoint) = undefined,
    Triangles: SingleArrayList(ComponentTriangle) = undefined,

    CountourEnds: SingleArrayList(u16) = undefined,
    Points: SingleArrayList(GlyfPoint) = undefined,
    Curves: SingleArrayList(bool) = undefined,
    Components: SingleArrayList(ComponentGlyph) = undefined,
};

pub const TrueTypeFontFile = struct {
    const Self = @This();

    header: TrueTypeHeader = undefined,
    horizentalHeaderTable: HorizontalHeaderTable = undefined,
    verticalHeaderTable: VerticalHeaderTable = undefined,
    longHorMetrics: SingleArrayList(longHorMetric) = undefined,
    maxP: MaxP = undefined,
    cMapIndexes: Map(u32, u32) = undefined,
    glyphs: Map(u32, Glyf) = undefined,
    glyphOffsetTe: TableEntry = undefined,
    glyphOffset: u32 = undefined,
    allocator: std.mem.Allocator = undefined,

    pub fn init(self: *Self, alloc: std.mem.Allocator) *Self {
        self.cMapIndexes = Map(u32, u32).init(alloc);
        self.longHorMetrics = SingleArrayList(longHorMetric).init(alloc);
        self.glyphs = Map(u32, Glyf).init(alloc);
        self.allocator = alloc;

        return self;
    }

    pub fn read_array(
        self: *Self,
        reader: std.fs.File.Reader,
        comptime T: type,
        length: usize,
    ) anyerror![]T {
        var arr: []T = try self.allocator.alloc(T, length);
        for (0..length) |x| {
            arr[x] = try reader.readIntBig(T);
        }
        return arr;
    }

    pub fn read_cmap(
        self: *Self,
        reader: std.fs.File.Reader,
        seeker: std.fs.File.SeekableStream,
    ) anyerror!void {
        var startPos = try seeker.getPos();
        var idx = try reader.readStructBig(CmapIndex);
        var subtablesStart = try seeker.getPos();
        for (0..idx.NumberSubtables) |i| {
            try seeker.seekTo(subtablesStart + (i * 8));
            var encoding = try reader.readStructBig(CmapEncoding);
            try seeker.seekTo(startPos + encoding.offset);
            var cmap = try reader.readStructBig(Cmap);

            if (encoding.platformID == 0 and cmap.format == 4) {
                var range = cmap.searchRange;
                var segcount = @as(u32, cmap.segCountX2) / 2;

                var endCode = try self.read_array(reader, u16, segcount);
                try seeker.seekTo(try seeker.getPos() + 2);
                var startCode = try self.read_array(reader, u16, segcount);
                var idDelta = try self.read_array(reader, u16, segcount);
                var idRangeOffset = try self.read_array(reader, u16, segcount * @as(u32, range));

                // HACK: support all of utf8 later
                for (0..255) |charCode| {
                    const cc: u16 = @truncate(charCode);
                    var found: bool = false;
                    for (0..segcount) |segIdx| {
                        if (endCode[segIdx] >= cc and startCode[segIdx] <= cc) {
                            if (idRangeOffset[segIdx] != 0) {
                                var z = idRangeOffset[segIdx + idRangeOffset[segIdx] / 2 + (charCode - startCode[segIdx])];
                                var delta = idDelta[segIdx];
                                try self.cMapIndexes.put(@truncate(charCode), z + delta);
                            } else {
                                try self.cMapIndexes.put(@truncate(charCode), idDelta[segIdx] + @as(u32, @truncate(charCode)));
                            }
                            found = true;
                        }
                    }
                    if (!found) {
                        try self.cMapIndexes.put(@truncate(charCode), 0);
                    }
                }

                //var iter = self.cMapIndexes.iterator();
                //while (iter.next()) |en| {
                //   print("key: {d} val: {d}\n", .{ en.key_ptr.*, en.value_ptr.* });
                // }
                return;
            }
        }
    }

    pub fn load_file(self: *Self, file_path: []const u8) !void {
        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        const reader = file.seekableStream();
        try self.load_bytes(file.reader(), reader);
    }

    pub fn load_bytes(
        self: *Self,
        reader: std.fs.File.Reader,
        seeker: std.fs.File.SeekableStream,
    ) !void {
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
            try seeker.seekTo(te.Offset);
            if (te.isId("head")) {
                self.header = try reader.readStructBig(TrueTypeHeader);
            } else if (te.isId("hhea")) {
                self.horizentalHeaderTable = try reader.readStructBig(HorizontalHeaderTable);
            } else if (te.isId("vhea")) {
                self.verticalHeaderTable = try reader.readStruct(VerticalHeaderTable);
            } else if (te.isId("hmtx")) {
                for (0..self.horizentalHeaderTable.numOfLongHorMetrics) |_| {
                    try self.longHorMetrics.append(try reader.readStructBig(longHorMetric));
                }
            } else if (te.isId("maxp")) {
                self.maxP = try reader.readStructBig(MaxP);
            } else if (te.isId("cmap")) {
                try self.read_cmap(reader, seeker);
            } else if (te.isId("loca")) {
                self.glyphOffsetTe = te;
            } else if (te.isId("glyf")) {
                self.glyphOffset = te.Offset;
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
