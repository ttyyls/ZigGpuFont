const std = @import("std");
const tessy = @cImport({
    @cInclude("tesselator.h");
});
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

pub fn read_array(
    alloc: std.mem.Allocator,
    reader: std.fs.File.Reader,
    comptime T: type,
    length: usize,
) anyerror![]T {
    var arr: []T = try alloc.alloc(T, length);
    for (0..length) |x| {
        arr[x] = try reader.readIntBig(T);
    }
    return arr;
}

pub fn GetGlyphOffset(
    te: TableEntry,
    header: TrueTypeHeader,
    seeker: std.fs.File.SeekableStream,
    reader: std.fs.File.Reader,
    index: u32,
) anyerror!u32 {
    if (header.IndexToLocFormat == 1) {
        const off = te.Offset + index * 4;
        try seeker.seekTo(off);
        const x = reader.readIntBig(u32);
        return x;
    }

    try seeker.seekTo(te.Offset + index * 2);
    return @as(u32, @intCast(try reader.readIntBig(u16) * 2));
}

pub const OutlineFlags = enum(u8) {
    OnCurve = 1,
    XIsByte = 2,
    YIsByte = 4,
    Repeat = 8,
    XDelta = 16,
    YDelta = 32,
};

pub fn isValidOutlineFlag(num: u8) bool {
    if ((num > 0) and ((num == 1) or ((num % 2 == 0) and (num <= 32)))) {
        return true;
    }
    return false;
}

pub const ComponentFlags = enum(u16) {
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
    isMidpoint: bool = false,
    isOnCurve: bool = false,
};

pub fn MidpointRounding(a: GlyfPoint, b: GlyfPoint) GlyfPoint {
    return GlyfPoint{ .X = ((a.X + b.X) / 2.0), .Y = ((a.X + b.X) / 2.0) };
}

pub fn Bezier(p0: f32, p1: f32, p2: f32, t: f32) f32 {
    return p1 + std.math.pow(f32, 1.0 - 2.0, 2.0) * (p0 - p1) + std.math.pow(f32, t, 2.0) * (p2 - p1);
}

pub const ComponentGlyph = struct { Flags: ComponentFlags };

pub const ComponentTriangle = struct {
    A: GlyfPoint,
    B: GlyfPoint,
    C: GlyfPoint,
};

pub const Glyf = struct {
    xMin: i16,
    yMin: i16,
    xMax: i16,
    yMax: i16,
    Triangles: SingleArrayList(ComponentTriangle) = undefined,
    Shapes: SingleArrayList(SingleArrayList(GlyfPoint)) = undefined,
    numberOfCountours: i16,
    CountourEnds: SingleArrayList(u16) = undefined,
    Points: SingleArrayList(GlyfPoint) = undefined,
    Curves: SingleArrayList(bool) = undefined,
    Components: SingleArrayList(ComponentGlyph) = undefined,

    pub fn readGlyph(
        allocator: std.mem.Allocator,
        seeker: std.fs.File.SeekableStream,
        reader: std.fs.File.Reader,
        byte: u8,
    ) anyerror!Glyf {
        _ = byte;
        var re: Glyf = undefined;
        var gd: GlyphDescription = try reader.readStructBig(GlyphDescription);

        re.numberOfCountours = gd.numberOfContours;
        re.CountourEnds = SingleArrayList(u16).init(allocator);
        re.xMin = gd.xMin;
        re.xMax = gd.xMax;
        re.yMin = gd.yMin;
        re.yMax = gd.yMax;
        re.Points = SingleArrayList(GlyfPoint).init(allocator);
        re.Curves = SingleArrayList(bool).init(allocator);
        re.Shapes = SingleArrayList(SingleArrayList(GlyfPoint)).init(allocator);

        var tmpXPoints = SingleArrayList(i32).init(allocator);
        var tmpYPoints = SingleArrayList(i32).init(allocator);
        var lst = SingleArrayList(bool).init(allocator);
        var flags = SingleArrayList(u8).init(allocator);
        var max: u32 = 0;

        if (gd.numberOfContours >= 0) {
            var endPtsOfContous = try read_array(
                allocator,
                reader,
                u16,
                @intCast(gd.numberOfContours),
            );
            for (0..@intCast(gd.numberOfContours)) |i| {
                try re.CountourEnds.append(endPtsOfContous[i]);
            }

            for (endPtsOfContous) |val| {
                max = @max(max, val);
            }
            max += 1;

            var instructionLength = (try read_array(allocator, reader, u16, 1))[0];
            _ = try read_array(allocator, reader, u8, instructionLength);

            var flagRes = try seeker.getPos();
            var tmpflags = try read_array(allocator, reader, u8, max * 2);

            var off: u32 = 0;
            var p: u32 = 0;
            while (p < max) {
                var f: u8 = tmpflags[off];
                off += 1;

                try flags.append(f);

                try lst.append((f & @intFromEnum(OutlineFlags.OnCurve)) == 1);

                if ((f & @intFromEnum(OutlineFlags.Repeat)) == 1) {
                    var z = tmpflags[off];
                    off += 1;
                    p += 1;

                    for (0..z) |_| {
                        try flags.append(f);
                        try lst.append((f & @intFromEnum(OutlineFlags.OnCurve)) == 1);
                    }
                }

                p += 1;
            }
            var xoff: usize = 0;

            try seeker.seekTo(flagRes + off);
            var xPoints = try read_array(allocator, reader, u8, max * 2);
            {
                // NOTE: this used to be &xPoints
                var arr = xPoints;
                var byteFlag = OutlineFlags.XIsByte;
                var deltaFlag = OutlineFlags.XDelta;

                xoff = 0;
                var xVal: i32 = 0;

                for (0..max) |i| {
                    if (i >= flags.items.len)
                        break;
                    var flag = flags.items[i];

                    //print("{d}\n", .{@intFromEnum(byteFlag)});
                    if (flag & @intFromEnum(byteFlag) != 0) {
                        if ((flag & @intFromEnum(deltaFlag)) != 0) {
                            xVal += arr[xoff];
                            xoff += 1;
                        } else {
                            xVal -= arr[xoff];
                            xoff += 1;
                        }
                        // zig fmt: off
                    } else if (
                        (flag & @intFromEnum(deltaFlag)) == 0 and (flag & @intFromEnum(byteFlag)) == 0) {
                        // zig fmt: on
                        const a = arr[xoff];
                        xoff += 1;
                        const b = arr[xoff];
                        xoff += 1;
                        // print("a: {}, b: {}", .{ @TypeOf(a), @TypeOf(b) });
                        xVal += @as(i16, a) << 8 | b;
                    }
                    try tmpXPoints.append(xVal);
                }
            }
            try seeker.seekTo(flagRes + off + @as(u64, @intCast(xoff)));
            var yPoints = try read_array(allocator, reader, u8, max * 2);
            {
                var arr = yPoints;
                var byteFlag = OutlineFlags.YIsByte;
                var deltaFlag = OutlineFlags.YDelta;

                xoff = 0;
                var xVal: i32 = 0;

                for (0..max) |i| {
                    if (i >= flags.items.len)
                        break;
                    var flag = flags.items[i];

                    if (flag & @intFromEnum(byteFlag) != 0) {
                        if ((flag & @intFromEnum(deltaFlag)) != 0) {
                            xVal += arr[xoff];
                            xoff += 1;
                        } else {
                            xVal -= arr[xoff];
                            xoff += 1;
                        }
                        // zig fmt: off
                    } else if (!((flag & @intFromEnum(deltaFlag)) != 0) 
                        and !((flag & @intFromEnum(byteFlag)) != 0)) {
                        // zig fmt: on
                        const a = arr[xoff];
                        xoff += 1;
                        const b = arr[xoff];
                        xoff += 1;
                        // print("a: {}, b: {}", .{ @TypeOf(a), @TypeOf(b) });
                        xVal += @as(i16, a) << 8 | b;
                    }
                    try tmpYPoints.append(xVal);
                }
            }

            try re.Points.append(GlyfPoint{
                .X = @floatFromInt(tmpXPoints.items[0]),
                .Y = @floatFromInt(tmpYPoints.items[0]),
            });
            try re.Curves.append(lst.items[0]);
            for (1..max) |i| {
                try re.Points.append(GlyfPoint{
                    .X = @floatFromInt(tmpXPoints.items[i]),
                    .Y = @floatFromInt(tmpYPoints.items[i]),
                    .isOnCurve = lst.items[i],
                });
                try re.Curves.append(lst.items[i]);
            }

            var points = SingleArrayList(GlyfPoint).init(allocator);
            for (0..re.Points.items.len) |i| {
                try points.append(re.Points.items[i]);

                // HACK: this is nonsense
                //if (re.ContourEnds.Contains((ushort) i))

                for (re.CountourEnds.items) |item| {
                    if (item == @as(u16, @intCast(i))) {
                        try re.Shapes.append(points);
                        points = SingleArrayList(GlyfPoint).init(allocator);
                    }
                }
            }

            // print("\n{d}\n", .{re.Points.items.len});
            // for (re.Points.items) |Contour| {
            //     print("Point: .X = {d}, .Y = {d}\n", .{ Contour.X, Contour.Y });
            // }

            for (0..re.Shapes.items.len) |BigIndex| {
                var shape = re.Shapes.items[BigIndex];
                var i: u32 = 1;
                while (i < shape.items.len) {
                    var a = shape.items[i];
                    var b = shape.items[i - 1];
                    if (!a.isOnCurve and !b.isOnCurve) {
                        var midPoint = MidpointRounding(a, b);
                        midPoint.isMidpoint = true;
                        try shape.insert(i, midPoint);
                        i += 1;
                    }
                    i += 1;
                }
            }

            // for (re.Shapes.items) |shape| {
            //     var shapes = shape.clone();
            //     shape.items.len = 0;
            //     shape.append(shapes[0]);
            //     for (1..shapes.len) |i| {
            //         if (!shapes[i].IsOnCurve and !shapes[i].isMidPoint) {
            //             var res: f32 = 15;
            //             var a = if (i == 0) shapes[shapes.len - 1] else shapes[i - 1];
            //             var b = shapes[i];
            //             var c = if ((i + 1) >= shapes.len) shapes[0] else shapes[i + 1];
            //
            //             for (0..res) |j| {
            //                 var t: u32 = j / res;
            //                 shape.append(GlyfPoint{
            //                     .X = Bezier(a.X, b.X, c.X, t),
            //                     .Y = Bezier(a.Y, b.Y, c.Y, t),
            //                 });
            //             }
            //         } else {
            //             shape.append(shapes[i]);
            //         }
            //     }
            // }
            // NOTE: come back here daddy
        }

        return re;
    }
};

pub const TrueTypeFontFile = struct {
    const Self = @This();

    header: TrueTypeHeader = undefined,
    horizentalHeaderTable: HorizontalHeaderTable = undefined,
    verticalHeaderTable: VerticalHeaderTable = undefined,
    longHorMetrics: SingleArrayList(longHorMetric) = undefined,
    maxP: MaxP = undefined,
    cMapIndexes: Map(u32, u32) = undefined,
    glyfs: Map(u32, Glyf) = undefined,
    glyphOffsetTe: TableEntry = undefined,
    glyphOffset: u32 = undefined,
    allocator: std.mem.Allocator = undefined,

    pub fn init(self: *Self, alloc: std.mem.Allocator) *Self {
        self.cMapIndexes = Map(u32, u32).init(alloc);
        self.longHorMetrics = SingleArrayList(longHorMetric).init(alloc);
        self.glyfs = Map(u32, Glyf).init(alloc);
        self.allocator = alloc;

        return self;
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

                var endCode = try read_array(self.allocator, reader, u16, segcount);
                try seeker.seekTo(try seeker.getPos() + 2);
                var startCode = try read_array(self.allocator, reader, u16, segcount);
                var idDelta = try read_array(self.allocator, reader, i16, segcount);
                var idRangeOffset = try read_array(self.allocator, reader, i16, segcount * @as(u32, range));

                // HACK: support all of utf8 later
                for (0..255) |charCode| {
                    const cc: u16 = @truncate(charCode);

                    var found: bool = false;
                    for (0..segcount) |segIdx| {
                        if (endCode[segIdx] >= cc and startCode[segIdx] <= cc) {
                            if (idRangeOffset[segIdx] != 0) {
                                var z = idRangeOffset[segIdx + @as(usize, @intCast(idRangeOffset[segIdx])) / 2 + (charCode - startCode[segIdx])];
                                var delta = idDelta[segIdx];
                                try self.cMapIndexes.put(@truncate(charCode), @intCast(z + delta));
                            } else {
                                const a: i16 = idDelta[segIdx];
                                const b: i32 = @intCast(charCode);
                                const pp = a + b;
                                try self.cMapIndexes.put(@truncate(charCode), @intCast(pp));
                            }
                            found = true;
                        }
                    }

                    if (!found) {
                        try self.cMapIndexes.put(@truncate(charCode), 0);
                    }
                }
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

        var i: u32 = 0;

        //print("{d}\n", .{off.NumTables});
        while (i < off.NumTables) {
            var te = try reader.readStructBig(TableEntry);
            // print("Reading table at offset 0x{x}\n", .{te.Offset});
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

        //var iterator = self.cMapIndexes.iterator();

        // while (iterator.next()) |entry| {
        //     print("K:{} v:{}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        // }

        // HACK: so many intcasts instead of just specifying that charcode is a u8 ?
        for (0..255) |charCode| {
            var maped: u32 = self.cMapIndexes.getEntry(@intCast(charCode)).?.value_ptr.*;
            const a = self.glyphOffset;
            const b = try GetGlyphOffset(
                self.glyphOffsetTe,
                self.header,
                seeker,
                reader,
                maped,
            );
            try seeker.seekTo(a + b);
            //print("self.glyphOffset: {}, GetGlyphOffset: {}, charCode: {}, seeker: {d}\n", .{ self.glyphOffset, b, charCode, a + b });
            try self.glyfs.put(@intCast(charCode), try Glyf.readGlyph(
                self.allocator,
                seeker,
                reader,
                @intCast(charCode),
            ));
        }

        // FIXME: iterator on hashmaps?
        // var iterator = self.glyfs.iterator();
        // for (iterator.next()) |en| {
        //     if (en.value_ptr.Components.count() != 0) {}
        // }
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

    std.debug.print("pre call of libtess2 \n", .{});

    const tess = tessy.tessNewTess(null);
    if (tess == null)
        print("Death\n", .{});

    tessy.tessSetOption(tess, tessy.TESS_CONSTRAINED_DELAUNAY_TRIANGULATION, 1);

    std.debug.print("post call of libtess2\n", .{});
}
