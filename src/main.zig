const std = @import("std");
const print = std.debug.print;
const SingleArrayList = std.ArrayList;
const Map = std.AutoHashMap;

pub const TrueTypeHeader = extern struct {
    Version: u32,
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

        if (@import("builtin").target.cpu.arch.endian() == .Big) {
            return [_]u8{ @truncate(a), @truncate(b), @truncate(c), @truncate(d) };
        } else {
            return [_]u8{ @truncate(d), @truncate(c), @truncate(b), @truncate(a) };
        }
    }
};

pub const HorizentalHeaderTable = struct {};
pub const VerticalHeaderTable = struct {};
pub const longHorMetrics = struct {};
pub const longHorMetric = struct {};
pub const MaxP = struct {};
pub const Glyf = struct {};

pub const TrueTypeFontFile = struct {
    const Self = @This();

    header: *TrueTypeHeader = undefined,
    horizentalHeaderTable: *HorizentalHeaderTable = undefined,
    longHorMetrics: *SingleArrayList(longHorMetric) = undefined,
    maxP: *MaxP = undefined,
    cMapIndexes: Map(u32, u32) = undefined,
    glyfs: Map(u32, Glyf) = undefined,

    allocator: std.mem.Allocator = undefined,

    pub fn init(self: *Self, alloc: std.mem.Allocator) *Self {
        self.cMapIndexes = Map(u32, u32).init(alloc);
        self.glyfs = Map(u32, Glyf).init(alloc);
        self.allocator = alloc;

        return self;
    }

    pub fn load_file(self: *Self, file_path: []const u8) !void {
        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        const reader = file.seekableStream();
        try self.load_bytes(file.reader(), reader);
    }

    pub fn load_bytes(self: *Self, reader: std.fs.File.Reader, seeker: std.fs.File.SeekableStream) !void {
        var off: OffsetTable = try reader.readStructBig(OffsetTable);

        const glyfOffsets = SingleArrayList(u32).init(self.allocator);
        const glyfOffset = 0;

        _ = glyfOffsets;
        _ = glyfOffset;

        var i: u32 = 0;

        print("{d}\n", .{off.NumTables});
        while (i < off.NumTables) {
            try seeker.seekTo(12 + (@sizeOf(TableEntry)) * i);
            var te = try reader.readStructBig(TableEntry);

            //print("{}\n", .{te});
            print("{s}\n", .{te.getId()});
            i += 1;
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var fontFile = TrueTypeFontFile{};
    _ = fontFile.init(alloc);
    try fontFile.load_file("Hack-Regular.ttf");
}
