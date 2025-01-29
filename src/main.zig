const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const ArenaAllocator = std.heap.ArenaAllocator;

pub const TomlValue = struct {
    data: Data,

    pub const Data = union(enum) {
        String: []const u8,
        Integer: i64,
        Float: f64,
        Boolean: bool,
        Array: []TomlValue,
        Table: StringHashMap(TomlValue),
        DateTime: []const u8,
    };
};

pub const TomlError = error{
    InvalidSyntax,
    InvalidValue,
    UnexpectedCharacter,
    UnexpectedEOF,
    OutOfMemory,
    Overflow,
    InvalidCharacter,
};

pub const Parser = struct {
    arena: *ArenaAllocator,
    source: []const u8,
    index: usize,
    currentTable: ?*StringHashMap(TomlValue),

    const Self = @This();

    pub fn init(arena: *ArenaAllocator, source: []const u8) Self {
        return .{
            .arena = arena,
            .source = source,
            .index = 0,
            .currentTable = null,
        };
    }

    pub fn parse(self: *Self) TomlError!StringHashMap(TomlValue) {
        var root = StringHashMap(TomlValue).init(self.arena.allocator());
        self.currentTable = null; // Start with no current table

        while (self.index < self.source.len) {
            self.skipWhitespace();
            if (self.index >= self.source.len) break;

            switch (self.source[self.index]) {
                '#' => self.skipComment(),
                '[' => try self.parseTableHeader(&root),
                else => {
                    if (self.currentTable) |table| {
                        try self.parseKeyValue(table);
                    } else {
                        try self.parseKeyValue(&root);
                    }
                },
            }
        }

        return root;
    }

    fn skipWhitespace(self: *Self) void {
        while (self.index < self.source.len) : (self.index += 1) {
            switch (self.source[self.index]) {
                ' ', '\t', '\r', '\n' => continue,
                else => break,
            }
        }
    }

    fn skipComment(self: *Self) void {
        while (self.index < self.source.len and self.source[self.index] != '\n') {
            self.index += 1;
        }
    }

    fn parseTableHeader(self: *Self, root: *StringHashMap(TomlValue)) !void {

        // Check if it's an array table
        const isArrayTable = self.index + 1 < self.source.len and self.source[self.index + 1] == '[';

        self.index += if (isArrayTable) 2 else 1; // Skip '[' or '[['
        self.skipWhitespace();

        var name = ArrayList(u8).init(self.arena.allocator());
        var inTableName = true;

        while (self.index < self.source.len and inTableName) {
            const c = self.source[self.index];
            switch (c) {
                ']' => {
                    if (isArrayTable) {
                        if (self.index + 1 >= self.source.len or self.source[self.index + 1] != ']') {
                            return TomlError.InvalidSyntax;
                        }
                        self.index += 2; // Skip ']]'
                    } else {
                        self.index += 1; // Skip single ']'
                    }
                    inTableName = false;
                },
                else => {
                    try name.append(c);
                    self.index += 1;
                },
            }
        }

        if (inTableName) {
            return TomlError.UnexpectedEOF;
        }

        // Trim trailing whitespace
        var tableName = try name.toOwnedSlice();
        while (tableName.len > 0 and (tableName[tableName.len - 1] == ' ' or tableName[tableName.len - 1] == '\t')) {
            tableName = tableName[0 .. tableName.len - 1];
        }

        if (isArrayTable) {
            const newTable = try self.arena.allocator().create(StringHashMap(TomlValue));
            newTable.* = StringHashMap(TomlValue).init(self.arena.allocator());

            if (root.get(tableName)) |existing| {
                switch (existing.data) {
                    .Array => |arr| {
                        var newArray = try self.arena.allocator().alloc(TomlValue, arr.len + 1);
                        @memcpy(newArray[0..arr.len], arr);
                        newArray[arr.len] = TomlValue{ .data = .{ .Table = newTable.* } };
                        try root.put(tableName, TomlValue{ .data = .{ .Array = newArray } });
                    },
                    else => return TomlError.InvalidSyntax, // Existing value is not an array
                }
            } else {
                var newArray = try self.arena.allocator().alloc(TomlValue, 1);
                newArray[0] = TomlValue{ .data = .{ .Table = newTable.* } };
                try root.put(tableName, TomlValue{ .data = .{ .Array = newArray } });
            }

            self.currentTable = newTable;
        } else {
            // Handle regular table
            const newTable = try self.arena.allocator().create(StringHashMap(TomlValue));
            newTable.* = StringHashMap(TomlValue).init(self.arena.allocator());
            try root.put(tableName, TomlValue{ .data = .{ .Table = newTable.* } });
            self.currentTable = newTable;
        }
    }

    fn parseKeyValue(self: *Self, table: *StringHashMap(TomlValue)) !void {
        self.skipWhitespace();
        if (self.index >= self.source.len) return;

        // Skip empty lines and comments
        if (self.source[self.index] == '\n' or self.source[self.index] == '#') {
            if (self.source[self.index] == '#') self.skipComment();
            self.index += 1;
            return;
        }

        var key = ArrayList(u8).init(self.arena.allocator());
        var foundEquals = false;

        // Parse key
        while (self.index < self.source.len) : (self.index += 1) {
            const c = self.source[self.index];
            switch (c) {
                '=' => {
                    foundEquals = true;
                    self.index += 1;
                    break;
                },
                ' ', '\t' => {
                    if (key.items.len > 0) {
                        // Look ahead for equals sign
                        var i = self.index;
                        while (i < self.source.len) : (i += 1) {
                            switch (self.source[i]) {
                                '=' => {
                                    foundEquals = true;
                                    self.index = i + 1;
                                    break;
                                },
                                ' ', '\t' => continue,
                                else => break,
                            }
                        }
                        if (foundEquals) break;
                    }
                },
                '\n', '\r' => break,
                else => try key.append(c),
            }
        }

        if (!foundEquals or key.items.len == 0) {
            return TomlError.InvalidSyntax;
        }

        self.skipWhitespace();

        const value = try self.parseValue();
        try table.put(try key.toOwnedSlice(), value);

        // Skip to end of line
        while (self.index < self.source.len) : (self.index += 1) {
            switch (self.source[self.index]) {
                '\n' => {
                    self.index += 1;
                    break;
                },
                '#' => {
                    self.skipComment();
                    break;
                },
                ' ', '\t', '\r' => continue,
                else => {},
            }
        }
    }

    fn parseValue(self: *Self) TomlError!TomlValue {
        self.skipWhitespace();
        if (self.index >= self.source.len) return TomlError.UnexpectedEOF;

        const c = self.source[self.index];
        return switch (c) {
            '"' => TomlValue{ .data = .{ .String = try self.parseString() } },
            't', 'f' => TomlValue{ .data = .{ .Boolean = try self.parseBoolean() } },
            '[' => try self.parseArray(),
            '{' => try self.parseInlineTable(),
            '0'...'9', '+', '-' => {
                const remaining = self.source[self.index..];
                if ((c == '-' or c == '+' or c == '1' or c == '2') and isLikelyDateTime(remaining)) {
                    return TomlValue{ .data = .{ .DateTime = try self.parseDateTime() } };
                }
                return try self.parseNumber();
            },
            '\n', '\r' => return TomlError.InvalidValue,
            '#' => {
                self.skipComment();
                return self.parseValue();
            },
            else => {
                std.debug.print("Unexpected character: {c} at index {d}\n", .{ c, self.index });
                return TomlError.InvalidValue;
            },
        };
    }

    fn parseString(self: *Self) ![]const u8 {
        self.index += 1; // Skip opening quote
        var str = ArrayList(u8).init(self.arena.allocator());

        while (self.index < self.source.len) : (self.index += 1) {
            const c = self.source[self.index];
            if (c == '"') {
                self.index += 1;
                return str.toOwnedSlice();
            } else if (c == '\\') {
                self.index += 1;
                if (self.index >= self.source.len) {
                    return TomlError.UnexpectedEOF;
                }
                const escaped = try getEscapedChar(self.source[self.index]);
                try str.append(escaped);
            } else {
                try str.append(c);
            }
        }

        return TomlError.UnexpectedEOF;
    }

    fn parseArray(self: *Self) !TomlValue {
        self.index += 1; // Skip '['
        var array = ArrayList(TomlValue).init(self.arena.allocator());

        while (self.index < self.source.len) {
            self.skipWhitespace();
            if (self.source[self.index] == ']') {
                self.index += 1;
                break;
            }

            const value = try self.parseValue();
            try array.append(value);

            self.skipWhitespace();
            if (self.source[self.index] == ',') {
                self.index += 1;
            }
        }

        return TomlValue{ .data = .{ .Array = try array.toOwnedSlice() } };
    }

    fn parseInlineTable(self: *Self) !TomlValue {
        self.index += 1; // Skip '{'
        var table = StringHashMap(TomlValue).init(self.arena.allocator());

        while (self.index < self.source.len) {
            self.skipWhitespace();
            if (self.source[self.index] == '}') {
                self.index += 1;
                break;
            }

            // Parse key
            var key = ArrayList(u8).init(self.arena.allocator());
            var foundEquals = false;

            while (self.index < self.source.len) : (self.index += 1) {
                const c = self.source[self.index];
                switch (c) {
                    '=' => {
                        foundEquals = true;
                        self.index += 1;
                        break;
                    },
                    ' ', '\t' => {
                        if (key.items.len > 0) {
                            // Look ahead for equals sign
                            var i = self.index;
                            while (i < self.source.len) : (i += 1) {
                                switch (self.source[i]) {
                                    '=' => {
                                        foundEquals = true;
                                        self.index = i + 1;
                                        break;
                                    },
                                    ' ', '\t' => continue,
                                    else => break,
                                }
                            }
                            if (foundEquals) break;
                        }
                    },
                    '}' => break,
                    ',' => break,
                    else => try key.append(c),
                }
            }

            if (!foundEquals or key.items.len == 0) {
                return TomlError.InvalidSyntax;
            }

            self.skipWhitespace();
            const value = try self.parseValue();
            try table.put(try key.toOwnedSlice(), value);

            self.skipWhitespace();
            if (self.index < self.source.len and self.source[self.index] == ',') {
                self.index += 1;
            }
        }

        return TomlValue{ .data = .{ .Table = table } };
    }

    fn parseDateTime(self: *Self) ![]const u8 {
        var str = ArrayList(u8).init(self.arena.allocator());

        while (self.index < self.source.len) : (self.index += 1) {
            const c = self.source[self.index];
            if (!isDateTimeChar(c)) break;
            try str.append(c);
        }

        return str.toOwnedSlice();
    }

    fn parseNumber(self: *Self) !TomlValue {
        var str = ArrayList(u8).init(self.arena.allocator());
        var has_decimal = false;

        while (self.index < self.source.len) : (self.index += 1) {
            switch (self.source[self.index]) {
                '0'...'9', '+', '-', 'e', 'E' => try str.append(self.source[self.index]),
                '.' => {
                    has_decimal = true;
                    try str.append(self.source[self.index]);
                },
                else => break,
            }
        }

        const num_str = try str.toOwnedSlice();
        return TomlValue{ .data = if (has_decimal)
            .{ .Float = try std.fmt.parseFloat(f64, num_str) }
        else
            .{ .Integer = try std.fmt.parseInt(i64, num_str, 10) } };
    }

    fn parseBoolean(self: *Self) !bool {
        if (self.index + 3 >= self.source.len) {
            return TomlError.UnexpectedEOF;
        }

        if (std.mem.eql(u8, self.source[self.index .. self.index + 4], "true")) {
            self.index += 4;
            return true;
        } else if (std.mem.eql(u8, self.source[self.index .. self.index + 5], "false")) {
            self.index += 5;
            return false;
        }

        return TomlError.InvalidValue;
    }
};

fn getEscapedChar(c: u8) !u8 {
    return switch (c) {
        'n' => '\n',
        't' => '\t',
        '\\' => '\\',
        '"' => '"',
        else => TomlError.InvalidSyntax,
    };
}

fn isDateTimeChar(c: u8) bool {
    return switch (c) {
        '0'...'9', '-', ':', 'T', 'Z', '+', '.' => true,
        else => false,
    };
}

fn isLikelyDateTime(source: []const u8) bool {
    if (source.len < 10) return false;
    return (std.mem.startsWith(u8, source, "19") or std.mem.startsWith(u8, source, "20")) and
        source[4] == '-';
}

pub fn getValue(root: StringHashMap(TomlValue), path: []const []const u8) ?TomlValue {
    if (path.len == 0) return null;

    var currentValue: ?TomlValue = null;

    // Get the first level value
    if (root.get(path[0])) |value| {
        currentValue = value;
    } else {
        return null;
    }

    // Navigate through the rest of the path
    var i: usize = 1;
    while (i < path.len) : (i += 1) {
        if (currentValue) |value| {
            switch (value.data) {
                .Table => |table| {
                    if (table.get(path[i])) |nextValue| {
                        currentValue = nextValue;
                    } else {
                        return null;
                    }
                },
                else => return null,
            }
        } else {
            return null;
        }
    }

    return currentValue;
}

pub fn printValue(value: TomlValue) void {
    switch (value.data) {
        .String => |str| std.debug.print("\"{s}\"", .{str}),
        .Integer => |int| std.debug.print("{d}", .{int}),
        .Float => |float| std.debug.print("{d}", .{float}),
        .Boolean => |boolean| std.debug.print("{}", .{boolean}),
        .DateTime => |dt| std.debug.print("{s}", .{dt}),
        .Array => |array| {
            std.debug.print("[", .{});
            for (array, 0..) |item, i| {
                printValue(item);
                if (i < array.len - 1) std.debug.print(", ", .{});
            }
            std.debug.print("]", .{});
        },
        .Table => |table| {
            std.debug.print("{{ ", .{});
            var it = table.iterator();
            var first = true;
            while (it.next()) |entry| {
                if (!first) std.debug.print(", ", .{});
                first = false;
                std.debug.print("{s}: ", .{entry.key_ptr.*});
                printValue(entry.value_ptr.*);
            }
            std.debug.print(" }}", .{});
        },
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.debug.print("Memory leak detected!\n", .{});
    }

    var arena = ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    // Get args
    var args = try std.process.argsWithAllocator(gpa.allocator());
    defer args.deinit();

    // Skip program name
    _ = args.skip();

    // Get filename argument
    const filename = args.next() orelse {
        std.debug.print("Usage: ztoml <filename>\n", .{});
        return error.MissingFilename;
    };

    // Read the file
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const toml_content = try arena.allocator().alloc(u8, file_size);
    _ = try file.readAll(toml_content);

    var parser = Parser.init(&arena, toml_content);
    const result = parser.parse() catch |err| {
        std.debug.print("Error parsing TOML: {}\n", .{err});
        return err;
    };

    _ = result;
}
