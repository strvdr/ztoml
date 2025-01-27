const std = @import("std");
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Just create the module for other projects to use
    _ = b.addModule("ztoml", .{
        .root_source_file = b.path("src/main.zig"),
    });

    // Keep the executable part if you want to run ztoml directly too
    const exe = b.addExecutable(.{
        .name = "ztoml",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);
}
