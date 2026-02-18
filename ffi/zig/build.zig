// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Build script for Hypatia Zig FFI library.
// Produces shared (.so/.dylib) and static (.a) libraries with C ABI exports.
// Run: zig build        (compile)
//      zig build test   (run all tests)

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Reusable dependency modules
    const json_writer_mod = b.createModule(.{
        .root_source_file = b.path("src/json_writer.zig"),
        .target = target,
        .optimize = optimize,
    });
    const file_ops_mod = b.createModule(.{
        .root_source_file = b.path("src/file_ops.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    // Shared library (for NIF / dlopen consumers)
    const shared_lib = b.addLibrary(.{
        .name = "hypatia_ffi",
        .linkage = .dynamic,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "json_writer", .module = json_writer_mod },
                .{ .name = "file_ops", .module = file_ops_mod },
            },
            .link_libc = true,
        }),
    });
    b.installArtifact(shared_lib);

    // Static library (for static linking)
    const static_lib = b.addLibrary(.{
        .name = "hypatia_ffi_static",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "json_writer", .module = json_writer_mod },
                .{ .name = "file_ops", .module = file_ops_mod },
            },
            .link_libc = true,
        }),
    });
    b.installArtifact(static_lib);

    // Unit tests for main module
    const main_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "json_writer", .module = json_writer_mod },
                .{ .name = "file_ops", .module = file_ops_mod },
            },
            .link_libc = true,
        }),
    });

    // Unit tests for json_writer
    const json_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/json_writer.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Unit tests for file_ops
    const file_ops_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/file_ops.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    // Integration tests
    const integration_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/integration_test.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "json_writer", .module = json_writer_mod },
                .{ .name = "file_ops", .module = file_ops_mod },
            },
            .link_libc = true,
        }),
    });

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(main_tests).step);
    test_step.dependOn(&b.addRunArtifact(json_tests).step);
    test_step.dependOn(&b.addRunArtifact(file_ops_tests).step);
    test_step.dependOn(&b.addRunArtifact(integration_tests).step);
}
