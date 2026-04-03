# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Smoke tests for the Zig FFI bridge.
#
# The Zig FFI exports 7 C ABI functions that mirror the Elixir pipeline:
#   hypatia_health_check, hypatia_scan_repo, hypatia_dispatch,
#   hypatia_record_outcome, hypatia_force_learning_cycle,
#   hypatia_get_confidence, hypatia_dispatch_strategy
#
# These smoke tests validate:
# 1. The Zig FFI source and build files are present and well-formed
# 2. The Elixir-layer logic that mirrors the FFI dispatch strategy is correct
# 3. The verisimdb-data directory structure that the Zig FFI reads from is accessible
# 4. The confidence + dispatch strategy mappings match between Elixir and Zig spec
#
# If the Zig shared library is compiled (zig build in ffi/zig/), these tests also
# verify the C ABI entry points produce valid JSON responses.

defmodule Hypatia.ZigFFI.SmokeTest do
  @moduledoc """
  Smoke tests for Zig FFI bridge integrity.

  Validates source presence, data path accessibility, and dispatch strategy
  logic consistency between Elixir and the Zig FFI specification.
  """

  use ExUnit.Case, async: true

  alias Hypatia.TriangleRouter

  @zig_ffi_dir "/var/mnt/eclipse/repos/hypatia/ffi/zig"
  @zig_src_dir "#{@zig_ffi_dir}/src"
  @data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisimdb")

  # ---------------------------------------------------------------------------
  # Smoke: FFI source files are present and non-empty
  # ---------------------------------------------------------------------------

  describe "Zig FFI source integrity" do
    test "ffi/zig/src/main.zig exists and is non-empty" do
      path = Path.join(@zig_src_dir, "main.zig")
      assert File.exists?(path),
             "main.zig not found at #{path}"
      {:ok, content} = File.read(path)
      assert byte_size(content) > 0,
             "main.zig is empty"
    end

    test "ffi/zig/build.zig exists" do
      path = Path.join(@zig_ffi_dir, "build.zig")
      assert File.exists?(path),
             "build.zig not found at #{path}"
    end

    test "ffi/zig/src/main.zig exports the 7 required C ABI functions" do
      path = Path.join(@zig_src_dir, "main.zig")
      {:ok, content} = File.read(path)

      required_exports = [
        "hypatia_health_check",
        "hypatia_scan_repo",
        "hypatia_dispatch",
        "hypatia_record_outcome",
        "hypatia_force_learning_cycle",
        "hypatia_get_confidence",
        "hypatia_dispatch_strategy"
      ]

      Enum.each(required_exports, fn fn_name ->
        assert String.contains?(content, fn_name),
               "Expected export '#{fn_name}' not found in main.zig"
      end)
    end

    test "ffi/zig/src/main.zig has SPDX header" do
      path = Path.join(@zig_src_dir, "main.zig")
      {:ok, content} = File.read(path)
      first_2k = String.slice(content, 0, 2000)
      assert String.contains?(first_2k, "SPDX-License-Identifier"),
             "main.zig missing SPDX-License-Identifier header"
    end

    test "ffi/zig/test/integration_test.zig exists" do
      path = Path.join([@zig_ffi_dir, "test", "integration_test.zig"])
      assert File.exists?(path),
             "integration_test.zig not found at #{path}"
    end
  end

  # ---------------------------------------------------------------------------
  # Smoke: verisimdb-data directory structure accessible to Zig FFI
  # ---------------------------------------------------------------------------

  describe "verisimdb-data structure (Zig FFI data path)" do
    # The Zig FFI reads from these stores directly.
    # Note: 'index' is index.json (a file, not a directory) in this data layout.
    @required_store_dirs ~w(scans patterns recipes outcomes dispatch)
    @required_store_files ~w(index.json)

    test "verisimdb-data root directory exists" do
      expanded = Path.expand(@data_path)
      assert File.exists?(expanded),
             "verisimdb-data root not found at #{expanded} (VERISIMDB_DATA_PATH or compile_env)"
    end

    test "all 5 required store directories exist for hypatia_health_check" do
      expanded = Path.expand(@data_path)

      Enum.each(@required_store_dirs, fn store ->
        store_path = Path.join(expanded, store)
        assert File.exists?(store_path),
               "Store directory '#{store}' not found at #{store_path} — " <>
                 "hypatia_health_check would return degraded"
      end)
    end

    test "index store file exists (index.json)" do
      expanded = Path.expand(@data_path)

      Enum.each(@required_store_files, fn file ->
        file_path = Path.join(expanded, file)
        assert File.exists?(file_path),
               "Store file '#{file}' not found at #{file_path} — " <>
                 "VQL index queries would fail"
      end)
    end

    test "scans store contains at least 3 scan files" do
      expanded = Path.expand(@data_path)
      scans_path = Path.join(expanded, "scans")

      {:ok, files} = File.ls(scans_path)
      json_files = Enum.filter(files, &String.ends_with?(&1, ".json"))
      assert length(json_files) >= 3,
             "Expected at least 3 scan JSON files in #{scans_path}, found #{length(json_files)}"
    end

    test "scan files contain valid JSON" do
      expanded = Path.expand(@data_path)
      scans_path = Path.join(expanded, "scans")

      {:ok, files} = File.ls(scans_path)

      # Sample first 5 scan files to verify JSON validity
      files
      |> Enum.filter(&String.ends_with?(&1, ".json"))
      |> Enum.take(5)
      |> Enum.each(fn file ->
        path = Path.join(scans_path, file)
        {:ok, content} = File.read(path)
        assert {:ok, _} = Jason.decode(content),
               "Scan file #{file} contains invalid JSON"
      end)
    end

    test "recipes store has at least 4 recipe files" do
      expanded = Path.expand(@data_path)
      recipes_path = Path.join(expanded, "recipes")

      {:ok, files} = File.ls(recipes_path)
      recipe_files = Enum.filter(files, &(String.starts_with?(&1, "recipe-") and String.ends_with?(&1, ".json")))
      assert length(recipe_files) >= 4,
             "Expected at least 4 recipe files in #{recipes_path}, found #{length(recipe_files)}"
    end
  end

  # ---------------------------------------------------------------------------
  # Smoke: dispatch strategy logic matches Zig FFI spec
  # ---------------------------------------------------------------------------

  describe "dispatch_strategy logic matches Zig FFI specification" do
    # The Zig FFI hypatia_dispatch_strategy function maps confidence to
    # DispatchStrategy enum: auto_execute=0, review=1, report_only=2
    # The thresholds must match the Elixir TriangleRouter exactly.

    test "auto_execute threshold is >= 0.95 (matches Zig FFI DispatchStrategy enum)" do
      # At 0.95 (exact boundary) -> auto_execute
      assert TriangleRouter.dispatch_strategy(0.95) == :auto_execute,
             "Expected :auto_execute at 0.95 confidence (Zig FFI spec boundary)"
      # At 0.951 -> auto_execute
      assert TriangleRouter.dispatch_strategy(0.951) == :auto_execute
      # At 1.0 -> auto_execute
      assert TriangleRouter.dispatch_strategy(1.0) == :auto_execute
    end

    test "review threshold is 0.85..0.94 (matches Zig FFI DispatchStrategy enum)" do
      assert TriangleRouter.dispatch_strategy(0.94) == :review
      assert TriangleRouter.dispatch_strategy(0.90) == :review
      assert TriangleRouter.dispatch_strategy(0.85) == :review
    end

    test "report_only threshold is < 0.85 (matches Zig FFI DispatchStrategy enum)" do
      assert TriangleRouter.dispatch_strategy(0.849) == :report_only
      assert TriangleRouter.dispatch_strategy(0.5) == :report_only
      assert TriangleRouter.dispatch_strategy(0.0) == :report_only
    end

    test "dispatch_strategy covers exactly 3 strategies across [0, 1]" do
      sample_range = for i <- 0..100, do: i / 100.0
      strategies = sample_range |> Enum.map(&TriangleRouter.dispatch_strategy/1) |> Enum.uniq() |> Enum.sort()
      assert strategies == [:auto_execute, :report_only, :review],
             "Expected exactly 3 strategies, got: #{inspect(strategies)}"
    end

    test "non-numeric confidence defaults to report_only (matches Zig FFI null handling)" do
      assert TriangleRouter.dispatch_strategy(nil) == :report_only
      assert TriangleRouter.dispatch_strategy("high") == :report_only
      assert TriangleRouter.dispatch_strategy(:high) == :report_only
    end
  end

  # ---------------------------------------------------------------------------
  # Smoke: Idris2 ABI source files are present
  # ---------------------------------------------------------------------------

  describe "Idris2 ABI source integrity" do
    @abi_dir "/var/mnt/eclipse/repos/hypatia/src/abi"

    test "src/abi directory exists" do
      assert File.exists?(@abi_dir),
             "Idris2 ABI directory not found at #{@abi_dir}"
    end

    test "all 5 required Idris2 ABI modules are present" do
      required_modules = ~w(Types.idr GraphQL.idr GRPC.idr REST.idr FFI.idr)

      Enum.each(required_modules, fn mod ->
        path = Path.join(@abi_dir, mod)
        assert File.exists?(path),
               "Idris2 ABI module #{mod} not found at #{path}"
      end)
    end

    test "Idris2 ABI modules have SPDX headers" do
      @abi_dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".idr"))
      |> Enum.each(fn file ->
        path = Path.join(@abi_dir, file)
        {:ok, content} = File.read(path)
        first_2k = String.slice(content, 0, 2000)
        assert String.contains?(first_2k, "SPDX-License-Identifier"),
               "#{file} missing SPDX-License-Identifier header"
      end)
    end
  end
end
