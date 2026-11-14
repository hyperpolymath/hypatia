# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.LlvmCovTest do
  use ExUnit.Case, async: true

  alias Hypatia.Coverage.LlvmCov

  @fixtures Path.expand("../fixtures/coverage", __DIR__)
  @prefix "/home/runner/work/demo/demo"

  defp load(name, opts \\ [root: @prefix]) do
    {:ok, facts} = @fixtures |> Path.join(name) |> File.read!() |> LlvmCov.parse(opts)
    facts
  end

  # `[line, col, count, has_count, is_region_entry, is_gap]`
  defp seg(line, col, count, has_count \\ true, entry \\ true, gap \\ false),
    do: [line, col, count, has_count, entry, gap]

  describe "line_counts/1 (llvm-cov line rule)" do
    test "max of the region-entry segments starting on the line" do
      segs = [
        seg(1, 1, 2),
        seg(1, 10, 7),
        seg(1, 20, 0, false, false),
        seg(2, 1, 0, false, false)
      ]

      assert %{1 => 7} = LlvmCov.line_counts(segs)
    end

    test "a line with no region start takes the wrapping segment's count" do
      segs = [seg(1, 1, 5), seg(4, 2, 0, false, false)]
      assert LlvmCov.line_counts(segs) == %{1 => 5, 2 => 5, 3 => 5, 4 => 5}
    end

    test "the wrapped count counts even when a smaller region starts on the line" do
      segs = [seg(1, 1, 9), seg(2, 3, 1), seg(2, 8, 9, true, false), seg(3, 1, 0, false, false)]
      assert %{1 => 9, 2 => 9, 3 => 9} = LlvmCov.line_counts(segs)
    end

    test "a line after a region end with no counted wrapper is not executable" do
      segs = [seg(1, 1, 3), seg(1, 10, 0, false, false), seg(3, 1, 4), seg(3, 5, 0, false, false)]
      counts = LlvmCov.line_counts(segs)
      assert counts == %{1 => 3, 3 => 4}
      refute Map.has_key?(counts, 2)
    end

    test "a skipped region (entry without count) at the start of a line makes it not executable" do
      segs = [
        seg(1, 1, 3),
        seg(1, 5, 0, false, false),
        seg(2, 1, 0, false, true),
        seg(2, 9, 0, false, false)
      ]

      assert LlvmCov.line_counts(segs) == %{1 => 3}
    end

    test "gap regions do not count as region starts" do
      segs = [seg(1, 1, 2), seg(1, 10, 99, true, true, true), seg(2, 1, 0, false, false)]
      assert %{1 => 2} = LlvmCov.line_counts(segs)
    end

    test "a counted zero is executable and not run" do
      assert LlvmCov.line_counts([seg(3, 1, 0), seg(3, 9, 0, false, false)]) == %{3 => 0}
    end

    test "no segments, no lines" do
      assert LlvmCov.line_counts([]) == %{}
    end
  end

  describe "real cargo llvm-cov export (no --branch, stable)" do
    @show_raw %{
      1 => 51,
      2 => 51,
      3 => 1,
      4 => 50,
      5 => 1,
      6 => 49,
      7 => 9,
      9 => 40,
      11 => 51,
      13 => 2,
      14 => 2,
      15 => 1,
      17 => 1,
      19 => 2,
      21 => 0,
      22 => 0,
      23 => 0,
      24 => 0,
      25 => 0,
      26 => 0,
      27 => 0,
      34 => 1,
      35 => 1,
      36 => 1,
      39 => 1,
      40 => 1,
      41 => 1,
      44 => 1,
      45 => 1,
      46 => 50,
      47 => 49,
      48 => 49,
      50 => 1,
      53 => 1,
      54 => 1,
      55 => 1,
      58 => 1,
      59 => 1,
      60 => 1
    }

    test "per-line counts equal `llvm-cov show` for the same run" do
      # @show_raw was read off `cargo llvm-cov --text` output of the fixture crate.
      facts = load("llvm_raw_nobranch.json")
      assert Map.keys(facts.files) == ["src/lib.rs"]
      assert facts.files["src/lib.rs"].lines == @show_raw
    end

    test "has no branch data: not measurable" do
      facts = load("llvm_raw_nobranch.json")
      assert facts.branch_measurable == false
      assert facts.files["src/lib.rs"].branches == %{}
    end

    test "the proven run (test filter `prop_`) is a subset of the raw run" do
      raw = load("llvm_raw_nobranch.json").files["src/lib.rs"].lines
      proven = load("llvm_proven_nobranch.json").files["src/lib.rs"].lines
      assert Map.keys(raw) == Map.keys(proven)
      assert Enum.all?(proven, fn {l, h} -> h <= raw[l] end)
      assert Enum.count(proven, fn {_, h} -> h > 0 end) == 20
      assert Enum.count(raw, fn {_, h} -> h > 0 end) == 32
    end
  end

  describe "declared branches must match parsed arms (COV-E002)" do
    defp mutated(fun) do
      doc = @fixtures |> Path.join("llvm_raw_branch.json") |> File.read!() |> Jason.decode!()

      LlvmCov.from_document(
        update_in(doc, ["data", Access.at(0), "files", Access.at(0)], fun),
        []
      )
    end

    test "a dropped branch entry is a parse error" do
      assert {:error, {:parse, msg}} = mutated(&Map.update!(&1, "branches", fn b -> tl(b) end))
      assert msg =~ "declares 10 branch arms"
    end

    test "a missing branches array is a parse error" do
      assert {:error, {:parse, _}} = mutated(&Map.delete(&1, "branches"))
    end

    test "totals that disagree with the files are a parse error" do
      doc = @fixtures |> Path.join("llvm_raw_branch.json") |> File.read!() |> Jason.decode!()
      doc = put_in(doc, ["data", Access.at(0), "totals", "branches", "count"], 12)
      assert {:error, {:parse, msg}} = LlvmCov.from_document(doc, [])
      assert msg =~ "totals"
    end

    test "the real fixtures are consistent" do
      assert {:ok, _} = mutated(& &1)
    end
  end

  describe "real cargo +nightly llvm-cov --branch export" do
    test "yields two arms per branch entry, keyed by line, column and arm" do
      facts = load("llvm_raw_branch.json")
      b = facts.files["src/lib.rs"].branches
      assert facts.branch_measurable
      # line 2: `if n < 0` was true once and false 50 times
      assert b[{2, 8, 0}] == 1
      assert b[{2, 8, 1}] == 50
      # line 4: `else if n == 0`
      assert b[{4, 15, 0}] == 1
      assert b[{4, 15, 1}] == 49
      # the clamp_add `if` on line 14: true once (250 + 10), false once (1 + 2)
      assert {b[{14, 8, 0}], b[{14, 8, 1}]} == {1, 1}
      # 5 branch entries -> 10 arms
      assert map_size(b) == 10
    end

    test "the proven run covers fewer arms than the raw run" do
      raw = load("llvm_raw_branch.json").files["src/lib.rs"].branches
      proven = load("llvm_proven_branch.json").files["src/lib.rs"].branches
      assert Map.keys(raw) == Map.keys(proven)
      assert Enum.count(raw, fn {_, h} -> h > 0 end) == 10
      assert Enum.count(proven, fn {_, h} -> h > 0 end) == 7
      assert Enum.all?(proven, fn {k, h} -> h <= raw[k] end)
    end
  end

  describe "paths" do
    test "root prefix makes filenames repo-relative, with or without trailing slash" do
      assert Map.keys(load("llvm_raw_branch.json", root: @prefix).files) == ["src/lib.rs"]
      assert Map.keys(load("llvm_raw_branch.json", root: @prefix <> "/").files) == ["src/lib.rs"]
    end

    test "without a root the absolute filename is kept (COV-E003 catches it later)" do
      assert Map.keys(load("llvm_raw_branch.json", []).files) == [@prefix <> "/src/lib.rs"]
    end

    test "a root that is not a prefix leaves the path alone" do
      assert Map.keys(load("llvm_raw_branch.json", root: "/elsewhere").files) == [
               @prefix <> "/src/lib.rs"
             ]
    end
  end

  describe "errors" do
    test "invalid JSON reports the byte position" do
      assert {:error, {:parse, "invalid JSON at byte " <> _}} = LlvmCov.parse("{\"data\": [")
    end

    test "valid JSON that is not an llvm-cov export" do
      assert {:error, {:parse, "not an llvm-cov export" <> _}} = LlvmCov.parse("[1,2]")
      assert {:error, {:parse, "not an llvm-cov export" <> _}} = LlvmCov.parse(~s({"data": []}))
    end

    test "a data entry with no files" do
      assert {:error, {:parse, "data[0].files missing" <> _}} = LlvmCov.parse(~s({"data": [{}]}))
    end

    test "a malformed segment names the file index" do
      json = ~s({"data": [{"files": [{"filename": "a.rs", "segments": [[1, 2]]}]}]})
      assert {:error, {:parse, "data[0].files[0]: malformed segment" <> _}} = LlvmCov.parse(json)
    end

    test "a malformed branch entry" do
      json = ~s({"data": [{"files": [{"filename": "a.rs", "segments": [], "branches": [[1]]}]}]})
      assert {:error, {:parse, "data[0].files[0]: malformed branch entry"}} = LlvmCov.parse(json)
    end
  end
end
