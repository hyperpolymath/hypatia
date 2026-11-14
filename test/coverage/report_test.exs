# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.ReportTest do
  use ExUnit.Case, async: true

  alias Hypatia.Coverage.{Evidence, Facts, LlvmCov, Report}

  @fixtures Path.expand("../fixtures/coverage", __DIR__)
  @demo_root Path.join(@fixtures, "demo_repo")
  @prefix "/home/runner/work/demo/demo"
  @sha "0123456789abcdef0123456789abcdef01234567"
  @url "https://github.com/example/demo/blob/#{@sha}/src/lib.rs"

  defp cov(name) do
    {:ok, f} = @fixtures |> Path.join(name) |> File.read!() |> LlvmCov.parse(root: @prefix)
    {:ok, f}
  end

  defp evidence, do: Evidence.load(Path.join(@fixtures, "evidence.json"))

  defp build(overrides \\ %{}) do
    Report.build(
      Map.merge(
        %{
          raw: cov("llvm_raw_branch.json"),
          proven: cov("llvm_proven_branch.json"),
          evidence: evidence(),
          repo: "example/demo",
          sha: @sha,
          root: @demo_root
        },
        overrides
      )
    )
  end

  defp codes(v), do: Enum.map(v["reasons"], & &1["code"])
  defp one(v, code), do: Enum.find(v["reasons"], &(&1["code"] == code))

  defp files(map), do: Facts.new(map)
  defp lines(path, map), do: files(%{path => %{lines: map, branches: %{}}})

  describe "the real fixture pair (judged)" do
    setup do: %{v: build()}

    test "verdict numbers", %{v: v} do
      assert v["outcome"] == "FAIL"
      assert v["shadow"] == true
      assert v["line"] == %{"proven" => 20, "raw" => 32, "total" => 38, "measurable" => true}
      assert v["branch"] == %{"proven" => 7, "raw" => 10, "total" => 10, "measurable" => true}
      assert v["repo"] == "example/demo" and v["commit"] == @sha
    end

    test "only the overclaim is flagged; no unjudging reason", %{v: v} do
      assert codes(v) == ["COV-E010"]

      assert one(v, "COV-E010")["detail"]["records"] == [
               %{"name" => "Demo::never_tested_total", "kind" => "P", "reason" => "empty_ladder"}
             ]
    end

    test "raw-only and not-run lines are grouped into ranges with permalinks", %{v: v} do
      raw_only = for i <- v["sections"]["raw_only_lines"]["items"], do: {i["from"], i["to"]}
      assert raw_only == [{3, 3}, {5, 5}, {17, 17}, {34, 36}, {39, 41}, {53, 55}]
      unrun = for i <- v["sections"]["unrun_lines"]["items"], do: {i["from"], i["to"], i["url"]}
      assert unrun == [{21, 24, @url <> "#L21-L24"}, {26, 27, @url <> "#L26-L27"}]
      [single | _] = v["sections"]["raw_only_lines"]["items"]
      assert single["url"] == @url <> "#L3"
    end

    test "branches: raw-only by line, none unrun", %{v: v} do
      assert for(i <- v["sections"]["raw_only_branches"]["items"], do: i["from"]) == [2, 4, 14]
      assert v["sections"]["unrun_branches"]["items"] == []
    end

    test "evidence summary lists every uncounted record with its reason", %{v: v} do
      assert v["evidence"]["total"] == 6 and v["evidence"]["counted"] == 2

      reasons =
        for r <- v["evidence"]["not_counted"], do: {r["name"], r["reason"], r["overclaim"]}

      assert reasons == [
               {"tests::example_negative", "example_test", false},
               {"tests::example_zero", "example_test", false},
               {"tests::example_clamp_ok", "example_test", false},
               {"Demo::never_tested_total", "empty_ladder", true}
             ]
    end

    test "the verdict is JSON-safe and round trips", %{v: v} do
      assert Jason.decode!(Report.to_json(v)) == v
    end

    test "Markdown has the shadow label, both tables and the lists", %{v: v} do
      md = Report.to_markdown(v)
      assert md =~ "## Hypatia coverage — FAIL  (commit 0123456)"
      assert md =~ "SHADOW — not a gate"
      assert md =~ "| Lines | 20/38 (52.6%) | 32/38 (84.2%) |"
      assert md =~ "| Branches | 7/10 (70.0%) | 10/10 (100.0%) |"
      assert md =~ "### Lines run only by unproven tests (raw only) (6)"
      assert md =~ "[src/lib.rs#L34-L36](#{@url}#L34-L36)"
      assert md =~ "### Lines not run by any test (2)"
      assert md =~ "### Why not counted (4 of 6; 2 counted)"

      assert md =~
               "`Demo::never_tested_total` (kind P): proof claim with an empty proof ladder [COV-E010]"

      assert md =~ "**COV-E010**"
    end
  end

  describe "branch data absent" do
    test "says 'not measurable', never 0%" do
      v = build(%{raw: cov("llvm_raw_nobranch.json"), proven: cov("llvm_proven_nobranch.json")})

      assert v["branch"] == %{
               "proven" => nil,
               "raw" => nil,
               "total" => nil,
               "measurable" => false
             }

      assert v["line"]["proven"] == 20
      md = Report.to_markdown(v)
      assert md =~ "| Branches | not measurable | not measurable |"
      assert md =~ "Branches: not measurable"
      refute md =~ "0.0%"
    end

    test "when only the proven run lacks branch data the pair is not measurable" do
      v = build(%{proven: cov("llvm_proven_nobranch.json")})
      assert v["branch"]["measurable"] == false
    end
  end

  describe "outcomes" do
    test "PASS when proven equals raw and raw is fully covered" do
      f =
        files(%{"a" => %{lines: %{1 => 2, 2 => 1}, branches: %{{1, 0, 0} => 1, {1, 0, 1} => 1}}})

      v = build(%{raw: {:ok, f}, proven: {:ok, f}, root: nil})
      assert v["outcome"] == "PASS"
      assert v["line"] == %{"proven" => 2, "raw" => 2, "total" => 2, "measurable" => true}
      assert Report.exit_code(v) == 0
      assert Report.to_markdown(v) =~ "| Lines | 2/2 (100.0%) | 2/2 (100.0%) |"
    end

    test "PASS on lines alone when branches are not measurable" do
      f = lines("a", %{1 => 1})
      assert build(%{raw: {:ok, f}, proven: {:ok, f}, root: nil})["outcome"] == "PASS"
    end

    test "FAIL when any measurable branch arm is not proven" do
      raw = files(%{"a" => %{lines: %{1 => 1}, branches: %{{1, 0, 0} => 1, {1, 0, 1} => 1}}})
      proven = files(%{"a" => %{lines: %{1 => 1}, branches: %{{1, 0, 0} => 1, {1, 0, 1} => 0}}})
      v = build(%{raw: {:ok, raw}, proven: {:ok, proven}, root: nil})
      assert v["outcome"] == "FAIL" and Report.exit_code(v) == 1
    end

    test "exit code is 2 for UNJUDGED" do
      assert Report.exit_code(build(%{raw: {:error, {:missing, "r"}}})) == 2
    end
  end

  describe "COV-E001 report missing" do
    test "raw missing: unjudged, no numbers at all" do
      v = build(%{raw: {:error, {:missing, "/x/raw.json"}}})
      assert v["outcome"] == "UNJUDGED"
      assert "COV-E001" in codes(v)
      assert one(v, "COV-E001")["detail"] == %{"input" => "raw", "path" => "/x/raw.json"}
      assert v["line"] == %{"proven" => nil, "raw" => nil, "total" => nil, "measurable" => true}
      assert v["branch"]["proven"] == nil
      md = Report.to_markdown(v)
      assert md =~ "| Lines | not judged | not judged |"
      refute md =~ "0.0%"
      refute md =~ "100.0%"
    end

    test "proven missing: raw numbers still shown, proven never invented" do
      v = build(%{proven: {:error, {:missing, "/x/p.json"}}})
      assert v["outcome"] == "UNJUDGED"
      assert v["line"]["raw"] == 32 and v["line"]["proven"] == nil
      assert Report.to_markdown(v) =~ "| Lines | not judged | 32/38 (84.2%) |"
    end

    test "evidence missing" do
      v = build(%{evidence: {:error, {:missing, "/x/e.json"}}})
      assert v["outcome"] == "UNJUDGED"
      assert one(v, "COV-E001")["detail"]["input"] == "evidence"
      assert v["evidence"]["total"] == nil
      assert Report.to_markdown(v) =~ "evidence list not available"
    end
  end

  describe "COV-E002 unparseable" do
    test "names the file and the location" do
      v = build(%{proven: {:error, {:parse, "/x/p.info", "line 7: bad DA record"}}})
      assert v["outcome"] == "UNJUDGED"
      assert one(v, "COV-E002")["message"] =~ "/x/p.info (line 7: bad DA record)"

      assert Report.to_markdown(v) =~
               "**COV-E002** proven input did not parse: /x/p.info (line 7: bad DA record)"
    end

    test "a raw report with no executable lines cannot be judged" do
      empty = files(%{"a" => %{lines: %{}, branches: %{}}})
      v = build(%{raw: {:ok, empty}, proven: {:ok, empty}, root: nil})
      assert v["outcome"] == "UNJUDGED"
      assert one(v, "COV-E002")["detail"]["where"] == "no executable lines"
      assert v["line"]["total"] == nil
    end
  end

  describe "COV-E003 paths not found" do
    test "shows a sample of the missing paths" do
      many = for i <- 1..8, into: %{}, do: {"gone/f#{i}.rs", %{lines: %{1 => 1}, branches: %{}}}
      f = files(Map.put(many, "src/lib.rs", %{lines: %{1 => 1}, branches: %{}}))
      v = build(%{raw: {:ok, f}, proven: {:ok, f}})
      assert v["outcome"] == "UNJUDGED"
      d = one(v, "COV-E003")["detail"]
      assert d["missing"] == 8 and d["of"] == 9
      assert d["sample"] == for(i <- 1..5, do: "gone/f#{i}.rs")
      assert one(v, "COV-E003")["message"] =~ "8 of 9 report path(s) not found under"
      # raw numbers are still true; proven is not judged
      assert v["line"]["proven"] == nil and v["line"]["raw"] == 9
    end

    test "unrelativised absolute paths are the typical cause" do
      {:ok, raw} =
        @fixtures |> Path.join("llvm_raw_branch.json") |> File.read!() |> LlvmCov.parse()

      v = build(%{raw: {:ok, raw}, proven: {:ok, raw}})
      assert "COV-E003" in codes(v)
      assert one(v, "COV-E003")["detail"]["sample"] == [@prefix <> "/src/lib.rs"]
    end

    test "without a root the check is skipped, and the report says so" do
      v = build(%{root: nil})
      refute "COV-E003" in codes(v)
      assert Report.to_markdown(v) =~ "no --root"
    end
  end

  describe "COV-E005 zero counted evidence" do
    test "tests ran but none qualified" do
      only_examples = [
        %{
          "id" => "e",
          "kind" => "E",
          "test_id" => %{"name" => "t"},
          "run" => %{"outcome" => "passed"}
        }
      ]

      v = build(%{evidence: {:ok, only_examples}})
      assert v["outcome"] == "UNJUDGED"
      assert one(v, "COV-E005")["detail"] == %{"records" => 1}
      assert v["line"]["proven"] == nil
      assert Report.to_markdown(v) =~ "**COV-E005** zero counted evidence"
    end

    test "an empty evidence list is zero counted evidence" do
      v = build(%{evidence: {:ok, []}})
      assert "COV-E005" in codes(v)
      assert v["outcome"] == "UNJUDGED"
    end

    test "min_cases is applied: raising it can drop evidence below the floor" do
      v = build(%{min_cases: 1000})
      assert "COV-E005" in codes(v)
      assert Enum.any?(v["evidence"]["not_counted"], &(&1["reason"] == "too_few_cases"))
    end
  end

  describe "COV-E010 overclaim" do
    test "is flagged but does not by itself make the verdict unjudged" do
      v = build()
      assert "COV-E010" in codes(v)
      assert v["outcome"] == "FAIL"
    end
  end

  describe "COV-E011 proven covers what raw does not" do
    test "a line" do
      raw = lines("src/lib.rs", %{1 => 1, 2 => 0})
      proven = lines("src/lib.rs", %{1 => 1, 2 => 3})
      v = build(%{raw: {:ok, raw}, proven: {:ok, proven}, root: nil})
      assert v["outcome"] == "UNJUDGED"
      d = one(v, "COV-E011")["detail"]
      assert d == %{"lines" => 1, "branches" => 0, "sample" => ["src/lib.rs:2"]}
      assert v["line"]["proven"] == nil and v["line"]["raw"] == 1
      assert v["sections"]["raw_only_lines"]["items"] == []
    end

    test "a branch arm" do
      raw = files(%{"a" => %{lines: %{1 => 1}, branches: %{{1, 0, 0} => 1, {1, 0, 1} => 0}}})
      proven = files(%{"a" => %{lines: %{1 => 1}, branches: %{{1, 0, 0} => 1, {1, 0, 1} => 1}}})
      v = build(%{raw: {:ok, raw}, proven: {:ok, proven}, root: nil})
      assert one(v, "COV-E011")["detail"]["branches"] == 1
      assert v["branch"]["proven"] == nil
    end

    test "a line the raw run never listed at all" do
      raw = lines("a", %{1 => 1})
      proven = lines("a", %{1 => 1, 9 => 1})
      assert "COV-E011" in codes(build(%{raw: {:ok, raw}, proven: {:ok, proven}, root: nil}))
    end

    test "equal or smaller proven coverage is consistent" do
      raw = lines("a", %{1 => 1, 2 => 1})

      refute "COV-E011" in codes(
               build(%{raw: {:ok, raw}, proven: {:ok, lines("a", %{1 => 1, 2 => 0})}, root: nil})
             )
    end
  end

  describe "caps" do
    test "lists are cut at 50 entries and the cut is reported" do
      # 120 isolated unrun lines -> 120 ranges
      raw_lines = for l <- 1..240//2, into: %{}, do: {l, 0}
      f = lines("a", raw_lines)
      v = build(%{raw: {:ok, f}, proven: {:ok, f}, root: nil, evidence: {:ok, [counted_r()]}})
      s = v["sections"]["unrun_lines"]
      assert length(s["items"]) == 50 and s["total"] == 120 and s["cut"] == 70
      assert Report.to_markdown(v) =~ "- … 70 more not shown (cap 50)"
    end

    test "the Why-not-counted list is capped too" do
      many =
        for i <- 1..60,
            do: %{
              "id" => "e#{i}",
              "kind" => "E",
              "fixtures" => %{"cannot_fail_by_design" => "example"},
              "run" => %{"outcome" => "passed"}
            }

      v = build(%{evidence: {:ok, [counted_r() | many]}})
      assert Report.to_markdown(v) =~ "- … 10 more not shown (cap 50)"
    end
  end

  describe "ranges/1 and permalink/5" do
    test "consecutive lines merge, gaps and paths split, duplicates collapse" do
      locs = [{"b", 2}, {"a", 3}, {"a", 1}, {"a", 2}, {"a", 2}, {"a", 7}, {"b", 3}]
      assert Report.ranges(locs) == [{"a", 1, 3}, {"a", 7, 7}, {"b", 2, 3}]
      assert Report.ranges([]) == []
    end

    test "permalink forms" do
      assert Report.permalink("o/r", "abc", "src/a b.rs", 4, 9) ==
               "https://github.com/o/r/blob/abc/src/a%20b.rs#L4-L9"

      assert Report.permalink("o/r", "abc", "x.rs", 4, 4) ==
               "https://github.com/o/r/blob/abc/x.rs#L4"
    end
  end

  describe "load_coverage/2" do
    test "sniffs llvm-cov JSON and LCOV; reports missing and unparseable files" do
      assert {:ok, %{branch_measurable: true}} =
               Report.load_coverage(Path.join(@fixtures, "llvm_raw_branch.json"), root: @prefix)

      tmp = Path.join(System.tmp_dir!(), "hypatia-cov-#{System.unique_integer([:positive])}")
      File.mkdir_p!(tmp)
      on_exit(fn -> File.rm_rf(tmp) end)

      lcov = Path.join(tmp, "c.info")
      File.write!(lcov, "SF:a\nDA:1,1\nend_of_record\n")
      assert {:ok, %{files: %{"a" => _}}} = Report.load_coverage(lcov)

      bad = Path.join(tmp, "bad.info")
      File.write!(bad, "SF:a\nDA:1\nend_of_record\n")
      assert {:error, {:parse, ^bad, "line 2: bad DA record" <> _}} = Report.load_coverage(bad)

      badjson = Path.join(tmp, "bad.json")
      File.write!(badjson, "{ nope")

      assert {:error, {:parse, ^badjson, "invalid JSON at byte" <> _}} =
               Report.load_coverage(badjson)

      assert {:error, {:missing, "/nonexistent/x"}} = Report.load_coverage("/nonexistent/x")
    end
  end

  describe "compute/2 and percent/2" do
    test "counts against raw's universe" do
      raw = lines("a", %{1 => 1, 2 => 1, 3 => 0})
      proven = lines("a", %{1 => 1, 2 => 0, 3 => 0})
      assert %{line: %{proven: 1, raw: 2, total: 3}} = Report.compute(raw, proven)
    end

    test "percent never fabricates a number" do
      assert Report.percent(1, 3) == 33.3
      assert Report.percent(0, 4) == 0.0
      assert Report.percent(3, 0) == nil
      assert Report.percent(nil, 3) == nil
      assert Report.percent(nil, nil) == nil
    end
  end

  defp counted_r do
    %{
      "id" => "r",
      "kind" => "R",
      "test_id" => %{"name" => "p"},
      "fixtures" => %{"silence" => "s", "firing" => %{"source" => "f"}},
      "run" => %{"outcome" => "passed", "property" => %{"seed" => "1", "cases" => 100}}
    }
  end
end
