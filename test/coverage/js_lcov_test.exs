# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.JsLcovTest do
  @moduledoc """
  Real c8 and Node native LCOV (test/fixtures/coverage/js_demo, see its README) through
  `Hypatia.Coverage.Lcov`, and the report task end to end with `--root` at the committed sources.
  """
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Hypatia.Coverage.{Facts, Lcov}
  alias Mix.Tasks.Hypatia.Coverage.Report, as: Task

  @dir Path.expand("../fixtures/coverage/js_demo", __DIR__)
  @sha "0123456789abcdef0123456789abcdef01234567"
  @src "src/classify.js"

  defp parse!(name) do
    {:ok, facts} = @dir |> Path.join(name) |> File.read!() |> Lcov.parse()
    facts
  end

  describe "c8 output" do
    test "parses: TN, FN, FNDA, FNF, FNH, LF, LH, BRF, BRH are accepted and cross-checked" do
      raw = File.read!(Path.join(@dir, "raw.lcov"))
      assert raw =~ "TN:\n" and raw =~ "FN:3,classify" and raw =~ "FNDA:" and raw =~ "FNF:3"
      assert raw =~ "LF:29" and raw =~ "BRF:16"

      facts = parse!("raw.lcov")
      assert Map.keys(facts.files) == [@src]
      f = facts.files[@src]
      # c8's LF counts the DA records, as the validator expects
      assert map_size(f.lines) == 29
      assert Enum.count(f.lines, fn {_, h} -> h > 0 end) == 23
      # neverCalled() body (lines 22-27) was not run
      assert Enum.all?(22..27, &(f.lines[&1] == 0))
    end

    test "branch arms are measurable and arm counts match BRF/BRH" do
      facts = parse!("raw.lcov")
      assert facts.branch_measurable
      arms = facts.files[@src].branches
      assert map_size(arms) == 16
      assert Enum.count(arms, fn {_, h} -> h > 0 end) == 16
      assert {_, _, 0} = arms |> Map.keys() |> hd()

      proven = parse!("proven.lcov")
      assert map_size(proven.files[@src].branches) == 11
      assert Enum.count(proven.files[@src].branches, fn {_, h} -> h > 0 end) == 7
    end

    test "the proven run's lines are a subset of the raw run's" do
      raw = parse!("raw.lcov")
      proven = parse!("proven.lcov")
      assert MapSet.subset?(Facts.covered_lines(proven), Facts.covered_lines(raw))
      assert map_size(proven.files[@src].lines) == map_size(raw.files[@src].lines)
    end

    test "known limit: V8 branch block ids differ between runs, so the arm sets are not subsets" do
      raw = parse!("raw.lcov")
      proven = parse!("proven.lcov")
      refute MapSet.subset?(Facts.covered_branches(proven), Facts.covered_branches(raw))
    end

    test "the lines-only variants have no branch data" do
      for n <- ~w(raw_lines proven_lines) do
        facts = parse!(n <> ".lcov")
        assert facts.branch_measurable == false
        assert map_size(facts.files[@src].lines) == 29
      end
    end
  end

  describe "Node native lcov reporter" do
    test "parses; LH comes before LF there, which is fine" do
      facts = parse!("node_native_raw.lcov")
      assert facts.branch_measurable
      assert map_size(facts.files[@src].lines) == 29
      assert map_size(facts.files[@src].branches) == 12
    end
  end

  describe "istanbul-style text" do
    test "BRDA with - means never reached (0); BRF and LF agree with parsed records" do
      text = """
      TN:
      SF:src/x.js
      FN:1,f
      FNF:1
      FNH:0
      FNDA:0,f
      DA:1,0
      DA:2,3
      LF:2
      LH:1
      BRDA:2,0,0,3
      BRDA:2,0,1,-
      BRF:2
      BRH:1
      end_of_record
      """

      assert {:ok, facts} = Lcov.parse(text)
      assert facts.branch_measurable
      assert facts.files["src/x.js"].branches == %{{2, 0, 0} => 3, {2, 0, 1} => 0}
    end

    test "a count that disagrees with the records is still a parse error" do
      assert {:error, {:parse, msg}} =
               Lcov.parse("SF:a.js\nDA:1,1\nLF:2\nLH:1\nend_of_record\n")

      assert msg =~ "LF:2"
    end

    test "BRF:0 with no BRDA: branches measurable, zero arms (a file with no branches)" do
      assert {:ok, f} = Lcov.parse("SF:a.js\nDA:1,1\nLF:1\nLH:1\nBRF:0\nBRH:0\nend_of_record\n")
      assert f.branch_measurable
    end
  end

  describe "report task end to end (--root at the committed JS sources)" do
    defp run(raw, proven) do
      argv = [
        "--raw",
        Path.join(@dir, raw),
        "--proven",
        Path.join(@dir, proven),
        "--evidence",
        Path.join(@dir, "evidence.json"),
        "--repo",
        "example/js-demo",
        "--sha",
        @sha,
        "--root",
        @dir,
        "--shadow"
      ]

      me = self()
      capture_io(fn -> send(me, {:r, Task.execute(argv)}) end)
      assert_received {:r, {:ok, 0, output}}
      output
    end

    test "lines only: proven 17/29 against raw 23/29, branches not measurable" do
      out = run("raw_lines.lcov", "proven_lines.lcov")
      assert out =~ "## Hypatia coverage — FAIL"
      assert out =~ "17/29"
      assert out =~ "23/29"
      assert out =~ "Branches | not measurable | not measurable"
      assert out =~ "src/classify.js#L22-L27"
      assert out =~ "2 counted"
    end

    test "c8 branch data from two runs is refused as inconsistent (COV-E011), not judged" do
      out = run("raw.lcov", "proven.lcov")
      assert out =~ "UNJUDGED"
      assert out =~ "COV-E011"
    end
  end
end
