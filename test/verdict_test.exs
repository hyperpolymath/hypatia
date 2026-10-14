# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.VerdictTest do
  @moduledoc """
  HYP-01: every finding names its support, and its level is derived.

  The Elixir mirror is checked against the two artefacts it mirrors rather
  than against restated constants: the wire names of the levels are read out
  of `src/Hypatia/ABI/Verdict.idr`, and the sound re-check is the one recorded
  in the positive control `tests/accept/LeanHoleRecheckSound.idr` (whose
  digests `scripts/recheck-lean-hole.sh` ties to a fresh Lean run).
  """

  use ExUnit.Case, async: true

  alias Hypatia.Verdict

  @repo_root Path.expand("..", __DIR__)
  @abi Path.join(@repo_root, "src/Hypatia/ABI/Verdict.idr")
  @control Path.join(@repo_root, "tests/accept/LeanHoleRecheckSound.idr")

  defp control_recheck do
    src = File.read!(@control)
    [_, target] = Regex.run(~r/^fixturePath = "([^"]+)"/m, src)
    [_, version] = Regex.run(~r/^  "(\d+\.\d+\.\d+)"$/m, src)
    [_, target_sha] = Regex.run(~r/^  \(MkSha256 "([0-9a-f]{64})"\) -- targetHash$/m, src)
    [_, artefact_sha] = Regex.run(~r/^  \(MkSha256 "([0-9a-f]{64})"\) -- artefactHash$/m, src)
    [_, line] = Regex.run(~r/^ +\(MkSpan fixturePath (\d+) \d+\) \d+$/m, src)

    {%{
       prover: "lean4",
       prover_version: version,
       target: target,
       target_sha256: target_sha,
       artefact_sha256: artefact_sha,
       result: "confirms"
     }, String.to_integer(line)}
  end

  defp hole_finding(target, line) do
    %{
      rule_module: "code_safety",
      type: "sorry",
      severity: "critical",
      file: target,
      line: line,
      reason: "sorry leaves proof hole",
      action: "flag"
    }
  end

  describe "level/2 mirrors Verdict.idr" do
    test "the wire names are exactly the ABI's levelName strings" do
      src = File.read!(@abi)

      names =
        Regex.scan(~r/^levelName Lv\w+ += "(\w+)"$/m, src, capture: :all_but_first)
        |> List.flatten()
        |> Enum.sort()

      assert names == ["belief", "epi", "sound"]
    end

    test "only a confirming mechanical re-check is sound" do
      token = [%{rule: "r/x"}]
      assert Verdict.level(%{kind: "mechanical", result: "confirms"}, token) == "sound"
      assert Verdict.level(%{kind: "mechanical", result: "refutes"}, token) == "epi"
      assert Verdict.level(%{kind: "pattern"}, token) == "epi"
      assert Verdict.level(%{kind: "observed", observations: 5}, token) == "epi"
      assert Verdict.level(%{kind: "observed", observations: 10_000}, token) == "epi"
    end

    test "no evidence token is belief, whatever the support claims short of a re-check" do
      assert Verdict.level(%{kind: "pattern"}, []) == "belief"
      assert Verdict.level(%{kind: "observed", observations: 5}, []) == "belief"
    end
  end

  describe "annotate/1" do
    test "a rule-module finding is pattern-supported, with one token, at level epi" do
      f = Verdict.annotate(%{rule_module: "code_safety", type: "x", file: "a.rs", line: 7})

      assert f.support == %{kind: "pattern"}
      assert f.level == "epi"

      assert [
               %{
                 rule: "code_safety/x",
                 rule_version: version,
                 span: %{file: "a.rs", start_line: 7, end_line: 7},
                 observations: 1
               }
             ] = f.evidence

      assert version == Verdict.rule_version()
    end

    test "a file- or repo-level finding gets line 0 in its span" do
      f = Verdict.annotate(%{rule_module: "git_state", type: "GS001", file: ".", line: nil})
      assert [%{span: %{start_line: 0, end_line: 0}}] = f.evidence
    end

    test "a rule cannot assert its own level" do
      forged = %{rule_module: "code_safety", type: "x", file: "a.rs", level: "sound"}
      assert Verdict.annotate(forged).level == "epi"
    end
  end

  describe "attach_recheck/3 — the only way to sound" do
    test "the positive control's Lean re-check makes the fixture's finding sound" do
      {recheck, line} = control_recheck()

      assert {:ok, f} =
               Verdict.attach_recheck(hole_finding(recheck.target, line), recheck, @repo_root)

      assert f.level == "sound"
      assert f.support.kind == "mechanical"
      assert f.support.artefact_sha256 == recheck.artefact_sha256
      assert Verdict.describe(f) == "sound: lean4 #{recheck.prover_version} re-check confirms"
    end

    test "the same finding without the re-check is epi" do
      {recheck, line} = control_recheck()
      assert Verdict.annotate(hole_finding(recheck.target, line)).level == "epi"
    end

    test "a refuting re-check is attached as recorded and derives epi" do
      {recheck, line} = control_recheck()
      refuted = %{recheck | result: "refutes"}

      assert {:ok, f} =
               Verdict.attach_recheck(hole_finding(recheck.target, line), refuted, @repo_root)

      assert f.level == "epi"
    end

    test "a target digest that does not match the bytes on disk is refused" do
      {recheck, line} = control_recheck()
      forged = %{recheck | target_sha256: String.duplicate("0", 64)}

      assert {:error, {:target_digest_mismatch, actual, _}} =
               Verdict.attach_recheck(hole_finding(recheck.target, line), forged, @repo_root)

      assert actual == recheck.target_sha256
    end

    test "a re-check of a different file is refused" do
      {recheck, line} = control_recheck()

      assert {:error, {:target_mismatch, "other.lean", _}} =
               Verdict.attach_recheck(hole_finding("other.lean", line), recheck, @repo_root)
    end

    test "malformed re-checks are refused before any file is read" do
      {recheck, line} = control_recheck()
      finding = hole_finding(recheck.target, line)

      assert {:error, {:invalid, :artefact_sha256, "F5D4"}} =
               Verdict.attach_recheck(finding, %{recheck | artefact_sha256: "F5D4"}, @repo_root)

      assert {:error, {:invalid, :prover, "z3"}} =
               Verdict.attach_recheck(finding, %{recheck | prover: "z3"}, @repo_root)

      assert {:error, {:invalid, :result, "maybe"}} =
               Verdict.attach_recheck(finding, %{recheck | result: "maybe"}, @repo_root)
    end
  end

  describe "CLI.collect_findings/2 — every finding names its support" do
    test "each code_safety finding over the soundness fixtures carries the verdict fields" do
      findings =
        Path.join(@repo_root, "test/soundness/fixtures")
        |> Hypatia.CLI.collect_findings([:code_safety])

      assert findings != []

      for f <- findings do
        assert %{kind: "pattern"} = f.support
        assert [%{rule: rule} | _] = f.evidence
        assert rule == "code_safety/#{f.type}"
        assert f.level == Verdict.level(f.support, f.evidence)
        assert f.level == "epi"
      end
    end
  end

  describe "level_counts/1" do
    test "always reports all three levels" do
      assert Verdict.level_counts([]) == %{"sound" => 0, "epi" => 0, "belief" => 0}

      assert Verdict.level_counts([%{level: "epi"}, %{level: "epi"}, %{level: "sound"}]) ==
               %{"sound" => 1, "epi" => 2, "belief" => 0}
    end
  end
end
