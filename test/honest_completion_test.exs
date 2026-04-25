# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.HonestCompletionTest do
  use ExUnit.Case, async: true
  alias Hypatia.Rules.HonestCompletion

  describe "parse_state_file/1" do
    test "extracts completion percentage" do
      content = """
      [current-position]
      completion-percentage = 95
      """
      assert {:ok, claims} = HonestCompletion.parse_state_file(content)
      assert claims.completion_percentage == 95
    end

    test "extracts test count" do
      content = """
      [project-context.counts]
      test-count = 2263
      """
      assert {:ok, claims} = HonestCompletion.parse_state_file(content)
      assert claims.claimed_tests == 2263
    end

    test "extracts phase" do
      content = """
      phase = "alpha-complete"
      """
      assert {:ok, claims} = HonestCompletion.parse_state_file(content)
      assert claims.phase == "alpha-complete"
    end

    test "handles missing fields gracefully" do
      assert {:ok, claims} = HonestCompletion.parse_state_file("# empty file")
      assert claims.completion_percentage == nil
      assert claims.claimed_tests == nil
    end

    test "rejects non-binary input" do
      assert {:error, :invalid_content} = HonestCompletion.parse_state_file(42)
    end
  end

  describe "generate_findings/2" do
    # Base evidence map with all keys the implementation accesses
    defp base_evidence(overrides \\ %{}) do
      Map.merge(
        %{believe_me_count: 0, sorry_count: 0, todo_count: 0, fixme_count: 0,
          admitted_count: 0, unsafe_coerce_count: 0, obj_magic_count: 0,
          postulate_count: 0, really_believe_me_count: 0, stub_count: 0,
          has_tests_dir: true, test_files: 10, has_ci: true,
          has_state_file: true, source_files: 50},
        overrides
      )
    end

    test "flags believe_me" do
      claims = %{}
      evidence = base_evidence(%{believe_me_count: 5})
      findings = HonestCompletion.generate_findings(claims, evidence)
      assert Enum.any?(findings, & &1.type == :dangerous_pattern)
    end

    test "flags no tests" do
      claims = %{}
      evidence = base_evidence(%{has_tests_dir: false, test_files: 0})
      findings = HonestCompletion.generate_findings(claims, evidence)
      assert Enum.any?(findings, & &1.type == :no_tests)
    end

    test "flags high TODO density" do
      claims = %{}
      evidence = base_evidence(%{todo_count: 100, source_files: 50})
      findings = HonestCompletion.generate_findings(claims, evidence)
      assert Enum.any?(findings, & &1.type == :high_todo_density)
    end

    test "clean repo has no findings" do
      claims = %{}
      evidence = base_evidence(%{todo_count: 10, fixme_count: 5, stub_count: 2,
                                  test_files: 20, source_files: 100})
      findings = HonestCompletion.generate_findings(claims, evidence)
      assert findings == []
    end
  end
end
