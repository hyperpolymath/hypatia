# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.GroupBDetectorsTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.StructuralDrift
  alias Hypatia.Rules.WorkflowAudit
  alias Hypatia.Rules.HonestCompletion

  describe "StructuralDrift.check_workflow_branch_refs/3 (SD021, #363)" do
    test "flags inline trigger branches that aren't real branches" do
      wf = %{"ci.yml" => "on:\n  push:\n    branches: [main, master, develop]\n"}
      findings = StructuralDrift.check_workflow_branch_refs(wf, ["main"])
      assert findings |> Enum.map(& &1.branch) |> Enum.sort() == ["develop", "master"]
      assert hd(findings).rule == "SD021"
      assert hd(findings).severity == :low
    end

    test "flags block-style dead branches, exempts globs and default" do
      body =
        "on:\n  pull_request:\n    branches:\n      - main\n      - feature/*\n      - legacy\n"

      findings = StructuralDrift.check_workflow_branch_refs(%{"ci.yml" => body}, ["main"])
      assert Enum.map(findings, & &1.branch) == ["legacy"]
    end

    test "silent when all trigger branches exist" do
      wf = %{"ci.yml" => "on:\n  push:\n    branches: [main, master]\n"}
      assert StructuralDrift.check_workflow_branch_refs(wf, ["main", "master"]) == []
    end
  end

  describe "WorkflowAudit.check_stale_continue_on_error/2 (WF023, #364)" do
    @coe "jobs:\n  x:\n    # remove when #104 lands\n    continue-on-error: true\n"

    test "flags a continue-on-error mask gated on a closed issue" do
      [f] = WorkflowAudit.check_stale_continue_on_error(%{"ci.yml" => @coe}, [104])
      assert f.rule == "WF023"
      assert f.issue == 104
      assert f.severity == :medium
    end

    test "silent when the gating issue is still open" do
      assert WorkflowAudit.check_stale_continue_on_error(%{"ci.yml" => @coe}, []) == []
    end

    test "silent when there is no continue-on-error" do
      body = "# remove when #104\n"
      assert WorkflowAudit.check_stale_continue_on_error(%{"ci.yml" => body}, [104]) == []
    end
  end

  describe "HonestCompletion.check_stale_issue_refs/2 (#366)" do
    test "flags a comment referencing a closed issue" do
      [f] =
        HonestCompletion.check_stale_issue_refs(%{"x.ex" => "# blocked by #50 upstream\n"}, [50])

      assert f.type == :stale_issue_reference
      assert f.issue == 50
    end

    test "silent when the referenced issue is still open" do
      assert HonestCompletion.check_stale_issue_refs(%{"x.ex" => "# blocked by #50\n"}, []) == []
    end
  end
end
