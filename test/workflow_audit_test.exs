# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.WorkflowAuditTest do
  use ExUnit.Case, async: true
  alias Hypatia.Rules.WorkflowAudit

  describe "check_missing_workflows/1" do
    test "flags all missing when empty" do
      findings = WorkflowAudit.check_missing_workflows([])
      assert length(findings) == 17
    end

    test "no findings when all present" do
      findings = WorkflowAudit.check_missing_workflows(WorkflowAudit.standard_workflows())
      assert findings == []
    end

    test "flags specific missing workflow" do
      present = WorkflowAudit.standard_workflows() -- ["hypatia-scan.yml"]
      findings = WorkflowAudit.check_missing_workflows(present)
      assert length(findings) == 1
      assert hd(findings).file == "hypatia-scan.yml"
      assert hd(findings).severity == :critical
    end
  end

  describe "check_unpinned_actions/1" do
    test "flags tag references" do
      content = """
      jobs:
        build:
          steps:
            - uses: actions/checkout@v4
            - uses: actions/configure-pages@v5
      """
      findings = WorkflowAudit.check_unpinned_actions(%{"ci.yml" => content})
      assert length(findings) == 2
      assert Enum.all?(findings, & &1.type == :unpinned_action)
    end

    test "ignores SHA-pinned actions" do
      content = """
      steps:
        - uses: actions/checkout@34e114876b0b11c390a56381ad16ebd13914f8d5
      """
      findings = WorkflowAudit.check_unpinned_actions(%{"ci.yml" => content})
      assert findings == []
    end

    test "provides known SHA when available" do
      content = "    - uses: actions/checkout@v4\n"
      [finding] = WorkflowAudit.check_unpinned_actions(%{"ci.yml" => content})
      assert finding.known_sha == "34e114876b0b11c390a56381ad16ebd13914f8d5"
    end
  end

  describe "check_permissions/1" do
    test "flags write-all" do
      content = "permissions: write-all\njobs:\n  build:"
      findings = WorkflowAudit.check_permissions(%{"ci.yml" => content})
      assert Enum.any?(findings, & &1.type == :broad_permissions)
    end

    test "flags missing permissions" do
      content = "name: CI\njobs:\n  build:\n    runs-on: ubuntu-latest"
      findings = WorkflowAudit.check_permissions(%{"ci.yml" => content})
      assert Enum.any?(findings, & &1.type == :missing_permissions)
    end

    test "accepts read-all" do
      content = "permissions: read-all\n# SPDX-License-Identifier: PMPL-1.0-or-later\njobs:"
      findings = WorkflowAudit.check_permissions(%{"ci.yml" => content})
      refute Enum.any?(findings, & &1.type == :broad_permissions)
      refute Enum.any?(findings, & &1.type == :missing_permissions)
    end

    test "flags missing SPDX header" do
      content = "permissions: read-all\njobs:\n  build:"
      findings = WorkflowAudit.check_permissions(%{"ci.yml" => content})
      assert Enum.any?(findings, & &1.type == :missing_spdx)
    end
  end

  describe "check_duplicates/2" do
    test "detects duplicate content" do
      contents = %{
        "ci.yml" => "same content here",
        "build.yml" => "same content here"
      }
      findings = WorkflowAudit.check_duplicates(Map.keys(contents), contents)
      assert length(findings) == 1
      assert hd(findings).type == :duplicate_workflow
    end

    test "no duplicates when content differs" do
      contents = %{
        "ci.yml" => "content A",
        "build.yml" => "content B"
      }
      findings = WorkflowAudit.check_duplicates(Map.keys(contents), contents)
      assert findings == []
    end
  end

  describe "audit/2" do
    test "returns comprehensive report" do
      report = WorkflowAudit.audit(["hypatia-scan.yml", "codeql.yml"], %{})
      assert is_map(report)
      assert report.workflow_count == 2
      assert report.missing_count == 15
      assert report.standard_coverage == round(2 / 17 * 100)
    end
  end
end
