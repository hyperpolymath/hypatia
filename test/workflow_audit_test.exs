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

  describe "check_npermissions_typo/1" do
    test "detects npermissions typo" do
      content = """
      name: CI
      npermissions:
        contents: read
      jobs:
        build:
          runs-on: ubuntu-latest
      """
      findings = WorkflowAudit.check_npermissions_typo(%{"ci.yml" => content})
      assert length(findings) == 1
      assert hd(findings).rule == "WF013"
      assert hd(findings).type == :npermissions_typo
      assert hd(findings).severity == :high
      assert String.contains?(hd(findings).reason, "npermissions")
    end

    test "returns empty when permissions is correct" do
      content = """
      name: CI
      permissions:
        contents: read
      jobs:
        build:
          runs-on: ubuntu-latest
      """
      findings = WorkflowAudit.check_npermissions_typo(%{"ci.yml" => content})
      assert findings == []
    end

    test "returns empty when no permissions key at all" do
      content = """
      name: CI
      jobs:
        build:
          runs-on: ubuntu-latest
      """
      findings = WorkflowAudit.check_npermissions_typo(%{"ci.yml" => content})
      assert findings == []
    end

    test "detects npermissions across multiple files" do
      contents = %{
        "ci.yml" => "npermissions:\n  contents: read\n",
        "deploy.yml" => "npermissions:\n  contents: write\n"
      }
      findings = WorkflowAudit.check_npermissions_typo(contents)
      assert length(findings) == 2
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

  describe "check_codeql_language_matrix_mismatch/2" do
    @codeql_js_ts """
    name: CodeQL Security Analysis
    on: [push, pull_request]
    jobs:
      analyze:
        runs-on: ubuntu-latest
        strategy:
          matrix:
            include:
              - language: javascript-typescript
                build-mode: none
        steps:
          - uses: github/codeql-action/init@SHA
            with:
              languages: ${{ matrix.language }}
    """

    @codeql_actions """
    name: CodeQL Security Analysis
    on: [push, pull_request]
    jobs:
      analyze:
        runs-on: ubuntu-latest
        strategy:
          matrix:
            include:
              - language: actions
                build-mode: none
    """

    test "flags javascript-typescript matrix on a repo with no JS/TS source" do
      contents = %{".github/workflows/codeql.yml" => @codeql_js_ts}
      findings =
        WorkflowAudit.check_codeql_language_matrix_mismatch(contents,
          has_codeql_supported_language: false
        )

      assert length(findings) == 1
      f = hd(findings)
      assert f.type == :codeql_language_matrix_mismatch
      assert f.severity == :high
      assert f.action == :switch_codeql_matrix_to_actions
      assert String.contains?(f.detail, "javascript-typescript")
    end

    test "does NOT flag when matrix is `actions` (workflow-only scan)" do
      contents = %{".github/workflows/codeql.yml" => @codeql_actions}
      findings =
        WorkflowAudit.check_codeql_language_matrix_mismatch(contents,
          has_codeql_supported_language: false
        )

      assert findings == []
    end

    test "does NOT flag when repo has CodeQL-supported source files (caller's repo-detection said true)" do
      # Even if codeql.yml lists javascript-typescript, if the repo actually
      # has JS/TS files the analyze job will scan them — no mismatch.
      contents = %{".github/workflows/codeql.yml" => @codeql_js_ts}
      findings =
        WorkflowAudit.check_codeql_language_matrix_mismatch(contents,
          has_codeql_supported_language: true
        )

      assert findings == []
    end

    test "ignores non-codeql workflow files even on a no-source-langs repo" do
      contents = %{
        ".github/workflows/ci.yml" => "language: javascript-typescript",
        ".github/workflows/hypatia-scan.yml" => "language: javascript-typescript"
      }

      findings =
        WorkflowAudit.check_codeql_language_matrix_mismatch(contents,
          has_codeql_supported_language: false
        )

      assert findings == []
    end

    test "flags each scanning-language listed in the matrix" do
      multi = """
      jobs:
        analyze:
          strategy:
            matrix:
              include:
                - language: python
                - language: go
                - language: actions
      """

      contents = %{".github/workflows/codeql.yml" => multi}
      findings =
        WorkflowAudit.check_codeql_language_matrix_mismatch(contents,
          has_codeql_supported_language: false
        )

      types = Enum.map(findings, & &1.type) |> Enum.uniq()
      assert types == [:codeql_language_matrix_mismatch]
      assert length(findings) == 2
      detail_text = findings |> Enum.map(& &1.detail) |> Enum.join("\n")
      assert String.contains?(detail_text, "python")
      assert String.contains?(detail_text, "go")
      refute String.contains?(detail_text, "actions ")
    end
  end
end
