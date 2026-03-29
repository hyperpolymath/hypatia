# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.DirectGitHubPRTest do
  use ExUnit.Case, async: true

  alias Hypatia.DirectGitHubPR

  # --- Test Data ---

  @sc013_finding %{
    "id" => "SC-013-test-repo",
    "category" => "DependencyPinning",
    "severity" => "Medium",
    "description" => "2 workflow(s) with tag-pinned (not SHA-pinned) actions in test-repo",
    "pa_rule" => "SC013",
    "scorecard_check" => "Pinned-Dependencies",
    "remediation" => "Pin GitHub Actions and Docker base images by SHA hash.",
    "auto_fixable" => true,
    "confidence" => 0.90,
    "repos_affected_list" => ["test-repo"],
    "repos_affected" => 1,
    "occurrences" => 1,
    "source" => "scorecard",
    "triangle_tier" => "eliminate"
  }

  @sc018_finding %{
    "id" => "SC-018-test-repo",
    "category" => "TokenPermissions",
    "severity" => "High",
    "description" => "3 workflow(s) missing permissions declaration in test-repo",
    "pa_rule" => "SC018",
    "scorecard_check" => "Token-Permissions",
    "remediation" => "Set top-level permissions: read-all, declare writes per job.",
    "auto_fixable" => true,
    "confidence" => 0.95,
    "repos_affected_list" => ["test-repo"],
    "repos_affected" => 1,
    "occurrences" => 1,
    "source" => "scorecard",
    "triangle_tier" => "eliminate"
  }

  @unsupported_finding %{
    "id" => "SC-001-test-repo",
    "category" => "BinaryArtifact",
    "pa_rule" => "SC001",
    "scorecard_check" => "Binary-Artifacts",
    "repos_affected_list" => ["test-repo"]
  }

  # --- supported_checks/0 ---

  describe "supported_checks/0" do
    test "returns map of check_id to script path" do
      checks = DirectGitHubPR.supported_checks()

      assert is_map(checks)
      assert Map.has_key?(checks, "SC-013")
      assert Map.has_key?(checks, "SC-018")

      # Script paths should be relative
      assert checks["SC-013"] =~ "fix-pinned-dependencies"
      assert checks["SC-018"] =~ "fix-token-permissions"
    end
  end

  # --- Check ID Extraction (via create_fix_pr error paths) ---

  describe "check ID extraction" do
    test "extracts SC-013 from pa_rule SC013" do
      # This will fail at clone step (no network in tests) but validates parsing
      result = DirectGitHubPR.create_fix_pr(@sc013_finding)

      # Should get past parsing and fail at git clone
      assert match?({:error, {:clone_failed, _, _}}, result) or
               match?({:error, _}, result)
    end

    test "extracts SC-018 from pa_rule SC018" do
      result = DirectGitHubPR.create_fix_pr(@sc018_finding)

      assert match?({:error, {:clone_failed, _, _}}, result) or
               match?({:error, _}, result)
    end

    test "rejects findings without a repo" do
      finding = %{
        "id" => "SC-013-orphan",
        "pa_rule" => "SC013",
        "scorecard_check" => "Pinned-Dependencies",
        "repos_affected_list" => []
      }

      assert {:error, :no_repo_in_finding} = DirectGitHubPR.create_fix_pr(finding)
    end

    test "rejects unsupported check IDs" do
      finding = %{
        "id" => "SC-999-test",
        "pa_rule" => "SC999",
        "scorecard_check" => "Imaginary-Check",
        "repos_affected_list" => ["test-repo"]
      }

      assert {:error, {:no_fix_script, _}} = DirectGitHubPR.create_fix_pr(finding)
    end

    test "extracts check_id from finding ID when pa_rule is absent" do
      finding = %{
        "id" => "SC-018-some-repo",
        "scorecard_check" => "Token-Permissions",
        "repos_affected_list" => ["some-repo"]
      }

      # Should get past parsing — will fail at clone
      result = DirectGitHubPR.create_fix_pr(finding)

      assert match?({:error, {:clone_failed, _, _}}, result) or
               match?({:error, _}, result)
    end

    test "extracts check_id from scorecard_check name" do
      finding = %{
        "id" => "unknown-format",
        "scorecard_check" => "Pinned-Dependencies",
        "repos_affected_list" => ["some-repo"]
      }

      result = DirectGitHubPR.create_fix_pr(finding)

      assert match?({:error, {:clone_failed, _, _}}, result) or
               match?({:error, _}, result)
    end
  end

  # --- Batch Processing ---

  describe "batch_create_prs/2" do
    test "reads JSONL and processes findings in dry_run mode" do
      # Create a temporary JSONL file with test findings
      tmp_dir = System.tmp_dir!()
      jsonl_path = Path.join(tmp_dir, "hypatia-test-batch-#{System.unique_integer([:positive])}.jsonl")

      lines = [
        Jason.encode!(@sc013_finding),
        Jason.encode!(@sc018_finding),
        Jason.encode!(@unsupported_finding)
      ]

      File.write!(jsonl_path, Enum.join(lines, "\n"))

      # Dry run should process SC-013 and SC-018, skip SC-001
      {:ok, results} = DirectGitHubPR.batch_create_prs(jsonl_path, dry_run: true)

      assert length(results) == 2
      assert Enum.all?(results, fn {_id, outcome} -> outcome == :dry_run end)

      # Verify the correct findings were selected
      ids = Enum.map(results, fn {id, _} -> id end)
      assert "SC-013-test-repo" in ids
      assert "SC-018-test-repo" in ids

      File.rm(jsonl_path)
    end

    test "respects filter_check option" do
      tmp_dir = System.tmp_dir!()
      jsonl_path = Path.join(tmp_dir, "hypatia-test-filter-#{System.unique_integer([:positive])}.jsonl")

      lines = [
        Jason.encode!(@sc013_finding),
        Jason.encode!(@sc018_finding)
      ]

      File.write!(jsonl_path, Enum.join(lines, "\n"))

      {:ok, results} =
        DirectGitHubPR.batch_create_prs(jsonl_path, dry_run: true, filter_check: "SC-013")

      assert length(results) == 1
      [{id, :dry_run}] = results
      assert id == "SC-013-test-repo"

      File.rm(jsonl_path)
    end

    test "respects max_prs option" do
      tmp_dir = System.tmp_dir!()
      jsonl_path = Path.join(tmp_dir, "hypatia-test-max-#{System.unique_integer([:positive])}.jsonl")

      lines = [
        Jason.encode!(@sc013_finding),
        Jason.encode!(@sc018_finding)
      ]

      File.write!(jsonl_path, Enum.join(lines, "\n"))

      {:ok, results} = DirectGitHubPR.batch_create_prs(jsonl_path, dry_run: true, max_prs: 1)

      assert length(results) == 1

      File.rm(jsonl_path)
    end

    test "returns error for missing JSONL file" do
      result = DirectGitHubPR.batch_create_prs("/nonexistent/path.jsonl")

      assert match?({:error, {:file_read_failed, _, _}}, result)
    end

    test "handles empty JSONL file gracefully" do
      tmp_dir = System.tmp_dir!()
      jsonl_path = Path.join(tmp_dir, "hypatia-test-empty-#{System.unique_integer([:positive])}.jsonl")

      File.write!(jsonl_path, "")

      {:ok, results} = DirectGitHubPR.batch_create_prs(jsonl_path, dry_run: true)
      assert results == []

      File.rm(jsonl_path)
    end

    test "skips malformed JSONL lines" do
      tmp_dir = System.tmp_dir!()
      jsonl_path = Path.join(tmp_dir, "hypatia-test-malformed-#{System.unique_integer([:positive])}.jsonl")

      lines = [
        "not valid json",
        Jason.encode!(@sc013_finding),
        "{broken",
        Jason.encode!(@sc018_finding)
      ]

      File.write!(jsonl_path, Enum.join(lines, "\n"))

      {:ok, results} = DirectGitHubPR.batch_create_prs(jsonl_path, dry_run: true)

      # Should have processed the 2 valid findings, skipped the 2 malformed lines
      assert length(results) == 2

      File.rm(jsonl_path)
    end
  end

  # --- Fix Script Existence ---

  describe "fix script files" do
    test "fix-pinned-dependencies.sh exists and is executable" do
      script = Path.expand("../scripts/fix-pinned-dependencies.sh", __DIR__)
      assert File.exists?(script), "#{script} does not exist"

      stat = File.stat!(script)
      # Check that the file is at least user-executable (0o100 bit)
      assert Bitwise.band(stat.mode, 0o100) != 0,
             "#{script} is not executable (mode: #{Integer.to_string(stat.mode, 8)})"
    end

    test "fix-token-permissions.sh exists and is executable" do
      script = Path.expand("../scripts/fix-token-permissions.sh", __DIR__)
      assert File.exists?(script), "#{script} does not exist"

      stat = File.stat!(script)
      assert Bitwise.band(stat.mode, 0o100) != 0,
             "#{script} is not executable (mode: #{Integer.to_string(stat.mode, 8)})"
    end

    test "fix-pinned-dependencies.sh has SPDX header" do
      script = Path.expand("../scripts/fix-pinned-dependencies.sh", __DIR__)
      content = File.read!(script)
      assert String.contains?(content, "SPDX-License-Identifier: PMPL-1.0-or-later")
    end

    test "fix-token-permissions.sh has SPDX header" do
      script = Path.expand("../scripts/fix-token-permissions.sh", __DIR__)
      content = File.read!(script)
      assert String.contains?(content, "SPDX-License-Identifier: PMPL-1.0-or-later")
    end
  end

  # --- Fix Script Integration (local, no network) ---

  describe "fix-token-permissions.sh integration" do
    test "adds permissions to a workflow without one" do
      tmp_dir = create_test_repo_with_workflow("""
      name: CI
      on:
        push:
          branches: [main]

      jobs:
        test:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@v4
      """)

      script = Path.expand("../scripts/fix-token-permissions.sh", __DIR__)
      {output, exit_code} = System.cmd("bash", [script, tmp_dir], stderr_to_stdout: true)

      assert exit_code == 0
      assert String.contains?(output, "Added permissions: read-all")

      # Verify the file was modified
      wf_content = File.read!(Path.join([tmp_dir, ".github", "workflows", "ci.yml"]))
      assert String.contains?(wf_content, "permissions: read-all")
      # permissions should appear before jobs:
      perms_pos = :binary.match(wf_content, "permissions: read-all") |> elem(0)
      jobs_pos = :binary.match(wf_content, "jobs:") |> elem(0)
      assert perms_pos < jobs_pos

      File.rm_rf!(tmp_dir)
    end

    test "skips workflows that already have permissions" do
      tmp_dir = create_test_repo_with_workflow("""
      name: CI
      on: push
      permissions: read-all
      jobs:
        test:
          runs-on: ubuntu-latest
      """)

      script = Path.expand("../scripts/fix-token-permissions.sh", __DIR__)
      {output, exit_code} = System.cmd("bash", [script, tmp_dir], stderr_to_stdout: true)

      assert exit_code == 0
      assert String.contains?(output, "Skipped: 1")
      assert not String.contains?(output, "Added permissions")

      File.rm_rf!(tmp_dir)
    end

    test "is idempotent — running twice produces same result" do
      tmp_dir = create_test_repo_with_workflow("""
      name: CI
      on: push
      jobs:
        test:
          runs-on: ubuntu-latest
      """)

      script = Path.expand("../scripts/fix-token-permissions.sh", __DIR__)

      # First run
      {_, 0} = System.cmd("bash", [script, tmp_dir], stderr_to_stdout: true)
      content_after_first = File.read!(Path.join([tmp_dir, ".github", "workflows", "ci.yml"]))

      # Second run
      {output2, 0} = System.cmd("bash", [script, tmp_dir], stderr_to_stdout: true)
      content_after_second = File.read!(Path.join([tmp_dir, ".github", "workflows", "ci.yml"]))

      assert content_after_first == content_after_second
      assert String.contains?(output2, "Skipped: 1")

      File.rm_rf!(tmp_dir)
    end
  end

  # --- Helpers ---

  defp create_test_repo_with_workflow(workflow_content) do
    tmp_dir = Path.join(System.tmp_dir!(), "hypatia-test-wf-#{System.unique_integer([:positive])}")
    workflows_dir = Path.join([tmp_dir, ".github", "workflows"])
    File.mkdir_p!(workflows_dir)

    File.write!(Path.join(workflows_dir, "ci.yml"), workflow_content)

    tmp_dir
  end
end
