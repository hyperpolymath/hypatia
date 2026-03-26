# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ScorecardCompliance do
  @moduledoc """
  Enforces OpenSSF Scorecard best practices as automated Elixir rules.
  
  These rules specifically target the 'High Risk' items flagged by Scorecard
  to prevent quality gate failures before they reach the remote repository.
  """

  # ─── Scorecard Critical Metrics ──────────────────────────────────────
  #
  # High-risk metric categories tracked by this module:
  # :token_permissions, :pinned_dependencies, :dependency_update_tool,
  # :code_review_enforcement, :maintained_status, :fuzzing_integration

  @doc """
  Audits a repository for Scorecard compliance markers.
  """
  def audit(repo_path, file_list, file_contents \\ %{}) do
    [
      check_least_privilege(file_contents),
      check_dependency_tool(repo_path, file_list),
      check_vulnerability_scanning(file_list),
      check_fuzzing(file_list),
      check_cii_best_practices(file_list)
    ]
    |> List.flatten()
  end

  @doc """
  Check for automated vulnerability/secret scanning (Vulnerability-Scanning).
  """
  def check_vulnerability_scanning(file_list) do
    has_scanner = Enum.any?(file_list, fn f -> 
      String.contains?(f, "secret-scanner.yml") or 
      String.contains?(f, "gitleaks") or 
      String.contains?(f, "trufflehog")
    end)

    if has_scanner do
      []
    else
      [%{type: :scorecard_violation, metric: :vulnerability_scanning, file: ".github/workflows/secret-scanner.yml",
         severity: :high, detail: "Missing automated secret/vulnerability scanning. Gitleaks or TruffleHog integration is required."}]
    end
  end

  @doc """
  Check for excessive workflow permissions (Token-Permissions).
  Mandates specific permission blocks instead of 'read-all' or 'write-all'.
  """
  def check_least_privilege(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      cond do
        String.contains?(content, "permissions: write-all") ->
          [%{type: :scorecard_violation, metric: :token_permissions, file: filename,
             severity: :critical, detail: "Dangerous 'write-all' permissions detected. Use granular permissions."}]
        
        String.contains?(content, "permissions: read-all") ->
          [%{type: :scorecard_violation, metric: :token_permissions, file: filename,
             severity: :high, detail: "Broad 'read-all' permissions detected. Scorecard mandates granular blocks (e.g., contents: read)."}]
        
        true -> []
      end
    end)
  end

  @doc """
  Check for automated dependency update configuration (Dependency-Update-Tool).
  """
  def check_dependency_tool(_repo_path, file_list) do
    has_dependabot = Enum.any?(file_list, fn f -> 
      String.ends_with?(f, ".github/dependabot.yml") or String.ends_with?(f, ".github/dependabot.yaml")
    end)

    if has_dependabot do
      []
    else
      [%{type: :scorecard_violation, metric: :dependency_update_tool, file: ".github/dependabot.yml",
         severity: :high, detail: "Missing Dependabot configuration. Automated dependency updates are required."}]
    end
  end

  @doc """
  Check for Fuzzing integration.
  """
  def check_fuzzing(file_list) do
    has_fuzzing = Enum.any?(file_list, fn f -> 
      String.contains?(f, "fuzz") or String.contains?(f, "clusterfuzz")
    end)

    if has_fuzzing do
      []
    else
      [%{type: :scorecard_violation, metric: :fuzzing, file: "tests/fuzz/",
         severity: :medium, detail: "No fuzz testing detected. Consider adding ClusterFuzzLite or similar."}]
    end
  end

  @doc """
  Check for CII Best Practices badge (or entry point for the badge).
  """
  def check_cii_best_practices(file_list) do
    has_marker = Enum.any?(file_list, fn f -> 
      String.ends_with?(f, "CII-ENFORCEMENT.adoc") or String.contains?(f, "best-practices.coreinfrastructure.org")
    end)

    if has_marker do
      []
    else
      [%{type: :scorecard_violation, metric: :cii_best_practices, file: "README.md",
         severity: :low, detail: "Missing CII Best Practices marker or badge."}]
    end
  end
end
