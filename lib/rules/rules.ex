# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules do
  @moduledoc """
  Facade module for all Hypatia rules, absorbed from the Logtalk engine.

  This replaces the need for Logtalk rule evaluation by providing
  all policy rules, security checks, and learning operations directly
  in Elixir. The Logtalk files in engine/ remain as documentation of
  the original rule declarations.

  ## Submodules

  - `SecurityErrors` — Error categories, SHA pins, secret detection, CodeQL, CWE mappings
  - `CicdRules` — Repo requirements, commit blocking, waste detection, license validation
  - `CodeSafety` — Language-specific dangerous pattern detection
  - `MigrationRules` — ReScript v12→v13 migration, merge conflict resolution
  - `Learning` — Fix outcome tracking, confidence, pattern promotion (GenServer)
  - `ForgeAdapters` — Multi-forge operations with input validation
  """

  alias Hypatia.Rules.SecurityErrors
  alias Hypatia.Rules.CicdRules
  alias Hypatia.Rules.CodeSafety
  alias Hypatia.Rules.MigrationRules

  @doc """
  Run a comprehensive scan on a file's content given its path and language.
  Returns a list of all findings across all rule categories.
  """
  def scan_file(content, file_path, language) do
    findings = []

    # Code safety patterns
    findings = findings ++ CodeSafety.scan_content(content, language)

    # Secret detection
    secrets = SecurityErrors.detect_secrets(content)
    findings = findings ++ Enum.map(secrets, fn label ->
      %{rule: :secret_detected, severity: :critical, description: "Secret found: #{label}"}
    end)

    # SPDX header check
    findings =
      if not String.contains?(content, "SPDX-License-Identifier:") and
           file_path not in [".gitignore", "LICENSE", "LICENSE.txt"] do
        [%{rule: :missing_spdx, severity: :medium,
           description: "Missing SPDX-License-Identifier header"} | findings]
      else
        findings
      end

    # License check (if SPDX present)
    findings =
      case Regex.run(~r/SPDX-License-Identifier:\s*(.+)/, content) do
        [_, spdx_id] ->
          case CicdRules.validate_license(String.trim(spdx_id)) do
            {:error, :wrong_license, bad} ->
              [%{rule: :wrong_license, severity: :high,
                 description: "Wrong license #{bad} — should be PMPL-1.0-or-later"} | findings]
            _ -> findings
          end
        _ -> findings
      end

    # Deprecated ReScript API check
    findings =
      if language == "rescript" do
        findings ++ Enum.map(MigrationRules.scan_deprecated_usage(content), fn dep ->
          %{rule: :deprecated_api, severity: dep.severity,
            description: "#{dep.api} deprecated — use #{dep.replacement} (#{dep.count} occurrences)"}
        end)
      else
        findings
      end

    findings
  end

  @doc """
  Check a workflow YAML file for common issues.
  Returns a list of findings.
  """
  def scan_workflow(content) do
    findings = []

    # Unpinned actions
    unpinned = Regex.scan(~r/uses:\s*([^\s]+@v\d+)/, content)
    findings = findings ++ Enum.map(unpinned, fn [_full, action_ref] ->
      suggestion = case SecurityErrors.pin_action(action_ref) do
        {:ok, pinned} -> " — fix: #{pinned}"
        _ -> ""
      end
      %{rule: :unpinned_action, severity: :high,
        description: "Unpinned action: #{action_ref}#{suggestion}"}
    end)

    # Missing permissions
    findings =
      if not Regex.match?(~r/^permissions:/m, content) do
        [%{rule: :missing_permissions, severity: :high,
           description: "Workflow missing permissions declaration — add permissions: read-all"} | findings]
      else
        findings
      end

    findings
  end

  @doc """
  Get the SHA pin for a GitHub Action reference, or nil if unknown.
  """
  defdelegate pin_action(action_ref), to: SecurityErrors

  @doc """
  Check if an issue type can be auto-fixed.
  """
  defdelegate auto_fixable?(issue_type), to: SecurityErrors

  @doc """
  Get the fix suggestion for an issue type.
  """
  defdelegate fix_suggestion(issue_type), to: SecurityErrors

  @doc """
  Get the prevention workflow for an issue type.
  """
  defdelegate prevention_workflow(issue_type), to: SecurityErrors

  @doc """
  Validate a CodeQL language matrix against repo languages.
  """
  defdelegate validate_codeql_matrix(repo_languages), to: SecurityErrors

  @doc """
  Check repo requirements (SECURITY.md, dependabot.yml, etc.)
  """
  defdelegate check_repo_requirements(repo_info), to: CicdRules

  @doc """
  Detect CI/CD waste patterns.
  """
  defdelegate detect_waste(repo_info), to: CicdRules
end
