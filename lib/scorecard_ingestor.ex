# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ScorecardIngestor do
  @moduledoc """
  Ingests OpenSSF Scorecard check results and maps them to hypatia patterns.

  Rather than rediscovering GitHub/OpenSSF's 20 well-documented checks from
  first principles, this module ingests their definitions directly and maps
  each check to a hypatia pattern + recipe for the safety triangle pipeline.

  ## How it works

  1. Runs `scorecard` CLI (or reads cached results) against repos
  2. Maps each failing check to a canonical pattern ID (SC-001 through SC-020)
  3. Creates dispatch entries with appropriate confidence and fix strategies
  4. Feeds into the same TriangleRouter pipeline as scan-based patterns

  ## Scorecard Checks (20 total)

  | ID     | Check                  | Risk     | Auto-fixable? |
  |--------|------------------------|----------|---------------|
  | SC-001 | Binary-Artifacts       | High     | No            |
  | SC-002 | Branch-Protection      | High     | Partial       |
  | SC-003 | CI-Tests               | Low      | No            |
  | SC-004 | CII-Best-Practices     | Low      | No            |
  | SC-005 | Code-Review            | High     | No            |
  | SC-006 | Contributors           | Low      | No (info)     |
  | SC-007 | Dangerous-Workflow     | Critical | Yes           |
  | SC-008 | Dependency-Update-Tool | High     | Yes           |
  | SC-009 | Fuzzing                | Medium   | Partial       |
  | SC-010 | License                | Low      | Yes           |
  | SC-011 | Maintained             | High     | No (info)     |
  | SC-012 | Packaging              | Medium   | No            |
  | SC-013 | Pinned-Dependencies    | Medium   | Yes           |
  | SC-014 | SAST                   | Medium   | Yes           |
  | SC-015 | SBOM                   | Medium   | Partial       |
  | SC-016 | Security-Policy        | Medium   | Yes           |
  | SC-017 | Signed-Releases        | High     | No            |
  | SC-018 | Token-Permissions      | High     | Yes           |
  | SC-019 | Vulnerabilities        | High     | Partial       |
  | SC-020 | Webhooks               | Critical | No            |
  """

  require Logger

  @scorecard_checks %{
    # Categories aligned with recipe target_categories for automatic matching
    "Binary-Artifacts" => %{
      id: "SC-001", pa_rule: "SC001", risk: :high, auto_fixable: false,
      category: "BinaryArtifact",
      description: "Compiled binaries found in source repository",
      remediation: "Remove generated executable artifacts. Build from source."
    },
    "Branch-Protection" => %{
      id: "SC-002", pa_rule: "SC002", risk: :high, auto_fixable: false,
      category: "BranchProtection",
      description: "Default branch lacks protection rules",
      remediation: "Enable branch protection: require reviews, prevent force push."
    },
    "CI-Tests" => %{
      id: "SC-003", pa_rule: "SC003", risk: :low, auto_fixable: false,
      category: "ContinuousIntegration",
      description: "No CI test runs detected before merging PRs",
      remediation: "Add CI workflow that runs tests on pull requests."
    },
    "CII-Best-Practices" => %{
      id: "SC-004", pa_rule: "SC004", risk: :low, auto_fixable: false,
      category: "BestPracticesBadge",
      description: "No OpenSSF Best Practices Badge",
      remediation: "Sign up for the OpenSSF Best Practices program."
    },
    "Code-Review" => %{
      id: "SC-005", pa_rule: "SC005", risk: :high, auto_fixable: false,
      category: "CodeReview",
      description: "Code changes merged without review",
      remediation: "Require code review approvals before merging."
    },
    "Contributors" => %{
      id: "SC-006", pa_rule: "SC006", risk: :low, auto_fixable: false,
      category: "ContributorDiversity",
      description: "Limited organizational diversity in contributors",
      remediation: "Informational only -- no remediation needed."
    },
    "Dangerous-Workflow" => %{
      id: "SC-007", pa_rule: "SC007", risk: :critical, auto_fixable: true,
      category: "DangerousWorkflow",
      description: "GitHub Actions workflow contains dangerous patterns",
      remediation: "Remove untrusted code checkouts, avoid secret logging."
    },
    "Dependency-Update-Tool" => %{
      id: "SC-008", pa_rule: "SC008", risk: :high, auto_fixable: true,
      category: "DependencyUpdate",
      description: "No automated dependency update tool configured",
      remediation: "Add .github/dependabot.yml or renovate.json configuration."
    },
    "Fuzzing" => %{
      id: "SC-009", pa_rule: "SC009", risk: :medium, auto_fixable: false,
      category: "FuzzTesting",
      description: "No fuzz testing detected",
      remediation: "Integrate with OSS-Fuzz or add language-specific fuzzing."
    },
    "License" => %{
      id: "SC-010", pa_rule: "SC010", risk: :low, auto_fixable: true,
      category: "LicenseCompliance",
      description: "No license file found",
      remediation: "Add LICENSE file with SPDX identifier to repository root."
    },
    "Maintained" => %{
      id: "SC-011", pa_rule: "SC011", risk: :high, auto_fixable: false,
      category: "ProjectMaintenance",
      description: "Repository shows insufficient maintenance activity",
      remediation: "Informational -- indicates project health."
    },
    "Packaging" => %{
      id: "SC-012", pa_rule: "SC012", risk: :medium, auto_fixable: false,
      category: "PackagePublishing",
      description: "Project not published as installable package",
      remediation: "Add packaging workflow for relevant package registry."
    },
    "Pinned-Dependencies" => %{
      id: "SC-013", pa_rule: "SC013", risk: :medium, auto_fixable: true,
      category: "DependencyPinning",
      description: "Dependencies not pinned to specific hashes",
      remediation: "Pin GitHub Actions and Docker base images by SHA hash."
    },
    "SAST" => %{
      id: "SC-014", pa_rule: "SC014", risk: :medium, auto_fixable: true,
      category: "StaticAnalysis",
      description: "No static analysis security testing detected",
      remediation: "Add CodeQL or equivalent SAST workflow."
    },
    "SBOM" => %{
      id: "SC-015", pa_rule: "SC015", risk: :medium, auto_fixable: false,
      category: "SoftwareBOM",
      description: "No Software Bill of Materials found",
      remediation: "Generate SBOM using CycloneDX or SPDX tools."
    },
    "Security-Policy" => %{
      id: "SC-016", pa_rule: "SC016", risk: :medium, auto_fixable: true,
      category: "SecurityPolicy",
      description: "No vulnerability disclosure policy (SECURITY.md)",
      remediation: "Add SECURITY.md documenting how to report vulnerabilities."
    },
    "Signed-Releases" => %{
      id: "SC-017", pa_rule: "SC017", risk: :high, auto_fixable: false,
      category: "ReleaseSigning",
      description: "Release artifacts not cryptographically signed",
      remediation: "Sign releases with GPG/sigstore and attach signatures."
    },
    "Token-Permissions" => %{
      id: "SC-018", pa_rule: "SC018", risk: :high, auto_fixable: true,
      category: "TokenPermissions",
      description: "Workflow tokens not following least privilege",
      remediation: "Set top-level permissions: read-all, declare writes per job."
    },
    "Vulnerabilities" => %{
      id: "SC-019", pa_rule: "SC019", risk: :high, auto_fixable: false,
      category: "KnownVulnerabilities",
      description: "Open vulnerabilities in code or dependencies",
      remediation: "Fix code vulnerabilities or update affected dependencies."
    },
    "Webhooks" => %{
      id: "SC-020", pa_rule: "SC020", risk: :critical, auto_fixable: false,
      category: "WebhookSecurity",
      description: "Webhooks configured without authentication tokens",
      remediation: "Set webhook secret tokens for request authentication."
    }
  }

  @risk_to_confidence %{
    critical: 0.99,
    high: 0.95,
    medium: 0.90,
    low: 0.85
  }

  @doc """
  Get all Scorecard check definitions.
  Returns a map of check_name => check_definition.
  """
  def checks, do: @scorecard_checks

  @doc """
  Parse Scorecard JSON output and convert failing checks to hypatia patterns.

  Accepts the JSON output from `scorecard --format json` and returns a list
  of pattern maps compatible with PatternRegistry and TriangleRouter.
  """
  def parse_scorecard_results(scorecard_json, repo_name) when is_map(scorecard_json) do
    checks = Map.get(scorecard_json, "checks", [])

    checks
    |> Enum.filter(fn check ->
      score = Map.get(check, "score", -1)
      score >= 0 and score < 8
    end)
    |> Enum.map(fn check -> check_to_pattern(check, repo_name) end)
    |> Enum.reject(&is_nil/1)
  end

  def parse_scorecard_results(_, _), do: []

  @doc """
  Run Scorecard against a GitHub repo and return failing check patterns.

  Requires the `scorecard` CLI to be installed and a GITHUB_AUTH_TOKEN
  env var for API access.
  """
  def scan_repo(repo_name) do
    github_url = "https://github.com/hyperpolymath/#{repo_name}"

    case System.cmd("scorecard", ["--repo", github_url, "--format", "json"],
           stderr_to_stdout: true,
           env: [{"SCORECARD_V6", "1"}]) do
      {output, 0} ->
        case Jason.decode(output) do
          {:ok, json} ->
            patterns = parse_scorecard_results(json, repo_name)
            {:ok, patterns}

          {:error, reason} ->
            Logger.error("Failed to parse scorecard output for #{repo_name}: #{inspect(reason)}")
            {:error, :parse_failed}
        end

      {output, code} ->
        Logger.warning("Scorecard exited #{code} for #{repo_name}: #{String.slice(output, 0, 200)}")
        {:error, {:exit_code, code}}
    end
  end

  @doc """
  Generate Scorecard patterns for a repo by inspecting local files directly.

  This is faster than running the full scorecard CLI -- it checks for the
  most common/fixable issues by looking at the repo's file structure:
  - SECURITY.md present?
  - Workflow permissions set to read-all?
  - Dependencies pinned by hash?
  - Dependabot/Renovate configured?
  - LICENSE file present?
  - CodeQL/SAST workflow present?
  """
  def local_scan(repo_path, repo_name) do
    checks = [
      &check_security_policy/2,
      &check_token_permissions/2,
      &check_dependency_update_tool/2,
      &check_license/2,
      &check_sast/2,
      &check_pinned_dependencies/2
    ]

    patterns =
      checks
      |> Enum.map(fn check_fn -> check_fn.(repo_path, repo_name) end)
      |> Enum.reject(&is_nil/1)

    {:ok, patterns}
  end

  @doc """
  Batch local scan across multiple repos.
  Returns {patterns, stats} where stats counts pass/fail per check.
  """
  def batch_local_scan(repo_paths) when is_list(repo_paths) do
    results =
      repo_paths
      |> Enum.flat_map(fn {repo_name, repo_path} ->
        case local_scan(repo_path, repo_name) do
          {:ok, patterns} -> patterns
          _ -> []
        end
      end)

    stats = %{
      total_repos: length(repo_paths),
      total_findings: length(results),
      by_check: results |> Enum.frequencies_by(fn p -> Map.get(p, "category") end)
    }

    {results, stats}
  end

  # --- Local Check Implementations ---

  defp check_security_policy(repo_path, repo_name) do
    security_paths = [
      Path.join(repo_path, "SECURITY.md"),
      Path.join([repo_path, ".github", "SECURITY.md"]),
      Path.join(repo_path, "security.md")
    ]

    unless Enum.any?(security_paths, &File.exists?/1) do
      make_pattern("SC-016", "Security-Policy", repo_name,
        "No SECURITY.md found in #{repo_name}")
    end
  end

  defp check_token_permissions(repo_path, repo_name) do
    workflows_dir = Path.join([repo_path, ".github", "workflows"])

    case File.ls(workflows_dir) do
      {:ok, files} ->
        yml_files = Enum.filter(files, fn f ->
          String.ends_with?(f, ".yml") or String.ends_with?(f, ".yaml")
        end)

        missing_perms =
          yml_files
          |> Enum.filter(fn f ->
            path = Path.join(workflows_dir, f)
            case File.read(path) do
              {:ok, content} -> not String.contains?(content, "permissions:")
              _ -> false
            end
          end)

        if length(missing_perms) > 0 do
          make_pattern("SC-018", "Token-Permissions", repo_name,
            "#{length(missing_perms)} workflow(s) missing permissions declaration in #{repo_name}")
        end

      _ -> nil
    end
  end

  defp check_dependency_update_tool(repo_path, repo_name) do
    dependabot = Path.join([repo_path, ".github", "dependabot.yml"])
    renovate1 = Path.join(repo_path, "renovate.json")
    renovate2 = Path.join(repo_path, ".renovaterc.json")

    unless File.exists?(dependabot) or File.exists?(renovate1) or File.exists?(renovate2) do
      make_pattern("SC-008", "Dependency-Update-Tool", repo_name,
        "No dependabot.yml or renovate.json found in #{repo_name}")
    end
  end

  defp check_license(repo_path, repo_name) do
    license_paths = [
      Path.join(repo_path, "LICENSE"),
      Path.join(repo_path, "LICENSE.txt"),
      Path.join(repo_path, "LICENSE.md"),
      Path.join(repo_path, "LICENCE"),
      Path.join(repo_path, "COPYING")
    ]

    unless Enum.any?(license_paths, &File.exists?/1) do
      make_pattern("SC-010", "License", repo_name,
        "No LICENSE file found in #{repo_name}")
    end
  end

  defp check_sast(repo_path, repo_name) do
    workflows_dir = Path.join([repo_path, ".github", "workflows"])

    files =
      case File.ls(workflows_dir) do
        {:ok, fs} -> fs
        _ -> []
      end

    contents =
      files
      |> Enum.map(fn f -> File.read(Path.join(workflows_dir, f)) end)
      |> Enum.flat_map(fn
        {:ok, c} -> [c]
        _ -> []
      end)

    has_non_codeql_sast =
      Enum.any?(contents, fn c ->
        String.contains?(c, "sonarcloud") or String.contains?(c, "semgrep") or
          String.contains?(c, "snyk")
      end)

    codeql_content =
      Enum.find(contents, fn c ->
        String.contains?(c, "codeql") or String.contains?(c, "CodeQL")
      end)

    cond do
      # No SAST tool of any kind — the original SC-014 missing finding.
      is_nil(codeql_content) and not has_non_codeql_sast ->
        make_pattern("SC-014", "SAST", repo_name,
          "No SAST tool (CodeQL/SonarCloud/Semgrep) detected in #{repo_name}")

      # Effective-vs-nominal: CodeQL workflow PRESENT but pointed at a
      # language the repo does not contain (and not `actions`). It runs
      # but records zero results, so Scorecard reports "0 commits checked"
      # even though a SAST workflow exists. Presence != efficacy.
      # Refs hyperpolymath/hypatia#261 (generalises modshells #72).
      not is_nil(codeql_content) and not has_non_codeql_sast and
          not codeql_effective?(codeql_content, repo_path) ->
        make_pattern("SC-014", "SAST", repo_name,
          "Nominal-only SAST in #{repo_name}: codeql.yml language matrix " <>
            "contains no language present in the repo and lacks `actions`, " <>
            "so CodeQL records zero results on every commit. " <>
            "Remediation: set the CodeQL matrix to `language: actions`.")

      true ->
        nil
    end
  end

  # A CodeQL workflow is *effective* if its language matrix includes
  # `actions` (always scannable — workflow YAML exists in every repo) or
  # at least one CodeQL-source language the repo actually contains.
  defp codeql_effective?(codeql_content, repo_path) do
    matrix = codeql_matrix_languages(codeql_content)

    "actions" in matrix or
      Enum.any?(matrix, fn lang -> lang in repo_codeql_languages(repo_path) end)
  end

  defp codeql_matrix_languages(content) do
    Regex.scan(~r/language:\s*([a-z0-9-]+)/i, content)
    |> Enum.map(fn [_, l] -> String.downcase(l) end)
    |> Enum.uniq()
  end

  defp repo_codeql_languages(repo_path) do
    case System.cmd("find", [repo_path, "-type", "f",
                             "-not", "-path", "*/.git/*",
                             "-not", "-path", "*/node_modules/*",
                             "-not", "-path", "*/_build/*",
                             "-not", "-path", "*/deps/*",
                             "-not", "-path", "*/target/*"],
                    stderr_to_stdout: true) do
      {out, 0} ->
        out
        |> String.split("\n", trim: true)
        |> Enum.map(&Path.extname/1)
        |> Enum.map(&Hypatia.Rules.SecurityErrors.codeql_language_for_ext/1)
        |> Enum.reject(&is_nil/1)
        |> Enum.uniq()

      _ ->
        []
    end
  end

  defp check_pinned_dependencies(repo_path, repo_name) do
    workflows_dir = Path.join([repo_path, ".github", "workflows"])

    case File.ls(workflows_dir) do
      {:ok, files} ->
        yml_files = Enum.filter(files, fn f ->
          String.ends_with?(f, ".yml") or String.ends_with?(f, ".yaml")
        end)

        unpinned =
          yml_files
          |> Enum.filter(fn f ->
            path = Path.join(workflows_dir, f)
            case File.read(path) do
              {:ok, content} ->
                # Check for actions using tags instead of SHA hashes
                # Match pattern: uses: owner/repo@v1 (tag) vs uses: owner/repo@abc123 (SHA)
                Regex.match?(~r/uses:\s+[\w-]+\/[\w-]+@v\d/, content)
              _ -> false
            end
          end)

        if length(unpinned) > 0 do
          make_pattern("SC-013", "Pinned-Dependencies", repo_name,
            "#{length(unpinned)} workflow(s) with tag-pinned (not SHA-pinned) actions in #{repo_name}")
        end

      _ -> nil
    end
  end

  # --- Pattern Builder ---

  defp make_pattern(sc_id, check_name, repo_name, description) do
    check_def = Map.get(@scorecard_checks, check_name, %{})
    risk = Map.get(check_def, :risk, :medium)
    confidence = Map.get(@risk_to_confidence, risk, 0.90)

    %{
      "id" => "#{sc_id}-#{fingerprint(repo_name)}",
      "category" => Map.get(check_def, :category, "Scorecard-Unknown"),
      "severity" => risk_to_severity(risk),
      "description" => description,
      "pa_rule" => Map.get(check_def, :pa_rule, "SC000"),
      "scorecard_check" => check_name,
      "remediation" => Map.get(check_def, :remediation, ""),
      "auto_fixable" => Map.get(check_def, :auto_fixable, false),
      "confidence" => confidence,
      "repos_affected_list" => [repo_name],
      "repos_affected" => 1,
      "occurrences" => 1,
      "source" => "scorecard",
      "triangle_tier" => if(Map.get(check_def, :auto_fixable, false), do: "eliminate", else: "control")
    }
  end

  defp check_to_pattern(check, repo_name) do
    name = Map.get(check, "name", "")
    score = Map.get(check, "score", -1)
    reason = Map.get(check, "reason", "")

    case Map.get(@scorecard_checks, name) do
      nil -> nil
      check_def ->
        risk = Map.get(check_def, :risk, :medium)

        %{
          "id" => "#{check_def.id}-#{fingerprint(repo_name)}",
          "category" => check_def.category,
          "severity" => risk_to_severity(risk),
          "description" => "#{name}: #{reason} (score: #{score}/10)",
          "pa_rule" => check_def.pa_rule,
          "scorecard_check" => name,
          "scorecard_score" => score,
          "remediation" => check_def.remediation,
          "auto_fixable" => check_def.auto_fixable,
          "confidence" => Map.get(@risk_to_confidence, risk, 0.90),
          "repos_affected_list" => [repo_name],
          "repos_affected" => 1,
          "occurrences" => 1,
          "source" => "scorecard",
          "triangle_tier" => if(check_def.auto_fixable, do: "eliminate", else: "control")
        }
    end
  end

  defp risk_to_severity(:critical), do: "Critical"
  defp risk_to_severity(:high), do: "High"
  defp risk_to_severity(:medium), do: "Medium"
  defp risk_to_severity(:low), do: "Low"
  defp risk_to_severity(_), do: "Medium"

  defp fingerprint(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/, "-")
    |> String.trim("-")
    |> String.slice(0, 20)
  end
end
