# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.CicdRules do
  @moduledoc """
  CI/CD policy rules absorbed from Logtalk engine/rules/cicd_rules.lgt.

  Defines repository must-have rules, commit blocking conditions,
  auto-fix capabilities, CI/CD waste detection, and license validation.
  """

  # ---------------------------------------------------------------------------
  # Repository Must-Have Rules
  # ---------------------------------------------------------------------------

  @repo_must_have [
    %{file: "SECURITY.md", condition: :public_repo},
    %{file: ".github/dependabot.yml", condition: :has_dependencies},
    %{file: ".github/workflows/scorecard.yml", condition: :public_repo},
    %{file: "permissions: read-all", condition: :all_workflows}
  ]

  def repo_must_have_rules, do: @repo_must_have

  def check_repo_requirements(repo_info) do
    Enum.flat_map(@repo_must_have, fn rule ->
      if requirement_applies?(rule.condition, repo_info) and
           not has_file?(repo_info, rule.file) do
        [%{missing: rule.file, condition: rule.condition}]
      else
        []
      end
    end)
  end

  defp requirement_applies?(:public_repo, info), do: Map.get(info, :visibility) == "public"
  defp requirement_applies?(:has_dependencies, info), do: Map.get(info, :has_deps, true)
  defp requirement_applies?(:all_workflows, _info), do: true

  defp has_file?(info, file), do: file in Map.get(info, :files, [])

  # ---------------------------------------------------------------------------
  # Commit Blocking Patterns
  # ---------------------------------------------------------------------------

  @blocked_patterns [
    %{id: :typescript_detected, glob: "*.ts", reason: "TypeScript banned — use ReScript"},
    %{id: :nodejs_detected, glob: "package-lock.json", reason: "Node.js banned — use Deno"},
    %{id: :golang_detected, glob: "*.go", reason: "Go banned — use Rust"},
    %{id: :python_detected, glob: "*.py", reason: "Python banned — use Julia/Rust",
      exception: "SaltStack"},
    %{id: :makefile_detected, glob: "Makefile", reason: "Makefiles banned — use justfile"},
    %{id: :unpinned_action, pattern: ~r/uses:.*@v[0-9]/, reason: "GitHub Actions must be SHA-pinned"},
    %{id: :missing_permissions, pattern: ~r/^permissions:/m, negative: true,
      reason: "Workflows must declare permissions"},
    %{id: :missing_spdx, pattern: ~r/^# SPDX-License-Identifier:/m, negative: true,
      reason: "Files must have SPDX headers"},
    # --- Rules derived from 2026-03-16 session dogfooding ---
    %{id: :agpl_license, pattern: ~r/SPDX-License-Identifier:\s*AGPL-3\.0/,
      reason: "AGPL-3.0 replaced by PMPL-1.0-or-later"},
    %{id: :innerhtml_usage, pattern: ~r/\.innerHTML\s*=|document\.write\(/,
      reason: "innerHTML/document.write banned — use rescript-dom-mounter SafeDOM",
      applies_to: ["*.js", "*.res"]},
    %{id: :eval_in_shell, pattern: ~r/\beval\b/,
      reason: "eval banned in shell scripts — use direct expansion or arrays",
      applies_to: ["*.sh"]},
    %{id: :hardcoded_tmp, pattern: ~r/["'\/]tmp\//,
      reason: "Hardcoded /tmp/ paths — use mktemp",
      applies_to: ["*.sh"], exception: "Containerfile"},
    %{id: :template_placeholder, pattern: ~r/\{\{(REPO|OWNER|FORGE|AUTHOR)\}\}/,
      reason: "Unfilled RSR template placeholder",
      exception: "rsr-template-repo"},
    %{id: :deno_all_perms, pattern: ~r/deno\s+run\s+-A\b/,
      reason: "Deno -A (all permissions) banned — use specific --allow-* flags"},
    %{id: :mu_plugin_no_guard, pattern: ~r/define\(\s*['"]WP_DEBUG['"]/,
      reason: "WordPress mu-plugins must guard constants with defined() check",
      applies_to: ["*/mu-plugins/*.php"]}
  ]

  def blocked_patterns, do: @blocked_patterns

  def check_commit_blocks(files_changed) do
    Enum.flat_map(@blocked_patterns, fn pattern ->
      matches = check_pattern(pattern, files_changed)
      if matches != [], do: [%{rule: pattern.id, reason: pattern.reason, files: matches}], else: []
    end)
  end

  defp check_pattern(%{glob: glob} = _pattern, files) do
    Enum.filter(files, fn f -> String.ends_with?(f, String.replace(glob, "*", "")) end)
  end

  defp check_pattern(%{pattern: _regex}, _files), do: []

  # ---------------------------------------------------------------------------
  # CI/CD Waste Detection
  # ---------------------------------------------------------------------------

  @waste_patterns [
    %{id: :duplicate_workflow, severity: :medium, auto_fixable: true,
      description: "Multiple workflows doing same thing"},
    %{id: :unused_publish_workflows, severity: :low, auto_fixable: true,
      description: "5+ platform-specific publish workflows"},
    %{id: :mirror_missing_secrets, severity: :medium, auto_fixable: true,
      description: "mirror.yml exists but no GITLAB_SSH_KEY/BITBUCKET_SSH_KEY"},
    %{id: :npm_in_workflow, severity: :high, auto_fixable: true,
      description: "npm install/pnpm despite npm-bun-blocker.yml"},
    %{id: :spec_repo_full_ci, severity: :medium, auto_fixable: true,
      description: "Spec-only repo with 10+ workflows"},
    %{id: :semgrep_language_mismatch, severity: :medium, auto_fixable: true,
      description: "semgrep.yml in repos without Python/Go/JS"},
    %{id: :excessive_workflow_count, severity: :low, auto_fixable: false,
      description: "More than 15 workflows — consolidate"},
    %{id: :missing_directory_workflow, severity: :low, auto_fixable: true,
      description: "zig-ffi.yml workflow but no zig/ directory"},

    # Workflow hygiene: irrelevant template workflows
    %{id: :irrelevant_ts_blocker, severity: :low, auto_fixable: true,
      description: "ts-blocker.yml in repo with no TypeScript or JavaScript"},
    %{id: :irrelevant_npm_blocker, severity: :low, auto_fixable: true,
      description: "npm-bun-blocker.yml in repo with no JS package ecosystem"},
    %{id: :irrelevant_jekyll, severity: :low, auto_fixable: true,
      description: "Jekyll workflow in repo with no _config.yml or Gemfile"},
    %{id: :irrelevant_guix_nix, severity: :low, auto_fixable: true,
      description: "guix-nix-policy.yml in repo with no Guix or Nix configuration"},
    %{id: :irrelevant_wellknown, severity: :low, auto_fixable: true,
      description: "wellknown-enforcement.yml in repo with no .well-known/ directory"},
    %{id: :irrelevant_rsr_antipattern, severity: :low, auto_fixable: true,
      description: "rsr-antipattern.yml in repo without RSR markers"},
    %{id: :redundant_scorecard_enforcer, severity: :low, auto_fixable: true,
      description: "scorecard-enforcer.yml redundant with scorecard.yml"},
    %{id: :redundant_instant_sync, severity: :low, auto_fixable: true,
      description: "instant-sync.yml redundant with mirror.yml"},
    %{id: :redundant_security_policy_wf, severity: :info, auto_fixable: true,
      description: "security-policy.yml checking for SECURITY.md that already exists"},

    # Missing language-appropriate CI
    %{id: :missing_julia_ci, severity: :high, auto_fixable: true,
      description: "Julia package without CI running Pkg.test()"},
    %{id: :missing_rust_ci, severity: :high, auto_fixable: true,
      description: "Rust crate without CI running cargo test"},
    %{id: :missing_elixir_ci, severity: :medium, auto_fixable: true,
      description: "Elixir project without CI running mix test"},
    %{id: :missing_zig_ci, severity: :medium, auto_fixable: true,
      description: "Zig project without CI running zig build test"},

    # --- Rules derived from 2026-03-18 session: Maximize Value ---
    %{id: :missing_github_actions_dependabot, severity: :medium, auto_fixable: true,
      description: "Dependabot missing github-actions ecosystem"},
    %{id: :missing_workflow_caching, severity: :low, auto_fixable: true,
      description: "Setup action missing built-in caching (setup-node, setup-python, setup-zig, etc.)"}
  ]

  def waste_patterns, do: @waste_patterns

  def detect_waste(repo_info) do
    workflows = Map.get(repo_info, :workflows, [])
    dirs = Map.get(repo_info, :directories, [])

    results = []

    results =
      if length(workflows) > 15 do
        [%{pattern: :excessive_workflow_count, count: length(workflows)} | results]
      else
        results
      end

    results =
      if "zig-ffi.yml" in workflows and "zig/" not in dirs and "ffi/zig/" not in dirs do
        [%{pattern: :missing_directory_workflow, workflow: "zig-ffi.yml"} | results]
      else
        results
      end

    results =
      if "mirror.yml" in workflows do
        secrets = Map.get(repo_info, :secrets, [])

        if "GITLAB_SSH_KEY" not in secrets and "BITBUCKET_SSH_KEY" not in secrets do
          [%{pattern: :mirror_missing_secrets} | results]
        else
          results
        end
      else
        results
      end

    # Workflow hygiene: detect irrelevant workflows based on repo tech stack
    languages = Map.get(repo_info, :languages, [])
    files = Map.get(repo_info, :files, [])

    irrelevant_checks = [
      {:irrelevant_ts_blocker, "ts-blocker.yml",
        fn -> "typescript" not in languages and "javascript" not in languages end},
      {:irrelevant_npm_blocker, "npm-bun-blocker.yml",
        fn -> "package.json" not in files and "package-lock.json" not in files end},
      {:irrelevant_jekyll, "jekyll.yml",
        fn -> "_config.yml" not in files and "Gemfile" not in files end},
      {:irrelevant_jekyll, "jekyll-gh-pages.yml",
        fn -> "_config.yml" not in files and "Gemfile" not in files end},
      {:irrelevant_guix_nix, "guix-nix-policy.yml",
        fn -> "flake.nix" not in files and "manifest.scm" not in files end},
      {:irrelevant_wellknown, "wellknown-enforcement.yml",
        fn -> ".well-known" not in dirs end},
      {:redundant_scorecard_enforcer, "scorecard-enforcer.yml",
        fn -> "scorecard.yml" in workflows end},
      {:redundant_instant_sync, "instant-sync.yml",
        fn -> "mirror.yml" in workflows end}
    ]

    results =
      Enum.reduce(irrelevant_checks, results, fn {pattern, wf, check_fn}, acc ->
        if wf in workflows and check_fn.() do
          [%{pattern: pattern, workflow: wf, auto_fixable: true} | acc]
        else
          acc
        end
      end)

    # Missing language-appropriate CI
    has_ci = Enum.any?(workflows, fn w -> w in ["ci.yml", "CI.yml", "test.yml"] end)

    results =
      if "Project.toml" in files and not has_ci do
        [%{pattern: :missing_julia_ci, auto_fixable: true} | results]
      else
        results
      end

    results =
      if "Cargo.toml" in files and not has_ci and "rust.yml" not in workflows and "build.yml" not in workflows do
        [%{pattern: :missing_rust_ci, auto_fixable: true} | results]
      else
        results
      end

    results =
      if "mix.exs" in files and not has_ci and "elixir.yml" not in workflows do
        [%{pattern: :missing_elixir_ci, auto_fixable: true} | results]
      else
        results
      end

    results
  end

  # ---------------------------------------------------------------------------
  # License Validation
  # ---------------------------------------------------------------------------

  @required_spdx "PMPL-1.0-or-later"
  @wrong_licenses ["MIT", "Apache-2.0", "PMPL-1.0-or-later", "AGPL-3.0", "GPL-3.0"]

  def required_spdx, do: @required_spdx

  def validate_license(spdx_id) do
    cond do
      spdx_id == @required_spdx -> :ok
      spdx_id == "MPL-2.0" -> :ok_fallback
      spdx_id in @wrong_licenses -> {:error, :wrong_license, spdx_id}
      true -> {:warning, :unknown_license, spdx_id}
    end
  end

  # ---------------------------------------------------------------------------
  # Error Catalog IDs
  # ---------------------------------------------------------------------------

  @error_catalog %{
    "ERR-WF-001" => %{type: :unpinned_action, severity: :high,
      detection: [:workflow_linter, :grep_pattern],
      prevention: [:pre_commit_hook, :ci_check, :code_review]},
    "ERR-WF-002" => %{type: :missing_permissions, severity: :high,
      detection: [:workflow_linter, :grep_pattern],
      prevention: [:pre_commit_hook, :ci_check, :template]},
    "ERR-WF-003" => %{type: :missing_spdx, severity: :medium,
      detection: [:grep_pattern],
      prevention: [:pre_commit_hook, :template]},
    "ERR-WF-004" => %{type: :codeql_mismatch, severity: :medium,
      detection: [:workflow_run_failure, :manual_review],
      prevention: [:language_detection_hook, :ci_check]},
    "ERR-WF-005" => %{type: :duplicate_workflow, severity: :low,
      detection: [:manual_review, :file_comparison],
      prevention: [:template_standardization]},
    "ERR-WF-006" => %{type: :undefined_secret, severity: :high,
      detection: [:workflow_run_failure],
      prevention: [:secret_validation_hook, :conditional_guards]},
    "ERR-WF-007" => %{type: :empty_workflow, severity: :low,
      detection: [:file_size_check],
      prevention: [:template_cleanup]},
    "ERR-WF-008" => %{type: :irrelevant_workflow, severity: :low,
      detection: [:workflow_hygiene_scan, :language_detection],
      prevention: [:template_customization, :hypatia_scan]},
    "ERR-WF-009" => %{type: :redundant_workflow, severity: :low,
      detection: [:workflow_hygiene_scan],
      prevention: [:template_customization]},
    "ERR-WF-010" => %{type: :missing_language_ci, severity: :high,
      detection: [:workflow_hygiene_scan, :language_detection],
      prevention: [:template_ci_generation, :hypatia_scan]},
    "ERR-DEP-001" => %{type: :vulnerable_dependency, severity: :critical,
      detection: [:dependabot, :trivy_scan],
      prevention: [:dependabot_auto_update, :renovate, :lockfile]},
    "ERR-SEC-001" => %{type: :missing_branch_protection, severity: :high,
      detection: [:scorecard, :gh_api_check],
      prevention: [:ruleset, :branch_protection_api]},
    "ERR-SEC-002" => %{type: :missing_security_md, severity: :medium,
      detection: [:file_existence_check],
      prevention: [:template]},
    "ERR-WF-011" => %{type: :missing_github_actions_dependabot, severity: :medium,
      detection: [:file_existence_check, :content_scan],
      prevention: [:template]},
    "ERR-WF-012" => %{type: :missing_workflow_caching, severity: :low,
      detection: [:workflow_audit, :content_scan],
      prevention: [:template]}
  }

  def error_catalog, do: @error_catalog
  def error_type(id), do: get_in(@error_catalog, [id, :type])
end
