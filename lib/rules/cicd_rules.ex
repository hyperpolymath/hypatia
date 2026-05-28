# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRules do
  @moduledoc """
  Primary Elixir CI/CD policy rules (migrated from legacy Logtalk engine).

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
    # `permissions: read-all` is a content requirement (every workflow file
    # must declare top-level `permissions:`), not a filename. It is checked
    # by Hypatia.Rules.WorkflowAudit.check_permissions/1, NOT by file-path
    # existence — the entry below is treated specially in `has_file?/2`.
    %{file: "permissions: read-all", condition: :all_workflows, kind: :content}
  ]

  def repo_must_have_rules, do: @repo_must_have

  def check_repo_requirements(repo_info) do
    Enum.flat_map(@repo_must_have, fn rule ->
      if requirement_applies?(rule.condition, repo_info) and
           not has_file?(repo_info, rule) do
        [%{missing: rule.file, condition: rule.condition}]
      else
        []
      end
    end)
  end

  defp requirement_applies?(:public_repo, info), do: Map.get(info, :visibility) == "public"
  defp requirement_applies?(:has_dependencies, info), do: Map.get(info, :has_deps, true)
  defp requirement_applies?(:all_workflows, _info), do: true

  # Content-only rules are handled by other rule modules (workflow_audit etc.)
  # and should never report missing-file findings here.
  defp has_file?(_info, %{kind: :content}), do: true

  defp has_file?(info, %{file: file}) do
    repo_path = Map.get(info, :repo_path)

    cond do
      # Repo-rooted check: nested paths like `.github/dependabot.yml` can
      # only be confirmed via on-disk inspection. The root_files list is
      # not enough — without this the rule was a false-positive factory.
      is_binary(repo_path) and File.exists?(Path.join(repo_path, file)) -> true
      file in Map.get(info, :files, []) -> true
      true -> false
    end
  end

  # ---------------------------------------------------------------------------
  # Commit Blocking Patterns
  # ---------------------------------------------------------------------------

  @blocked_patterns [
    # Lang-policy refresh 2026-05-25: TypeScript / ReScript / migrated-JS
    # all replaced by AffineScript (the estate's go-forward language).
    # Existing approved carve-outs (e.g. `.d.ts` declaration files, Deno
    # test-runner .ts in `affinescript-deno-test/`, JS shims in
    # `affinescript-cli/`) are honoured via ScannerSuppression — these
    # entries gate NEW occurrences only.
    %{id: :typescript_detected, glob: "*.ts", reason: "TypeScript banned -- use AffineScript"},
    %{id: :rescript_detected, glob: "*.res", reason: "ReScript banned -- use AffineScript (org policy 2026-05-25; see #57 migration assistant)"},
    %{id: :rescript_interface_detected, glob: "*.resi", reason: "ReScript banned -- use AffineScript (org policy 2026-05-25; see #57 migration assistant)"},
    %{id: :nodejs_detected, glob: "package-lock.json", reason: "Node.js banned -- use Deno"},
    %{id: :golang_detected, glob: "*.go", reason: "Go banned -- use Rust"},
    # Python ban is total — no exceptions (the former SaltStack carve-out
    # was removed by org policy 2026-05-18). ScannerSuppression also
    # hard-refuses to suppress cicd_rules/banned_language_file.
    %{id: :python_detected, glob: "*.py", reason: "Python banned -- use Julia/Rust"},
    # V-lang ban (org policy 2026-05-28). Estate default for APIs/FFIs/
    # gateways/client SDKs is Zig; Idris2 owns ABIs. Path-prefix
    # allowlist covers:
    #   - developer-ecosystem/v-ecosystem/** (V R&D carve-out)
    #   - asdf-augmenters/asdf-plugin-collection/plugins/vlang/** (V toolchain installer)
    #   - hyperpolymath-archive/asdf-vlang-plugin/** (archived V toolchain plugin)
    #   - formal/**, theories/**, proofs/coq/**, proofs/canonical-proof-suite/**,
    #     proofs/verification/coq/**, academic/formal-verification/**,
    #     docs/proofs/**, fixtures/code_safety/** — Coq `.v` proofs share
    #     the `.v` extension; never delete or rewrite these.
    #   - linguist/samples/** — github-linguist language-detection samples
    #   - HOL/examples/PSL/** — Verilog test files (HOL theorem prover repo)
    #   - echidna/examples/**, echidna/tests/live_goals/** — Coq proof
    #     examples co-located with echidna's V toolchain.
    %{id: :vlang_detected, glob: "*.v",
      reason: "V-lang banned -- use Zig for APIs/FFIs/gateways/SDKs, Idris2 for ABIs (org policy 2026-05-28)",
      path_allow_prefixes: [
        "developer-ecosystem/v-ecosystem/",
        "asdf-augmenters/asdf-plugin-collection/plugins/vlang/",
        "hyperpolymath-archive/asdf-vlang-plugin/",
        "/formal/",
        "/theories/",
        "/proofs/coq",
        "/proofs/canonical-proof-suite/",
        "/proofs/verification/coq/",
        "/academic/formal-verification/",
        "/docs/proofs/",
        "/fixtures/code_safety/",
        "/linguist/samples/",
        "/HOL/examples/PSL/",
        "/echidna/examples/",
        "/echidna/tests/live_goals/"
      ]},
    %{id: :vmod_detected, glob: "v.mod",
      reason: "V-lang `v.mod` manifest banned -- use Zig `build.zig.zon` (org policy 2026-05-28)",
      path_allow_prefixes: [
        "developer-ecosystem/v-ecosystem/",
        "asdf-augmenters/asdf-plugin-collection/plugins/vlang/",
        "hyperpolymath-archive/asdf-vlang-plugin/"
      ]},
    %{id: :makefile_detected, glob: "Makefile", reason: "Makefiles banned -- use justfile"},
    # Jekyll banned 2026-05-25 — estate uses `hyperpolymath/casket-ssg`
    # (Haskell SSG) for GitHub Pages. The pre-existing :irrelevant_jekyll
    # waste pattern below catches Jekyll workflows in non-Jekyll repos;
    # these new entries flag the Jekyll-specific filenames everywhere
    # they appear, so the GHA workflow is caught EVEN IF a repo also
    # carries a stale _config.yml / Gemfile pair that previously made
    # the waste pattern's "irrelevant" check return false.
    %{id: :jekyll_workflow_detected, glob: "jekyll.yml",
      reason: "Jekyll banned -- migrate GitHub Pages to casket-ssg (hyperpolymath/casket-ssg). See affinescript/.github/workflows/casket-pages.yml for the canonical pattern."},
    %{id: :jekyll_gh_pages_workflow_detected, glob: "jekyll-gh-pages.yml",
      reason: "Jekyll banned -- migrate GitHub Pages to casket-ssg (hyperpolymath/casket-ssg). See affinescript/.github/workflows/casket-pages.yml for the canonical pattern."},
    %{id: :jekyll_config_detected, glob: "_config.yml",
      reason: "Jekyll banned -- _config.yml is Jekyll's site config. Migrate to casket-ssg (hyperpolymath/casket-ssg)."},
    %{id: :gemfile_detected, glob: "Gemfile",
      reason: "Gemfile banned (no Ruby/Jekyll in estate) -- if this is for Jekyll, migrate to casket-ssg (hyperpolymath/casket-ssg). If for non-Jekyll Ruby, file an exemption request: Ruby itself is not in the allowed-language table."},
    %{id: :unpinned_action,
      pattern: ~r/uses:\s+[a-zA-Z0-9_.-]+\/[a-zA-Z0-9_.\/-]+@(v[0-9][a-zA-Z0-9.-]*|main|master)/,
      reason: "GitHub Actions and reusable workflows must be SHA-pinned"},
    %{id: :missing_permissions, pattern: ~r/^permissions:/m, negative: true,
      reason: "Workflows must declare permissions"},
    %{id: :missing_spdx, pattern: ~r/^# SPDX-License-Identifier:/m, negative: true,
      reason: "Files must have SPDX headers"},
    # --- Rules derived from 2026-03-16 session dogfooding ---
    %{id: :agpl_license, pattern: ~r/SPDX-License-Identifier:\s*AGPL-3\.0/,
      reason: "AGPL-3.0 replaced by MPL-2.0",
      exception_repos: ["game-server-admin", "idaptik", "airborne-submarine-squadron"]},
    %{id: :innerhtml_usage, pattern: ~r/\.innerHTML\s*=|document\.write\(/,
      reason: "innerHTML/document.write banned -- use rescript-dom-mounter SafeDOM",
      applies_to: ["*.js", "*.res"]},
    %{id: :eval_in_shell, pattern: ~r/\beval\b/,
      reason: "eval banned in shell scripts -- use direct expansion or arrays",
      applies_to: ["*.sh"]},
    %{id: :download_then_run_shell, pattern: ~r/\b(curl|wget)\b[^\n|;]*\|\s*(sh|bash)\b/,
      reason: "download-then-run banned -- verify checksum/signature before execution",
      applies_to: ["*.sh", "*.yml", "*.yaml"]},
    %{id: :js_insecure_random_security_context,
      pattern: ~r/(?i)\b(session|token|nonce|secret|auth|csrf)\w*\b\s*[:=][^\n]*Math\.random\(/,
      reason: "Math.random in security-sensitive context -- use crypto.randomUUID/getRandomValues",
      applies_to: ["*.js", "*.ts"]},
    %{id: :hardcoded_tmp, pattern: ~r/["'\/]tmp\//,
      reason: "Hardcoded /tmp/ paths -- use mktemp",
      applies_to: ["*.sh"], exception: "Containerfile"},
    %{id: :template_placeholder, pattern: ~r/\{\{(REPO|OWNER|FORGE|AUTHOR)\}\}/,
      reason: "Unfilled RSR template placeholder",
      exception: "rsr-template-repo"},
    %{id: :deno_all_perms, pattern: ~r/deno\s+run\s+-A\b/,
      reason: "Deno -A (all permissions) banned -- use specific --allow-* flags"},
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

  defp check_pattern(%{glob: glob} = pattern, files) do
    suffix = String.replace(glob, "*", "")
    allow_prefixes = Map.get(pattern, :path_allow_prefixes, [])

    files
    |> Enum.filter(fn f -> String.ends_with?(f, suffix) end)
    |> Enum.reject(fn f -> Enum.any?(allow_prefixes, &String.contains?(f, &1)) end)
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
      description: "More than 15 workflows -- consolidate"},
    %{id: :missing_directory_workflow, severity: :low, auto_fixable: true,
      description: "zig-ffi.yml workflow but no zig/ directory"},

    # Workflow hygiene: irrelevant template workflows
    %{id: :irrelevant_ts_blocker, severity: :low, auto_fixable: true,
      description: "ts-blocker.yml in repo with no TypeScript or JavaScript"},
    %{id: :irrelevant_npm_blocker, severity: :low, auto_fixable: true,
      description: "npm-bun-blocker.yml in repo with no JS package ecosystem"},
    %{id: :irrelevant_jekyll, severity: :medium, auto_fixable: true,
      description: "Jekyll workflow in repo with no _config.yml or Gemfile (estate policy bans Jekyll; replacement is casket-ssg)"},
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

    # Standalone workflows subsumed by governance-reusable.yml (per the
    # hyperpolymath/standards governance-reusable.yml header). When
    # `governance.yml` is present in a repo, every name in
    # @subsumed_standalones below is redundant — its logic runs via the
    # reusable and the standalone copy drifts independently.
    %{id: :redundant_subsumed_standalone, severity: :medium, auto_fixable: true,
      description: "Standalone workflow whose logic is already exercised by governance.yml (calls governance-reusable.yml)"},

    # rust-ci.yml exists but the repo has no Cargo.toml at root and no .rs
    # files anywhere — guaranteed install/build failure. Cousin of
    # :missing_rust_ci, which catches the inverse (Cargo.toml without CI).
    %{id: :irrelevant_rust_ci, severity: :high, auto_fixable: true,
      description: "rust-ci.yml in repo with no Cargo.toml at root and no .rs files (guaranteed install failure)"},

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

    # Subsumed standalones: when `governance.yml` is present in the repo,
    # each of these standalone workflow files is redundant — its logic
    # already runs via the standards governance-reusable.yml. Authoritative
    # list per the reusable's own header comment in hyperpolymath/standards.
    subsumed_standalones = [
      "workflow-linter.yml",
      "language-policy.yml",
      "quality.yml",
      "security-policy.yml",
      "guix-nix-policy.yml",
      "npm-bun-blocker.yml",
      "ts-blocker.yml",
      "rsr-antipattern.yml",
      "wellknown-enforcement.yml"
    ]

    results =
      if "governance.yml" in workflows do
        Enum.reduce(subsumed_standalones, results, fn wf, acc ->
          if wf in workflows do
            [%{pattern: :redundant_subsumed_standalone, workflow: wf, auto_fixable: true} | acc]
          else
            acc
          end
        end)
      else
        results
      end

    # rust-ci.yml without any Rust code in the repo. Mirrors
    # :missing_rust_ci (which detects the inverse) but uses the same
    # file-presence and *.rs walk heuristics that BaselineHealth uses.
    has_rs_anywhere =
      Map.get(repo_info, :rs_file_count, 0) > 0 or
        Enum.any?(files, &String.ends_with?(&1, ".rs"))

    results =
      if "rust-ci.yml" in workflows and "Cargo.toml" not in files and not has_rs_anywhere do
        [%{pattern: :irrelevant_rust_ci, workflow: "rust-ci.yml", auto_fixable: true} | results]
      else
        results
      end

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
      if "mix.exs" in files and not has_ci and
           "elixir.yml" not in workflows and "mix.yml" not in workflows and
           "beam.yml" not in workflows and "elixir-ci.yml" not in workflows do
        [%{pattern: :missing_elixir_ci, auto_fixable: true} | results]
      else
        results
      end

    results
  end

  # ---------------------------------------------------------------------------
  # License Validation
  # ---------------------------------------------------------------------------

  @required_spdx "MPL-2.0"
  # `MPL-2.0` is intentionally absent here — it's the required identifier
  # (the first/second clauses of validate_license/2 short-circuit before
  # this list is consulted). MPL-1.0 / MPL-1.0-or-later are the only
  # legacy MPL identifiers seen in estate history; both must rewrite to
  # MPL-2.0 (org policy refresh 2026-05-25, applies to docs AND code).
  @wrong_licenses ["MIT", "Apache-2.0", "MPL-1.0", "MPL-1.0-or-later", "AGPL-3.0", "GPL-3.0"]

  # Repos that legitimately use AGPL-3.0-or-later (co-developed with family, etc.)
  @agpl_exception_repos ["game-server-admin", "idaptik", "airborne-submarine-squadron"]

  def required_spdx, do: @required_spdx
  def agpl_exception_repos, do: @agpl_exception_repos

  @doc """
  Validate a license SPDX identifier, optionally scoped to a repo name.
  AGPL-3.0-or-later is permitted for repos in @agpl_exception_repos.
  """
  def validate_license(spdx_id, repo_name \\ nil)

  def validate_license(spdx_id, repo_name) do
    cond do
      double_suffix?(spdx_id) -> {:error, :spdx_double_suffix, spdx_id}
      spdx_id == @required_spdx -> :ok
      spdx_id == "MPL-2.0" -> :ok_fallback
      spdx_id in ["AGPL-3.0", "AGPL-3.0-or-later"] and repo_name in @agpl_exception_repos ->
        :ok_agpl_exception
      spdx_id in @wrong_licenses -> {:error, :wrong_license, spdx_id}
      true -> {:warning, :unknown_license, spdx_id}
    end
  end

  # Detect SPDX identifiers with a duplicated `-or-later` (or `+`) suffix —
  # the rhodibot regex-regression class observed 2026-05-27 in
  # the-nash-equilibrium#41. The deployed bot's auto-fix applies
  # `s/AGPL-3.0/AGPL-3.0-or-later/` without a word-boundary anchor, so
  # files that already carry `-or-later` end up with the suffix duplicated.
  # A canonical SPDX identifier never repeats its `-or-later` clause and
  # the `+` short-form is mutually exclusive with `-or-later` (so
  # `…-or-later+` and `…+-or-later` are also malformed). Returns true for
  # malformed strings; validate_license/2 maps that to
  # `{:error, :spdx_double_suffix, …}` (ERR-LIC-001 in the catalog).
  defp double_suffix?(spdx_id) when is_binary(spdx_id) do
    String.contains?(spdx_id, "-or-later-or-later") or
      String.contains?(spdx_id, "-or-later+") or
      Regex.match?(~r/\+-or-later\b/, spdx_id)
  end

  defp double_suffix?(_), do: false

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
    "ERR-LIC-001" => %{type: :spdx_double_suffix, severity: :high,
      detection: [:grep_pattern, :validate_license],
      prevention: [:pre_commit_hook, :rhodibot_regex_word_boundary]},
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
      prevention: [:template]},
    "ERR-WF-013" => %{type: :missing_timeout_minutes, severity: :medium,
      detection: [:workflow_audit, :content_scan],
      prevention: [:template, :pre_commit_hook]},
    "ERR-GIT-001" => %{type: :crlf_blob_without_gitattributes, severity: :medium,
      detection: [:git_state, :content_scan],
      prevention: [:gitattributes_template]},
    "ERR-PR-001" => %{type: :obsolete_pr_target_sha_stale, severity: :info,
      detection: [:gh_api_check, :pr_inventory],
      prevention: [:branch_protection]}
  }

  def error_catalog, do: @error_catalog
  def error_type(id), do: get_in(@error_catalog, [id, :type])
end
