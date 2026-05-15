# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.WorkflowAudit do
  @moduledoc """
  CI/CD workflow audit rules.

  Verifies that repositories have the standard RSR workflow set,
  checks SHA pins against known-good values, detects duplicates,
  and validates permissions declarations.
  """

  # ─── Standard RSR workflows (17 total) ─────────────────────────────────

  @standard_workflows [
    "hypatia-scan.yml",
    "codeql.yml",
    "scorecard.yml",
    "quality.yml",
    "mirror.yml",
    "instant-sync.yml",
    "guix-nix-policy.yml",
    "rsr-antipattern.yml",
    "security-policy.yml",
    "wellknown-enforcement.yml",
    "workflow-linter.yml",
    "npm-bun-blocker.yml",
    "ts-blocker.yml",
    "jekyll-gh-pages.yml",
    "jekyll.yml",
    "scorecard-enforcer.yml",
    "secret-scanner.yml"
  ]

  # ─── Known-good SHA pins (2026-02-04 baseline) ────────────────────────

  @known_good_shas %{
    "actions/checkout@v4" => "34e114876b0b11c390a56381ad16ebd13914f8d5",
    "actions/checkout@v5" => "93cb6efe18208431cddfb8368fd83d5badbf9bfd",
    "github/codeql-action@v3" => "6624720a57d4c312633c7b953db2f2da5bcb4c3a",
    "github/codeql-action@v4" => "d4b3ca9fa7f69d38bfcd667bdc45bc373d16277e",
    "denoland/setup-deno@v2" => "909cc5acb0fdd60627fb858598759246509fa755",
    "ossf/scorecard-action@v2.4.0" => "62b2cac7ed8198b15735ed49ab1e5cf35480ba46",
    "trufflesecurity/trufflehog@main" => "7ee2e0fdffec27d19ccbb8fb3dcf8a83b9d7f9e8",
    "dtolnay/rust-toolchain@stable" => "4be9e76fd7c4901c61fb841f559994984270fce7",
    "Swatinem/rust-cache@v2" => "779680da715d629ac1d338a641029a2f4372abb5",
    "codecov/codecov-action@v5" => "671740ac38dd9b0130fbe1cec585b89eea48d3de",
    "editorconfig-checker/action-editorconfig-checker@main" => "4054fa83a075fdf090bd098bdb1c09aaf64a4169",
    "slsa-framework/slsa-github-generator@v2.1.0" => "f7dd8c54c2067bafc12ca7a55595d5ee9b75204a",
    "webfactory/ssh-agent@v0.9.0" => "dc588b651fe13675774614f8e6a936a468676387",
    "actions/configure-pages@v5" => "983d7736d9b0ae728b81ab479565c72886d7745b",
    "actions/jekyll-build-pages@v1" => "44a6e6beabd48582f863aeeb6cb2151cc1716697",
    "actions/upload-pages-artifact@v3" => "56afc609e74202658d3ffba0e8f6dda462b719fa",
    "actions/deploy-pages@v4" => "d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e",
    "ruby/setup-ruby@v1" => "09a7688d3b55cf0e976497ff046b70949eeaccfd",
    "hyperpolymath/a2ml-validate-action@main" => "cb3c1e298169dc5ac2b42e257068b0fb5920cd5e",
    "hyperpolymath/k9-validate-action@main" => "236f0035cc159051c8dd5dc7cd8af1e8cf961462",
    "hyperpolymath/panic-attacker/.github/workflows/scan-and-report.yml@main" =>
      "21fc3f00a088c954912936f4a68970621b82c2e6"
  }

  def standard_workflows, do: @standard_workflows
  def known_good_shas, do: @known_good_shas

  # ─── Audit functions ───────────────────────────────────────────────────

  @doc """
  Audit workflow directory. Takes a list of workflow filenames and
  optionally their contents (as a map of filename => content).
  """
  def audit(workflow_files, workflow_contents \\ %{}, opts \\ []) do
    missing = check_missing_workflows(workflow_files, opts)
    unpinned = check_unpinned_actions(workflow_contents)
    wrong_pins = check_wrong_pins(workflow_contents)
    permission_issues = check_permissions(workflow_contents)
    flawed_regexes = check_flawed_regex(workflow_contents)
    duplicates = check_duplicates(workflow_files, workflow_contents)
    caching_issues = check_caching(workflow_contents)
    run_context_issues = check_github_context_in_run(workflow_contents)
    download_then_run_issues = check_download_then_run(workflow_contents)
    nperm_typos = check_npermissions_typo(workflow_contents)
    codeql_lang_mismatch = check_codeql_language_matrix_mismatch(workflow_contents, opts)

    %{
      findings:
        missing ++ unpinned ++ wrong_pins ++ permission_issues ++ duplicates ++
          caching_issues ++ run_context_issues ++ download_then_run_issues ++ nperm_typos ++
          codeql_lang_mismatch,
      missing_count: length(missing),
      unpinned_count: length(unpinned),
      wrong_pin_count: length(wrong_pins),
      permission_issues: length(permission_issues),
      flawed_regex_count: length(flawed_regexes),
      duplicate_count: length(duplicates),
      caching_issues: length(caching_issues),
      run_context_issues: length(run_context_issues),
      download_then_run_issues: length(download_then_run_issues),
      npermissions_typo_count: length(nperm_typos),
      codeql_lang_mismatch_count: length(codeql_lang_mismatch),
      workflow_count: length(workflow_files),
      standard_coverage: coverage_percentage(workflow_files)
    }
  end

  @doc """
  Check which standard workflows are missing.

  `opts[:has_codeql_supported_language]` (boolean): when false, `codeql.yml`
  is dropped from the required set — CodeQL has nothing to scan in a repo
  with no JavaScript / Python / Go / Java / Ruby / C# / C++ / Swift files,
  so demanding the workflow would produce a perpetual missing-file finding
  that no one can usefully close. Hypatia's Rust + Elixir + Idris mix
  triggered this exact FP in the #237 self-scan.
  """
  def check_missing_workflows(workflow_files, opts \\ []) do
    requireds =
      if Keyword.get(opts, :has_codeql_supported_language, true) do
        @standard_workflows
      else
        @standard_workflows -- ["codeql.yml"]
      end

    Enum.flat_map(requireds, fn wf ->
      if wf in workflow_files do
        []
      else
        [%{type: :missing_workflow, file: wf,
           severity: severity_for_workflow(wf),
           action: :create}]
      end
    end)
  end

  @doc """
  Check for GitHub Actions that use tag references instead of SHA pins.
  """
  def check_unpinned_actions(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      # Match uses: owner/repo@vN.N.N or @main/@master (tag/branch, not SHA)
      Regex.scan(~r/uses:\s*([a-zA-Z0-9_.-]+\/[a-zA-Z0-9_.\/-]+)@((?:v[\d][\w.-]*|main|master))\s*$/m, content)
      |> Enum.map(fn [_full, action, ref] ->
        severity = if ref in ["main", "master"], do: :high, else: :medium

        %{
          type: :unpinned_action,
          file: filename,
          action_ref: "#{action}@#{ref}",
          severity: severity,
          action: :pin_sha,
          known_sha: Map.get(@known_good_shas, "#{action}@#{ref}")
        }
      end)
    end)
  end

  @doc """
  Check for SHA pins that don't match the local known-good table.

  Severity is `:info`, not `:medium`: a SHA that's not in the local table is
  *unverified*, not *wrong*. The known-good table can't keep pace with the
  full dependency graph, so when this rule fires at medium it generates
  more noise than signal (see hypatia#237 triage — 148 of 225 findings).
  Only escalate manually when the current SHA is on a known-vulnerable list.
  """
  def check_wrong_pins(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      # Match uses: owner/repo@sha
      Regex.scan(~r/uses:\s*([^\s#]+)@([0-9a-f]{40})/m, content)
      |> Enum.flat_map(fn [_full, action, sha] ->
        # Check if we have a known-good SHA for any version of this action
        matching = Enum.find(@known_good_shas, fn {ref, _} ->
          String.starts_with?(ref, action <> "@")
        end)

        case matching do
          {_ref, known_sha} when known_sha != sha ->
            [%{type: :wrong_sha_pin, file: filename, action_ref: action,
               current_sha: sha, expected_sha: known_sha,
               severity: :info, fix: :update_pin}]
          _ -> []
        end
      end)
    end)
  end

  @doc """
  Check for missing or overly broad permissions declarations.
  """
  def check_permissions(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      findings = []

      # Check for write-all (should be read-all + specific)
      findings = if String.contains?(content, "permissions: write-all") do
        [%{type: :broad_permissions, file: filename,
           detail: "permissions: write-all should be permissions: read-all with specific overrides",
           severity: :high, action: :narrow_permissions} | findings]
      else
        findings
      end

      # Check for missing permissions declaration entirely
      findings = if not Regex.match?(~r/^permissions:/m, content) do
        [%{type: :missing_permissions, file: filename,
           detail: "No permissions declaration -- add permissions: read-all",
           severity: :medium, action: :add_permissions} | findings]
      else
        findings
      end

      # Check for missing SPDX header
      findings = if not String.contains?(content, "SPDX-License-Identifier:") do
        [%{type: :missing_spdx, file: filename,
           detail: "No SPDX-License-Identifier header",
           severity: :low, action: :add_spdx} | findings]
      else
        findings
      end

      findings
    end)
  end

  @doc """
  Check for setup actions that could use built-in caching but don't.
  """
  def check_caching(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      # Patterns for setup actions that support 'cache:'
      cacheable_actions = [
        {~r/uses:\s*actions\/setup-node@v\d+/, "setup-node"},
        {~r/uses:\s*actions\/setup-python@v\d+/, "setup-python"},
        {~r/uses:\s*actions\/setup-go@v\d+/, "setup-go"},
        {~r/uses:\s*actions\/setup-java@v\d+/, "setup-java"},
        {~r/uses:\s*mlugg\/setup-zig@v\d+/, "setup-zig"}
      ]

      Enum.flat_map(cacheable_actions, fn {pattern, name} ->
        if Regex.match?(pattern, content) and not String.contains?(content, "cache:") do
          [%{type: :missing_caching, file: filename,
             detail: "#{name} missing built-in caching (e.g., cache: 'npm' or cache: true)",
             severity: :low, action: :enable_caching}]
        else
          []
        end
      end)
    end)
  end

  @doc """
  Detect potential duplicate workflows (same content, different names).
  """
  def check_duplicates(_workflow_files, workflow_contents) when map_size(workflow_contents) < 2, do: []
  def check_duplicates(_workflow_files, workflow_contents) do
    # Group by content hash
    contents_list = Enum.map(workflow_contents, fn {name, content} ->
      {name, :erlang.md5(content)}
    end)

    contents_list
    |> Enum.group_by(fn {_, hash} -> hash end)
    |> Enum.flat_map(fn {_hash, entries} ->
      if length(entries) > 1 do
        names = Enum.map(entries, fn {name, _} -> name end)
        [%{type: :duplicate_workflow, files: names,
           severity: :low, action: :consolidate}]
      else
        []
      end
    end)
  end

  # ─── WF013: npermissions typo ───────────────────────────────────────────

  @doc """
  WF013: Detect 'npermissions:' typo in workflow files.
  GitHub Actions silently ignores unknown top-level keys, so a typo like
  'npermissions:' means the workflow runs with overly broad default permissions.
  Severity: high.
  Action: rename to 'permissions:'.
  """
  def check_npermissions_typo(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      if Regex.match?(~r/^npermissions:/m, content) do
        [%{
          rule: "WF013",
          type: :npermissions_typo,
          file: filename,
          severity: :high,
          reason: "Workflow has 'npermissions' typo -- should be 'permissions'. GitHub Actions silently ignores this, running with overly broad defaults.",
          action: :fix_typo
        }]
      else
        []
      end
    end)
  end

  @doc """
  Detect direct GitHub context interpolation inside run blocks.

  Only fires for **actor-controllable** context expressions — the values a
  forked-PR contributor or issue/comment author can choose:

    * `github.head_ref` / `github.event.pull_request.head.ref`
    * `github.event.pull_request.title|body`
    * `github.event.issue.title|body`
    * `github.event.comment.body` / `github.event.review.body`
    * `github.event.workflow_run.head_branch` / `display_title`
    * `github.event.commits[*].message|author.name|author.email`

  Admin-controlled context (`github.repository`, `github.sha`, `github.ref_name`
  on a push event from a privileged author, `vars.*`, etc.) is not an
  injection vector — it can only be set by repo owners and is constrained by
  GitHub. Interpolating those is style guidance, not a security finding, so
  it doesn't fire here.

  Note: `github.actor` is *only* attacker-controllable when the workflow runs
  on `pull_request_target` from a fork. Pre-2026-05 audit was conservative
  (always flagged); we narrow to the actor-controlled set to restore signal.
  """
  def check_github_context_in_run(workflow_contents) do
    # Actor-controllable expressions. Anchored loosely so we catch the
    # expression wherever it appears in a run block — the previous full
    # `run: |` block-shape match missed inline `run:` single-liners.
    run_context_re =
      ~r/run:[\s\S]*?\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref)|event\.issue\.(?:title|body)|event\.comment\.body|event\.review\.body|event\.workflow_run\.(?:head_branch|display_title)|event\.commits)[^}]*\}\}/m

    unsafe_json_payload_re =
      ~r/-d\s*".*\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref)|event\.issue\.(?:title|body)|event\.comment\.body|event\.review\.body|event\.workflow_run\.(?:head_branch|display_title)|event\.commits)/s

    Enum.flat_map(workflow_contents, fn {filename, content} ->
      content = strip_comments(content)
      findings = []

      findings =
        if Regex.match?(run_context_re, content) do
          [%{
            type: :actions_expression_injection,
            file: filename,
            detail:
              "GitHub context value is interpolated directly inside a run block. Move context into env and use jq/quoted variables.",
            severity: :critical,
            action: :sanitize_context
          } | findings]
        else
          findings
        end

      findings =
        if Regex.match?(unsafe_json_payload_re, content) do
          [%{
            type: :unsafe_curl_payload,
            file: filename,
            detail:
              "curl JSON payload is assembled with inline GitHub context interpolation. Build payload with jq --arg.",
            severity: :high,
            action: :use_jq_payload
          } | findings]
        else
          findings
        end

      findings
    end)
  end

  @doc """
  Detect download-and-execute shell patterns in workflow run blocks.

  Comment-aware: YAML comment lines (`^# ...`) and shell comments inside
  run blocks are stripped before matching, so a workflow that says
  `# safer than curl | sh` doesn't get flagged for the pattern in its own
  description (see hypatia#237 triage — docs.yml and mirror.yml both
  baselined this FP).
  """
  def check_download_then_run(workflow_contents) do
    download_then_run_re = ~r/\b(?:curl|wget)\b[^\n|;]*\|\s*(?:sh|bash)\b/

    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)
      if Regex.match?(download_then_run_re, stripped) do
        [%{
          type: :download_then_run,
          file: filename,
          detail:
            "Workflow executes remote script directly (curl/wget piped to shell). Download, verify checksum/signature, then execute.",
          severity: :high,
          action: :verify_download_integrity
        }]
      else
        []
      end
    end)
  end

  @doc """
  Detect codeql.yml whose language matrix lists a CodeQL source-scanning
  language (`javascript-typescript`, `python`, `go`, etc.) when the repo
  has no source files in any such language.

  Caught 12 repos across the fleet at the time of writing — each one had
  `language: javascript-typescript` on a Julia / Rust / Coq / Idris repo,
  so the CodeQL `analyze` job exited "configuration error: no source
  files" on every Dependabot PR and blocked the merge.

  The `actions` language is always safe (scans workflow files which every
  repo has). Only the source-scanning languages need the repo-side
  guarantee.

  `opts[:has_codeql_supported_language]` — same boolean threaded through
  from `Hypatia.CLI.has_codeql_supported_language?/1`. When `false` and
  codeql.yml lists a scanning language, this rule fires.
  """
  def check_codeql_language_matrix_mismatch(workflow_contents, opts \\ []) do
    if Keyword.get(opts, :has_codeql_supported_language, true) do
      []
    else
      # Languages CodeQL scans by reading source files in the repo (as
      # opposed to `actions`, which scans workflow YAML).
      scanning_langs = ~w(javascript-typescript javascript typescript python go java ruby csharp c-cpp swift kotlin)

      Enum.flat_map(workflow_contents, fn {filename, content} ->
        if codeql_workflow?(filename) do
          Enum.flat_map(scanning_langs, fn lang ->
            if Regex.match?(~r/language:\s*#{Regex.escape(lang)}(?:\s|$)/m, content) do
              [%{
                type: :codeql_language_matrix_mismatch,
                file: filename,
                detail:
                  "codeql.yml lists `language: #{lang}` but the repo has no source files in any CodeQL-scannable language. The analyze job will exit 'no source files' on every run. Switch the matrix to `actions` (which scans workflow files — every repo has those).",
                severity: :high,
                action: :switch_codeql_matrix_to_actions
              }]
            else
              []
            end
          end)
        else
          []
        end
      end)
    end
  end

  defp codeql_workflow?(filename) do
    base = Path.basename(filename)
    base in ["codeql.yml", "codeql.yaml", "codeql-analysis.yml", "codeql-analysis.yaml"]
  end

  # ─── Helpers ───────────────────────────────────────────────────────────

  # Strip YAML / shell line comments from a workflow body before pattern
  # matching. Used by the security-pattern checks (download-then-run,
  # actions-expression-injection, unsafe-curl-payload) so a workflow that
  # *describes* the bad pattern in a comment ("safer than curl | sh") is
  # not itself flagged for it. Leaves shebangs and inline trailing-comments
  # untouched — only full-line comments are removed, which is the most
  # common false-positive case.
  defp strip_comments(content) do
    Regex.replace(~r/^[ \t]*#[^\n]*$/m, content, "")
  end

  defp coverage_percentage(workflow_files) do
    present = Enum.count(@standard_workflows, &(&1 in workflow_files))
    round(present / length(@standard_workflows) * 100)
  end

  defp severity_for_workflow(wf) do
    case wf do
      "hypatia-scan.yml" -> :critical
      "codeql.yml" -> :high
      "scorecard.yml" -> :high
      "quality.yml" -> :high
      "mirror.yml" -> :high
      "secret-scanner.yml" -> :high
      "security-policy.yml" -> :medium
      _ -> :low
    end
  end

  @doc """
  Check for flawed regex patterns in workflow files.
  Detects common mistakes like unescaped dots, overly broad matches,
  and regex patterns that could cause false positives in CI checks.
  """
  def check_flawed_regex(workflow_contents) when is_map(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      flaws = []

      # Detect unescaped dots in grep patterns (e.g., grep "foo.bar" matches "fooXbar")
      flaws =
        if Regex.match?(~r/grep\s+(-[a-zA-Z]*\s+)*["'][^"']*(?<!\\)\.[^"']*["']/, content) do
          [%{rule: "flawed_regex", severity: :low, file: filename,
             description: "grep with unescaped dot -- may match unintended characters"} | flaws]
        else
          flaws
        end

      # Detect grep -E with * quantifier without preceding atom
      flaws =
        if Regex.match?(~r/grep\s+-[a-zA-Z]*E[a-zA-Z]*\s+["'][^"']*(?<![.\w\\])\*/, content) do
          [%{rule: "flawed_regex", severity: :low, file: filename,
             description: "grep -E with bare * quantifier -- likely needs .* or \\w*"} | flaws]
        else
          flaws
        end

      flaws
    end)
  end

  def check_flawed_regex(_), do: []
end
