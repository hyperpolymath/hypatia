# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.CiFailureTriage do
  @moduledoc """
  Symbolic triage of a failed CI run: which layer failed, what to do, and
  when to stop.

  Hypatia's other rules read a repository. These rules read what a run left
  behind (a job log, its exit code and conclusion, the gate's check states)
  and answer one question: is this red the code's fault? Most estate reds in
  November 2026 were not: a rate-limited setup step, a merge ref that vanished
  when the PR merged, a cancelled run, an unreachable git host, a lockfile pin
  the runner refuses. Treating those as code failures sends a bot to "fix"
  code that is fine, and loops.

  ## Rule ids

  Every id is `hypatia/ci_failure_triage/<name>` (standards A5: `<domain>/<name>`,
  no wildcard ids). `rules/0` lists them with their layer, action and budget.

  ## Precedence (first layer wins)

  A log often shows several symptoms; the earliest layer is the cause:

      1. :platform   the runner or a service failed (rate limit, merge-ref race,
                     cancellation, unreachable host)
      2. :config     the run was set up wrongly (lockfile pin, secret or
                     variable, App key format, canon lockstep)
      3. :toolchain  the tool crashed (SIGSEGV)
      4. :code       the code or its tests failed (tests, bench regression)

  Within a layer, the rule listed first in `@rules` wins. `classify/2`
  returns every match, and `primary` is the first match by
  `{layer rank, rule order}`.

  ## Actions and loop limit

  Each rule names one action:

    * `:none`    not a failure of this change (cancelled; a stale merge ref
                 for a PR that already merged). Nothing to do.
    * `:rerun`   transient. Rerun, at most `retries` times for the same
                 `{repo, workflow, sha, rule}`.
    * `:fix`     a known recipe fixes it (`fix` names it). Never a rerun.
    * `:owner`   needs a person: a judgement, a credential, or a setting this
                 system may not change (required checks, rulesets).
    * `:report`  a code failure, for the code's owner or the fixer bots.
    * `:upstream` a rollup job that only reports other jobs' failures. Triage
                 those jobs instead.

  `decide/2` applies the loop limit. A transient rule whose budget is spent
  becomes `:owner` with `persistent: true`, so a rerun loop always ends: the
  attempt count only rises, and every budget is at most 2.

  ## What a fix may never do

  A decision from these rules never bypasses a gate. It never drops or
  renames a required check, edits a ruleset, uses an admin merge, touches
  licence or SPDX text, or loosens a canon/spine gate to make a pin pass.
  `forbidden_actions/0` lists these so a dispatcher can check before it acts.
  """

  @domain "hypatia/ci_failure_triage"

  @layers [:platform, :config, :toolchain, :code]

  # Each rule: name, layer, patterns (any matches), action, retries, fix,
  # escalation, reason. Order inside a layer is precedence.
  @rules [
    %{
      name: "runner_cancelled",
      layer: :platform,
      patterns: [
        # Runner-authored lines only. A bare "The operation was canceled." is
        # also ordinary exception text (.NET TaskCanceledException) and must
        # not hide a real failure; `conclusion: cancelled` is authoritative.
        ~r/##\[error\]The operation was canceled/,
        ~r/##\[error\]The run was canceled by/
      ],
      action: :none,
      retries: 0,
      fix: nil,
      escalation: :none,
      reason:
        "The run was cancelled (a newer push, concurrency, or a person). A cancelled " <>
          "job is not a failure of the change. If a later job turned red because " <>
          "an upstream job was cancelled, that job runs with `if: always()`: see WF029."
    },
    %{
      name: "api_rate_limit",
      layer: :platform,
      patterns: [
        ~r/API rate limit exceeded/i,
        ~r/exceeded a secondary rate limit/i,
        ~r/x-ratelimit-remaining:\s*0\b/i
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "GitHub API rate limit (often in a setup step such as setup-bun fetching a " <>
          "release list). Not a code failure. Rerun after the limit resets; if it " <>
          "persists, the token or App installation is shared by too many jobs."
    },
    %{
      name: "merge_ref_race",
      layer: :platform,
      patterns: [~r/couldn't find remote ref refs\/pull\/\d+\/merge/],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "The PR's merge ref was gone when checkout ran, usually because the PR " <>
          "merged or closed mid-run. For a merged or closed PR the run is stale: " <>
          "nothing to do. For an open PR, rerun once (the ref is rebuilt)."
    },
    %{
      name: "external_host_unreachable",
      layer: :platform,
      patterns: [
        ~r/fatal: unable to access '[^']+': (Could not resolve host|Failed to connect|Connection timed out|Operation timed out|Recv failure|gnutls_handshake|The requested URL returned error: 5\d\d)/,
        ~r/Could not resolve host:\s*(\S+)/,
        ~r/Failed to connect to (\S+) port \d+/,
        ~r/The requested URL returned error: 5\d\d/
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "A git or download host outside the repo did not answer (for example " <>
          "git.sr.ht in an Idris2 pack build, exit 128). Rerun later; if a host is " <>
          "often down, the owner decides whether to mirror or vendor it."
    },
    %{
      name: "api_server_error",
      layer: :platform,
      patterns: [
        ~r/\bgh: HTTP 50[234]\b/,
        ~r/\bHTTP 50[234]: /,
        ~r/Server Error \(HTTP 50[234]\)/
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "The GitHub API answered 502/503/504. Transient, or a query too heavy for the " <>
          "API (a timeout shows as 502). Rerun; if it persists, split the query."
    },
    %{
      name: "scan_range_missing_commit",
      layer: :platform,
      patterns: [
        ~r/fatal: ambiguous argument '[0-9a-f]{7,40}\^?\.\.[0-9a-f]{7,40}': unknown revision/,
        ~r/fatal: bad object [0-9a-f]{40}/
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "A diff-range scan (gitleaks, trufflehog) named a commit that is not in the " <>
          "checkout: the base moved (rebase, force-push, base update) or the clone is too " <>
          "shallow. Rerun once; if it repeats, fetch the base commit before scanning."
    },
    %{
      name: "token_invalid",
      layer: :config,
      patterns: [
        ~r/Bad credentials/,
        # git over HTTPS: the server answered and refused the credential.
        ~r/fatal: unable to access '[^']+': The requested URL returned error: 40[13]/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The token the job uses was refused (HTTP 401): expired, revoked or wrong. " <>
          "Rotate it. Only a person can do this; every job on that token fails until then."
    },
    %{
      name: "actions_lock_invalid_pin",
      layer: :config,
      patterns: [~r/invalid lockfile pin/i],
      action: :fix,
      retries: 0,
      fix: "regenerate-actions-lock-leaf",
      escalation: :owner,
      reason:
        "The Actions runner refused an actions.lock leaf at `Set up job`. The known " <>
          "cause is a leaf whose `ref` repeats its own SHA (ActionsLock " <>
          "`self_sha_leaf_ref`). Regenerate the lock with the current gh-actions-lock; " <>
          "if the bad leaf comes from a standards commit, fix it there (solutions at source)."
    },
    %{
      name: "actions_lock_unresolvable_pin",
      layer: :config,
      patterns: [
        ~r/lockfile verification did not produce a result for this action/,
        ~r/Unable to resolve action `[^`]+`, unable to find version/
      ],
      action: :fix,
      retries: 0,
      fix: "regenerate-actions-lock",
      escalation: :owner,
      reason:
        "actions.lock pins an action to a commit GitHub cannot resolve (a stale or " <>
          "deleted pin; DEBT-REGISTER CI-1). Regenerate the lock against live refs."
    },
    %{
      name: "app_key_wrong_format",
      layer: :config,
      patterns: [
        ~r/BEGIN OPENSSH PRIVATE KEY/,
        ~r/DECODER routines::unsupported/,
        ~r/error:1E08010C/,
        ~r/Invalid keyData/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The GitHub App private key is not a PEM key the App token action can read " <>
          "(an OpenSSH key was pasted). Generate a private key from the App's settings " <>
          "page and store that .pem file's contents in the secret. Only a person can do this."
    },
    %{
      name: "secret_or_variable_missing",
      layer: :config,
      patterns: [
        ~r/appId option is required/,
        ~r/Input required and not supplied:\s*(app[-_]id|private[-_]key|client[-_]id)/i,
        ~r/\[@octokit\/auth-app\].*required/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A required App id or key reached the step empty. Common cause: a value read " <>
          "as `vars.X` was stored as a secret (or the reverse), so the expression is " <>
          "empty. Store it where the workflow reads it. Only a person can do this."
    },
    %{
      name: "pages_not_enabled",
      layer: :config,
      patterns: [
        ~r/Get Pages site failed/,
        ~r/verify that the repository has Pages enabled/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "GitHub Pages is not enabled for the repository (configure-pages cannot find " <>
          "a site). Enabling it is a repository setting: escalate, never fix in code."
    },
    %{
      name: "canon_lockstep",
      layer: :config,
      patterns: [
        ~r/the canon and the spine are not in lockstep/i,
        ~r/canon[^\n]{0,40}lockstep[^\n]{0,40}(FAIL|mismatch)/i
      ],
      action: :fix,
      retries: 0,
      fix: "bump-canon-pin-to-published",
      escalation: :owner,
      reason:
        "The repo is pinned to an older canon than standards publishes. Bump the " <>
          "canon pin to the published version. Never loosen the spine gate to pass."
    },
    %{
      name: "toolchain_segfault",
      layer: :toolchain,
      # Only when the crashing command is a known runtime or compiler. A
      # segfault in a program the change built is a code failure
      # (`process_segfault`).
      patterns: [
        ~r/Segmentation fault[^\n]*\b(mix|elixir|erl|beam\.smp|rustc|cargo|ghc|idris2|zig|pack|hypatia)\b/,
        ~r/\b(beam\.smp|rustc|idris2|ghc)\b[^\n]*\b(SIGSEGV|signal 11)\b/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A tool crashed with SIGSEGV (exit 139), not a test verdict. Do not rerun: " <>
          "the estate case was deterministic (OTP 28.5.0.8 miscompiling PinSites in " <>
          "`mix hypatia.rsr_score`, hypatia#1062). Record the runtime version and " <>
          "escalate; pin a known-good runtime rather than editing code to dodge it."
    },
    %{
      name: "bench_regression",
      layer: :code,
      patterns: [
        ~r/regress\w*[^\n]{0,80}\b\d+(\.\d+)?x\b/i,
        ~r/\b\d+(\.\d+)?x\b[^\n]{0,80}(baseline|threshold|limit)/i
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A benchmark exceeded its baseline. Whether to fix the code or accept a new " <>
          "baseline is a judgement for the owner; a bot may not rebaseline."
    },
    %{
      name: "format_diff",
      layer: :code,
      patterns: [~r/(^|\s)Diff in \S+:\d+:/],
      action: :fix,
      retries: 0,
      fix: "run-formatter",
      escalation: :code_owner,
      reason: "A formatter check found unformatted code. Run the formatter and commit."
    },
    %{
      name: "compile_error",
      layer: :code,
      patterns: [
        ~r/error\[E\d{4}\]/,
        ~r/error: could not compile/,
        ~r/== Compilation error in file/
      ],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason: "The code did not compile. Report to the change's author."
    },
    %{
      name: "runtime_exception",
      layer: :code,
      patterns: [~r/\*\* \(\w+(Error|Exception)\)/, ~r/thread '[^']+' panicked at/],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason: "A program raised or panicked. Report to its owner with the evidence line."
    },
    %{
      name: "process_segfault",
      layer: :code,
      patterns: [~r/Segmentation fault/, ~r/exit code 139\b/, ~r/\bSIGSEGV\b/],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason:
        "A process crashed with SIGSEGV (exit 139) and the log does not name a known " <>
          "runtime or compiler, so treat it as the program's own crash and report it."
    },
    %{
      name: "test_failure",
      layer: :code,
      patterns: [
        ~r/\b\d+ tests?, [1-9]\d* failures?/,
        ~r/test result: FAILED/,
        ~r/(^|\s)FAILED\s+\S+::/
      ],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason: "Tests failed. This is a code failure; report it to the change's author."
    },
    %{
      name: "upstream_artifact_missing",
      layer: :code,
      patterns: [
        ~r/Artifact not found for name/,
        ~r/Unable to download artifact\(s\)/,
        ~r/No artifacts found|Unable to find any artifacts/
      ],
      action: :upstream,
      retries: 0,
      fix: nil,
      escalation: :none,
      reason:
        "An artifact a job expects was never uploaded: the job that makes it failed, " <>
          "crashed (for example a scanner SIGSEGV) or was cancelled. Triage that job."
    },
    %{
      name: "rollup_cascade",
      layer: :code,
      patterns: [~r/\d+ of \d+ [\w -]+ (did not report success|did not complete)/],
      action: :upstream,
      retries: 0,
      fix: nil,
      escalation: :none,
      reason:
        "A status rollup is red because jobs it waits on are red or missing. Triage " <>
          "those jobs, not this one; fixing the rollup would hide them."
    }
  ]

  @forbidden [
    :admin_merge,
    :drop_required_check,
    :rename_required_check,
    :edit_ruleset,
    :edit_licence_or_spdx,
    :loosen_canon_gate,
    :skip_or_quarantine_test,
    # A security finding (Hypatia critical, a Sonar security rating) stays
    # open until a person resolves it; no rule closes, baselines or hides it.
    :close_or_baseline_security_finding
  ]

  @doc "The rule id for a rule name."
  def id(name), do: "#{@domain}/#{name}"

  # Rules that are not log patterns: they take check states, workflow sets or
  # workflow files. Listed so `rules/0` is the whole inventory.
  @supplemental [
    %{
      name: "required_check_never_reported",
      layer: :config,
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      input: :check_states,
      reason: "A required check never reported (check_gate/2)."
    },
    %{
      name: "scanner_pin_drift",
      layer: :config,
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      input: :workflow_set,
      reason: "panic-attack pinned at different commits (check_scanner_pin_drift/1)."
    },
    %{
      name: "always_after_cancel",
      layer: :config,
      action: :fix,
      retries: 0,
      fix: "always-to-not-cancelled",
      escalation: :owner,
      input: :workflow,
      reason: "if: always() job that downloads its needs' artifacts (WorkflowAudit WF029)."
    }
  ]

  @doc """
  Every rule, in precedence order, without its patterns. Each carries its
  `route` under cicd-squabbler ADR 0003: `first-line` (the squabbler acts),
  `referral` (Hypatia answers), `owner-only` or `noise`.
  """
  def rules do
    (Enum.map(@rules, &Map.put(&1, :input, :log)) ++ @supplemental)
    |> Enum.sort_by(&rank/1)
    |> Enum.map(fn r ->
      r |> Map.delete(:patterns) |> Map.put(:id, id(r.name)) |> Map.put(:route, route(r.action))
    end)
  end

  @doc """
  The ADR 0003 route for an action. Hypatia never reruns or fixes on its own:
  a `rerun` or `fix` decision is advice to the first-line fixer.
  """
  def route(:rerun), do: "first-line"
  def route(:fix), do: "first-line"
  def route(:owner), do: "owner-only"
  def route(:none), do: "noise"
  def route(_), do: "referral"

  @doc "Actions a decision from these rules may never take."
  def forbidden_actions, do: @forbidden

  @doc """
  Classifies a failed job from its log text and context.

  Context keys (all optional): `:exit_code`, `:conclusion` (GitHub's job
  conclusion, e.g. "cancelled"), `:pr_state` (`:open`, `:merged`, `:closed`).

  Returns `%{primary: finding | nil, matches: [finding]}`. A finding carries
  `rule`, `layer`, `action`, `retries`, `fix`, `escalation`, `reason`,
  `evidence` (the first matching line) and `line`.
  """
  def classify(log, ctx \\ %{}) when is_binary(log) do
    lines = log |> String.split("\n") |> Enum.with_index(1)

    matches =
      @rules
      |> Enum.flat_map(fn rule ->
        case first_hit(rule, lines) do
          nil -> []
          {text, n} -> [finding(rule, text, n)]
        end
      end)
      |> add_conclusion(ctx)
      |> refine(ctx)
      |> Enum.sort_by(&{rank(&1), order(&1)})

    %{primary: List.first(matches), matches: matches}
  end

  @doc """
  Applies the loop limit. `attempts` is how many reruns this
  `{repo, workflow, sha, rule}` has already had. Returns the finding with its
  final `action`, and `persistent: true` once a transient budget is spent.
  """
  def decide(nil, _attempts), do: nil

  def decide(%{action: :rerun, retries: budget} = finding, attempts)
      when is_integer(attempts) and attempts >= budget do
    %{finding | action: finding.escalation}
    |> Map.put(:persistent, true)
  end

  def decide(finding, _attempts), do: Map.put(finding, :persistent, false)

  @doc """
  Required checks that never reported ("Expected — Waiting for status to be
  reported"). `required` and `reported` are check context names. A reported
  name that differs only in case or surrounding space is named as the likely
  intended check (name drift); otherwise the workflow never ran for this
  event (a path filter, an event filter, or a deleted workflow).

  Both outcomes go to the owner: required checks are settings this system
  never changes.
  """
  def check_gate(required, reported) when is_list(required) and is_list(reported) do
    norm = fn s -> s |> String.trim() |> String.downcase() end
    reported_set = MapSet.new(reported)
    by_norm = Map.new(reported, &{norm.(&1), &1})

    for name <- required, not MapSet.member?(reported_set, name) do
      near = Map.get(by_norm, norm.(name))

      %{
        rule: id("required_check_never_reported"),
        layer: :config,
        action: :owner,
        retries: 0,
        fix: nil,
        escalation: :owner,
        check: name,
        near_match: near,
        reason:
          if near do
            "Required check #{inspect(name)} never reported, but #{inspect(near)} did. " <>
              "The required name and the job name have drifted apart."
          else
            "Required check #{inspect(name)} never reported. Its workflow did not run " <>
              "for this event (path or event filter, or the workflow is gone)."
          end
      }
    end
  end

  @scanner_pin ~r/^\s*(PANIC_ATTACK_SHA|PA_SHA|panic[-_]attack[-_]sha)\s*:\s*["']?([0-9a-f]{40})/im
  # A reusable workflow's input (`panic-ref`, `panic-rev`, `panic-attack-sha`)
  # whose `default:` is the pin.
  @scanner_input ~r/^[ \t]*(panic(?:[-_]attack)?[-_](?:sha|ref|rev|commit))[ \t]*:[ \t]*\n(?:[ \t]+(?!default:)[^\n]*\n){0,6}?[ \t]+default:[ \t]*["']?([0-9a-f]{40})/im

  @doc """
  Scanner pin drift: panic-attack pinned at different commits across a set of
  workflow files (`%{label => content}`, labels may name several repos).
  Returns one finding per pin site when more than one commit is pinned, and
  `[]` otherwise. Which commit is canonical is the owner's call.
  """
  def check_scanner_pin_drift(contents) when is_map(contents) do
    sites =
      for {label, content} <- contents,
          re <- [@scanner_pin, @scanner_input],
          [_, key, sha] <- Regex.scan(re, content) do
        %{file: label, key: key, sha: String.downcase(sha)}
      end
      |> Enum.uniq()

    shas = sites |> Enum.map(& &1.sha) |> Enum.uniq() |> Enum.sort()

    if length(shas) > 1 do
      for site <- Enum.sort_by(sites, &{&1.file, &1.key}) do
        %{
          rule: id("scanner_pin_drift"),
          layer: :config,
          action: :owner,
          retries: 0,
          fix: nil,
          escalation: :owner,
          file: site.file,
          key: site.key,
          sha: site.sha,
          pinned: shas,
          reason:
            "panic-attack is pinned at #{length(shas)} different commits " <>
              "(#{Enum.map_join(shas, ", ", &String.slice(&1, 0, 8))}); findings " <>
              "differ by lane. Pick one commit and pin it everywhere."
        }
      end
    else
      []
    end
  end

  # ── internals ─────────────────────────────────────────────────────────

  defp first_hit(rule, lines) do
    Enum.find_value(lines, fn {text, n} ->
      if Enum.any?(rule.patterns, &Regex.match?(&1, text)), do: {String.trim(text), n}
    end)
  end

  defp finding(rule, text, n) do
    %{
      rule: id(rule.name),
      layer: rule.layer,
      action: rule.action,
      retries: rule.retries,
      fix: rule.fix,
      escalation: rule.escalation,
      reason: rule.reason,
      route: route(rule.action),
      evidence: text,
      line: n
    }
  end

  # A conclusion of "cancelled" is authoritative even when the log says nothing.
  defp add_conclusion(matches, %{conclusion: c}) when c in ["cancelled", :cancelled] do
    cancelled = id("runner_cancelled")

    if Enum.any?(matches, &(&1.rule == cancelled)) do
      matches
    else
      rule = Enum.find(@rules, &(&1.name == "runner_cancelled"))
      [finding(rule, "conclusion: cancelled", 0) | matches]
    end
  end

  defp add_conclusion(matches, _ctx), do: matches

  # Context that changes a rule's action.
  defp refine(matches, ctx) do
    race = id("merge_ref_race")

    # A merge ref that vanished because the PR merged or closed is a stale
    # run, not a failure.
    Enum.map(matches, fn
      %{rule: ^race} = f when is_map_key(ctx, :pr_state) ->
        if ctx.pr_state in [:merged, :closed], do: %{f | action: :none, retries: 0}, else: f

      f ->
        f
    end)
    |> add_exit_code(ctx)
  end

  defp add_exit_code(matches, %{exit_code: 139}) do
    segv = [id("toolchain_segfault"), id("process_segfault")]

    if Enum.any?(matches, &(&1.rule in segv)) do
      matches
    else
      rule = Enum.find(@rules, &(&1.name == "process_segfault"))
      [finding(rule, "exit code 139", 0) | matches]
    end
  end

  defp add_exit_code(matches, _ctx), do: matches

  defp rank(%{layer: layer}), do: Enum.find_index(@layers, &(&1 == layer))

  defp order(%{rule: rule_id}) do
    Enum.find_index(@rules, &(id(&1.name) == rule_id)) || length(@rules)
  end
end
