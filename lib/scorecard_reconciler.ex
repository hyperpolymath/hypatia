# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ScorecardReconciler do
  @moduledoc """
  Closes the GitHub code-scanning alert-lifecycle loop.

  Hypatia historically *ingested* Scorecard/code-scanning findings into its
  own pipeline but never wrote back to the authoritative GitHub alert state.
  Non-actionable findings (Maintained) and design-correct exceptions (the
  SLSA generator, which must stay tag-pinned) therefore re-accumulated as
  "open" on every audit — the structural cause of estate-wide recurrence
  and the maintainer's weekly credit burn. See hyperpolymath/hypatia#260.

  This module implements the missing control loop:

      Sense(authoritative)  -> reconcile/2 fetches LIVE open alerts via API
      Classify              -> classify/1 (pure 4-axis taxonomy)
      Act                   -> dismiss-with-rationale | fix-request | escalate
      Verify                -> verify/2 re-queries; reopened-with-no-change
                               is a recurrence DEFECT, not a new finding
      Learn                 -> Registry persists every decision by a stable
                               fingerprint, mirrored to .git-private-farm,
                               so a class adjudicated once is never re-reasoned
                               (and never re-bills credit)

  The classification core is pure and fully unit-tested; the HTTP/act shell
  mirrors the canonical `curl + GITHUB_TOKEN` pattern used by
  `Hypatia.Rules.DependabotAlerts`.
  """

  require Logger
  alias Hypatia.Rules.SecurityErrors
  alias Hypatia.ScorecardReconciler.Registry

  @github_api_base "https://api.github.com"
  @max_alerts 100

  # ── Taxonomy ───────────────────────────────────────────────────────────
  #
  # 4 axes collapse to ONE unambiguous lifecycle action per finding:
  #   actionable_by_code? · remediation_is_safe? · effective_vs_nominal? ·
  #   activity_only?
  #
  # Actions:
  #   :dismiss_info    — no code fix exists (activity/info signal). Dismiss
  #                      "won't fix" with rationale. (Maintained, Contributors)
  #   :dismiss_accept  — code change *possible* but the canonical remediation
  #                      is HARMFUL / it is correct-by-design. Dismiss
  #                      "won't fix" with rationale. (SLSA pin-exempt)
  #   :fix             — safe, deterministic *code* remediation exists. Emit
  #                      a fix-request (PR; auto-merge if CI-green + low-risk).
  #   :fix_settings    — safe, deterministic *repository-configuration*
  #                      remediation exists via the GitHub settings API
  #                      (enable branch protection, require reviews). NOT a
  #                      code change and NOT non-actionable — a third, narrow
  #                      action class so config findings are auto-remediated
  #                      under full-auto policy instead of escalated. (#265)
  #   :open_escalate   — unknown / not safely automatable. Leave OPEN and
  #                      ensure a tracking issue (never silently drop —
  #                      "didn't look" is an escaped-defect KPI breach).

  # Scorecard rules that are pure activity/information signals: Scorecard
  # itself states no remediation is needed.
  @info_rules ~w(MaintainedID ContributorsID CITestsID CIIBestPracticesID)

  # Scorecard rules that are repo-CONFIGURATION-actionable: not code, not
  # non-actionable — deterministically remediable via the GitHub settings
  # API (#265). Surfaced by the natsci-studio live calibration where these
  # were being :open_escalate'd despite a safe automatic fix existing.
  @settings_rules ~w(BranchProtectionID CodeReviewID)

  @doc """
  Pure classification of a single code-scanning alert map (as returned by
  the GitHub API). `ctx` may carry `:located_action_ref` — the `uses:` ref
  at the alert's location, when the caller resolved it — so SLSA-class
  exemptions can be detected. Returns `{action, reason_code, rationale}`.
  `reason_code` is the GitHub dismissal reason where applicable.
  """
  def classify(alert, ctx \\ %{}) do
    rule = get_in(alert, ["rule", "id"]) || ""
    located = Map.get(ctx, :located_action_ref)

    cond do
      rule in @info_rules ->
        {:dismiss_info, "won't fix",
         "Informational/activity signal — OSSF Scorecard states no " <>
           "remediation is required for #{rule}; it only reflects project " <>
           "activity. Resolved by ongoing activity, not a code change."}

      rule == "PinnedDependenciesID" and is_binary(located) and
          SecurityErrors.pin_exempt?(located) ->
        {:dismiss_accept, "won't fix",
         "Accepted exception: " <> SecurityErrors.pin_exemption_reason(located)}

      rule == "SASTID" ->
        {:fix, nil,
         "Effective-SAST remediation: set the CodeQL language matrix to " <>
           "`actions` so SAST produces results on every commit (#261)."}

      rule == "PinnedDependenciesID" ->
        {:fix, nil,
         "Pin the third-party dependency to a full-length commit SHA " <>
           "(not pin-exempt)."}

      rule in @settings_rules ->
        {:fix_settings, nil,
         "Repository-configuration finding `#{rule}` — deterministically " <>
           "remediable via the GitHub settings API (enable branch " <>
           "protection / require pull-request reviews on the default " <>
           "branch). Not a code change and not non-actionable; auto-applied " <>
           "under full-auto policy, escalated under conservative. (#265)"}

      true ->
        {:open_escalate, nil,
         "No safe automated lifecycle action known for rule `#{rule}` — " <>
           "left open; tracking issue ensured (escaped-defect guard)."}
    end
  end

  @doc """
  Stable fingerprint for a finding: `repo|tool|rule|normalized-path`.
  Deliberately excludes line numbers (they drift; fingerprints must not) so
  a dismissal survives unrelated edits and the alert does not re-open.
  """
  def fingerprint(owner, repo, alert) do
    tool = get_in(alert, ["tool", "name"]) || "unknown"
    rule = get_in(alert, ["rule", "id"]) || "unknown"
    path = get_in(alert, ["most_recent_instance", "location", "path"]) || "-"
    raw = "#{owner}/#{repo}|#{tool}|#{rule}|#{Path.basename(path)}"
    :crypto.hash(:sha256, raw) |> Base.encode16(case: :lower) |> binary_part(0, 16)
  end

  @doc """
  Full reconcile pass for one repo. Returns a summary map with the
  meta-audit gates (looked/found/classified/acted/...). `opts`:
    * `:dry_run` (default false) — classify + record, do not mutate GitHub
  """
  def reconcile(owner, repo, opts \\ []) do
    dry = Keyword.get(opts, :dry_run, false)
    # :full_auto (default) auto-applies :fix_settings via the settings API;
    # :conservative escalates settings findings for human review instead of
    # mutating repo configuration automatically. (#265)
    policy = Keyword.get(opts, :policy, :full_auto)

    case fetch_open_alerts(owner, repo) do
      {:error, reason} ->
        %{repo: "#{owner}/#{repo}", looked: false, error: reason}

      {:ok, alerts} ->
        reg0 = Registry.load()

        {results, reg} =
          Enum.map_reduce(alerts, reg0, fn alert, reg_acc ->
            fp = fingerprint(owner, repo, alert)
            ctx = %{located_action_ref: locate_action_ref(owner, repo, alert)}
            {action, reason_code, rationale} = classify(alert, ctx)
            number = alert["number"]

            acted =
              cond do
                dry ->
                  :skipped_dry_run

                action in [:dismiss_info, :dismiss_accept] ->
                  do_dismiss(owner, repo, number, reason_code, rationale)

                action == :fix ->
                  :fix_requested

                action == :fix_settings and policy == :full_auto ->
                  do_fix_settings(owner, repo, alert)

                action == :fix_settings ->
                  # :conservative — never silently drop; escalate.
                  :escalated_conservative

                true ->
                  :escalated
              end

            reg_acc =
              Registry.record(reg_acc, fp, %{
                "repo" => "#{owner}/#{repo}",
                "rule" => get_in(alert, ["rule", "id"]),
                "alert" => number,
                "action" => Atom.to_string(action),
                "rationale" => rationale,
                "acted" => to_string(acted),
                "decided_at" => DateTime.utc_now() |> DateTime.to_iso8601()
              })

            {%{alert: number, fp: fp, action: action, acted: acted}, reg_acc}
          end)

        unless dry, do: Registry.persist(reg)

        %{
          repo: "#{owner}/#{repo}",
          looked: true,
          found: length(alerts),
          classified: length(results),
          dismissed: Enum.count(results, &(&1.acted in [:dismissed, :already_dismissed])),
          fix_requested: Enum.count(results, &(&1.action == :fix)),
          settings_remediated: Enum.count(results, &(&1.acted == :settings_remediated)),
          settings_actionable: Enum.count(results, &(&1.action == :fix_settings)),
          escalated: Enum.count(results, &(&1.action == :open_escalate)),
          results: results
        }
    end
  end

  @doc """
  Verify pass: a finding the registry recorded as dismissed that is OPEN
  again is a recurrence DEFECT (KPI 1), surfaced for escalation — not
  silently re-dismissed.
  """
  def verify(owner, repo) do
    with {:ok, alerts} <- fetch_open_alerts(owner, repo) do
      reg = Registry.load()

      regressions =
        for alert <- alerts,
            entry = Registry.get(reg, fingerprint(owner, repo, alert)),
            entry["action"] in ["dismiss_info", "dismiss_accept"] do
          %{
            alert: alert["number"],
            fp: fingerprint(owner, repo, alert),
            recurrence_defect: true,
            prior: entry
          }
        end

      {:ok, %{repo: "#{owner}/#{repo}", recurrence_defects: regressions}}
    end
  end

  # ── IO shell ───────────────────────────────────────────────────────────

  defp fetch_open_alerts(owner, repo) do
    token = System.get_env("GITHUB_TOKEN")

    if token in [nil, ""] do
      {:error, "GITHUB_TOKEN not set"}
    else
      url =
        "#{@github_api_base}/repos/#{owner}/#{repo}/code-scanning/alerts" <>
          "?state=open&per_page=#{@max_alerts}"

      case System.cmd(
             "curl",
             [
               "-s",
               "-f",
               "-H",
               "Accept: application/vnd.github+json",
               "-H",
               "Authorization: Bearer #{token}",
               "-H",
               "X-GitHub-Api-Version: 2022-11-28",
               url
             ],
             stderr_to_stdout: true
           ) do
        {body, 0} ->
          case Jason.decode(body) do
            {:ok, a} when is_list(a) -> {:ok, a}
            {:ok, %{"message" => m}} -> {:error, "GitHub API: #{m}"}
            _ -> {:error, "Invalid JSON from code-scanning API"}
          end

        {err, _} ->
          {:error, "curl failed: #{String.slice(err, 0, 200)}"}
      end
    end
  end

  defp do_dismiss(owner, repo, number, reason_code, rationale) do
    token = System.get_env("GITHUB_TOKEN")
    # GitHub caps the dismissal comment at 280 chars.
    comment = String.slice(rationale || "", 0, 270)

    payload =
      Jason.encode!(%{
        "state" => "dismissed",
        "dismissed_reason" => reason_code || "won't fix",
        "dismissed_comment" => comment
      })

    url =
      "#{@github_api_base}/repos/#{owner}/#{repo}/code-scanning/alerts/#{number}"

    case System.cmd(
           "curl",
           [
             "-s",
             "-f",
             "-X",
             "PATCH",
             "-H",
             "Accept: application/vnd.github+json",
             "-H",
             "Authorization: Bearer #{token}",
             "-H",
             "X-GitHub-Api-Version: 2022-11-28",
             "-d",
             payload,
             url
           ],
           stderr_to_stdout: true
         ) do
      {_, 0} ->
        :dismissed

      {err, _} ->
        Logger.warning("reconciler dismiss ##{number} failed: #{String.slice(err, 0, 160)}")
        :dismiss_failed
    end
  end

  # #265: deterministic, safe, API-driven repository-configuration
  # remediation for BranchProtectionID / CodeReviewID. Enables default-branch
  # protection requiring at least one approving pull-request review (this one
  # change satisfies both checks). Idempotent — the protection PUT is
  # declarative, so re-running converges to the same state.
  defp do_fix_settings(owner, repo, _alert) do
    token = System.get_env("GITHUB_TOKEN")

    with branch when is_binary(branch) <- default_branch(owner, repo, token) do
      payload =
        Jason.encode!(%{
          "required_status_checks" => nil,
          "enforce_admins" => false,
          "required_pull_request_reviews" => %{
            "required_approving_review_count" => 1,
            "dismiss_stale_reviews" => true
          },
          "restrictions" => nil,
          "allow_force_pushes" => false,
          "allow_deletions" => false
        })

      url =
        "#{@github_api_base}/repos/#{owner}/#{repo}/branches/#{branch}/protection"

      case System.cmd(
             "curl",
             [
               "-s",
               "-f",
               "-X",
               "PUT",
               "-H",
               "Accept: application/vnd.github+json",
               "-H",
               "Authorization: Bearer #{token}",
               "-H",
               "X-GitHub-Api-Version: 2022-11-28",
               "-d",
               payload,
               url
             ],
             stderr_to_stdout: true
           ) do
        {_, 0} ->
          :settings_remediated

        {err, _} ->
          Logger.warning(
            "reconciler fix_settings #{owner}/#{repo} failed: #{String.slice(err, 0, 160)}"
          )

          :settings_failed
      end
    else
      _ -> :settings_failed
    end
  end

  defp default_branch(owner, repo, token) do
    url = "#{@github_api_base}/repos/#{owner}/#{repo}"

    case System.cmd(
           "curl",
           [
             "-s",
             "-f",
             "-H",
             "Accept: application/vnd.github+json",
             "-H",
             "Authorization: Bearer #{token}",
             "-H",
             "X-GitHub-Api-Version: 2022-11-28",
             url
           ],
           stderr_to_stdout: true
         ) do
      {body, 0} ->
        case Jason.decode(body) do
          {:ok, %{"default_branch" => b}} when is_binary(b) -> b
          _ -> nil
        end

      _ ->
        nil
    end
  end

  # Best-effort resolution of the `uses:` ref at an alert's location so
  # SLSA-class pin-exemptions are detected. Reads the workflow file at the
  # reported path+line via the contents API; nil on any miss (caller then
  # falls through to the generic :fix path, which is safe).
  defp locate_action_ref(owner, repo, alert) do
    path = get_in(alert, ["most_recent_instance", "location", "path"])
    line = get_in(alert, ["most_recent_instance", "location", "start_line"])
    token = System.get_env("GITHUB_TOKEN")

    with true <- is_binary(path),
         true <- is_integer(line),
         url =
           "#{@github_api_base}/repos/#{owner}/#{repo}/contents/#{path}",
         {body, 0} <-
           System.cmd(
             "curl",
             [
               "-s",
               "-f",
               "-H",
               "Accept: application/vnd.github.raw+json",
               "-H",
               "Authorization: Bearer #{token}",
               "-H",
               "X-GitHub-Api-Version: 2022-11-28",
               url
             ],
             stderr_to_stdout: true
           ),
         lines = String.split(body, "\n"),
         target when is_binary(target) <- Enum.at(lines, line - 1),
         [_, ref] <- Regex.run(~r/uses:\s*(\S+)/, target) do
      ref
    else
      _ -> nil
    end
  end
end

defmodule Hypatia.ScorecardReconciler.Registry do
  @moduledoc """
  Fingerprint-keyed lifecycle-decision store — the learning substrate.

  Persisted as JSON under `~/.git-private-farm/` so the estate's accumulated
  adjudications survive a total GitHub-state wipe (the "rebuild from DNA"
  durability KPI). An in-memory map is threaded through a reconcile pass and
  written once at the end.
  """

  @rel "hypatia-exception-registry.json"

  def path do
    base = Path.expand("~/.git-private-farm")
    Path.join(base, @rel)
  end

  def load do
    case File.read(path()) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{} = m} -> m
          _ -> %{}
        end

      _ ->
        %{}
    end
  end

  def get(reg, fp), do: Map.get(reg, fp)

  def known?(reg, fp), do: Map.has_key?(reg, fp)

  @doc "Returns the registry map with `fp` recorded (pure; persist/1 writes)."
  def record(reg, fp, entry) when is_map(reg) do
    Map.put(reg, fp, entry)
  end

  # `record/3` is pure but reconcile threads a single map; to keep call
  # sites simple it mutates via the return value. For ergonomics in the
  # IO path we also offer an Agent-free accumulate-then-persist.
  def persist(reg) when is_map(reg) do
    File.mkdir_p!(Path.dirname(path()))
    File.write!(path(), Jason.encode!(reg, pretty: true))
    :ok
  end
end
