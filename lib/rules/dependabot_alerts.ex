# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.DependabotAlerts do
  @moduledoc """
  GitHub Dependabot alert querying and risk assessment.

  Queries the GitHub REST API for active Dependabot alerts on repositories,
  classifies them by severity and age, and generates findings for the
  safety triangle pipeline.

  Requires GITHUB_TOKEN environment variable with `security_events` scope.

  Dispatches to:
  - rhodibot: auto-update for low/medium severity
  - echidnabot: deep analysis for high/critical
  - panicbot: immediate action for critical with known exploits

  Rule IDs: DA001-DA004
  """

  require Logger

  @github_api_base "https://api.github.com"

  # Maximum alerts to fetch per repo (API pagination)
  @max_alerts_per_repo 100

  # Age thresholds for staleness scoring (days)
  @stale_thresholds %{
    critical: 3,
    high: 7,
    medium: 30,
    low: 90
  }

  # ─── DA001: Open Dependabot alerts ─────────────────────────────────────

  @doc """
  DA001: Query open Dependabot alerts for a repository.
  Returns findings grouped by severity with age-based staleness scoring.

  Requires `owner/repo` format and a valid GITHUB_TOKEN.
  """
  def da001_open_alerts(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        alerts
        |> Enum.filter(&(&1["state"] == "open"))
        |> Enum.map(fn alert ->
          severity = get_in(alert, ["security_advisory", "severity"]) || "unknown"
          cve = get_in(alert, ["security_advisory", "cve_id"])
          package = get_in(alert, ["security_vulnerability", "package", "name"]) || "unknown"
          ecosystem = get_in(alert, ["security_vulnerability", "package", "ecosystem"]) || "unknown"
          created = alert["created_at"]
          age_days = age_in_days(created)

          stale_threshold = Map.get(@stale_thresholds, String.to_existing_atom(severity), 90)
          is_stale = age_days > stale_threshold

          # Floor the exported severity at :medium so fresh low-severity
          # alerts are NOT silently filtered out by the CLI's default
          # severity threshold (which is "medium" -- rank 3). Native
          # severity from the GitHub advisory is preserved in
          # detail.severity below for downstream consumers that want to
          # see the raw classification. Three low-severity Dependabot
          # alerts on 007 sat open for weeks because this floor was not
          # in place -- every `:low` finding was dropped by the CLI
          # filter before reaching any consumer (2026-04-17 audit, see
          # 007-lang/audits/audit-dependabot-automation-gap-2026-04-17.md).
          mapped_severity = case severity do
            "critical" -> :critical
            "high" -> :high
            "medium" -> :medium
            "low" -> :medium
            _ -> :medium
          end

          native_severity = case severity do
            "critical" -> :critical
            "high" -> :high
            "medium" -> :medium
            "low" -> :low
            _ -> :medium
          end

          # Escalate stale alerts (using native severity for the
          # "stale + low/medium" test so we don't double-promote).
          escalated_severity =
            if is_stale and native_severity in [:medium, :low] do
              :high
            else
              mapped_severity
            end

          %{
            rule: "DA001",
            file: "#{ecosystem}/#{package}",
            severity: escalated_severity,
            reason: build_alert_reason(severity, package, cve, age_days, is_stale),
            action: determine_action(native_severity, is_stale),
            detail: %{
              alert_number: alert["number"],
              cve: cve,
              package: package,
              ecosystem: ecosystem,
              severity: severity,
              age_days: age_days,
              is_stale: is_stale,
              created_at: created,
              url: alert["html_url"]
            }
          }
        end)

      {:error, reason} ->
        Logger.warning("DA001: Failed to fetch Dependabot alerts: #{reason}")
        []
    end
  end

  # ─── DA002: Alert severity summary ─────────────────────────────────────

  @doc """
  DA002: Generate a severity summary finding if alert counts exceed thresholds.
  This is a meta-finding that triggers fleet-wide attention.
  """
  def da002_severity_summary(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        open = Enum.filter(alerts, &(&1["state"] == "open"))
        by_severity = Enum.group_by(open, fn a ->
          get_in(a, ["security_advisory", "severity"]) || "unknown"
        end)

        critical_count = length(Map.get(by_severity, "critical", []))
        high_count = length(Map.get(by_severity, "high", []))
        total = length(open)

        findings = []

        findings =
          if critical_count > 0 do
            [%{
              rule: "DA002",
              file: "#{owner}/#{repo}",
              severity: :critical,
              reason: "#{critical_count} critical Dependabot alert(s) -- immediate patching required",
              action: :escalate,
              detail: %{critical: critical_count, high: high_count, total: total}
            } | findings]
          else
            findings
          end

        findings =
          if high_count >= 5 do
            [%{
              rule: "DA002",
              file: "#{owner}/#{repo}",
              severity: :high,
              reason: "#{high_count} high-severity Dependabot alert(s) -- batch update recommended",
              action: :batch_update,
              detail: %{high: high_count, total: total}
            } | findings]
          else
            findings
          end

        findings =
          if total >= 10 do
            [%{
              rule: "DA002",
              file: "#{owner}/#{repo}",
              severity: :medium,
              reason: "#{total} total open Dependabot alert(s) -- dependency hygiene review needed",
              action: :review,
              detail: %{total: total, by_severity: Map.new(by_severity, fn {k, v} -> {k, length(v)} end)}
            } | findings]
          else
            findings
          end

        findings

      {:error, _} -> []
    end
  end

  # ─── DA003: Stale alerts ───────────────────────────────────────────────

  @doc """
  DA003: Detect alerts that have been open past their severity-appropriate threshold.
  Critical alerts stale after 3 days, high after 7, medium after 30, low after 90.
  """
  def da003_stale_alerts(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        alerts
        |> Enum.filter(&(&1["state"] == "open"))
        |> Enum.filter(fn alert ->
          severity = get_in(alert, ["security_advisory", "severity"]) || "medium"
          threshold = Map.get(@stale_thresholds, String.to_existing_atom(severity), 90)
          age_in_days(alert["created_at"]) > threshold
        end)
        |> Enum.map(fn alert ->
          severity = get_in(alert, ["security_advisory", "severity"]) || "medium"
          package = get_in(alert, ["security_vulnerability", "package", "name"]) || "unknown"
          age = age_in_days(alert["created_at"])
          threshold = Map.get(@stale_thresholds, String.to_existing_atom(severity), 90)

          %{
            rule: "DA003",
            file: package,
            severity: :high,
            reason: "Dependabot alert for #{package} (#{severity}) is #{age} days old (threshold: #{threshold} days) -- overdue for remediation",
            action: :escalate,
            detail: %{
              alert_number: alert["number"],
              package: package,
              original_severity: severity,
              age_days: age,
              threshold_days: threshold
            }
          }
        end)

      {:error, _} -> []
    end
  end

  # ─── DA004: Dismissed without fix ──────────────────────────────────────

  @doc """
  DA004: Detect alerts dismissed without a fix (risk acceptance without documentation).
  Severity: medium.
  Action: review dismissal reason, ensure documented.
  """
  def da004_dismissed_without_fix(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        alerts
        |> Enum.filter(fn a ->
          a["state"] == "dismissed" and
          a["dismissed_reason"] in ["tolerable_risk", "no_bandwidth", nil]
        end)
        |> Enum.map(fn alert ->
          severity = get_in(alert, ["security_advisory", "severity"]) || "unknown"
          package = get_in(alert, ["security_vulnerability", "package", "name"]) || "unknown"
          reason = alert["dismissed_reason"] || "no reason given"

          %{
            rule: "DA004",
            file: package,
            severity: :medium,
            reason: "Dependabot alert for #{package} (#{severity}) dismissed as '#{reason}' -- ensure risk is documented",
            action: :review,
            detail: %{
              alert_number: alert["number"],
              package: package,
              original_severity: severity,
              dismissed_reason: reason,
              dismissed_at: alert["dismissed_at"]
            }
          }
        end)

      {:error, _} -> []
    end
  end

  # ─── Comprehensive scan ───────────────────────────────────────────────

  @doc """
  Run all Dependabot alert checks for a repository.
  Requires `owner` and `repo` as separate strings.

  Returns `{:ok, result}` or `{:error, reason}`.
  """
  def scan(owner, repo) do
    token = System.get_env("GITHUB_TOKEN")

    if token == nil or token == "" do
      {:error, "GITHUB_TOKEN not set -- cannot query Dependabot alerts"}
    else
      findings =
        da001_open_alerts(owner, repo) ++
        da002_severity_summary(owner, repo) ++
        da003_stale_alerts(owner, repo) ++
        da004_dismissed_without_fix(owner, repo)

      # Deduplicate -- DA001 and DA003 may overlap
      deduped =
        findings
        |> Enum.uniq_by(fn f ->
          {f.rule, Map.get(f.detail, :alert_number, f.file)}
        end)

      {:ok, %{
        findings: deduped,
        total: length(deduped),
        by_severity: group_by_severity(deduped),
        dispatch: dispatch_recommendations(deduped)
      }}
    end
  end

  @doc """
  Scan from a local repo path -- extracts owner/repo from git remote.
  """
  def scan_from_path(repo_path) do
    case extract_owner_repo(repo_path) do
      {:ok, owner, repo} -> scan(owner, repo)
      {:error, reason} -> {:error, reason}
    end
  end

  # ─── Safety Triangle dispatch (DA005-DA008) ───────────────────────────
  #
  # Classifies a Dependabot alert through the Safety Triangle so the
  # FleetDispatcher can route it to the right bot at the right confidence.
  # Mirrors `Hypatia.Rules.ProofObligation`'s recipe shape: the returned
  # map is wrapped as `{:dependabot_fix, recipe, pattern}` and consumed
  # by `FleetDispatcher.dispatch_routed_action/1`.
  #
  #   eliminate  -> robot-repo-automaton auto-bumps the dep (auto_execute)
  #                 or rhodibot opens a PR (review) depending on confidence
  #   substitute -> rhodibot opens a PR for human review (major bump /
  #                 breaking change suspected)
  #   control    -> sustainabot advisory (no auto-fix: dismissed-without-fix,
  #                 no patched upstream version, transitive-only)
  #
  # Confidence floor 0.85 (review), ceiling 0.98; auto-execute threshold
  # is FleetDispatcher's 0.95 -- so "eliminate" alone does not guarantee
  # auto-execute; the bump type and severity jointly decide.

  # Ecosystems where semver is strictly enforced -- patch/minor bumps are
  # almost always non-breaking, so we rate them with higher confidence.
  @strict_semver_ecosystems ~w(rust npm pip pypi composer nuget maven packagist)

  @doc """
  DA005: Classify a raw Dependabot alert through the Safety Triangle.

  Returns one of:
  - `{:eliminate, confidence}` -- safe auto-bump (patch/minor within semver)
  - `{:substitute, reason}` -- needs PR review (major bump / breaking change)
  - `{:control, reason}` -- no safe fix (dismissed, no upstream patch, etc.)

  This is prover-agnostic -- the same classifier works across all
  ecosystems (npm/rust/pypi/go/maven/etc.). Confidence is tuned so that
  patch bumps on strict-semver ecosystems land in auto-execute, minor
  bumps in review, and anything looking like a major bump falls out of
  the auto-fix path entirely.
  """
  @spec classify_alert(map()) ::
          {:eliminate, float()}
          | {:substitute, String.t()}
          | {:control, String.t()}
  def classify_alert(alert) when is_map(alert) do
    state = Map.get(alert, "state", "open")
    severity = get_in(alert, ["security_advisory", "severity"]) || "medium"
    ecosystem = get_in(alert, ["security_vulnerability", "package", "ecosystem"]) || ""
    from_range = get_in(alert, ["security_vulnerability", "vulnerable_version_range"])
    to_version = get_in(alert, ["security_vulnerability", "first_patched_version", "identifier"])
    dismissed_reason = Map.get(alert, "dismissed_reason")

    cond do
      state == "dismissed" and dismissed_reason in ["tolerable_risk", "no_bandwidth", nil] ->
        {:control, "dismissed as '#{dismissed_reason || "no reason"}' -- human acceptance of risk"}

      state == "fixed" ->
        {:control, "already fixed upstream -- no action needed"}

      is_nil(to_version) or to_version == "" ->
        {:control, "no upstream patch available yet"}

      true ->
        bump = classify_bump(from_range, to_version)
        confidence = eliminate_confidence(severity, bump, ecosystem)

        case {bump, confidence} do
          {:major, _} ->
            {:substitute, "major version bump -- potentially breaking"}

          {_, c} when c >= 0.85 ->
            {:eliminate, c}

          {_, _} ->
            {:substitute, "low-confidence patch -- review before applying"}
        end
    end
  end

  @doc """
  DA006: Convert a raw Dependabot alert into a recipe map compatible with
  `FleetDispatcher.dispatch_routed_action/1`.

  The returned map is wrapped as `{:dependabot_fix, recipe, pattern}` by
  `fixes_from_alerts/3`. Fields:

  - `"id"` -- deterministic recipe ID (`da-` + hash of alert number + repo)
  - `"type"` -- `"dependabot_fix"`
  - `"triangle_tier"` -- `"eliminate" | "substitute" | "control"`
  - `"confidence"` -- 0.0..1.0
  - `"ecosystem"`, `"package"`, `"from_version"`, `"to_version"`, `"cve"`,
    `"severity"`, `"manifest_path"`, `"alert_number"`, `"alert_url"`
  - `"auto_fixable"` -- true iff eliminate tier and confidence >= 0.95
  - `"requires_human"` -- true for control tier
  - `"description"` -- log/PR-body summary line
  - `"repo"` -- `owner/repo`
  """
  @spec to_recipe(map(), keyword()) :: map()
  def to_recipe(alert, opts \\ []) when is_map(alert) do
    repo = Keyword.get(opts, :repo, "unknown")
    classification = classify_alert(alert)

    {tier, confidence, reason} =
      case classification do
        {:eliminate, c} -> {"eliminate", c, nil}
        {:substitute, r} -> {"substitute", 0.85, r}
        {:control, r} -> {"control", 0.50, r}
      end

    severity = get_in(alert, ["security_advisory", "severity"]) || "unknown"
    package = get_in(alert, ["security_vulnerability", "package", "name"]) || "unknown"
    ecosystem = get_in(alert, ["security_vulnerability", "package", "ecosystem"]) || "unknown"
    from_range = get_in(alert, ["security_vulnerability", "vulnerable_version_range"])
    to_version = get_in(alert, ["security_vulnerability", "first_patched_version", "identifier"])
    cve = get_in(alert, ["security_advisory", "cve_id"])
    manifest_path = get_in(alert, ["dependency", "manifest_path"])
    alert_number = alert["number"]
    alert_url = alert["html_url"]

    auto_fixable = tier == "eliminate" and confidence >= 0.95

    description =
      case tier do
        "eliminate" ->
          "Bump #{ecosystem}/#{package} from #{from_range || "?"} to #{to_version} (#{severity})"

        "substitute" ->
          "Review #{ecosystem}/#{package} bump to #{to_version} -- #{reason}"

        "control" ->
          "Dependabot alert #{alert_number} on #{ecosystem}/#{package} -- #{reason}"
      end

    %{
      "id" => recipe_id(alert_number, repo),
      "type" => "dependabot_fix",
      "triangle_tier" => tier,
      "confidence" => confidence,
      "ecosystem" => ecosystem,
      "package" => package,
      "from_version" => from_range,
      "to_version" => to_version,
      "cve" => cve,
      "severity" => severity,
      "manifest_path" => manifest_path,
      "alert_number" => alert_number,
      "alert_url" => alert_url,
      "auto_fixable" => auto_fixable,
      "requires_human" => tier == "control",
      "description" => description,
      "repo" => repo
    }
  end

  @doc """
  DA007: Convert a list of raw alerts into `{:dependabot_fix, recipe, pattern}`
  tuples ready for `FleetDispatcher.dispatch_routed_action/1`.

  `pattern` is a pared-down shape matching what FleetDispatcher expects:
  it carries `file`, `description`, `severity_score`, and `routed_repo`
  so downstream helpers can log / dispatch uniformly.
  """
  @spec fixes_from_alerts([map()], String.t(), String.t()) :: [
          {:dependabot_fix, map(), map()}
        ]
  def fixes_from_alerts(alerts, owner, repo) when is_list(alerts) do
    full_repo = "#{owner}/#{repo}"

    alerts
    |> Enum.filter(&(&1["state"] == "open"))
    |> Enum.map(fn alert ->
      recipe = to_recipe(alert, repo: full_repo)

      pattern = %{
        "id" => Map.get(recipe, "id"),
        "file" => Map.get(recipe, "manifest_path") || "dependencies",
        "description" => Map.get(recipe, "description"),
        "severity" => Map.get(recipe, "severity"),
        "severity_score" => severity_to_score(Map.get(recipe, "severity")),
        "routed_repo" => full_repo,
        "source" => "dependabot"
      }

      {:dependabot_fix, recipe, pattern}
    end)
  end

  @doc """
  DA008: Fetch alerts for a repo and produce routable dispatch tuples.

  Returns `{:ok, [tuples]}` or `{:error, reason}`. Does not dispatch --
  callers pipe the tuples into `FleetDispatcher.dispatch_routed_action/1`
  (or the batched dispatcher) so gate review, rate limiting, and
  exclusion-registry checks apply uniformly.
  """
  @spec fixes_from_repo(String.t(), String.t()) ::
          {:ok, [{:dependabot_fix, map(), map()}]} | {:error, String.t()}
  def fixes_from_repo(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} -> {:ok, fixes_from_alerts(alerts, owner, repo)}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Human-readable summary of a dependabot_fix recipe for logs.
  """
  @spec summary(map()) :: String.t()
  def summary(recipe) when is_map(recipe) do
    tier = Map.get(recipe, "triangle_tier", "unknown")
    pkg = Map.get(recipe, "package", "?")
    eco = Map.get(recipe, "ecosystem", "?")
    conf = Map.get(recipe, "confidence", 0.0)
    "DependabotFix[#{tier}] #{eco}/#{pkg} conf=#{:erlang.float_to_binary(conf, decimals: 2)}"
  end

  # Classify the kind of version bump from the vulnerable range and the
  # first patched version. This is a heuristic: if either string is
  # missing or un-parseable, default to :minor (the safer middle bucket).
  defp classify_bump(nil, _), do: :minor
  defp classify_bump(_, nil), do: :minor

  defp classify_bump(from_range, to_version) when is_binary(from_range) and is_binary(to_version) do
    from = extract_first_major(from_range)
    to_major = first_semver_part(to_version)

    cond do
      is_nil(from) or is_nil(to_major) -> :minor
      to_major > from -> :major
      to_major == from -> :patch_or_minor
      true -> :minor
    end
  end

  # Pull the first major version integer out of a range like
  # ">= 1.2.0, < 1.2.5" or "< 3.0.0" or "1.2.3 - 1.2.5".
  defp extract_first_major(range_str) do
    case Regex.run(~r/(\d+)\./, range_str) do
      [_, n] -> String.to_integer(n)
      _ -> nil
    end
  end

  defp first_semver_part(version) do
    case String.split(version, ".", parts: 2) do
      [maj | _] ->
        case Integer.parse(String.trim_leading(maj, "v")) do
          {n, _} -> n
          :error -> nil
        end

      _ ->
        nil
    end
  end

  # Eliminate-tier confidence based on severity, bump kind, and ecosystem
  # semver strictness. Patch-only bumps on strict-semver ecosystems are
  # the highest-confidence auto-fix target.
  defp eliminate_confidence(severity, bump, ecosystem) do
    base =
      case {severity, bump} do
        {"critical", :patch_or_minor} -> 0.97
        {"high", :patch_or_minor} -> 0.95
        {"medium", :patch_or_minor} -> 0.92
        {"low", :patch_or_minor} -> 0.88
        {"critical", :minor} -> 0.93
        {"high", :minor} -> 0.90
        {"medium", :minor} -> 0.87
        {"low", :minor} -> 0.82
        _ -> 0.80
      end

    if String.downcase(ecosystem) in @strict_semver_ecosystems do
      min(base + 0.01, 0.98)
    else
      max(base - 0.03, 0.70)
    end
  end

  defp severity_to_score("critical"), do: 1.0
  defp severity_to_score("high"), do: 0.8
  defp severity_to_score("medium"), do: 0.5
  defp severity_to_score("low"), do: 0.2
  defp severity_to_score(_), do: 0.5

  # Deterministic recipe ID: "da-" + 12-char sha256 prefix of (repo, alert_number).
  defp recipe_id(alert_number, repo) do
    hash =
      :crypto.hash(:sha256, "#{repo}:da:#{alert_number}")
      |> Base.encode16(case: :lower)
      |> String.slice(0, 12)

    "da-#{hash}"
  end

  # ─── GitHub API ───────────────────────────────────────────────────────

  defp fetch_alerts(owner, repo) do
    token = System.get_env("GITHUB_TOKEN")

    if token == nil or token == "" do
      {:error, "GITHUB_TOKEN not set"}
    else
      url = "#{@github_api_base}/repos/#{owner}/#{repo}/dependabot/alerts?state=open&per_page=#{@max_alerts_per_repo}"

      case System.cmd("curl", [
        "-s", "-f",
        "-H", "Accept: application/vnd.github+json",
        "-H", "Authorization: Bearer #{token}",
        "-H", "X-GitHub-Api-Version: 2022-11-28",
        url
      ], stderr_to_stdout: true) do
        {body, 0} ->
          case Jason.decode(body) do
            {:ok, alerts} when is_list(alerts) -> {:ok, alerts}
            {:ok, %{"message" => msg}} -> {:error, "GitHub API: #{msg}"}
            {:error, _} -> {:error, "Invalid JSON response from GitHub API"}
          end

        {error, _} ->
          {:error, "curl failed: #{String.slice(error, 0, 200)}"}
      end
    end
  end

  defp extract_owner_repo(repo_path) do
    case System.cmd("git", ["remote", "get-url", "origin"],
                    cd: repo_path, stderr_to_stdout: true) do
      {url, 0} ->
        trimmed = String.trim(url)

        cond do
          # SSH format: git@github.com:owner/repo.git
          String.contains?(trimmed, "github.com:") ->
            [_, path] = String.split(trimmed, "github.com:", parts: 2)
            parse_owner_repo_from_path(path)

          # HTTPS format: https://github.com/owner/repo.git
          String.contains?(trimmed, "github.com/") ->
            [_, path] = String.split(trimmed, "github.com/", parts: 2)
            parse_owner_repo_from_path(path)

          true ->
            {:error, "Remote URL is not a GitHub URL: #{trimmed}"}
        end

      _ ->
        {:error, "Could not get remote URL"}
    end
  end

  defp parse_owner_repo_from_path(path) do
    clean = path |> String.trim() |> String.trim_trailing(".git")

    case String.split(clean, "/", parts: 2) do
      [owner, repo] -> {:ok, owner, repo}
      _ -> {:error, "Could not parse owner/repo from: #{path}"}
    end
  end

  # ─── Helpers ──────────────────────────────────────────────────────────

  defp age_in_days(nil), do: 0

  defp age_in_days(iso_string) when is_binary(iso_string) do
    case DateTime.from_iso8601(iso_string) do
      {:ok, dt, _} ->
        DateTime.diff(DateTime.utc_now(), dt, :day)

      _ -> 0
    end
  end

  defp build_alert_reason(severity, package, cve, age_days, is_stale) do
    base = "Dependabot: #{severity} vulnerability in #{package}"
    cve_part = if cve, do: " (#{cve})", else: ""
    age_part = " -- #{age_days} day(s) old"
    stale_part = if is_stale, do: " [STALE]", else: ""
    base <> cve_part <> age_part <> stale_part
  end

  defp determine_action(severity, is_stale) do
    case {severity, is_stale} do
      {:critical, _} -> :escalate
      {:high, true} -> :escalate
      {:high, false} -> :update
      {:medium, true} -> :update
      {:medium, false} -> :review
      {:low, _} -> :review
      _ -> :review
    end
  end

  defp group_by_severity(findings) do
    findings
    |> Enum.group_by(& &1.severity)
    |> Enum.map(fn {sev, items} -> {sev, length(items)} end)
    |> Map.new()
  end

  defp dispatch_recommendations(findings) do
    Enum.map(findings, fn finding ->
      bot = case finding.action do
        :escalate -> :panicbot
        :update -> :rhodibot
        :batch_update -> :rhodibot
        :review -> :sustainabot
        _ -> :sustainabot
      end

      confidence = case finding.severity do
        :critical -> 0.98
        :high -> 0.92
        :medium -> 0.85
        :low -> 0.70
        _ -> 0.50
      end

      %{bot: bot, confidence: confidence, rule: finding.rule,
        action: finding.action, reason: finding.reason}
    end)
  end
end
