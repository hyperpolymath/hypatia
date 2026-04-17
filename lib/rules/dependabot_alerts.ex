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
          # severity threshold (which is "medium" — rank 3). Native
          # severity from the GitHub advisory is preserved in
          # detail.severity below for downstream consumers that want to
          # see the raw classification. Three low-severity Dependabot
          # alerts on 007 sat open for weeks because this floor was not
          # in place — every `:low` finding was dropped by the CLI
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
              reason: "#{critical_count} critical Dependabot alert(s) — immediate patching required",
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
              reason: "#{high_count} high-severity Dependabot alert(s) — batch update recommended",
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
              reason: "#{total} total open Dependabot alert(s) — dependency hygiene review needed",
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
            reason: "Dependabot alert for #{package} (#{severity}) is #{age} days old (threshold: #{threshold} days) — overdue for remediation",
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
            reason: "Dependabot alert for #{package} (#{severity}) dismissed as '#{reason}' — ensure risk is documented",
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
      {:error, "GITHUB_TOKEN not set — cannot query Dependabot alerts"}
    else
      findings =
        da001_open_alerts(owner, repo) ++
        da002_severity_summary(owner, repo) ++
        da003_stale_alerts(owner, repo) ++
        da004_dismissed_without_fix(owner, repo)

      # Deduplicate — DA001 and DA003 may overlap
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
  Scan from a local repo path — extracts owner/repo from git remote.
  """
  def scan_from_path(repo_path) do
    case extract_owner_repo(repo_path) do
      {:ok, owner, repo} -> scan(owner, repo)
      {:error, reason} -> {:error, reason}
    end
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
    age_part = " — #{age_days} day(s) old"
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
