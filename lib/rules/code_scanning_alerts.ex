# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.CodeScanningAlerts do
  @moduledoc """
  GitHub Code Scanning alert querying (CodeQL + third-party SARIF).

  Queries the GitHub REST API for code-scanning alerts on a repository,
  classifies by severity, and generates findings for the safety triangle
  pipeline. Surfaces CodeQL findings (and any other SARIF uploads --
  including Hypatia's own, via the `hypatia` category) alongside the
  rest of the scanner output so a single Hypatia run sees everything
  GitHub's security tab is showing.

  Requires GITHUB_TOKEN with `code_scanning_alerts: read` permission
  (fine-grained PAT) or `security_events` scope (classic PAT).

  Rule IDs: CSA001-CSA004
  """

  require Logger

  @github_api_base "https://api.github.com"
  @max_alerts_per_repo 100

  # Stale thresholds (days), keyed by alert severity. Mirrors the
  # DependabotAlerts cadence: critical findings escalate fastest.
  @stale_thresholds %{
    critical: 3,
    high: 7,
    medium: 30,
    low: 90,
    note: 90,
    warning: 30,
    error: 7
  }

  # Dismissal reasons accepted by policy without further review.
  @accepted_dismissals ~w(false\ positive used\ in\ tests won't\ fix)

  # ─── CSA001: Open code-scanning alerts ─────────────────────────────────

  @doc """
  CSA001: List all open code-scanning alerts on the repo. Each alert's
  severity is taken from its rule definition (critical/high/medium/low,
  or CodeQL's note/warning/error). The Hypatia-side severity is mapped
  to the same canonical four-bucket scale used by other rule modules so
  the CLI's severity threshold works uniformly.
  """
  def csa001_open_alerts(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        alerts
        |> Enum.filter(&(&1["state"] == "open"))
        |> Enum.map(fn alert ->
          rule_id = get_in(alert, ["rule", "id"]) || "unknown"
          severity_raw = get_in(alert, ["rule", "severity"]) || "warning"
          security_severity = get_in(alert, ["rule", "security_severity_level"])
          description = get_in(alert, ["rule", "description"]) || rule_id
          tool = get_in(alert, ["tool", "name"]) || "unknown"
          path = get_in(alert, ["most_recent_instance", "location", "path"]) || ""
          line = get_in(alert, ["most_recent_instance", "location", "start_line"])

          created = alert["created_at"]
          age_days = age_in_days(created)
          mapped_severity = map_severity(security_severity || severity_raw)
          stale_threshold = Map.get(@stale_thresholds, mapped_severity, 30)
          is_stale = age_days > stale_threshold

          %{
            rule: "CSA001",
            file: path,
            severity: mapped_severity,
            reason: build_alert_reason(tool, rule_id, description, age_days, is_stale),
            action: determine_action(mapped_severity, is_stale),
            detail: %{
              alert_number: alert["number"],
              tool: tool,
              rule_id: rule_id,
              rule_severity: severity_raw,
              security_severity_level: security_severity,
              path: path,
              line: line,
              age_days: age_days,
              is_stale: is_stale,
              created_at: created,
              url: alert["html_url"]
            }
          }
        end)

      {:error, reason} ->
        Logger.warning("CSA001: Failed to fetch code-scanning alerts: #{reason}")
        []
    end
  end

  # ─── CSA002: Severity summary ──────────────────────────────────────────

  @doc """
  CSA002: Meta-finding when open alert counts exceed thresholds.
  Triggers at any critical, ≥5 high, or ≥10 total open alerts.
  """
  def csa002_severity_summary(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        open = Enum.filter(alerts, &(&1["state"] == "open"))

        by_severity =
          Enum.group_by(open, fn a ->
            sev = get_in(a, ["rule", "security_severity_level"]) || get_in(a, ["rule", "severity"])
            map_severity(sev)
          end)

        critical_count = length(Map.get(by_severity, :critical, []))
        high_count = length(Map.get(by_severity, :high, []))
        total = length(open)

        findings = []

        findings =
          if critical_count > 0 do
            [%{
               rule: "CSA002",
               file: "#{owner}/#{repo}",
               severity: :critical,
               reason:
                 "#{critical_count} critical code-scanning alert(s) -- immediate triage required",
               action: :escalate,
               detail: %{critical: critical_count, high: high_count, total: total}
             }
             | findings]
          else
            findings
          end

        findings =
          if high_count >= 5 do
            [%{
               rule: "CSA002",
               file: "#{owner}/#{repo}",
               severity: :high,
               reason:
                 "#{high_count} high-severity code-scanning alert(s) -- batch remediation recommended",
               action: :batch_update,
               detail: %{high: high_count, total: total}
             }
             | findings]
          else
            findings
          end

        findings =
          if total >= 10 do
            [%{
               rule: "CSA002",
               file: "#{owner}/#{repo}",
               severity: :medium,
               reason: "#{total} total open code-scanning alert(s) -- security hygiene review",
               action: :review,
               detail: %{
                 total: total,
                 by_severity:
                   Map.new(by_severity, fn {k, v} -> {to_string(k), length(v)} end)
               }
             }
             | findings]
          else
            findings
          end

        findings

      {:error, _} -> []
    end
  end

  # ─── CSA003: Stale open alerts ─────────────────────────────────────────

  @doc """
  CSA003: Open code-scanning alerts older than the severity-appropriate
  threshold. Critical alerts stale after 3 days, high after 7, medium
  after 30, low after 90.
  """
  def csa003_stale_alerts(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        alerts
        |> Enum.filter(&(&1["state"] == "open"))
        |> Enum.filter(fn alert ->
          sev =
            map_severity(
              get_in(alert, ["rule", "security_severity_level"]) ||
                get_in(alert, ["rule", "severity"]) || "medium"
            )

          threshold = Map.get(@stale_thresholds, sev, 30)
          age_in_days(alert["created_at"]) > threshold
        end)
        |> Enum.map(fn alert ->
          rule_id = get_in(alert, ["rule", "id"]) || "unknown"

          sev =
            map_severity(
              get_in(alert, ["rule", "security_severity_level"]) ||
                get_in(alert, ["rule", "severity"]) || "medium"
            )

          age = age_in_days(alert["created_at"])
          threshold = Map.get(@stale_thresholds, sev, 30)
          path = get_in(alert, ["most_recent_instance", "location", "path"]) || ""

          %{
            rule: "CSA003",
            file: path,
            severity: :high,
            reason:
              "Code-scanning alert #{rule_id} (#{sev}) at #{path} is #{age} days old " <>
                "(threshold: #{threshold} days) -- overdue for remediation",
            action: :escalate,
            detail: %{
              alert_number: alert["number"],
              rule_id: rule_id,
              path: path,
              original_severity: sev,
              age_days: age,
              threshold_days: threshold
            }
          }
        end)

      {:error, _} -> []
    end
  end

  # ─── CSA004: Dismissed without documented resolution ───────────────────

  @doc """
  CSA004: Alerts dismissed with no documented reason (or with a vague
  one). Real dismissals carry a `dismissed_reason` in the accepted
  vocabulary (`false positive`, `won't fix`, `used in tests`); anything
  else is policy-suspicious and should be reviewed.
  """
  def csa004_dismissed_without_fix(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        alerts
        |> Enum.filter(fn a ->
          a["state"] == "dismissed" and
            a["dismissed_reason"] not in @accepted_dismissals
        end)
        |> Enum.map(fn alert ->
          rule_id = get_in(alert, ["rule", "id"]) || "unknown"
          reason = alert["dismissed_reason"] || "no reason given"
          path = get_in(alert, ["most_recent_instance", "location", "path"]) || ""

          %{
            rule: "CSA004",
            file: path,
            severity: :medium,
            reason:
              "Code-scanning alert #{rule_id} dismissed as '#{reason}' " <>
                "-- ensure dismissal is documented and justified",
            action: :review,
            detail: %{
              alert_number: alert["number"],
              rule_id: rule_id,
              path: path,
              dismissed_reason: reason,
              dismissed_comment: alert["dismissed_comment"],
              dismissed_at: alert["dismissed_at"]
            }
          }
        end)

      {:error, _} -> []
    end
  end

  # ─── Comprehensive scan ────────────────────────────────────────────────

  @doc """
  Run all code-scanning checks for a repository.
  """
  def scan(owner, repo) do
    token = System.get_env("GITHUB_TOKEN")

    if token == nil or token == "" do
      {:error, "GITHUB_TOKEN not set -- cannot query code-scanning alerts"}
    else
      findings =
        csa001_open_alerts(owner, repo) ++
          csa002_severity_summary(owner, repo) ++
          csa003_stale_alerts(owner, repo) ++
          csa004_dismissed_without_fix(owner, repo)

      deduped =
        findings
        |> Enum.uniq_by(fn f ->
          {f.rule, Map.get(f.detail, :alert_number, f.file)}
        end)

      {:ok, %{
        findings: deduped,
        total: length(deduped),
        by_severity: group_by_severity(deduped)
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

  # ─── Self-referential filter ───────────────────────────────────────────

  @doc """
  Returns true when the alert is one Hypatia previously uploaded for this
  module (CSA001-CSA004). These rules are lenses over GitHub's code-scanning
  alerts; their findings are themselves uploaded back to GitHub as SARIF and
  become new alerts under `tool: Hypatia`, rule id
  `hypatia/code_scanning_alerts/CSA00X`.

  Without this filter, each scan generates new CSA alerts about the previous
  scan's CSA alerts — boj-server accumulated 30+ self-referential alerts on
  `cartridges/*/ffi/cartridge_shim.zig` this way (alert numbers 357-386).
  The message field showed the self-echo: the `description` slot was filled
  with the previous round's rendered `build_alert_reason/5` output, so each
  round nests one level deeper.

  Public so the regression test in `test/code_scanning_alerts_test.exs` can
  exercise it without hitting the GitHub API.
  """
  def self_referential_alert?(alert) do
    tool = get_in(alert, ["tool", "name"])
    rule_id = get_in(alert, ["rule", "id"]) || ""
    tool == "Hypatia" and String.starts_with?(rule_id, "hypatia/code_scanning_alerts/")
  end

  # ─── GitHub API ────────────────────────────────────────────────────────

  defp fetch_alerts(owner, repo) do
    token = System.get_env("GITHUB_TOKEN")

    if token == nil or token == "" do
      {:error, "GITHUB_TOKEN not set"}
    else
      url =
        "#{@github_api_base}/repos/#{owner}/#{repo}/code-scanning/alerts" <>
          "?per_page=#{@max_alerts_per_repo}"

      case System.cmd("curl", [
             "-s",
             "-f",
             "-H",
             "Accept: application/vnd.github+json",
             "-H",
             "Authorization: Bearer #{token}",
             "-H",
             "X-GitHub-Api-Version: 2022-11-28",
             url
           ], stderr_to_stdout: true) do
        {body, 0} ->
          case Jason.decode(body) do
            {:ok, alerts} when is_list(alerts) ->
              {:ok, Enum.reject(alerts, &self_referential_alert?/1)}

            {:ok, %{"message" => msg}} ->
              {:error, "GitHub API: #{msg}"}

            {:error, _} ->
              {:error, "Invalid JSON response from GitHub API"}
          end

        {error, _} ->
          {:error, "curl failed: #{String.slice(error, 0, 200)}"}
      end
    end
  end

  defp extract_owner_repo(repo_path) do
    case System.cmd("git", ["remote", "get-url", "origin"],
           cd: repo_path,
           stderr_to_stdout: true
         ) do
      {url, 0} ->
        trimmed = String.trim(url)

        cond do
          String.contains?(trimmed, "github.com:") ->
            [_, path] = String.split(trimmed, "github.com:", parts: 2)
            parse_owner_repo_from_path(path)

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

  # ─── Helpers ───────────────────────────────────────────────────────────

  # Normalise the heterogeneous severity surface (CodeQL uses note/
  # warning/error, third-party SARIF often uses critical/high/medium/low,
  # GitHub's `security_severity_level` uses critical/high/medium/low) onto
  # Hypatia's canonical bucket scale so the CLI's severity threshold
  # works uniformly across all rule modules.
  defp map_severity(sev) when is_binary(sev) do
    case String.downcase(sev) do
      "critical" -> :critical
      "high" -> :high
      "error" -> :high
      "medium" -> :medium
      "warning" -> :medium
      "low" -> :low
      "note" -> :low
      _ -> :medium
    end
  end

  defp map_severity(sev) when is_atom(sev), do: map_severity(Atom.to_string(sev))
  defp map_severity(_), do: :medium

  defp age_in_days(nil), do: 0

  defp age_in_days(iso_string) when is_binary(iso_string) do
    case DateTime.from_iso8601(iso_string) do
      {:ok, dt, _} -> DateTime.diff(DateTime.utc_now(), dt, :day)
      _ -> 0
    end
  end

  defp build_alert_reason(tool, rule_id, description, age_days, is_stale) do
    base = "Code scanning (#{tool}): #{rule_id} -- #{description}"
    age_part = " -- #{age_days} day(s) old"
    stale_part = if is_stale, do: " [STALE]", else: ""
    base <> age_part <> stale_part
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
end
