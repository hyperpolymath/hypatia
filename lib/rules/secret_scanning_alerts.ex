# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.SecretScanningAlerts do
  @moduledoc """
  GitHub Secret Scanning alert querying.

  Queries the GitHub REST API for active secret-scanning alerts on
  repositories and generates findings for the safety triangle pipeline.

  A secret-scanning alert means GitHub identified a credential committed
  to the repo (API token, private key, etc.). Every open alert is treated
  as :critical -- leaked secrets are by definition not "advisory" risk,
  and the dismissal vocabulary (`revoked`, `used_in_tests`, `false_positive`)
  is the place to mark accepted ones.

  ## Token

  A workflow's own `GITHUB_TOKEN` cannot read secret-scanning alerts: no
  workflow permission covers them. They need a fine-grained PAT or GitHub
  App token with `Secret scanning alerts: Read`. That token is read from
  `HYPATIA_SECRET_ALERTS_TOKEN` first, then `GITHUB_TOKEN`, so a dedicated
  read-only token never has to be exported under the general name.

  ## Errors

  The per-rule functions (`ssa001_open_alerts/2` and so on) log a fetch
  failure and return `[]`, as the other API-backed rules do. `sweep/2`
  instead returns `{:error, reason}`, so an estate-wide sweep can tell a
  refused or disabled repo apart from a clean one.

  Rule IDs: SSA001-SSA004
  """

  require Logger

  @github_api_base "https://api.github.com"
  @max_alerts_per_repo 100

  # Stale thresholds (days). A revoked secret left in history is still a
  # finding -- but a fresh open alert is much more urgent.
  @stale_threshold_days 7

  # Dismissal reasons that are accepted by policy without further review.
  @accepted_resolutions ~w(revoked used_in_tests pattern_deleted pattern_edited)

  # ─── SSA001: Open secret-scanning alerts ───────────────────────────────

  @doc """
  SSA001: List all open secret-scanning alerts on the repo.

  Every open alert is :critical -- a real credential is sitting in the
  git history. The triangle classifier deals with whether it's
  fixable (rotate + remove) vs. a documented test fixture.
  """
  def ssa001_open_alerts(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} ->
        open_alert_findings(alerts)

      {:error, reason} ->
        Logger.warning("SSA001: Failed to fetch secret-scanning alerts: #{reason}")
        []
    end
  end

  @doc false
  def open_alert_findings(alerts) do
    alerts
    |> Enum.filter(&(&1["state"] == "open"))
    |> Enum.map(fn alert ->
      secret_type = alert["secret_type_display_name"] || alert["secret_type"] || "unknown"
      created = alert["created_at"]
      age_days = age_in_days(created)
      is_stale = age_days > @stale_threshold_days

      %{
        rule: "SSA001",
        file: secret_type,
        severity: :critical,
        reason: build_alert_reason(secret_type, age_days, is_stale),
        action: :escalate,
        detail: %{
          alert_number: alert["number"],
          secret_type: alert["secret_type"],
          secret_type_display: alert["secret_type_display_name"],
          age_days: age_days,
          is_stale: is_stale,
          created_at: created,
          url: alert["html_url"],
          locations_url: alert["locations_url"]
        }
      }
    end)
  end

  # ─── SSA002: Severity summary ──────────────────────────────────────────

  @doc """
  SSA002: Meta-finding if open alert count exceeds zero. Any leaked
  secret is a critical security event -- we surface a repo-level marker
  so the dashboard can highlight the repo, not just the individual alert.
  """
  def ssa002_severity_summary(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} -> summary_findings(owner, repo, alerts)
      {:error, _} -> []
    end
  end

  @doc false
  def summary_findings(owner, repo, alerts) do
    open = Enum.filter(alerts, &(&1["state"] == "open"))
    count = length(open)

    if count > 0 do
      [
        %{
          rule: "SSA002",
          file: "#{owner}/#{repo}",
          severity: :critical,
          reason: "#{count} open secret-scanning alert(s) -- rotate and purge from history",
          action: :escalate,
          detail: %{
            total: count,
            by_type:
              open
              |> Enum.group_by(&(&1["secret_type"] || "unknown"))
              |> Map.new(fn {k, v} -> {k, length(v)} end)
          }
        }
      ]
    else
      []
    end
  end

  # ─── SSA003: Stale open alerts ─────────────────────────────────────────

  @doc """
  SSA003: Open secret-scanning alerts older than the stale threshold.
  Leaked secrets must be rotated within days, not weeks. Findings are
  always :critical regardless of age (the secret is leaked either way),
  but staleness is surfaced in the reason for triage prioritisation.
  """
  def ssa003_stale_alerts(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} -> stale_findings(alerts)
      {:error, _} -> []
    end
  end

  @doc false
  def stale_findings(alerts) do
    alerts
    |> Enum.filter(&(&1["state"] == "open"))
    |> Enum.filter(fn alert ->
      age_in_days(alert["created_at"]) > @stale_threshold_days
    end)
    |> Enum.map(fn alert ->
      secret_type = alert["secret_type_display_name"] || alert["secret_type"] || "unknown"
      age = age_in_days(alert["created_at"])

      %{
        rule: "SSA003",
        file: secret_type,
        severity: :critical,
        reason:
          "Secret-scanning alert for #{secret_type} is #{age} days old " <>
            "(threshold: #{@stale_threshold_days} days) -- overdue for rotation",
        action: :escalate,
        detail: %{
          alert_number: alert["number"],
          secret_type: alert["secret_type"],
          age_days: age,
          threshold_days: @stale_threshold_days
        }
      }
    end)
  end

  # ─── SSA004: Dismissed without acceptable resolution ───────────────────

  @doc """
  SSA004: Alerts resolved with no documented resolution reason, or with
  a vague reason. Real resolutions go through the `revoked`,
  `used_in_tests`, `false_positive`, `pattern_deleted`, `pattern_edited`
  vocabulary; anything else (including nil) is policy-suspicious.
  """
  def ssa004_dismissed_without_fix(owner, repo) do
    case fetch_alerts(owner, repo) do
      {:ok, alerts} -> dismissed_findings(alerts)
      {:error, _} -> []
    end
  end

  @doc false
  def dismissed_findings(alerts) do
    alerts
    |> Enum.filter(fn a ->
      a["state"] == "resolved" and
        a["resolution"] not in @accepted_resolutions
    end)
    |> Enum.map(fn alert ->
      secret_type = alert["secret_type_display_name"] || alert["secret_type"] || "unknown"
      resolution = alert["resolution"] || "no reason given"

      %{
        rule: "SSA004",
        file: secret_type,
        severity: :high,
        reason:
          "Secret-scanning alert for #{secret_type} resolved as '#{resolution}' " <>
            "-- confirm rotation completed and document acceptance reason",
        action: :review,
        detail: %{
          alert_number: alert["number"],
          secret_type: alert["secret_type"],
          resolution: resolution,
          resolved_at: alert["resolved_at"],
          resolution_comment: alert["resolution_comment"]
        }
      }
    end)
  end

  # ─── Comprehensive scan ────────────────────────────────────────────────

  @doc """
  Run all secret-scanning checks for a repository.
  Returns `{:ok, result}` or `{:error, reason}`.
  """
  def scan(owner, repo) do
    if resolve_token() == nil do
      {:error, missing_token_message()}
    else
      findings =
        ssa001_open_alerts(owner, repo) ++
          ssa002_severity_summary(owner, repo) ++
          ssa003_stale_alerts(owner, repo) ++
          ssa004_dismissed_without_fix(owner, repo)

      {:ok, result(findings)}
    end
  end

  @doc """
  Run all secret-scanning checks for a repository with one API call,
  returning `{:error, reason}` when the alerts cannot be fetched.

  Built for an estate-wide sweep: unlike `scan/2`, a refused token, a
  repository with secret scanning disabled or a network failure is an
  error, never an empty (clean-looking) result.
  """
  def sweep(owner, repo) do
    if resolve_token() == nil do
      {:error, missing_token_message()}
    else
      case fetch_alerts(owner, repo) do
        {:ok, alerts} -> {:ok, result(findings_from_alerts(owner, repo, alerts))}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @doc false
  def findings_from_alerts(owner, repo, alerts) do
    open_alert_findings(alerts) ++
      summary_findings(owner, repo, alerts) ++
      stale_findings(alerts) ++
      dismissed_findings(alerts)
  end

  defp result(findings) do
    deduped =
      Enum.uniq_by(findings, fn f ->
        {f.rule, Map.get(f.detail, :alert_number, f.file)}
      end)

    %{findings: deduped, total: length(deduped), by_severity: group_by_severity(deduped)}
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

  # ─── GitHub API ────────────────────────────────────────────────────────

  @doc false
  # The dedicated token wins; the general one is the fallback. An empty
  # value counts as unset, so `HYPATIA_SECRET_ALERTS_TOKEN=""` falls back.
  def resolve_token do
    Enum.find_value(["HYPATIA_SECRET_ALERTS_TOKEN", "GITHUB_TOKEN"], fn name ->
      case System.get_env(name) do
        token when is_binary(token) and token != "" -> token
        _ -> nil
      end
    end)
  end

  defp missing_token_message do
    "GITHUB_TOKEN not set (nor HYPATIA_SECRET_ALERTS_TOKEN) -- " <>
      "cannot query secret-scanning alerts"
  end

  defp fetch_alerts(owner, repo) do
    case resolve_token() do
      nil ->
        {:error, "GITHUB_TOKEN not set"}

      token ->
        url =
          "#{@github_api_base}/repos/#{owner}/#{repo}/secret-scanning/alerts" <>
            "?per_page=#{@max_alerts_per_repo}"

        # No `-f`: on an HTTP error GitHub's JSON body carries the reason
        # (for example "Secret scanning is disabled on this repository"),
        # and `-f` would discard it. The status code is appended instead.
        case System.cmd(
               "curl",
               [
                 "-s",
                 "-w",
                 "\n%{http_code}",
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
          {output, 0} -> parse_response(output)
          {error, _} -> {:error, "curl failed: #{String.slice(error, 0, 200)}"}
        end
    end
  end

  @doc false
  # `output` is the response body followed by a newline and the HTTP status.
  def parse_response(output) do
    {body, status} =
      case String.split(output, "\n") |> Enum.reverse() do
        [code | rest] -> {rest |> Enum.reverse() |> Enum.join("\n"), String.trim(code)}
        [] -> {"", ""}
      end

    case {status, Jason.decode(body)} do
      {"200", {:ok, alerts}} when is_list(alerts) ->
        {:ok, alerts}

      {code, {:ok, %{"message" => msg}}} ->
        {:error, "GitHub API #{code}: #{msg}"}

      {"200", _} ->
        {:error, "Invalid JSON response from GitHub API"}

      {code, _} ->
        {:error, "GitHub API returned HTTP #{code}"}
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

  defp age_in_days(nil), do: 0

  defp age_in_days(iso_string) when is_binary(iso_string) do
    case DateTime.from_iso8601(iso_string) do
      {:ok, dt, _} -> DateTime.diff(DateTime.utc_now(), dt, :day)
      _ -> 0
    end
  end

  defp build_alert_reason(secret_type, age_days, is_stale) do
    base = "Secret scanning: leaked #{secret_type}"
    age_part = " -- #{age_days} day(s) old"
    stale_part = if is_stale, do: " [STALE -- rotate immediately]", else: ""
    base <> age_part <> stale_part
  end

  defp group_by_severity(findings) do
    findings
    |> Enum.group_by(& &1.severity)
    |> Enum.map(fn {sev, items} -> {sev, length(items)} end)
    |> Map.new()
  end
end
