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

  ## Locations

  The alert list carries only a `locations_url`. For every alert that becomes a
  file-scoped finding (SSA001, SSA003, SSA004) that URL is read once, and the
  finding's `file` and `line` come from the first location with a path. A
  secret found outside the repository's files (an issue, a pull-request
  comment) or a failed lookup gives the repository-level file `"."`, and
  `detail.location` records why. The repo summary (SSA002) is always `"."`.

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
  # Pages followed before giving up loudly (100 alerts per page).
  @max_pages 50
  # Per-request transfer deadline, so one stalled repo cannot hang a sweep.
  @request_timeout_seconds 30

  # Stale thresholds (days). A revoked secret left in history is still a
  # finding -- but a fresh open alert is much more urgent.
  @stale_threshold_days 7

  # Dismissal reasons that are accepted by policy without further review.
  @accepted_resolutions ~w(revoked used_in_tests pattern_deleted pattern_edited)

  # `file` of a finding that names no file: the repository as a whole.
  @repo_level_file "."
  # Locations read per alert (one page, no pagination: the first file wins).
  @max_locations_per_alert 100
  # Pages of locations read before the alert is reported at repository level.
  @max_location_pages 10

  # ─── SSA001: Open secret-scanning alerts ───────────────────────────────

  @doc """
  SSA001: List all open secret-scanning alerts on the repo.

  Every open alert is :critical -- a real credential is sitting in the
  git history. The triangle classifier deals with whether it's
  fixable (rotate + remove) vs. a documented test fixture.
  """
  def ssa001_open_alerts(owner, repo) do
    case fetch_alerts(owner, repo, &open?/1) do
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
      |> locate(alert)
    end)
  end

  # ─── SSA002: Severity summary ──────────────────────────────────────────

  @doc """
  SSA002: Meta-finding if open alert count exceeds zero. Any leaked
  secret is a critical security event -- we surface a repo-level marker
  so the dashboard can highlight the repo, not just the individual alert.
  """
  def ssa002_severity_summary(owner, repo) do
    case fetch_alerts(owner, repo, fn _ -> false end) do
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
          file: @repo_level_file,
          severity: :critical,
          reason: "#{count} open secret-scanning alert(s) -- rotate and purge from history",
          action: :escalate,
          detail: %{
            repo: "#{owner}/#{repo}",
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
    case fetch_alerts(owner, repo, &stale_open?/1) do
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
      |> locate(alert)
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
    case fetch_alerts(owner, repo, &suspicious_resolved?/1) do
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
      |> locate(alert)
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
      # One fetch (and one location lookup per alert) for all four rules.
      findings =
        case fetch_alerts(owner, repo) do
          {:ok, alerts} ->
            findings_from_alerts(owner, repo, alerts)

          {:error, reason} ->
            Logger.warning("SSA001-SSA004: Failed to fetch secret-scanning alerts: #{reason}")
            []
        end

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

  # `locate?` picks the alerts whose file location is read, one request each:
  # only those a rule reports at a file. The repo summary (SSA002) reads none.
  defp fetch_alerts(owner, repo, locate? \\ &needs_location?/1) do
    case resolve_token() do
      nil ->
        {:error, "GITHUB_TOKEN not set"}

      token ->
        url =
          "#{@github_api_base}/repos/#{owner}/#{repo}/secret-scanning/alerts" <>
            "?per_page=#{@max_alerts_per_repo}"

        request = &request(&1, token)

        with {:ok, alerts} <- collect_pages(url, request) do
          {:ok, attach_locations(alerts, request, locate?)}
        end
    end
  end

  @doc false
  # Follow GitHub's `Link: rel="next"` pages until the last one. A list cut
  # short at one page would report older open alerts as absent, so hitting
  # the page cap is an error, never a truncated success. Every `next` URL
  # must stay on the API host: the token goes with each request.
  def collect_pages(url, request, acc \\ [], page \\ 1)

  def collect_pages(_url, _request, _acc, page) when page > @max_pages do
    {:error,
     "more than #{@max_pages * @max_alerts_per_repo} alerts -- " <>
       "refusing to report a truncated list"}
  end

  def collect_pages(url, request, acc, page) do
    with {:ok, output} <- request.(url),
         {:ok, alerts, next} <- parse_page(output) do
      cond do
        next == nil ->
          {:ok, acc ++ alerts}

        String.starts_with?(next, @github_api_base <> "/") ->
          collect_pages(next, request, acc ++ alerts, page + 1)

        true ->
          {:error, "refusing to follow a pagination link off #{@github_api_base}"}
      end
    end
  end

  @doc false
  # Read the location of every alert that becomes a file-scoped finding
  # (SSA001/SSA003 open, SSA004 resolved without an accepted reason) and
  # record it on the alert under "hypatia_location". A failed lookup is
  # recorded, not fatal: the finding still reports the alert, at repository
  # level, with the reason.
  def attach_locations(alerts, request, locate? \\ &needs_location?/1) do
    Enum.map(alerts, fn alert ->
      if locate?.(alert) do
        Map.put(alert, "hypatia_location", fetch_location(alert["locations_url"], request))
      else
        alert
      end
    end)
  end

  defp needs_location?(alert), do: open?(alert) or suspicious_resolved?(alert)

  defp open?(alert), do: alert["state"] == "open"

  defp stale_open?(alert),
    do: open?(alert) and age_in_days(alert["created_at"]) > @stale_threshold_days

  defp suspicious_resolved?(alert),
    do: alert["state"] == "resolved" and alert["resolution"] not in @accepted_resolutions

  @doc false
  # Reads pages of locations until one names a file, following the `next`
  # link (the first commit location can sit behind many wiki or comment
  # locations), up to @max_location_pages pages. The token goes with every
  # request, so every URL must be on the API host.
  def fetch_location(url, request) when is_binary(url) do
    separator = if String.contains?(url, "?"), do: "&", else: "?"
    find_location(url <> separator <> "per_page=#{@max_locations_per_alert}", request, 1)
  end

  def fetch_location(_url, _request), do: %{"error" => "the alert has no locations_url"}

  defp find_location(_url, _request, page) when page > @max_location_pages do
    %{
      "error" =>
        "no location names a file in the repository in the first " <>
          "#{@max_location_pages * @max_locations_per_alert} locations"
    }
  end

  defp find_location(url, request, page) do
    if String.starts_with?(url, @github_api_base <> "/") do
      with {:ok, output} <- request.(url),
           {:ok, locations, next} <- parse_page(output) do
        case location_from(locations) do
          %{"path" => _} = found -> found
          not_found when next == nil -> not_found
          _ -> find_location(next, request, page + 1)
        end
      else
        {:error, reason} -> %{"error" => reason}
      end
    else
      %{"error" => "refusing to follow a locations URL off #{@github_api_base}"}
    end
  end

  @doc false
  # The first `commit` location with a file path: only a commit location
  # names a file of the scanned repository. A `wiki_commit` location also
  # has a path, but in the separate `<repo>.wiki.git`; issue bodies,
  # pull-request comments and discussions name no file. GitHub writes the
  # path from the repository root with a leading slash (`/config/app.env`);
  # the finding's `file` is repository-relative, like every other rule's.
  def location_from(locations) do
    Enum.find_value(
      locations,
      %{"error" => "no location names a file in the repository"},
      fn
        %{"type" => "commit", "details" => %{"path" => path} = details}
        when is_binary(path) ->
          case String.trim_leading(path, "/") do
            "" -> nil
            rel -> %{"path" => rel, "start_line" => details["start_line"]}
          end

        _ ->
          nil
      end
    )
  end

  # Set a finding's `file` and `line` from its alert's location, falling
  # back to the repository level with the reason in `detail.location`.
  defp locate(finding, alert) do
    case alert["hypatia_location"] do
      %{"path" => path, "start_line" => line} ->
        finding
        |> Map.put(:file, path)
        |> put_line(line)
        |> put_location(%{status: "found", path: path, start_line: line})

      %{"error" => reason} ->
        finding
        |> Map.put(:file, @repo_level_file)
        |> put_location(%{status: "unavailable", reason: reason})

      _ ->
        finding
        |> Map.put(:file, @repo_level_file)
        |> put_location(%{status: "not_fetched"})
    end
  end

  defp put_line(finding, line) when is_integer(line) and line >= 1,
    do: Map.put(finding, :line, line)

  defp put_line(finding, _line), do: finding

  defp put_location(finding, location),
    do: Map.update!(finding, :detail, &Map.put(&1, :location, location))

  # No `-f`: on an HTTP error GitHub's JSON body carries the reason (for
  # example "Secret scanning is disabled on this repository"), and `-f` would
  # discard it. `-i` keeps the headers for the `Link` pagination header, and
  # the status code is appended after the body. `--max-time` stops one stalled
  # repository from hanging an estate-wide sweep.
  defp request(url, token) do
    case System.cmd(
           "curl",
           [
             "-s",
             "-i",
             "--max-time",
             Integer.to_string(@request_timeout_seconds),
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
      {output, 0} ->
        {:ok, output}

      {_, 28} ->
        {:error, "GitHub API request timed out after #{@request_timeout_seconds}s"}

      {error, code} ->
        {:error, "curl failed (exit #{code}): #{String.slice(error, 0, 200)}"}
    end
  end

  @doc false
  # `output` is curl's `-i` output: one or more header blocks (an HTTPS proxy
  # adds its own "Connection established" block first), the body, then a
  # newline and the HTTP status. Returns the alerts and the next page's URL.
  def parse_page(output) do
    {headers, rest} = split_headers(output)

    with {:ok, alerts} <- parse_response(rest) do
      {:ok, alerts, next_link(headers)}
    end
  end

  defp split_headers(output, headers \\ "") do
    if String.starts_with?(output, "HTTP/") do
      case String.split(output, ~r/\r?\n\r?\n/, parts: 2) do
        [block, rest] -> split_headers(rest, block)
        [block] -> {block, ""}
      end
    else
      {headers, output}
    end
  end

  defp next_link(headers) do
    headers
    |> String.split(~r/\r?\n/)
    |> Enum.find_value(fn line ->
      with [_, value] <- Regex.run(~r/\Alink:\s*(.*)\z/i, line),
           [_, next] <- Regex.run(~r/<([^>]+)>\s*;\s*rel="next"/, value) do
        next
      else
        _ -> nil
      end
    end)
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
