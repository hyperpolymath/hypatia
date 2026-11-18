# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Reviewer do
  @moduledoc """
  PR reviewer mode -- second-opinion code review via Hypatia rule modules.

  Given a PR diff (from the GitHub API), runs all applicable rule modules
  against the changed files and produces structured findings suitable for
  posting as PR review comments. This module comments but does NOT
  approve or reject -- it is strictly advisory.

  ## Usage

      # From a GitHub PR number:
      Hypatia.Reviewer.review_pr("hyperpolymath", "hypatia", 42)

      # From a raw diff string:
      Hypatia.Reviewer.review_diff(diff_text)

  ## Output

  Returns a list of `%Hypatia.Reviewer.Comment{}` structs, each mapping
  to a single inline review comment with file path, line number, body,
  and severity.
  """

  require Logger

  alias Hypatia.Rules

  alias Hypatia.Rules.{
    ScorecardCompliance,
    GreenWeb,
    WorkflowAudit
  }

  # ─── Types ──────────────────────────────────────────────────────────

  defmodule Comment do
    @moduledoc """
    A single review comment to post on a PR.

    - `path` -- file path relative to repo root
    - `line` -- line number in the diff (1-based)
    - `body` -- Markdown comment body
    - `severity` -- `:info`, `:low`, `:medium`, `:high`, or `:critical`
    - `rule` -- rule identifier string
    """
    defstruct [:path, :line, :body, :severity, :rule]

    @type t :: %__MODULE__{
            path: String.t(),
            line: non_neg_integer() | nil,
            body: String.t(),
            severity: :info | :low | :medium | :high | :critical,
            rule: String.t()
          }
  end

  # ─── Public API ─────────────────────────────────────────────────────

  @doc """
  Review a GitHub PR by owner/repo/number.

  Fetches the PR diff via the GitHub API (requires GITHUB_TOKEN in env),
  parses changed files, runs rule modules, and returns a list of
  `%Comment{}` structs.
  """
  def review_pr(owner, repo, pr_number) do
    case fetch_pr_diff(owner, repo, pr_number) do
      {:ok, diff} ->
        comments = review_diff(diff)
        {:ok, comments}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Review a raw unified diff string.

  Parses the diff into per-file hunks, runs all applicable rule modules,
  and returns a list of `%Comment{}` structs.
  """
  def review_diff(diff) when is_binary(diff) do
    diff
    |> parse_diff()
    |> Enum.flat_map(&review_file/1)
  end

  @doc """
  Format review comments as a Markdown summary suitable for a PR comment body.
  """
  def format_summary(comments) when is_list(comments) do
    if comments == [] do
      "> **Hypatia Review**: No issues found. :white_check_mark:"
    else
      grouped = Enum.group_by(comments, & &1.severity)

      severity_order = [:critical, :high, :medium, :low, :info]

      severity_emoji = %{
        critical: ":red_circle:",
        high: ":orange_circle:",
        medium: ":yellow_circle:",
        low: ":blue_circle:",
        info: ":white_circle:"
      }

      lines =
        severity_order
        |> Enum.filter(&Map.has_key?(grouped, &1))
        |> Enum.flat_map(fn severity ->
          items = Map.get(grouped, severity, [])
          emoji = Map.get(severity_emoji, severity, ":question:")

          header =
            "### #{emoji} #{severity |> to_string() |> String.capitalize()} (#{length(items)})\n"

          detail_lines =
            Enum.map(items, fn c ->
              loc = if c.line, do: ":#{c.line}", else: ""
              "- **`#{c.path}#{loc}`** -- #{c.body} (`#{c.rule}`)"
            end)

          [header | detail_lines] ++ [""]
        end)

      [
        "> **Hypatia Review** -- #{length(comments)} finding(s)\n",
        "---\n"
        | lines
      ]
      |> Enum.join("\n")
    end
  end

  # ─── Diff Parsing ──────────────────────────────────────────────────

  @doc false
  def parse_diff(diff) do
    diff
    |> String.split(~r/^diff --git /m)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&parse_file_diff/1)
    |> Enum.reject(&is_nil/1)
  end

  defp parse_file_diff(file_diff) do
    lines = String.split(file_diff, "\n")

    # Extract filename from "a/path b/path" header
    case List.first(lines) do
      nil ->
        nil

      header ->
        header = String.trim(header)

        case Regex.run(~r{\sb/(.+)$}, header) do
          [_, path] ->
            # Extract only added lines with their line numbers
            added_lines = extract_added_lines(lines)
            full_content = added_lines |> Enum.map(fn {_n, text} -> text end) |> Enum.join("\n")

            %{
              path: path,
              added_lines: added_lines,
              content: full_content,
              raw: file_diff
            }

          _ ->
            nil
        end
    end
  end

  defp extract_added_lines(diff_lines) do
    {_current_line, added} =
      Enum.reduce(diff_lines, {0, []}, fn line, {current, acc} ->
        cond do
          # Hunk header: @@ -old,count +new,count @@
          String.starts_with?(line, "@@") ->
            case Regex.run(~r/\+(\d+)/, line) do
              [_, start] -> {String.to_integer(start), acc}
              _ -> {current, acc}
            end

          # Added line
          String.starts_with?(line, "+") and not String.starts_with?(line, "+++") ->
            text = String.slice(line, 1..-1//1)
            {current + 1, [{current, text} | acc]}

          # Context or removed line
          String.starts_with?(line, "-") ->
            {current, acc}

          # Context line (no prefix)
          true ->
            {current + 1, acc}
        end
      end)

    Enum.reverse(added)
  end

  # ─── Per-File Review ───────────────────────────────────────────────

  defp review_file(%{path: path, content: content, added_lines: added_lines}) do
    language = detect_language(path)
    findings = []

    # Run general file scan (code safety, secrets, SPDX)
    findings = findings ++ run_file_scan(content, path, language)

    # Run workflow-specific checks
    findings =
      if is_workflow?(path) do
        findings ++ run_workflow_checks(path, content)
      else
        findings
      end

    # Run scorecard checks if relevant files changed
    findings =
      if is_ci_config?(path) do
        findings ++ run_scorecard_checks(path, content)
      else
        findings
      end

    # Run green web checks if deployment-related
    findings =
      if is_deployment_file?(path) do
        findings ++ run_green_web_checks(path, content)
      else
        findings
      end

    # Convert findings to comments, mapping to diff line numbers
    Enum.map(findings, fn finding ->
      line = find_closest_line(finding, added_lines)

      %Comment{
        path: path,
        line: line,
        body: Map.get(finding, :description) || Map.get(finding, :detail, "Finding detected"),
        severity: Map.get(finding, :severity, :medium),
        rule: Map.get(finding, :rule) || Map.get(finding, :type, "unknown") |> to_string()
      }
    end)
  end

  # ─── Rule Module Runners ───────────────────────────────────────────

  defp run_file_scan(content, path, language) do
    try do
      result = Rules.scan_file(content, path, language)
      normalize_findings(result)
    rescue
      _ -> []
    end
  end

  defp run_workflow_checks(path, content) do
    try do
      workflow_contents = %{path => content}

      # One file only: repository-wide workflow rules (WF032) need every
      # workflow, so they are skipped here.
      case WorkflowAudit.audit([path], workflow_contents, partial: true) do
        %{findings: findings} -> normalize_findings(findings)
        result -> normalize_findings(result)
      end
    rescue
      _ -> []
    end
  end

  defp run_scorecard_checks(path, content) do
    try do
      file_contents = %{path => content}
      normalize_findings(ScorecardCompliance.check_least_privilege(file_contents))
    rescue
      _ -> []
    end
  end

  defp run_green_web_checks(path, content) do
    try do
      file_contents = %{path => content}

      normalize_findings(GreenWeb.check_hosting_provider([], file_contents)) ++
        normalize_findings(GreenWeb.check_container_registry([], file_contents))
    rescue
      _ -> []
    end
  end

  # Ensure findings are always a flat list of maps, safe for ++
  defp normalize_findings(result) when is_list(result) do
    Enum.flat_map(result, fn
      item when is_map(item) -> [item]
      _ -> []
    end)
  end

  defp normalize_findings(%{findings: findings}) when is_list(findings) do
    normalize_findings(findings)
  end

  defp normalize_findings(_), do: []

  # ─── Helpers ───────────────────────────────────────────────────────

  defp detect_language(path) do
    case Path.extname(path) |> String.downcase() do
      ".ex" -> :elixir
      ".exs" -> :elixir
      ".erl" -> :erlang
      ".hrl" -> :erlang
      ".rs" -> :rust
      ".res" -> :rescript
      ".resi" -> :rescript
      ".js" -> :javascript
      ".mjs" -> :javascript
      ".jsx" -> :javascript
      ".ts" -> :typescript
      ".tsx" -> :typescript
      ".py" -> :python
      ".rb" -> :ruby
      ".go" -> :go
      ".zig" -> :zig
      ".idr" -> :idris
      ".gleam" -> :gleam
      ".yml" -> :yaml
      ".yaml" -> :yaml
      ".toml" -> :toml
      ".json" -> :json
      ".sh" -> :shell
      ".bash" -> :shell
      _ -> :unknown
    end
  end

  defp is_workflow?(path) do
    String.contains?(path, ".github/workflows/") and
      (String.ends_with?(path, ".yml") or String.ends_with?(path, ".yaml"))
  end

  defp is_ci_config?(path) do
    is_workflow?(path) or
      String.ends_with?(path, "dependabot.yml") or
      String.ends_with?(path, "dependabot.yaml")
  end

  defp is_deployment_file?(path) do
    basename = Path.basename(path) |> String.downcase()

    basename in [
      "containerfile",
      "dockerfile",
      "docker-compose.yml",
      "docker-compose.yaml",
      "compose.yml",
      "compose.yaml",
      "vercel.json",
      "netlify.toml",
      "fly.toml",
      "render.yaml",
      "railway.toml",
      "wrangler.toml",
      "wrangler.jsonc"
    ]
  end

  defp find_closest_line(finding, added_lines) do
    # If the finding has a line reference, use it
    case Map.get(finding, :line) do
      nil ->
        # Otherwise use the first added line
        case added_lines do
          [{n, _} | _] -> n
          _ -> nil
        end

      line_num when is_integer(line_num) ->
        # Map the file line number to the nearest added line
        case Enum.find(added_lines, fn {n, _} -> n >= line_num end) do
          {n, _} ->
            n

          nil ->
            case List.last(added_lines) do
              {n, _} -> n
              nil -> nil
            end
        end

      _ ->
        nil
    end
  end

  # ─── GitHub API ────────────────────────────────────────────────────

  defp fetch_pr_diff(owner, repo, pr_number) do
    token = System.get_env("GITHUB_TOKEN") || ""
    url = "https://api.github.com/repos/#{owner}/#{repo}/pulls/#{pr_number}"

    headers =
      [
        {~c"Accept", ~c"application/vnd.github.v3.diff"},
        {~c"User-Agent", ~c"hypatia-reviewer/1.0"}
      ] ++
        if token != "" do
          [{~c"Authorization", String.to_charlist("token #{token}")}]
        else
          []
        end

    get_same_origin(url, headers)
  end

  @max_redirects 3
  @timeout_ms 15_000

  # GET that follows a redirect only to the origin (scheme, host and port)
  # of the first request, at most @max_redirects times. GitHub answers a
  # request for a renamed repository with a 301 to
  # https://api.github.com/repositories/<id>/..., so the review still runs,
  # while the token never reaches another origin or a plain-HTTP downgrade.
  # A Location carrying userinfo is refused too, so a redirect cannot add
  # credentials of its own. :httpc's own redirect handling stays off (#1195).
  # `timeout_ms` bounds the whole chain, not each hop: every request gets
  # what is left of one monotonic deadline.
  @doc false
  def get_same_origin(url, headers, hops \\ @max_redirects, timeout_ms \\ @timeout_ms) do
    follow_same_origin(url, headers, hops, System.monotonic_time(:millisecond) + timeout_ms)
  end

  defp follow_same_origin(url, headers, hops, deadline) do
    remaining = deadline - System.monotonic_time(:millisecond)

    if remaining <= 0 do
      {:error, {:http_error, :timeout}}
    else
      case :httpc.request(
             :get,
             {String.to_charlist(url), headers},
             [{:timeout, remaining}, {:autoredirect, false}],
             []
           ) do
        {:ok, {{_, 200, _}, _resp_headers, body}} ->
          {:ok, to_string(body)}

        {:ok, {{_, status, _}, resp_headers, _}} when status in [301, 302, 303, 307, 308] ->
          with {:ok, target} <- redirect_target(url, resp_headers),
               true <- same_origin?(url, target) and hops > 0 do
            follow_same_origin(target, headers, hops - 1, deadline)
          else
            _ -> {:error, {:http_status, status}}
          end

        {:ok, {{_, status, _}, _, _}} ->
          {:error, {:http_status, status}}

        {:error, reason} ->
          {:error, {:http_error, reason}}
      end
    end
  end

  defp redirect_target(url, resp_headers) do
    with {_, location} <- List.keyfind(resp_headers, ~c"location", 0),
         location = to_string(location),
         nil <- URI.parse(location).userinfo do
      {:ok, url |> URI.merge(location) |> URI.to_string()}
    else
      _ -> :error
    end
  end

  # Host names compare case-insensitively (ASCII), as DNS does.
  defp same_origin?(from, to) do
    origin(from) == origin(to)
  end

  defp origin(url) do
    uri = URI.parse(url)
    {downcase(uri.scheme), downcase(uri.host), uri.port}
  end

  defp downcase(nil), do: nil
  defp downcase(text), do: String.downcase(text, :ascii)
end
