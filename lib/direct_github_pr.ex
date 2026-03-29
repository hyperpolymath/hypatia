# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.DirectGitHubPR do
  @moduledoc """
  Creates fix PRs directly against GitHub repos for scorecard findings.

  Instead of routing through the gitbot-fleet GraphQL dispatch pipeline, this
  module clones a target repo, runs the appropriate fix script, and opens a PR
  via the `gh` CLI. This is the fast path for well-understood, high-confidence
  scorecard fixes (SC-013 Pinned-Dependencies, SC-018 Token-Permissions).

  ## Usage

      # Single finding from a batch JSONL line
      finding = %{"id" => "SC-013-myrepo", "scorecard_check" => "Pinned-Dependencies", ...}
      {:ok, pr_url} = Hypatia.DirectGitHubPR.create_fix_pr(finding)

      # Batch mode — reads JSONL, creates PRs with rate limiting
      {:ok, results} = Hypatia.DirectGitHubPR.batch_create_prs("findings.jsonl")

  ## Fix Script Mapping

  | Check ID | Scorecard Check      | Fix Script                       |
  |----------|----------------------|----------------------------------|
  | SC-013   | Pinned-Dependencies  | scripts/fix-pinned-dependencies.sh |
  | SC-018   | Token-Permissions    | scripts/fix-token-permissions.sh   |

  ## Rate Limiting

  Batch mode inserts a configurable delay between PRs (default: 5 seconds) to
  avoid triggering GitHub's secondary rate limits. The delay is configurable via
  the `:delay_ms` option.
  """

  require Logger

  @hypatia_root Path.expand("../../", __DIR__)
  @git_author_name "Jonathan D.A. Jewell"
  @git_author_email "6759885+hyperpolymath@users.noreply.github.com"
  @github_org "hyperpolymath"
  @default_delay_ms 5_000

  # Maps scorecard check IDs to fix scripts (relative to hypatia root)
  @fix_scripts %{
    "SC-013" => "scripts/fix-pinned-dependencies.sh",
    "SC-018" => "scripts/fix-token-permissions.sh"
  }

  # Maps scorecard check names to IDs (for findings keyed by name)
  @check_name_to_id %{
    "Pinned-Dependencies" => "SC-013",
    "Token-Permissions" => "SC-018"
  }

  # --- Public API ---

  @doc """
  Create a fix PR for a single scorecard finding.

  The finding map must include:
  - `"scorecard_check"` or `"pa_rule"` — identifies which fix to apply
  - `"repos_affected_list"` — list of repo names (uses the first)

  Returns `{:ok, pr_url}` on success or `{:error, reason}` on failure.
  """
  @spec create_fix_pr(map()) :: {:ok, String.t()} | {:error, term()}
  def create_fix_pr(finding) when is_map(finding) do
    with {:ok, repo_name} <- extract_repo_name(finding),
         {:ok, check_id} <- extract_check_id(finding),
         {:ok, fix_script} <- lookup_fix_script(check_id),
         {:ok, tmp_dir} <- clone_repo(repo_name),
         {:ok, branch_name} <- create_branch(tmp_dir, check_id),
         :ok <- run_fix_script(fix_script, tmp_dir),
         {:ok, :has_changes} <- check_for_changes(tmp_dir),
         :ok <- commit_changes(tmp_dir, check_id, finding),
         :ok <- push_branch(tmp_dir, branch_name),
         {:ok, pr_url} <- open_pr(tmp_dir, branch_name, check_id, finding) do
      Logger.info("[direct-pr] Created PR for #{repo_name} (#{check_id}): #{pr_url}")
      cleanup_tmp(tmp_dir)
      {:ok, pr_url}
    else
      {:error, :no_changes} ->
        Logger.info("[direct-pr] No changes needed for finding #{inspect(finding["id"])}")
        {:ok, :no_changes}

      {:error, reason} = err ->
        Logger.warning("[direct-pr] Failed to create PR: #{inspect(reason)}")
        err
    end
  end

  @doc """
  Batch-create PRs from a JSONL file of scorecard findings.

  Each line in the JSONL file is a JSON-encoded finding map. Only findings with
  a supported fix script (SC-013, SC-018) are processed — others are skipped.

  ## Options

  - `:delay_ms` — milliseconds to wait between PRs (default: #{@default_delay_ms})
  - `:dry_run` — if true, log what would be done without creating PRs (default: false)
  - `:max_prs` — maximum number of PRs to create (default: unlimited)
  - `:filter_check` — only process findings matching this check ID (e.g., "SC-013")

  Returns `{:ok, results}` where results is a list of `{finding_id, outcome}` tuples.
  """
  @spec batch_create_prs(String.t(), keyword()) :: {:ok, list()} | {:error, term()}
  def batch_create_prs(jsonl_path, opts \\ []) do
    delay_ms = Keyword.get(opts, :delay_ms, @default_delay_ms)
    dry_run = Keyword.get(opts, :dry_run, false)
    max_prs = Keyword.get(opts, :max_prs, :infinity)
    filter_check = Keyword.get(opts, :filter_check, nil)

    case File.read(jsonl_path) do
      {:ok, content} ->
        findings =
          content
          |> String.split("\n", trim: true)
          |> Enum.map(&decode_jsonl_line/1)
          |> Enum.reject(&is_nil/1)
          |> maybe_filter_check(filter_check)
          |> Enum.filter(&has_supported_fix?/1)
          |> maybe_limit(max_prs)

        Logger.info("[direct-pr] Processing #{length(findings)} findings from #{jsonl_path}")

        results =
          findings
          |> Enum.with_index(1)
          |> Enum.map(fn {finding, idx} ->
            finding_id = Map.get(finding, "id", "unknown-#{idx}")

            if dry_run do
              check_id = extract_check_id(finding) |> elem(1)
              repo = extract_repo_name(finding) |> elem(1)
              Logger.info("[direct-pr] DRY RUN [#{idx}/#{length(findings)}]: #{repo} (#{check_id})")
              {finding_id, :dry_run}
            else
              Logger.info("[direct-pr] Processing [#{idx}/#{length(findings)}]: #{finding_id}")

              outcome =
                case create_fix_pr(finding) do
                  {:ok, pr_url} -> {:ok, pr_url}
                  {:error, reason} -> {:error, reason}
                end

              # Rate limit between PRs (skip delay after the last one)
              if idx < length(findings) do
                Process.sleep(delay_ms)
              end

              {finding_id, outcome}
            end
          end)

        succeeded = Enum.count(results, fn {_, outcome} -> match?({:ok, _}, outcome) end)
        failed = Enum.count(results, fn {_, outcome} -> match?({:error, _}, outcome) end)
        Logger.info("[direct-pr] Batch complete: #{succeeded} succeeded, #{failed} failed")

        {:ok, results}

      {:error, reason} ->
        {:error, {:file_read_failed, jsonl_path, reason}}
    end
  end

  @doc """
  List supported scorecard checks that have fix scripts.
  Returns a map of check_id => script_path.
  """
  @spec supported_checks() :: map()
  def supported_checks, do: @fix_scripts

  # --- Internal: Finding Parsing ---

  defp extract_repo_name(finding) do
    case Map.get(finding, "repos_affected_list", []) do
      [repo | _] when is_binary(repo) and repo != "" ->
        {:ok, repo}

      _ ->
        case Map.get(finding, "routed_repo") do
          repo when is_binary(repo) and repo != "" -> {:ok, repo}
          _ -> {:error, :no_repo_in_finding}
        end
    end
  end

  defp extract_check_id(finding) do
    # Try pa_rule field first (e.g., "SC013" -> "SC-013")
    pa_rule = Map.get(finding, "pa_rule", "")

    cond do
      # Direct check ID format: "SC-013"
      String.starts_with?(pa_rule, "SC") and String.contains?(pa_rule, "-") ->
        {:ok, pa_rule}

      # Compact format: "SC013" -> "SC-013"
      Regex.match?(~r/^SC\d{3}$/, pa_rule) ->
        {:ok, String.replace(pa_rule, ~r/^SC(\d{3})$/, "SC-\\1")}

      # Fall back to scorecard_check name
      true ->
        check_name = Map.get(finding, "scorecard_check", "")
        case Map.get(@check_name_to_id, check_name) do
          nil ->
            # Try extracting from the finding ID (e.g., "SC-013-myrepo")
            case Regex.run(~r/^(SC-\d{3})/, Map.get(finding, "id", "")) do
              [_, id] -> {:ok, id}
              _ -> {:error, {:unknown_check, pa_rule, check_name}}
            end

          id ->
            {:ok, id}
        end
    end
  end

  defp lookup_fix_script(check_id) do
    case Map.get(@fix_scripts, check_id) do
      nil -> {:error, {:no_fix_script, check_id}}
      script -> {:ok, Path.join(@hypatia_root, script)}
    end
  end

  defp has_supported_fix?(finding) do
    case extract_check_id(finding) do
      {:ok, check_id} -> Map.has_key?(@fix_scripts, check_id)
      _ -> false
    end
  end

  # --- Internal: Git Operations ---

  defp clone_repo(repo_name) do
    tmp_dir = Path.join(System.tmp_dir!(), "hypatia-pr-#{repo_name}-#{System.unique_integer([:positive])}")

    github_url = "https://github.com/#{@github_org}/#{repo_name}.git"

    case System.cmd("gh", ["repo", "clone", "#{@github_org}/#{repo_name}", tmp_dir, "--", "--depth=1"],
           stderr_to_stdout: true) do
      {_output, 0} ->
        {:ok, tmp_dir}

      {output, code} ->
        Logger.warning("[direct-pr] Clone failed for #{github_url} (exit #{code}): #{String.slice(output, 0, 200)}")
        {:error, {:clone_failed, repo_name, code}}
    end
  end

  defp create_branch(tmp_dir, check_id) do
    # Normalize check_id for branch name: SC-013 -> scorecard-fix-SC-013
    branch_name = "hypatia/scorecard-fix-#{String.downcase(check_id)}"

    case System.cmd("git", ["checkout", "-b", branch_name], cd: tmp_dir, stderr_to_stdout: true) do
      {_output, 0} ->
        {:ok, branch_name}

      {output, code} ->
        {:error, {:branch_failed, branch_name, code, output}}
    end
  end

  defp run_fix_script(fix_script, tmp_dir) do
    unless File.exists?(fix_script) do
      {:error, {:script_not_found, fix_script}}
    else
      case System.cmd("bash", [fix_script, tmp_dir], stderr_to_stdout: true) do
        {output, 0} ->
          Logger.debug("[direct-pr] Fix script output:\n#{output}")
          :ok

        {output, 2} ->
          # Exit code 2 means some resolutions failed but fixes were applied
          Logger.warning("[direct-pr] Fix script had warnings:\n#{output}")
          :ok

        {output, code} ->
          {:error, {:script_failed, fix_script, code, String.slice(output, 0, 500)}}
      end
    end
  end

  defp check_for_changes(tmp_dir) do
    case System.cmd("git", ["status", "--porcelain"], cd: tmp_dir, stderr_to_stdout: true) do
      {"", 0} ->
        {:error, :no_changes}

      {output, 0} when byte_size(output) > 0 ->
        {:ok, :has_changes}

      {_, _} ->
        {:error, :no_changes}
    end
  end

  defp commit_changes(tmp_dir, check_id, finding) do
    description = Map.get(finding, "description", "Scorecard fix #{check_id}")
    remediation = Map.get(finding, "remediation", "")
    check_name = Map.get(finding, "scorecard_check", check_id)

    commit_msg = """
    fix(scorecard): #{check_name} — #{short_description(check_id)}

    Automated fix applied by Hypatia neurosymbolic CI/CD intelligence.

    Check: #{check_name} (#{check_id})
    Finding: #{description}
    Remediation: #{remediation}

    Applied fix script from hypatia/scripts/.
    See: https://github.com/ossf/scorecard/blob/main/docs/checks.md

    Signed-off-by: #{@git_author_name} <#{@git_author_email}>
    """

    # Configure git author for the commit
    env = [
      {"GIT_AUTHOR_NAME", @git_author_name},
      {"GIT_AUTHOR_EMAIL", @git_author_email},
      {"GIT_COMMITTER_NAME", @git_author_name},
      {"GIT_COMMITTER_EMAIL", @git_author_email}
    ]

    with {_, 0} <- System.cmd("git", ["add", "-A"], cd: tmp_dir, env: env, stderr_to_stdout: true),
         {_, 0} <-
           System.cmd("git", ["commit", "-m", String.trim(commit_msg)],
             cd: tmp_dir,
             env: env,
             stderr_to_stdout: true
           ) do
      :ok
    else
      {output, code} ->
        {:error, {:commit_failed, code, String.slice(output, 0, 300)}}
    end
  end

  defp push_branch(tmp_dir, branch_name) do
    case System.cmd("git", ["push", "-u", "origin", branch_name],
           cd: tmp_dir,
           stderr_to_stdout: true) do
      {_output, 0} ->
        :ok

      {output, code} ->
        {:error, {:push_failed, branch_name, code, String.slice(output, 0, 300)}}
    end
  end

  defp open_pr(tmp_dir, branch_name, check_id, finding) do
    check_name = Map.get(finding, "scorecard_check", check_id)
    repo_name = Map.get(finding, "repos_affected_list", ["unknown"]) |> List.first()

    title = "fix(scorecard): #{check_name} — automated #{short_description(check_id)}"

    # Truncate title to 72 chars for GitHub conventions
    title =
      if String.length(title) > 72 do
        String.slice(title, 0, 69) <> "..."
      else
        title
      end

    body = """
    ## Scorecard Fix: #{check_name} (#{check_id})

    **Automated PR created by [Hypatia](https://github.com/hyperpolymath/hypatia) neurosymbolic CI/CD intelligence.**

    ### What changed

    #{fix_description(check_id)}

    ### Finding details

    - **Check:** #{check_name}
    - **ID:** #{check_id}
    - **Description:** #{Map.get(finding, "description", "N/A")}
    - **Remediation:** #{Map.get(finding, "remediation", "N/A")}

    ### Verification

    After merging, re-run OpenSSF Scorecard to confirm the check passes:

    ```bash
    scorecard --repo https://github.com/#{@github_org}/#{repo_name} --checks #{check_name}
    ```

    ---

    *This PR was generated by Hypatia's DirectGitHubPR module. If this fix is incorrect, please close the PR and file an issue on [hypatia](https://github.com/hyperpolymath/hypatia/issues).*
    """

    case System.cmd(
           "gh",
           [
             "pr",
             "create",
             "--title",
             title,
             "--body",
             String.trim(body),
             "--head",
             branch_name,
             "--base",
             "main"
           ],
           cd: tmp_dir,
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        pr_url = output |> String.trim()
        {:ok, pr_url}

      {output, code} ->
        # If main doesn't exist, try master
        if String.contains?(output, "No commits") or
             String.contains?(output, "invalid reference") or
             String.contains?(output, "not found") do
          retry_pr_with_base(tmp_dir, branch_name, title, body, "master")
        else
          {:error, {:pr_create_failed, code, String.slice(output, 0, 400)}}
        end
    end
  end

  defp retry_pr_with_base(tmp_dir, branch_name, title, body, base) do
    case System.cmd(
           "gh",
           [
             "pr",
             "create",
             "--title",
             title,
             "--body",
             String.trim(body),
             "--head",
             branch_name,
             "--base",
             base
           ],
           cd: tmp_dir,
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        {:ok, output |> String.trim()}

      {output, code} ->
        {:error, {:pr_create_failed, code, String.slice(output, 0, 400)}}
    end
  end

  # --- Internal: Cleanup ---

  defp cleanup_tmp(tmp_dir) do
    # Best-effort cleanup — don't fail if it doesn't work
    File.rm_rf(tmp_dir)
  end

  # --- Internal: Descriptions ---

  defp short_description("SC-013"), do: "pin dependencies to SHA hashes"
  defp short_description("SC-018"), do: "add token permissions"
  defp short_description(id), do: "fix #{id}"

  defp fix_description("SC-013") do
    """
    Pinned all GitHub Actions references from tag-based (`@v4`) to SHA-based
    (`@abc123def... # v4`) pins. This prevents supply-chain attacks where a tag
    is moved to point at malicious code. The original tag is preserved as a
    trailing comment for human readability.
    """
  end

  defp fix_description("SC-018") do
    """
    Added `permissions: read-all` to workflow files that lacked a top-level
    `permissions` declaration. This follows the principle of least privilege:
    the GITHUB_TOKEN starts with read-only access, and individual jobs can
    escalate only the specific permissions they need.
    """
  end

  defp fix_description(_), do: "Applied automated scorecard fix."

  # --- Internal: JSONL Parsing ---

  defp decode_jsonl_line(line) do
    case Jason.decode(line) do
      {:ok, map} when is_map(map) -> map
      _ -> nil
    end
  end

  defp maybe_filter_check(findings, nil), do: findings

  defp maybe_filter_check(findings, check_id) do
    Enum.filter(findings, fn finding ->
      case extract_check_id(finding) do
        {:ok, ^check_id} -> true
        _ -> false
      end
    end)
  end

  defp maybe_limit(findings, :infinity), do: findings
  defp maybe_limit(findings, max) when is_integer(max), do: Enum.take(findings, max)
end
