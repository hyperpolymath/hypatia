# SPDX-License-Identifier: MPL-2.0
defmodule Mix.Tasks.Hypatia.PrEligibility do
  @moduledoc """
  Query AM010 admin-merge eligibility for a specific pull request.

  Uses BP008 phantom-context detection and the PR's status-check rollup to
  determine whether the PR is blocked only by phantom required contexts —
  contexts that are configured in branch protection but never emit a
  check-run. Such PRs are safe to admin-merge (rule AM010).

  ## Usage

      mix hypatia.pr_eligibility --owner OWNER --repo REPO --pr NUMBER

  ## Output

  JSON on stdout:

      {
        "eligible": true | false,
        "reason": "AM010" | null,
        "phantom_contexts": [...],
        "required_contexts": [...]
      }

  Exit code 0 regardless of eligibility result. Exit code 2 on argument
  or API error.

  ## Environment

      GITHUB_TOKEN           Required. Must have repo read + statuses read scope.
      HYPATIA_DISPATCH_PAT   Alternative token (used if GITHUB_TOKEN unset).

  ## CLI escript variant

  When invoked as the escript entry-point via `hypatia pr-eligibility`,
  the same options are accepted:

      hypatia pr-eligibility --owner OWNER --repo REPO --pr NUMBER

  See `Hypatia.CLI.run_pr_eligibility/3` which delegates here.
  """

  use Mix.Task

  alias Hypatia.Rules.BranchProtection
  alias Hypatia.Rules.AdminMergeEligibility

  @shortdoc "Query AM010 admin-merge eligibility for a PR (BP008 phantom check)"

  @impl true
  def run(argv) do
    {opts, _rest, _invalid} =
      OptionParser.parse(argv,
        switches: [owner: :string, repo: :string, pr: :integer]
      )

    owner = opts[:owner]
    repo = opts[:repo]
    pr_number = opts[:pr]

    unless owner && repo && pr_number do
      Mix.raise(
        "Usage: mix hypatia.pr_eligibility --owner OWNER --repo REPO --pr NUMBER"
      )
    end

    result = check_eligibility(owner, repo, pr_number)
    IO.puts(Jason.encode!(result, pretty: false))
  end

  @doc """
  Core eligibility logic. Returns a map suitable for JSON output.

  Called both from the Mix task and from `Hypatia.CLI` when the escript
  receives `pr-eligibility` as the command.
  """
  @spec check_eligibility(String.t(), String.t(), pos_integer()) :: map()
  def check_eligibility(owner, repo, pr_number) do
    # Step 1: Fetch required status-check contexts from branch protection.
    required_contexts = fetch_required_contexts(owner, repo)

    # Step 2: Run BP008 to find phantom contexts (required but never emitting).
    phantom_findings = BranchProtection.bp008_phantom_required_context(owner, repo)
    phantom_contexts =
      phantom_findings
      |> Enum.map(fn finding -> get_in(finding, [:detail, :phantom_context]) end)
      |> Enum.reject(&is_nil/1)

    # Step 3: Fetch this PR's status-check rollup via GraphQL.
    rollup = fetch_pr_rollup(owner, repo, pr_number)

    # Step 4: Call AM010 state checker.
    pr_state = %{
      required_contexts: required_contexts,
      phantom_contexts: phantom_contexts,
      rollup: rollup
    }

    case AdminMergeEligibility.am010_phantom_only_blocker?(pr_state) do
      {:eligible, "AM010"} ->
        %{
          eligible: true,
          reason: "AM010",
          phantom_contexts: phantom_contexts,
          required_contexts: required_contexts
        }

      _ ->
        %{
          eligible: false,
          reason: nil,
          phantom_contexts: phantom_contexts,
          required_contexts: required_contexts
        }
    end
  end

  # ─── GitHub API helpers ───────────────────────────────────────────────

  defp fetch_required_contexts(owner, repo) do
    case System.cmd(
           "gh",
           [
             "api",
             "repos/#{owner}/#{repo}/branches/main/protection/required_status_checks",
             "--jq",
             ".contexts // (.checks // [] | map(.context)) | unique"
           ],
           stderr_to_stdout: true
         ) do
      {out, 0} ->
        case Jason.decode(out) do
          {:ok, list} when is_list(list) -> Enum.filter(list, &is_binary/1)
          _ -> []
        end

      {err, _} ->
        IO.puts(:stderr, "Warning: could not fetch required status checks: #{String.trim(err)}")
        []
    end
  end

  defp fetch_pr_rollup(owner, repo, pr_number) do
    # GraphQL query for statusCheckRollup. We want each context's name and
    # conclusion so AM010 can classify passing vs. failing vs. absent entries.
    query = """
    query($owner: String!, $repo: String!, $number: Int!) {
      repository(owner: $owner, name: $repo) {
        pullRequest(number: $number) {
          commits(last: 1) {
            nodes {
              commit {
                statusCheckRollup {
                  contexts(first: 100) {
                    nodes {
                      ... on CheckRun {
                        name
                        conclusion
                        status
                      }
                      ... on StatusContext {
                        context
                        state
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    """

    case System.cmd(
           "gh",
           [
             "api",
             "graphql",
             "--field",
             "owner=#{owner}",
             "--field",
             "repo=#{repo}",
             "--field",
             "number=#{pr_number}",
             "--field",
             "query=#{query}"
           ],
           stderr_to_stdout: true
         ) do
      {out, 0} ->
        case Jason.decode(out) do
          {:ok, data} ->
            nodes =
              get_in(data, [
                "data",
                "repository",
                "pullRequest",
                "commits",
                "nodes"
              ]) || []

            rollup_nodes =
              nodes
              |> List.last(%{})
              |> get_in(["commit", "statusCheckRollup", "contexts", "nodes"]) || []

            # Normalise both CheckRun and StatusContext shapes into the map
            # form expected by AM010: %{"name" => name, "conclusion" => conclusion}.
            Enum.flat_map(rollup_nodes, fn node ->
              cond do
                is_binary(Map.get(node, "name")) ->
                  # CheckRun node: map status → conclusion for in-progress entries
                  conc =
                    Map.get(node, "conclusion") ||
                      status_to_conclusion(Map.get(node, "status"))

                  [%{"name" => Map.get(node, "name"), "conclusion" => conc}]

                is_binary(Map.get(node, "context")) ->
                  # StatusContext node: map state → conclusion
                  conc = state_to_conclusion(Map.get(node, "state"))
                  [%{"name" => Map.get(node, "context"), "conclusion" => conc}]

                true ->
                  []
              end
            end)

          _ ->
            []
        end

      {err, _} ->
        IO.puts(:stderr, "Warning: could not fetch PR rollup: #{String.trim(err)}")
        []
    end
  end

  # Map GraphQL CheckRun status → conclusion-equivalent string.
  # IN_PROGRESS / QUEUED / WAITING → nil (non-terminal; AM010 ALARP mitigation
  # counts these as present-in-rollup so they are NOT treated as absent phantoms).
  defp status_to_conclusion("COMPLETED"), do: nil
  defp status_to_conclusion(_), do: "IN_PROGRESS"

  # Map StatusContext state → conclusion-equivalent string.
  defp state_to_conclusion("SUCCESS"), do: "SUCCESS"
  defp state_to_conclusion("FAILURE"), do: "FAILURE"
  defp state_to_conclusion("ERROR"), do: "FAILURE"
  defp state_to_conclusion("PENDING"), do: "IN_PROGRESS"
  defp state_to_conclusion(_), do: nil
end
