# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ReviewCheckRunCollector do
  @moduledoc """
  Read-only GitHub Checks API adapter for positively identified pre-commit.ci
  PR runs. This is S-04, separate from GitHub bot review comments.

  A successful check is an external service assertion, not an independently
  verified clean repository or an authorisation to merge or learn. No observed
  check is UNKNOWN, never PASS. Other CI products cannot impersonate this
  provider by naming their check "pre-commit.ci - pr".

  GitHub's REST check runs may be paginated and are not an atomic database
  snapshot. The collector reads the full bounded inventory twice and refuses
  inconsistent lists or a moving PR head. Only GET requests are issued.
  """

  alias Hypatia.ReviewGithubCollector
  alias Hypatia.ReviewObservation

  @page_size 100
  @max_budget 10
  @default_budget 5
  @app_id 68672
  @app_slug "pre-commit-ci"
  @check_name "pre-commit.ci - pr"
  @sha ~r/\A[0-9a-f]{40}\z/i
  @repo ~r/\A[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+\z/
  @outcomes ~w(success failure neutral skipped cancelled timed_out action_required startup_failure stale)

  @doc """
  Read the subject PR and all paginated check runs at its exact head.

  The optional :api one-argument callback is solely an injectable GET API
  client, returning {:ok, decoded_json} or {:error, reason}. Production
  delegates to the GitHub collector's bounded gh CLI GET transport.
  """
  @spec collect(String.t(), pos_integer(), keyword()) :: {:ok, map()} | {:error, term()}
  def collect(repo, number, opts \\ []) do
    api = Keyword.get(opts, :api, &ReviewGithubCollector.gh_get/1)
    budget = Keyword.get(opts, :page_budget, @default_budget)

    cond do
      not valid_repo?(repo) ->
        {:error, :invalid_repository}

      not (is_integer(number) and number > 0) ->
        {:error, :invalid_pull_request}

      not is_function(api, 1) ->
        {:error, :invalid_api_client}

      not (is_integer(budget) and budget >= 1 and budget <= @max_budget) ->
        {:error, :invalid_page_budget}

      true ->
        collect_snapshot(repo, number, api, budget)
    end
  end

  defp collect_snapshot(repo, number, api, budget) do
    subject_path = "repos/#{repo}/pulls/#{number}"

    with {:ok, start_pr} <- get_object(api, subject_path),
         {:ok, head} <- subject_head(start_pr, repo, number),
         path = "repos/#{repo}/commits/#{head}/check-runs",
         {:ok, first} <- fetch_check_runs(api, path, budget),
         {:ok, second} <- fetch_check_runs(api, path, budget),
         :ok <- same_inventory(first, second),
         {:ok, end_pr} <- get_object(api, subject_path),
         {:ok, ^head} <- subject_head(end_pr, repo, number),
         {:ok, observations} <- classify(first, repo, number, head) do
      {:ok,
       %{
         "repository" => repo,
         "pull_request" => number,
         "head_sha" => head,
         "source" => "github_checks_api_via_gh",
         "collection" => "repeated_stable_observation",
         "coverage" => "pre_commit_ci_pr_check_runs_only",
         "transport_provenance" => "github_api_observed",
         "provider" => "pre-commit.ci",
         "matching_checks" => length(observations),
         "provider_observed" => observations != [],
         "coverage_verdict" => "not_established",
         "observations" => Enum.sort_by(observations, &get_in(&1, ["source", "event_id"]))
       }}
    else
      {:ok, _other_head} -> {:error, :head_changed_during_collection}
      {:error, _reason} = error -> error
    end
  end

  defp valid_repo?(repo) when is_binary(repo) do
    Regex.match?(@repo, repo) and
      Enum.all?(String.split(repo, "/"), &(&1 not in [".", ".."]))
  end

  defp valid_repo?(_), do: false

  defp subject_head(
         %{
           "number" => actual_number,
           "head" => %{"sha" => sha},
           "base" => %{"repo" => %{"full_name" => actual_repo}}
         },
         repo,
         number
       ) do
    cond do
      actual_number != number ->
        {:error, :subject_number_mismatch}

      not (is_binary(actual_repo) and String.downcase(actual_repo) == String.downcase(repo)) ->
        {:error, :subject_repository_mismatch}

      not (is_binary(sha) and Regex.match?(@sha, sha)) ->
        {:error, :invalid_head_sha}

      true ->
        {:ok, String.downcase(sha)}
    end
  end

  defp subject_head(_other, _repo, _number), do: {:error, :invalid_subject_shape}

  defp get_object(api, path) do
    case api.(path) do
      {:ok, %{} = object} -> {:ok, object}
      {:ok, _} -> {:error, {:invalid_api_object, path}}
      {:error, _} = error -> error
      _ -> {:error, {:invalid_api_result, path}}
    end
  end

  defp fetch_check_runs(api, path, budget), do: fetch_page(api, path, 1, budget, [])

  defp fetch_page(api, path, page, budget, acc) do
    endpoint = "#{path}?per_page=#{@page_size}&page=#{page}&filter=all"

    case api.(endpoint) do
      {:ok, %{"total_count" => total, "check_runs" => batch}}
      when is_integer(total) and total >= 0 and is_list(batch) and
             length(batch) <= @page_size ->
        accumulated = [batch | acc]
        received = Enum.reduce(accumulated, 0, fn x, count -> count + length(x) end)

        cond do
          received > total ->
            {:error, {:inconsistent_total_count, path}}

          length(batch) == @page_size and page >= budget ->
            {:error, {:page_budget_exhausted, path}}

          length(batch) == @page_size ->
            fetch_page(api, path, page + 1, budget, accumulated)

          received != total ->
            {:error, {:incomplete_check_inventory, path}}

          true ->
            records = accumulated |> Enum.reverse() |> List.flatten()

            ids =
              Enum.map(records, fn
                %{"id" => id} when is_integer(id) and id > 0 -> id
                _ -> nil
              end)

            if Enum.all?(ids, &is_integer/1) and length(ids) == length(Enum.uniq(ids)) do
              {:ok, records}
            else
              {:error, {:invalid_or_duplicate_check_ids, path}}
            end
        end

      {:ok, _other} ->
        {:error, {:invalid_check_runs_shape, endpoint}}

      {:error, _} = error ->
        error

      _ ->
        {:error, {:invalid_check_runs_result, endpoint}}
    end
  end

  defp same_inventory(left, right) when left == right, do: :ok
  defp same_inventory(_left, _right), do: {:error, :check_inventory_changed}

  defp classify(runs, repo, number, head) do
    Enum.reduce_while(runs, {:ok, []}, fn run, {:ok, accepted} ->
      case from_run(run, repo, number, head) do
        {:ok, record} -> {:cont, {:ok, [record | accepted]}}
        :ignore -> {:cont, {:ok, accepted}}
        {:error, reason} -> {:halt, {:error, {:invalid_provider_check, reason}}}
      end
    end)
    |> case do
      {:ok, items} -> {:ok, Enum.reverse(items)}
      error -> error
    end
  end

  defp from_run(
         %{"name" => @check_name, "app" => %{"id" => @app_id, "slug" => @app_slug}} = run,
         repo,
         number,
         head
       ) do
    with {:ok, id} <- positive_id(run["id"]),
         :ok <- matching_sha(run["head_sha"], head),
         {:ok, status, conclusion} <- check_state(run["status"], run["conclusion"]),
         {:ok, observation} <-
           ReviewObservation.admit(
             %{
               "provider" => "pre-commit.ci",
               "scope" => "hooks",
               "repo" => repo,
               "pr_number" => number,
               "reviewed_sha" => head,
               "source_event_id" => "github:check_run:#{id}",
               "source_url" => "https://github.com/#{repo}/pull/#{number}",
               "status" => status,
               # Native conclusion is a provider claim, not a count of
               # independently reproduced hook findings.
               "has_findings" => nil
             },
             head
           ) do
      {:ok,
       Map.merge(observation, %{
         "check_conclusion" => conclusion,
         "check_status" => run["status"],
         "provider_run_status" => "github_check_observed",
         "provider_app_id" => Integer.to_string(@app_id)
       })}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # The app identity and exact PR check name must BOTH match. A renamed check
  # or unrecognised app is unknown, not evidence of success.
  defp from_run(_other, _repo, _number, _head), do: :ignore

  defp positive_id(id) when is_integer(id) and id > 0, do: {:ok, id}
  defp positive_id(_), do: {:error, :invalid_check_id}

  defp matching_sha(value, head) when is_binary(value) do
    if Regex.match?(@sha, value) and String.downcase(value) == head,
      do: :ok,
      else: {:error, :check_head_mismatch}
  end

  defp matching_sha(_value, _head), do: {:error, :missing_check_head}

  defp check_state(state, conclusion) when state in ~w(queued in_progress pending) do
    if is_nil(conclusion),
      do: {:ok, "running", "unknown"},
      else: {:error, :running_with_conclusion}
  end

  defp check_state("completed", conclusion) when conclusion in @outcomes do
    {:ok, "completed", conclusion}
  end

  defp check_state(_state, _conclusion), do: {:error, :unsupported_check_state}
end
