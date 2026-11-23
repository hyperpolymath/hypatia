# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ReviewGithubCollector do
  @moduledoc """
  Read-only GitHub REST collector for structured external-review observations.

  This is an *observation* boundary, not a review verdict. It obtains PR
  metadata, inline review comments and review submissions through the
  authenticated `gh api` CLI, checks GitHub account IDs, and passes a
  deliberately small subset to `Hypatia.ReviewObservation.admit/2`.

  A bot's body is treated only as untrusted data: known machine markers select
  an observation *scope*, never a command, patch, approval or proof. Issue
  discussion summaries (including "no findings" and quota reports) are not
  imported here: they have no API-level reviewed-commit binding. No absence
  of comments is classified as a clean review.

  The PR head is fetched before and after pagination; a moving head aborts
  the whole collection. Any failed API call or exhausted pagination budget
  aborts it too, rather than returning a misleading partial inventory.

  JSON returned by the GitHub API is *not* a canonical I-JSON/JCS receipt.
  This collector does not persist, sign, hash, dispatch or learn anything.
  """

  alias Hypatia.ReviewObservation

  # Verified on the REST-backed GitHub connector, 2026-11-23. Require BOTH
  # numeric actor ID and login from the authenticated API, not a display name
  # in Markdown. Reconfirm these IDs when updating the provider adapter.
  @bot_accounts %{
    199_175_422 => {"codex", "chatgpt-codex-connector[bot]"},
    136_622_811 => {"coderabbit", "coderabbitai[bot]"}
  }

  @sha ~r/\A[0-9a-f]{40}\z/i
  @repo ~r/\A[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+\z/
  @page_size 100
  @default_page_budget 5
  @max_page_budget 10
  @max_response_bytes 4_000_000

  @doc """
  Collect a bounded PR snapshot.

  The optional `:api` function has the contract
  `(GitHub REST relative path -> {:ok, decoded map/list} | {:error, reason})`.
  Production defaults to `gh api --method GET`; test fixtures inject a pure
  function. `:page_budget` is an integer from 1 through 10 (default 5).
  """
  @spec collect(String.t(), pos_integer(), keyword()) :: {:ok, map()} | {:error, term()}
  def collect(repo, number, opts \\ [])

  def collect(repo, number, opts)
      when is_binary(repo) and is_integer(number) and number > 0 and is_list(opts) do
    api = Keyword.get(opts, :api, &gh_get/1)
    budget = Keyword.get(opts, :page_budget, @default_page_budget)

    cond do
      not valid_repo?(repo) ->
        {:error, :invalid_repository}

      not is_function(api, 1) ->
        {:error, :invalid_api_client}

      not (is_integer(budget) and budget >= 1 and budget <= @max_page_budget) ->
        {:error, :invalid_page_budget}

      true ->
        collect_snapshot(repo, number, api, budget)
    end
  end

  def collect(_repo, _number, _opts), do: {:error, :invalid_subject}

  @doc "Only the two independently identified GitHub bot accounts are admitted in v0."
  def bot_accounts, do: @bot_accounts

  defp collect_snapshot(repo, number, api, budget) do
    pr_path = "repos/#{repo}/pulls/#{number}"
    comments_path = pr_path <> "/comments"
    reviews_path = pr_path <> "/reviews"

    # Two reads of each paginated inventory detect ordinary additions,
    # deletions, edits and page shifts even when the PR head has not moved.
    # These REST reads are not atomic: success is a *repeated observation*,
    # not a proof that no concurrent mutation was possible.
    with {:ok, start_pr} <- fetch_object(api, pr_path),
         {:ok, head} <- subject_head(start_pr, repo, number),
         {:ok, comments} <- fetch_pages(api, comments_path, budget),
         {:ok, reviews} <- fetch_pages(api, reviews_path, budget),
         {:ok, comments_again} <- fetch_pages(api, comments_path, budget),
         {:ok, reviews_again} <- fetch_pages(api, reviews_path, budget),
         :ok <- inventory_agrees(comments, comments_again, comments_path),
         :ok <- inventory_agrees(reviews, reviews_again, reviews_path),
         {:ok, end_pr} <- fetch_object(api, pr_path),
         {:ok, ^head} <- subject_head(end_pr, repo, number) do
      # Never downgrade a malformed, positively identified finding to an
      # ignored event. Its schema failure invalidates the whole snapshot.
      events =
        Enum.map(comments, &from_comment(&1, repo, number, head, reviews)) ++
          Enum.map(reviews, &from_review(&1, repo, number, head, comments))

      case Enum.reduce_while(events, {:ok, {[], 0}}, fn
             {:ok, observation}, {:ok, {found, skipped}} ->
               {:cont, {:ok, {[observation | found], skipped}}}

             {:ignore, _reason}, {:ok, {found, skipped}} ->
               {:cont, {:ok, {found, skipped + 1}}}

             {:error, reason}, _acc ->
               {:halt, {:error, {:invalid_source_event, reason}}}
           end) do
        {:ok, {observations, ignored}} ->
          {:ok,
           %{
             "repository" => repo,
             "pull_request" => number,
             "head_sha" => head,
             "collection" => "repeated_stable_observation",
             "source" => "github_rest_via_gh",
             "transport_provenance" => "github_api_observed",
             "coverage" => "bot_comment_and_review_submissions_only",
             "ignored_source_events" => ignored,
             "observations" => Enum.sort_by(observations, &get_in(&1, ["source", "event_id"]))
           }}

        {:error, _reason} = error ->
          error
      end
    else
      {:ok, _moved_head} ->
        {:error, :head_changed_during_collection}

      {:error, _reason} = error ->
        error
    end
  end

  defp inventory_agrees(left, right, _path) when left == right, do: :ok
  defp inventory_agrees(_left, _right, path), do: {:error, {:source_inventory_changed, path}}

  defp valid_repo?(repo) do
    Regex.match?(@repo, repo) and
      Enum.all?(String.split(repo, "/"), &(&1 not in [".", ".."]))
  end

  defp subject_head(
         %{
           "number" => actual_number,
           "head" => %{"sha" => head},
           "base" => %{"repo" => %{"full_name" => actual_repo}}
         },
         repo,
         number
       ) do
    cond do
      actual_number != number ->
        {:error, :pull_request_number_mismatch}

      not (is_binary(actual_repo) and String.downcase(actual_repo) == String.downcase(repo)) ->
        {:error, :pull_request_repository_mismatch}

      not (is_binary(head) and Regex.match?(@sha, head)) ->
        {:error, :missing_or_invalid_head}

      true ->
        {:ok, String.downcase(head)}
    end
  end

  defp subject_head(_unexpected, _repo, _number),
    do: {:error, :unexpected_pull_request_shape}

  defp fetch_object(api, path) do
    case api.(path) do
      {:ok, %{} = payload} -> {:ok, payload}
      {:ok, _unexpected} -> {:error, {:unexpected_api_shape, path}}
      {:error, _reason} = error -> error
      _ -> {:error, {:unexpected_api_result, path}}
    end
  end

  defp fetch_pages(api, path, budget) do
    with {:ok, records} <- fetch_page(api, path, 1, budget, []),
         :ok <- validate_event_ids(records, path) do
      {:ok, records}
    end
  end

  defp fetch_page(api, path, page, budget, acc) do
    endpoint = "#{path}?per_page=#{@page_size}&page=#{page}"

    case api.(endpoint) do
      {:ok, batch} when is_list(batch) and length(batch) <= @page_size ->
        updated = [batch | acc]

        cond do
          length(batch) < @page_size -> {:ok, updated |> Enum.reverse() |> List.flatten()}
          page >= budget -> {:error, {:page_budget_exhausted, path}}
          true -> fetch_page(api, path, page + 1, budget, updated)
        end

      {:ok, _unexpected} ->
        {:error, {:unexpected_api_shape, endpoint}}

      {:error, _reason} = error ->
        error

      _ ->
        {:error, {:unexpected_api_result, endpoint}}
    end
  end

  defp validate_event_ids(records, path) do
    ids =
      Enum.map(records, fn
        %{"id" => id} when is_integer(id) and id > 0 -> id
        _ -> nil
      end)

    if Enum.all?(ids, &is_integer/1) and length(Enum.uniq(ids)) == length(ids) do
      :ok
    else
      {:error, {:invalid_or_duplicate_event_ids, path}}
    end
  end

  defp from_comment(%{} = comment, repo, number, head, reviews) do
    with {:ok, provider} <- known_bot(comment),
         true <- is_nil(comment["in_reply_to_id"]),
         {:ok, scope} <- comment_scope(provider, comment["body"]) do
      with {:ok, id} <- positive_id(comment["id"]),
           {:ok, observation} <-
             ReviewObservation.admit(
               %{
                 "provider" => provider,
                 "scope" => scope,
                 "repo" => repo,
                 "pr_number" => number,
                 # The current attachment may move on rebase; the original
                 # reviewed commit does not. Never fall back to commit_id.
                 "reviewed_sha" => comment["original_commit_id"],
                 "source_event_id" => "github:review_comment:#{id}",
                 "source_url" => comment["html_url"],
                 "status" => comment_review_status(comment, reviews),
                 "has_findings" => true
               },
               head
             ) do
        {:ok, Map.put(observation, "provider_run_status", "unknown")}
      else
        other -> {:error, {:malformed_recognised_finding, other}}
      end
    else
      _ -> {:ignore, :not_a_verified_root_finding_comment}
    end
  end

  defp from_comment(_other, _repo, _number, _head, _reviews),
    do: {:ignore, :invalid_comment}

  defp comment_review_status(comment, reviews) do
    review_id = comment["pull_request_review_id"]

    if Enum.any?(reviews, fn
         %{"id" => ^review_id, "state" => "DISMISSED"} when is_integer(review_id) -> true
         _ -> false
       end) do
      "cancelled"
    else
      # A published finding exists, but it does not certify that the
      # provider's wider review run completed successfully.
      "completed"
    end
  end

  defp from_review(%{} = review, repo, number, head, comments) do
    with {:ok, provider} <- known_bot(review),
         {:ok, scope} <- review_scope(review, comments, provider) do
      with {:ok, id} <- positive_id(review["id"]),
           {:ok, status} <- review_status(review["state"]),
           {:ok, observation} <-
             ReviewObservation.admit(
               %{
                 "provider" => provider,
                 "scope" => scope,
                 "repo" => repo,
                 "pr_number" => number,
                 "reviewed_sha" => review["commit_id"],
                 "source_event_id" => "github:review:#{id}",
                 "source_url" => review["html_url"],
                 "status" => status,
                 # Submission status is not a completed provider-run verdict.
                 "has_findings" => nil
               },
               head
             ) do
        {:ok, Map.put(observation, "provider_run_status", "unknown")}
      else
        other -> {:error, {:malformed_recognised_review, other}}
      end
    else
      _ -> {:ignore, :unknown_review_scope}
    end
  end

  defp from_review(_other, _repo, _number, _head, _comments),
    do: {:ignore, :invalid_review}

  # Without a provider-authored finding linked by native GitHub review ID,
  # the review submission has no reliable code/security scope. Do not
  # assign "code" to unknown security reviews or quota-only submissions.
  defp review_scope(review, comments, provider) do
    scopes =
      for comment <- comments,
          comment["pull_request_review_id"] == review["id"],
          {:ok, ^provider} <- [known_bot(comment)],
          is_nil(comment["in_reply_to_id"]),
          {:ok, scope} <- [comment_scope(provider, comment["body"])],
          do: scope

    case Enum.uniq(scopes) do
      [scope] -> {:ok, scope}
      _ -> {:ignore, :unknown_or_mixed_review_scope}
    end
  end

  defp known_bot(%{"user" => %{"id" => id, "login" => login, "type" => "Bot"}}) do
    case Map.get(@bot_accounts, id) do
      {provider, ^login} -> {:ok, provider}
      _ -> {:ignore, :unrecognised_bot_identity}
    end
  end

  defp known_bot(_), do: {:ignore, :missing_bot_identity}

  defp comment_scope("codex", body) when is_binary(body) do
    cond do
      String.contains?(body, "<!-- codex-security-review-finding:v1 -->") ->
        {:ok, "security"}

      String.contains?(body, "<!-- codex-code-review-finding:v1 -->") ->
        {:ok, "code"}

      true ->
        {:ignore, :unrecognised_codex_comment}
    end
  end

  defp comment_scope("coderabbit", body) when is_binary(body) do
    if String.contains?(body, "<!-- cr-comment:v1:") do
      {:ok, "code"}
    else
      {:ignore, :unrecognised_coderabbit_comment}
    end
  end

  defp comment_scope(_provider, _body), do: {:ignore, :missing_finding_marker}

  defp review_status("PENDING"), do: {:ok, "running"}
  defp review_status("DISMISSED"), do: {:ok, "cancelled"}

  # GitHub submission state is not provider execution status. In particular,
  # COMMENTED may mean quota exhaustion and APPROVED is not a Hypatia verdict.
  defp review_status(state) when state in ~w(COMMENTED APPROVED CHANGES_REQUESTED),
    do: {:ok, "unavailable"}

  defp review_status(_state), do: {:ignore, :unknown_review_state}

  defp positive_id(id) when is_integer(id) and id > 0, do: {:ok, id}
  defp positive_id(_), do: {:error, :invalid_source_event_id}

  @doc """
  Fetch one endpoint through an existing authenticated gh session.

  Uses *only GET* and an argument vector (no shell), never logs raw failures,
  credentials or response bodies. This decodes GitHub's API payload; it does
  not claim strict I-JSON/JCS canonicalisation of output.
  """
  @api_timeout_ms 20_000

  def gh_get(path) when is_binary(path) do
    # System.cmd itself has no timeout. A linked task owns the port; shutting
    # it down bounds a stalled read without logging untrusted response text.
    task = Task.async(fn -> gh_command(path) end)

    case Task.yield(task, @api_timeout_ms) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      {:exit, _reason} -> {:error, :github_api_unavailable}
      nil -> {:error, :github_api_timeout}
    end
  end

  def gh_get(_), do: {:error, :invalid_api_endpoint}

  defp gh_command(path) do
    try do
      case System.cmd(
             "gh",
             ["api", "--hostname", "github.com", "--method", "GET", path],
             env: [{"GH_PROMPT_DISABLED", "1"}],
             stderr_to_stdout: true
           ) do
        {body, 0} when byte_size(body) <= @max_response_bytes ->
          case Jason.decode(body) do
            {:ok, value} -> {:ok, value}
            {:error, _} -> {:error, :invalid_github_api_json}
          end

        {_body, 0} ->
          {:error, :github_api_response_too_large}

        {_body, _status} ->
          {:error, :github_api_unavailable}
      end
    rescue
      _ -> {:error, :github_cli_unavailable}
    catch
      :exit, _ -> {:error, :github_api_unavailable}
    end
  end
end
