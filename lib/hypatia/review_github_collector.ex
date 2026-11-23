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
      not valid_repo?(repo) -> {:error, :invalid_repository}
      not is_function(api, 1) -> {:error, :invalid_api_client}
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

    with {:ok, start_pr} <- fetch_object(api, pr_path),
         {:ok, head} <- subject_head(start_pr, repo, number),
         {:ok, comments} <- fetch_pages(api, pr_path <> "/comments", budget),
         {:ok, reviews} <- fetch_pages(api, pr_path <> "/reviews", budget),
         {:ok, end_pr} <- fetch_object(api, pr_path),
         {:ok, ^head} <- subject_head(end_pr, repo, number) do
      {observations, ignored} =
        (Enum.map(comments, &from_comment(&1, repo, number, head)) ++
           Enum.map(reviews, &from_review(&1, repo, number, head)))
        |> Enum.reduce({[], 0}, fn
          {:ok, observation}, {found, skipped} -> {[observation | found], skipped}
          {:ignore, _reason}, {found, skipped} -> {found, skipped + 1}
        end)

      {:ok,
       %{
         "repository" => repo,
         "pull_request" => number,
         "head_sha" => head,
         "collection" => "complete_bounded_snapshot",
         "source" => "github_rest_via_gh",
         "transport_provenance" => "github_api_observed",
         "coverage" => "bot_comment_and_review_submissions_only",
         "ignored_source_events" => ignored,
         "observations" =>
           Enum.sort_by(observations, &get_in(&1, ["source", "event_id"]))
       }}
    else
      {:ok, _moved_head} ->
        {:error, :head_changed_during_collection}

      {:error, _reason} = error ->
        error
    end
  end

  defp valid_repo?(repo) do
    Regex.match?(@repo, repo) and
      Enum.all?(String.split(repo, "/"), &(&1 not in [".", ".."]))
  end

  defp subject_head(pr, repo, number) do
    head = get_in(pr, ["head", "sha"])
    actual_repo = get_in(pr, ["base", "repo", "full_name"])

    cond do
      pr["number"] != number -> {:error, :pull_request_number_mismatch}
      not (is_binary(actual_repo) and String.downcase(actual_repo) == String.downcase(repo)) ->
        {:error, :pull_request_repository_mismatch}
      not (is_binary(head) and Regex.match?(@sha, head)) ->
        {:error, :missing_or_invalid_head}
      true -> {:ok, String.downcase(head)}
    end
  end

  defp fetch_object(api, path) do
    case api.(path) do
      {:ok, %{} = payload} -> {:ok, payload}
      {:ok, _unexpected} -> {:error, {:unexpected_api_shape, path}}
      {:error, _reason} = error -> error
      _ -> {:error, {:unexpected_api_result, path}}
    end
  end

  defp fetch_pages(api, path, budget), do: fetch_page(api, path, 1, budget, [])

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

  defp from_comment(%{} = comment, repo, number, head) do
    with {:ok, provider} <- known_bot(comment),
         true <- is_nil(comment["in_reply_to_id"]),
         {:ok, scope} <- comment_scope(provider, comment["body"]),
         {:ok, id} <- positive_id(comment["id"]) do
      ReviewObservation.admit(
        %{
          "provider" => provider,
          "scope" => scope,
          "repo" => repo,
          "pr_number" => number,
          "reviewed_sha" => comment["commit_id"],
          "source_event_id" => "github:review_comment:#{id}",
          "source_url" => comment["html_url"],
          "status" => "completed",
          "has_findings" => true
        },
        head
      )
      |> admit_or_ignore()
    else
      _ -> {:ignore, :not_a_verified_root_finding_comment}
    end
  end

  defp from_comment(_other, _repo, _number, _head),
    do: {:ignore, :invalid_comment}

  defp from_review(%{} = review, repo, number, head) do
    with {:ok, provider} <- known_bot(review),
         {:ok, id} <- positive_id(review["id"]),
         {:ok, status} <- review_status(review["state"]) do
      ReviewObservation.admit(
        %{
          "provider" => provider,
          "scope" => "code",
          "repo" => repo,
          "pr_number" => number,
          "reviewed_sha" => review["commit_id"],
          "source_event_id" => "github:review:#{id}",
          "source_url" => review["html_url"],
          "status" => status,
          # GitHub review submission itself does not attest to zero findings.
          "has_findings" => nil
        },
        head
      )
      |> admit_or_ignore()
    else
      _ -> {:ignore, :not_a_verified_review_submission}
    end
  end

  defp from_review(_other, _repo, _number, _head), do: {:ignore, :invalid_review}

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
  defp review_status(state) when state in ~w(COMMENTED APPROVED CHANGES_REQUESTED DISMISSED),
    do: {:ok, "completed"}
  defp review_status(_state), do: {:ignore, :unknown_review_state}

  defp positive_id(id) when is_integer(id) and id > 0, do: {:ok, id}
  defp positive_id(_), do: {:ignore, :invalid_source_event_id}

  defp admit_or_ignore({:ok, observation}), do: {:ok, observation}
  defp admit_or_ignore({:error, reason}), do: {:ignore, {:invalid_source_record, reason}}

  @doc """
  Fetch one endpoint through an existing authenticated gh session.

  Uses *only GET* and an argument vector (no shell), never logs raw failures,
  credentials or response bodies. This decodes GitHub's API payload; it does
  not claim strict I-JSON/JCS canonicalisation of output.
  """
  def gh_get(path) when is_binary(path) do
    try do
      case System.cmd(
             "gh",
             ["api", "--method", "GET", path],
             env: [{"GH_PROMPT_DISABLED", "1"}],
             stderr_to_stdout: false
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
      ErlangError -> {:error, :github_cli_unavailable}
    end
  end

  def gh_get(_), do: {:error, :invalid_api_endpoint}
end
