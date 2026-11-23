# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.ReviewGithubCollectorTest do
  use ExUnit.Case, async: true

  alias Hypatia.ReviewGithubCollector, as: Collector

  @sha String.duplicate("a", 40)
  @other String.duplicate("b", 40)
  @repo "stuff-of-awe/awesome-zig"
  @number 20
  @path "repos/stuff-of-awe/awesome-zig/pulls/20"
  @codex %{"id" => 199_175_422, "login" => "chatgpt-codex-connector[bot]", "type" => "Bot"}
  @rabbit %{"id" => 136_622_811, "login" => "coderabbitai[bot]", "type" => "Bot"}

  defp pr(sha \\ @sha) do
    %{
      "number" => @number,
      "head" => %{"sha" => sha},
      "base" => %{"repo" => %{"full_name" => @repo}}
    }
  end

  defp comment(overrides \\ %{}) do
    Map.merge(
      %{
        "id" => 301,
        "user" => @codex,
        "commit_id" => @sha,
        "html_url" => "https://github.com/stuff-of-awe/awesome-zig/pull/20#discussion_r301",
        "body" => "Potential bad lock\n<!-- codex-code-review-finding:v1 -->"
      },
      overrides
    )
  end

  defp review(overrides \\ %{}) do
    Map.merge(
      %{
        "id" => 501,
        "user" => @codex,
        "commit_id" => @sha,
        "html_url" => "https://github.com/stuff-of-awe/awesome-zig/pull/20#pullrequestreview-501",
        "state" => "COMMENTED",
        "body" => "No issues"
      },
      overrides
    )
  end

  defp api_responses(comments \\ [], reviews \\ []) do
    %{@path => {:ok, pr()}}
    |> Map.put(@path <> "/comments?per_page=100&page=1", {:ok, comments})
    |> Map.put(@path <> "/reviews?per_page=100&page=1", {:ok, reviews})
  end

  defp fake_api(responses) do
    fn endpoint ->
      Map.get(responses, endpoint, {:error, {:unprovided_endpoint, endpoint}})
    end
  end

  test "admits a real bot's inline finding without elevating trust" do
    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: fake_api(api_responses([comment()])))

    assert snapshot["collection"] == "complete_bounded_snapshot"
    assert snapshot["head_sha"] == @sha
    assert snapshot["ignored_source_events"] == 0
    assert [item] = snapshot["observations"]
    assert item["source"]["provider"] == "codex"
    assert item["source"]["scope"] == "code"
    assert item["source"]["event_id"] == "github:review_comment:301"
    assert item["assessment"] == "reported_findings"
    assert item["authenticity"] == "unverified"
    assert item["qualification"] == "unqualified"
    refute item["merge_approval"]
    refute item["learning_eligible"]
    # No model text, prompt or executable patch leaves the collector.
    refute inspect(snapshot) =~ "Potential bad lock"
  end

  test "distinguishes security from code scope using the known bot's marker" do
    security = comment(%{"body" => "<!-- codex-security-review-finding:v1 -->"})
    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: fake_api(api_responses([security])))

    assert [item] = snapshot["observations"]
    assert item["source"]["scope"] == "security"
  end

  test "recognises CodeRabbit only after matching GitHub's numeric bot identity" do
    rabbit = comment(%{
      "user" => @rabbit,
      "id" => 302,
      "body" => "Suggested patch: delete checks <!-- cr-comment:v1:deadbeef -->"
    })

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: fake_api(api_responses([rabbit])))

    assert [item] = snapshot["observations"]
    assert item["source"]["provider"] == "coderabbit"
    refute inspect(item) =~ "delete checks"
  end

  test "a spoofed login or forged marker from a human is not authenticated" do
    impostor = comment(%{
      "user" => %{"id" => 999, "login" => "chatgpt-codex-connector[bot]", "type" => "Bot"}
    })

    human = comment(%{
      "id" => 303,
      "user" => %{"id" => 42, "login" => "human", "type" => "User"}
    })

    wrong_type = comment(%{
      "id" => 304,
      "user" => Map.put(@codex, "type", "User")
    })

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([impostor, human, wrong_type]))
             )

    assert snapshot["observations"] == []
    assert snapshot["ignored_source_events"] == 3
  end

  test "unmarked bot conversation and follow-up replies are not findings" do
    unmarked = comment(%{"body" => "LGTM"})
    reply = comment(%{"id" => 309, "in_reply_to_id" => 301})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([unmarked, reply]))
             )

    assert snapshot["observations"] == []
    assert snapshot["ignored_source_events"] == 2
  end

  test "an API review labelled APPROVED never becomes a merge approval" do
    submitted = review(%{"state" => "APPROVED"})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([], [submitted]))
             )

    assert [item] = snapshot["observations"]
    assert item["source"]["event_id"] == "github:review:501"
    assert item["assessment"] == "finding_summary_missing"
    assert item["finding_signal"] == "unknown"
    refute item["merge_approval"]
    refute item["learning_eligible"]
  end

  test "a quiet reviewer, or no events, is never a verified clean result" do
    assert {:ok, empty} =
             Collector.collect(@repo, @number, api: fake_api(api_responses()))

    assert empty["observations"] == []
    assert empty["coverage"] == "bot_comment_and_review_submissions_only"

    assert {:ok, with_review} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([], [review()]))
             )

    assert [item] = with_review["observations"]
    assert item["assessment"] == "finding_summary_missing"
    assert item["finding_signal"] == "unknown"
  end

  test "a review of the old head remains stale even when the API has not moved" do
    stale = comment(%{"commit_id" => @other})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: fake_api(api_responses([stale])))

    assert [item] = snapshot["observations"]
    assert item["assessment"] == "stale_head"
    assert item["subject"]["reviewed_sha"] == @other
  end

  test "a changing PR head invalidates the entire snapshot" do
    {:ok, counter} = Agent.start_link(fn -> 0 end)

    api = fn endpoint ->
      cond do
        endpoint == @path ->
          calls = Agent.get_and_update(counter, fn count -> {count, count + 1} end)
          {:ok, pr(if(calls == 0, do: @sha, else: @other))}

        endpoint == @path <> "/comments?per_page=100&page=1" ->
          {:ok, [comment()]}

        endpoint == @path <> "/reviews?per_page=100&page=1" ->
          {:ok, []}

        true ->
          {:error, :unexpected_endpoint}
      end
    end

    assert {:error, :head_changed_during_collection} =
             Collector.collect(@repo, @number, api: api)
  end

  test "pagination refuses to return partial findings when its budget is exhausted" do
    # A full last page means there may be more data: the result is UNKNOWN.
    full_page = List.duplicate(comment(), 100)

    api = fake_api(
      api_responses()
      |> Map.put(@path <> "/comments?per_page=100&page=1", {:ok, full_page})
    )

    assert {:error, {:page_budget_exhausted, path}} =
             Collector.collect(@repo, @number, api: api, page_budget: 1)

    assert path == @path <> "/comments"
  end

  test "pagination reads a second page and does not invent a missing event" do
    full_page = List.duplicate(%{"id" => nil}, 100)

    responses =
      api_responses()
      |> Map.put(@path <> "/comments?per_page=100&page=1", {:ok, full_page})
      |> Map.put(@path <> "/comments?per_page=100&page=2", {:ok, [comment()]})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: fake_api(responses), page_budget: 2)

    assert length(snapshot["observations"]) == 1
    assert snapshot["ignored_source_events"] == 100
  end

  test "unavailable API, malformed JSON-shaped response and wrong PR identity fail closed" do
    unavailable =
      Map.put(api_responses(), @path <> "/reviews?per_page=100&page=1", {:error, :rate_limited})

    assert {:error, :rate_limited} =
             Collector.collect(@repo, @number, api: fake_api(unavailable))

    bad_list = Map.put(api_responses(), @path <> "/comments?per_page=100&page=1", {:ok, %{}})

    assert {:error, {:unexpected_api_shape, _}} =
             Collector.collect(@repo, @number, api: fake_api(bad_list))

    wrong_repo =
      Map.put(
        api_responses(),
        @path,
        {:ok, put_in(pr(), ["base", "repo", "full_name"], "elsewhere/other")}
      )

    assert {:error, :pull_request_repository_mismatch} =
             Collector.collect(@repo, @number, api: fake_api(wrong_repo))
  end

  test "invalid subject and budgets are refused before any API operation" do
    bad_api = fn _endpoint -> flunk("API was called for invalid input") end

    assert {:error, :invalid_repository} =
             Collector.collect("../escape", 20, api: bad_api)

    assert {:error, :invalid_page_budget} =
             Collector.collect(@repo, 20, api: bad_api, page_budget: 0)

    assert {:error, :invalid_subject} =
             Collector.collect(@repo, -1, api: bad_api)
  end
end
