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
        "original_commit_id" => @sha,
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

    assert snapshot["collection"] == "repeated_stable_observation"
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

  test "a linked APPROVED review never becomes a merge approval" do
    submitted = review(%{"state" => "APPROVED"})
    linked = comment(%{"pull_request_review_id" => 501})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([linked], [submitted]))
             )

    assert length(snapshot["observations"]) == 2

    item =
      Enum.find(snapshot["observations"], fn observation ->
        observation["source"]["event_id"] == "github:review:501"
      end)

    assert item["source"]["scope"] == "code"
    assert item["assessment"] == "review_unavailable"
    assert item["finding_signal"] == "unknown"
    assert item["provider_run_status"] == "unknown"
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

    assert with_review["observations"] == []
    assert with_review["ignored_source_events"] == 1
  end

  test "a review of the old head remains stale even when the API has not moved" do
    stale = comment(%{"commit_id" => @sha, "original_commit_id" => @other})

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
    full_page = Enum.map(1..100, fn id -> %{"id" => id} end)

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
  test "a finding retains original reviewed commit rather than rebound attachment" do
    moved = comment(%{"original_commit_id" => @other, "commit_id" => @sha})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: fake_api(api_responses([moved])))

    assert [item] = snapshot["observations"]
    assert item["assessment"] == "stale_head"
    assert item["subject"]["reviewed_sha"] == @other

    current = comment(%{"original_commit_id" => @sha, "commit_id" => @other})

    assert {:ok, current_snapshot} =
             Collector.collect(@repo, @number, api: fake_api(api_responses([current])))

    assert [current_item] = current_snapshot["observations"]
    assert current_item["assessment"] == "reported_findings"
  end

  test "malformed recognised bot findings fail the entire collection" do
    for broken <- [
          comment(%{"original_commit_id" => nil}),
          comment(%{"original_commit_id" => "not-a-sha"}),
          comment(%{"html_url" => "https://github.com/stuff-of-awe/other/pull/20"}),
          comment(%{"id" => nil})
        ] do
      assert {:error, {:invalid_source_event, _reason}} =
               Collector.collect(@repo, @number,
                 api: fake_api(api_responses([broken]))
               )
    end
  end

  test "malformed nested PR metadata is a structured error, not a crash" do
    invalid_subjects = [
      %{"number" => 20, "head" => "invalid", "base" => %{}},
      %{"number" => 20, "head" => %{"sha" => @sha}, "base" => %{"repo" => "bad"}},
      %{"number" => 20, "head" => [], "base" => %{}}
    ]

    for pr_subject <- invalid_subjects do
      responses = Map.put(api_responses(), @path, {:ok, pr_subject})

      assert {:error, :unexpected_pull_request_shape} =
               Collector.collect(@repo, @number, api: fake_api(responses))
    end
  end

  test "COMMENTED quota submissions are not counted as completed code reviews" do
    linked = comment(%{"pull_request_review_id" => 501})
    quota = review(%{"state" => "COMMENTED", "body" => "Quota exhausted"})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([linked], [quota]))
             )

    review_item =
      Enum.find(snapshot["observations"], fn observation ->
        observation["source"]["event_id"] == "github:review:501"
      end)

    assert review_item["review_status"] == "unavailable"
    assert review_item["assessment"] == "review_unavailable"
    assert "completed_review" in review_item["missing_evidence"]
    refute review_item["merge_approval"]
  end

  test "dismissed review submissions and linked findings are withdrawn" do
    linked = comment(%{"pull_request_review_id" => 501})
    withdrawn = review(%{"state" => "DISMISSED"})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([linked], [withdrawn]))
             )

    assert length(snapshot["observations"]) == 2
    assert Enum.all?(snapshot["observations"], &(&1["review_status"] == "cancelled"))
    assert Enum.all?(snapshot["observations"], &(&1["finding_signal"] == "unknown"))
  end

  test "a security review is never re-labelled as a code review" do
    security = comment(%{
      "pull_request_review_id" => 501,
      "body" => "<!-- codex-security-review-finding:v1 -->"
    })

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([security], [review()]))
             )

    assert length(snapshot["observations"]) == 2
    assert Enum.all?(snapshot["observations"], fn item ->
             item["source"]["scope"] == "security"
           end)
  end

  test "reviews with mixed linked scopes are not invented as one scope" do
    security = comment(%{
      "id" => 303,
      "pull_request_review_id" => 501,
      "body" => "<!-- codex-security-review-finding:v1 -->"
    })

    code = comment(%{"id" => 302, "pull_request_review_id" => 501})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number,
               api: fake_api(api_responses([code, security], [review()]))
             )

    assert length(snapshot["observations"]) == 2
    refute Enum.any?(snapshot["observations"], fn item ->
             item["source"]["event_id"] == "github:review:501"
           end)
  end

  test "changed comment inventory is not labelled complete despite a stable head" do
    {:ok, counter} = Agent.start_link(fn -> 0 end)
    first_page = @path <> "/comments?per_page=100&page=1"
    second_page = @path <> "/comments?per_page=100&page=2"
    reviews_page = @path <> "/reviews?per_page=100&page=1"

    api = fn endpoint ->
      cond do
        endpoint == @path ->
          {:ok, pr()}

        endpoint == first_page ->
          reads = Agent.get_and_update(counter, fn count -> {count, count + 1} end)
          start_id = if reads == 0, do: 1, else: 2
          {:ok, Enum.map(start_id..(start_id + 99), fn id -> %{"id" => id} end)}

        endpoint == second_page ->
          {:ok, []}

        endpoint == reviews_page ->
          {:ok, []}

        true ->
          {:error, :unexpected_endpoint}
      end
    end

    assert {:error, {:source_inventory_changed, path}} =
             Collector.collect(@repo, @number, api: api, page_budget: 2)

    assert path == @path <> "/comments"
  end

  test "duplicate or missing API event identities invalidate the inventory" do
    for entries <- [[%{"id" => 10}, %{"id" => 10}], [%{"body" => "no id"}]] do
      assert {:error, {:invalid_or_duplicate_event_ids, path}} =
               Collector.collect(@repo, @number,
                 api: fake_api(api_responses(entries))
               )

      assert path == @path <> "/comments"
    end
  end

end
