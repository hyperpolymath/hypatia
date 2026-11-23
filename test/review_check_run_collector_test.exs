# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.ReviewCheckRunCollectorTest do
  use ExUnit.Case, async: true

  alias Hypatia.ReviewCheckRunCollector, as: Collector

  @repo "stuff-of-awe/awesome-zig"
  @number 20
  @sha String.duplicate("a", 40)
  @other String.duplicate("b", 40)
  @path "repos/stuff-of-awe/awesome-zig/pulls/20"
  @checks "repos/stuff-of-awe/awesome-zig/commits/#{@sha}/check-runs"
  @first_page @checks <> "?per_page=100&page=1&filter=all"
  @second_page @checks <> "?per_page=100&page=2&filter=all"

  defp pr do
    %{
      "number" => @number,
      "head" => %{"sha" => @sha},
      "base" => %{"repo" => %{"full_name" => @repo}}
    }
  end

  defp check(overrides \\ %{}) do
    Map.merge(
      %{
        "id" => 101,
        "name" => "pre-commit.ci - pr",
        "app" => %{"id" => 68672, "slug" => "pre-commit-ci"},
        "head_sha" => @sha,
        "status" => "completed",
        "conclusion" => "success",
        "html_url" => "https://results.pre-commit.ci/run/github/irrelevant"
      },
      overrides
    )
  end

  defp response(runs \\ []) do
    %{
      @path => {:ok, pr()},
      @first_page => {:ok, %{"total_count" => length(runs), "check_runs" => runs}}
    }
  end

  defp api(responses) do
    fn endpoint -> Map.get(responses, endpoint, {:error, :unexpected_endpoint}) end
  end

  test "missing pre-commit.ci is not a pass even when all other checks succeeded" do
    generic = check(%{"app" => %{"id" => 99, "slug" => "github-actions"}})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: api(response([generic])))

    assert snapshot["provider_observed"] == false
    assert snapshot["matching_checks"] == 0
    assert snapshot["observations"] == []
    assert snapshot["coverage_verdict"] == "not_established"
  end

  test "a native successful pre-commit.ci check remains an unverified assertion" do
    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: api(response([check()])))

    assert snapshot["provider_observed"]
    assert snapshot["matching_checks"] == 1
    assert [observation] = snapshot["observations"]
    assert observation["source"]["provider"] == "pre-commit.ci"
    assert observation["source"]["scope"] == "hooks"
    assert observation["source"]["event_id"] == "github:check_run:101"
    assert observation["source"]["url"] == "https://github.com/#{@repo}/pull/#{@number}"
    assert observation["review_status"] == "completed"
    assert observation["check_conclusion"] == "success"
    assert observation["finding_signal"] == "unknown"
    assert observation["assessment"] == "finding_summary_missing"
    assert observation["qualification"] == "unqualified"
    assert observation["learning_eligible"] == false
    assert observation["merge_approval"] == false
    assert snapshot["coverage_verdict"] == "not_established"
  end

  test "failed and skipped checks do not turn into independent findings" do
    runs = [
      check(%{"id" => 102, "conclusion" => "failure"}),
      check(%{"id" => 103, "conclusion" => "skipped"})
    ]

    assert {:ok, snapshot} = Collector.collect(@repo, @number, api: api(response(runs)))
    assert length(snapshot["observations"]) == 2

    assert Enum.map(snapshot["observations"], & &1["check_conclusion"]) == [
             "failure",
             "skipped"
           ]

    assert Enum.all?(snapshot["observations"], fn item ->
             item["finding_signal"] == "unknown" and
               item["learning_eligible"] == false and item["merge_approval"] == false
           end)
  end

  test "pending check has unknown conclusion and cannot declare clean" do
    waiting = check(%{"status" => "in_progress", "conclusion" => nil})

    assert {:ok, snapshot} =
             Collector.collect(@repo, @number, api: api(response([waiting])))

    assert [item] = snapshot["observations"]
    assert item["review_status"] == "running"
    assert item["check_conclusion"] == "unknown"
    assert item["assessment"] == "review_pending"
  end

  test "forged check name, slug or numeric app identifier never authenticates" do
    spoofed = [
      check(%{"app" => %{"id" => 42, "slug" => "pre-commit-ci"}}),
      check(%{"app" => %{"id" => 68672, "slug" => "other-ci"}}),
      check(%{"name" => "pre-commit.ci - push"}),
      check(%{"name" => "pre-commit.ci - pr ", "id" => 103})
    ]

    spoofed =
      spoofed
      |> Enum.with_index(201)
      |> Enum.map(fn {run, id} -> Map.put(run, "id", id) end)

    assert {:ok, snapshot} = Collector.collect(@repo, @number, api: api(response(spoofed)))
    assert snapshot["observations"] == []
    refute snapshot["provider_observed"]
  end

  test "positively identified check with invalid state or SHA fails the inventory" do
    for invalid <- [
          check(%{"head_sha" => @other}),
          check(%{"status" => "completed", "conclusion" => nil}),
          check(%{"status" => "in_progress", "conclusion" => "success"}),
          check(%{"status" => "completed", "conclusion" => "some_future_state"}),
          check(%{"id" => nil})
        ] do
      assert {:error, {:invalid_provider_check, _}} =
               Collector.collect(@repo, @number, api: api(response([invalid])))
    end
  end

  test "empty or incomplete pagination never yields clean coverage" do
    assert {:ok, empty} = Collector.collect(@repo, @number, api: api(response()))
    refute empty["provider_observed"]

    incomplete = Map.put(response(), @first_page, {:ok, %{"total_count" => 2, "check_runs" => []}})

    assert {:error, {:incomplete_check_inventory, _}} =
             Collector.collect(@repo, @number, api: api(incomplete))
  end

  test "duplicate or malformed source ids fail closed" do
    for malformed <- [
          [check(%{"id" => 101}), check(%{"id" => 101})],
          [check(%{"id" => nil})],
          [%{"name" => "Unrelated check"}]
        ] do
      assert {:error, {:invalid_or_duplicate_check_ids, _}} =
               Collector.collect(@repo, @number, api: api(response(malformed)))
    end
  end

  test "page budget exhaustion fails, even if the last page was full" do
    full = Enum.map(1..100, fn id -> check(%{"id" => id}) end)
    responses = Map.put(response(), @first_page, {:ok, %{"total_count" => 100, "check_runs" => full}})

    assert {:error, {:page_budget_exhausted, _}} =
             Collector.collect(@repo, @number, api: api(responses), page_budget: 1)

    responses =
      responses
      |> Map.put(@second_page, {:ok, %{"total_count" => 100, "check_runs" => []}})

    assert {:ok, snapshot} = Collector.collect(@repo, @number, api: api(responses), page_budget: 2)
    assert length(snapshot["observations"]) == 100
  end

  test "changed inventory between reads is rejected despite constant PR head" do
    {:ok, counter} = Agent.start_link(fn -> 0 end)

    varying = fn endpoint ->
      if endpoint == @path do
        {:ok, pr()}
      else
        count = Agent.get_and_update(counter, fn index -> {index, index + 1} end)
        {:ok, %{"total_count" => 1, "check_runs" => [check(%{"id" => 101 + count})]}}
      end
    end

    assert {:error, :check_inventory_changed} =
             Collector.collect(@repo, @number, api: varying)
  end

  test "moved PR head, bad PR shape, API error or malformed checks refuse collection" do
    {:ok, counter} = Agent.start_link(fn -> 0 end)

    moved = fn endpoint ->
      if endpoint == @path do
        count = Agent.get_and_update(counter, fn index -> {index, index + 1} end)
        if count == 0, do: {:ok, pr()}, else: {:ok, put_in(pr(), ["head", "sha"], @other)}
      else
        {:ok, %{"total_count" => 0, "check_runs" => []}}
      end
    end

    assert {:error, :head_changed_during_collection} =
             Collector.collect(@repo, @number, api: moved)

    assert {:error, :invalid_subject_shape} =
             Collector.collect(@repo, @number, api: api(Map.put(response(), @path, {:ok, %{"head" => []}})))

    assert {:error, :network_failure} =
             Collector.collect(@repo, @number, api: api(Map.put(response(), @path, {:error, :network_failure})))

    assert {:error, {:invalid_check_runs_shape, _}} =
             Collector.collect(
               @repo,
               @number,
               api: api(Map.put(response(), @first_page, {:ok, %{"check_runs" => []}}))
             )
  end

  test "subject validation rejects path tricks and invalid limits" do
    for invalid <- ["../repo", "owner/..", "owner//repo"] do
      assert {:error, :invalid_repository} = Collector.collect(invalid, @number, api: api(response()))
    end

    assert {:error, :invalid_pull_request} = Collector.collect(@repo, -1, api: api(response()))
    assert {:error, :invalid_page_budget} =
             Collector.collect(@repo, @number, api: api(response()), page_budget: 0)
  end
end
