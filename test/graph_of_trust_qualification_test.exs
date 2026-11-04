# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

defmodule Hypatia.Neural.GraphOfTrustQualificationTest do
  use ExUnit.Case, async: true

  alias Hypatia.Neural.GraphOfTrust
  alias Hypatia.OutcomeTracker

  @recipes [
    %{"id" => "recipe-shared-fixture"},
    %{"id" => "recipe-a-only-fixture"},
    %{"id" => "recipe-b-only-fixture"}
  ]

  defp observation(repo, recipe, outcome, verification) do
    %{
      "repo" => repo,
      "bot" => "bot-qualification-fixture",
      "recipe_id" => recipe,
      "outcome" => outcome,
      "verification" => verification
    }
  end

  defp verified(repo, recipe \\ "recipe-shared-fixture") do
    observation(repo, recipe, "success", "verified")
  end

  defp graph(rows), do: GraphOfTrust.build(rows, @recipes)
  defp without_time(graph), do: %{graph | last_computed: nil}

  test "verified shared repairs create bidirectional positive cross-repo edges" do
    graph = graph([verified("repo-a-fixture"), verified("repo-b-fixture")])

    assert MapSet.new(graph.cross_repo_edges) ==
             MapSet.new([
               {"repo-a-fixture", "repo-b-fixture", 1.0},
               {"repo-b-fixture", "repo-a-fixture", 1.0}
             ])

    assert {"recipe-shared-fixture", "repo-a-fixture", 1.0} in graph.edges
    assert map_size(graph.trust_scores) > 0
    assert Enum.all?(graph.trust_scores, fn {_id, score} -> score >= 0.0 and score <= 1.0 end)
  end

  test "unqualified and contradictory successes cannot create repository trust relationships" do
    rows =
      for repo <- ["repo-a-fixture", "repo-b-fixture"],
          verification <- [
            "unverified",
            "scan_failed",
            "scan_skipped",
            "still_present",
            "incomplete",
            nil
          ] do
        observation(repo, "recipe-shared-fixture", "success", verification)
      end

    missing_verification = verified("repo-a-fixture") |> Map.delete("verification")
    graph = graph([missing_verification | rows])

    assert graph.cross_repo_edges == []
    assert graph.edges == []
    refute Map.has_key?(graph.nodes, "repo-a-fixture")
    refute Map.has_key?(graph.nodes, "repo-b-fixture")
    assert GraphOfTrust.trusted_bots(graph) == []
  end

  test "ignored observations cannot alter a qualified graph through nodes or direct edges" do
    qualified = [verified("repo-a-fixture")]
    baseline = qualified |> graph() |> without_time()

    ignored = [
      verified("repo-b-fixture") |> Map.put("verification", "unverified"),
      verified("repo-c-fixture") |> Map.delete("verification"),
      nil,
      [],
      42,
      "not-an-outcome-object"
    ]

    assert qualified |> Kernel.++(ignored) |> graph() |> without_time() == baseline
  end

  test "qualified failures and false positives retain the existing non-positive-edge policy" do
    negatives = [
      observation("repo-b-fixture", "recipe-shared-fixture", "failure", "failed"),
      observation("repo-b-fixture", "recipe-shared-fixture", "failure", "incomplete"),
      observation("repo-b-fixture", "recipe-shared-fixture", "failure", "verified"),
      observation("repo-b-fixture", "recipe-shared-fixture", "false_positive", "still_present"),
      observation("repo-b-fixture", "recipe-shared-fixture", "false_positive", "verified"),
      observation(
        "repo-b-fixture",
        "recipe-shared-fixture",
        "false_positive",
        "rollback_requested"
      )
      |> Map.merge(%{"caused_by" => "batch_rollback", "batch_id" => "batch-fixture"})
    ]

    assert Enum.all?(negatives, &OutcomeTracker.qualified_outcome?/1)
    baseline = graph([verified("repo-a-fixture")])
    graph = graph([verified("repo-a-fixture") | negatives])

    assert graph.nodes["repo-b-fixture"] == :repo
    assert graph.cross_repo_edges == []
    assert graph.edges == baseline.edges
    assert Enum.all?(graph.edges, fn {_from, _to, weight} -> weight >= 0.0 end)
  end

  test "Jaccard weights use only qualified shared recipe sets" do
    qualified = [
      verified("repo-a-fixture"),
      verified("repo-a-fixture", "recipe-a-only-fixture"),
      verified("repo-b-fixture"),
      verified("repo-b-fixture", "recipe-b-only-fixture")
    ]

    ignored = [
      verified("repo-a-fixture", "recipe-b-only-fixture")
      |> Map.put("verification", "unverified"),
      verified("repo-c-fixture") |> Map.put("verification", "scan_failed")
    ]

    expected =
      MapSet.new([
        {"repo-a-fixture", "repo-b-fixture", 1 / 3},
        {"repo-b-fixture", "repo-a-fixture", 1 / 3}
      ])

    assert MapSet.new(graph(qualified ++ ignored).cross_repo_edges) == expected
    assert MapSet.new(graph(qualified ++ qualified).cross_repo_edges) == expected
  end

  test "the historical pattern identity remains supported for qualified records" do
    rows =
      for repo <- ["repo-a-fixture", "repo-b-fixture"] do
        verified(repo)
        |> Map.put("pattern", "recipe-shared-fixture")
        |> Map.delete("recipe_id")
      end

    assert MapSet.new(graph(rows).cross_repo_edges) ==
             MapSet.new([
               {"repo-a-fixture", "repo-b-fixture", 1.0},
               {"repo-b-fixture", "repo-a-fixture", 1.0}
             ])
  end

  test "recipe inventory and unqualified-only histories remain neutral" do
    for rows <- [
          [],
          [observation("repo-a-fixture", "recipe-shared-fixture", "success", "unverified")]
        ] do
      result = graph(rows)
      assert result.edges == []

      for recipe <- @recipes do
        assert GraphOfTrust.trust_score(result, recipe["id"]) == 0.5
      end

      assert Enum.all?(GraphOfTrust.trusted_recipes(result), fn {_id, score} -> score == 0.5 end)
    end
  end

  test "a qualified negative-only graph cannot award positive trust" do
    for {outcome, verification} <- [{"failure", "failed"}, {"false_positive", "still_present"}] do
      result =
        graph([observation("negative-only-repo", "recipe-shared-fixture", outcome, verification)])

      assert result.edges == []
      assert result.nodes["negative-only-repo"] == :repo
      assert Enum.all?(result.trust_scores, fn {_id, score} -> score == 0.5 end)
    end
  end

  test "neutral recipe inventory and negative-only nodes cannot perturb supported ranking" do
    supported = [verified("repo-a-fixture")]
    baseline = GraphOfTrust.build(supported, [%{"id" => "recipe-shared-fixture"}])
    assert baseline.trust_scores["recipe-shared-fixture"] > 0.5
    extra_recipes = for n <- 1..500, do: %{"id" => "unsupported-#{n}"}

    negatives = [
      observation("negative-only-repo", "unused-negative-recipe", "failure", "failed")
      |> Map.put("bot", "negative-only-bot")
    ]

    expanded =
      GraphOfTrust.build(supported ++ negatives, [
        %{"id" => "recipe-shared-fixture"} | extra_recipes
      ])

    for {id, score} <- baseline.trust_scores do
      assert expanded.trust_scores[id] == score
    end

    assert expanded.trust_scores["negative-only-repo"] == 0.5
    assert expanded.trust_scores["negative-only-bot"] == 0.5
    assert Enum.all?(extra_recipes, fn recipe -> expanded.trust_scores[recipe["id"]] == 0.5 end)
    assert GraphOfTrust.trust_score(expanded, "unknown-entity") == 0.5
  end
end
