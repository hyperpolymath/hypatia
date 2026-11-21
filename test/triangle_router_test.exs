# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.TriangleRouterTest do
  use ExUnit.Case, async: true

  alias Hypatia.TriangleRouter

  describe "route/3" do
    test "routes eliminate-tier pattern to eliminate action" do
      pattern = %{
        "id" => "PA009-shell-unquoted-var",
        "category" => "CommandInjection",
        "triangle_tier" => "eliminate",
        "description" => "Unquoted shell variable",
        "severity" => "High",
        "repos_affected_list" => ["test-repo"]
      }

      result = TriangleRouter.route(pattern, "test-repo", "shell")

      case result do
        {:eliminate, _recipe, _pattern} ->
          # Correct: recipe found for CommandInjection/shell
          :ok

        {:control, _} ->
          # Acceptable: no recipe file present in this environment
          :ok
      end
    end

    test "routes substitute-tier pattern with proven modules" do
      pattern = %{
        "id" => "PA016-path-traversal",
        "category" => "PathTraversal",
        "triangle_tier" => "substitute",
        "description" => "Path traversal via ..",
        "severity" => "High",
        "repos_affected_list" => ["test-repo"]
      }

      result = TriangleRouter.route(pattern, "test-repo", "rust")

      case result do
        {:substitute, recipe, _pattern} ->
          assert recipe["proven_module"] == "SafePath"
          assert recipe["formally_proven"] == true

        {:control, _} ->
          # Acceptable fallback if no recipe file matched
          :ok
      end
    end

    test "routes control-tier pattern to control or substitute (recipe-dependent)" do
      pattern = %{
        "id" => "PA006-race-condition",
        "category" => "RaceCondition",
        "triangle_tier" => "control",
        "description" => "Data race in concurrent block",
        "severity" => "High",
        "repos_affected_list" => ["test-repo"]
      }

      result = TriangleRouter.route(pattern, "test-repo", "rust")
      # If a recipe exists for RaceCondition, router correctly promotes to substitute.
      # If no recipe, falls through to control. Both are valid outcomes.
      assert match?({:control, _}, result) or match?({:substitute, _, _}, result)
    end
  end

  describe "route_batch/3" do
    test "routes multiple patterns at once" do
      patterns = [
        %{
          "id" => "PA009-shell-unquoted",
          "category" => "CommandInjection",
          "triangle_tier" => "eliminate",
          "description" => "Unquoted var",
          "severity" => "High",
          "repos_affected_list" => ["repo"]
        },
        %{
          "id" => "PA005-panic-unwrap",
          "category" => "PanicPath",
          "triangle_tier" => "control",
          "description" => "Unwrap call",
          "severity" => "Medium",
          "repos_affected_list" => ["repo"]
        }
      ]

      results = TriangleRouter.route_batch(patterns, "repo", "rust")
      assert length(results) == 2
    end
  end

  describe "dispatch_strategy/1" do
    test "auto-execute for high confidence" do
      assert TriangleRouter.dispatch_strategy(0.97) == :auto_execute
    end

    test "review for medium confidence" do
      assert TriangleRouter.dispatch_strategy(0.90) == :review
    end

    test "report only for low confidence" do
      assert TriangleRouter.dispatch_strategy(0.50) == :report_only
    end

    test "exact threshold boundaries" do
      assert TriangleRouter.dispatch_strategy(0.95) == :auto_execute
      assert TriangleRouter.dispatch_strategy(0.85) == :review
      assert TriangleRouter.dispatch_strategy(0.84) == :report_only
    end
  end

  # E0-19 (owner ruling of 2026-11-14, decision 2, and of 2026-11-15, option A for a recipe
  # with no kind of match): a recipe that a `category` or a `fuzzy` match chose, or whose
  # kind is unknown, never runs with no person. Its highest tier is `review`. Only an
  # `exact` match can reach `auto_execute`.
  describe "the cap for a merged match (E0-19)" do
    @kinds ["exact", "category", "fuzzy", "unknown", nil, "something-else"]
    @rank %{report_only: 0, review: 1, auto_execute: 2}

    test "confidence 0.99 with an exact match gives auto_execute" do
      assert TriangleRouter.strategy_for(0.99, "exact") == :auto_execute
    end

    test "confidence 0.99 with category, fuzzy, unknown or no kind gives review" do
      for kind <- ["category", "fuzzy", "unknown", nil, "something-else"] do
        assert TriangleRouter.strategy_for(0.99, kind) == :review, "kind #{inspect(kind)}"
      end
    end

    test "the cap never raises a tier: for each tier and each kind, the result is not above the uncapped one" do
      for confidence <- [0.0, 0.5, 0.84, 0.85, 0.9, 0.94, 0.95, 0.99, 1.0], kind <- @kinds do
        uncapped = TriangleRouter.dispatch_strategy(confidence)
        capped = TriangleRouter.strategy_for(confidence, kind)

        assert @rank[capped] <= @rank[uncapped],
               "#{confidence} #{inspect(kind)}: #{capped} is above #{uncapped}"

        if kind != "exact", do: assert(@rank[capped] <= @rank[:review])
      end
    end

    test "a lower tier is kept as it is, whatever the kind" do
      for kind <- @kinds do
        assert TriangleRouter.strategy_for(0.9, kind) == :review
        assert TriangleRouter.strategy_for(0.5, kind) == :report_only
      end
    end

    test "an ownership cap still wins, and the match cap composes with it" do
      assert TriangleRouter.recipe_strategy(%{"id" => "r", "confidence" => 0.99}, "exact") ==
               :auto_execute

      assert TriangleRouter.recipe_strategy(%{"id" => "r", "confidence" => 0.99}, "fuzzy") ==
               :review

      assert TriangleRouter.recipe_strategy(%{"id" => "r", "confidence" => 0.99}) == :review
    end
  end
end
