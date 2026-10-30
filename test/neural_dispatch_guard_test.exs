# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Neural.DispatchGuardTest do
  use ExUnit.Case, async: true

  alias Hypatia.Neural.DispatchGuard
  alias Hypatia.TriangleRouter

  @pattern %{"id" => "PA-test", "routed_repo" => "example/repo"}

  test "coordinator unavailable cannot preserve an automatic elimination" do
    original = {:eliminate, %{"confidence" => 0.99}, @pattern}

    assert {:eliminate, recipe, pattern} = DispatchGuard.enforce(original, :unavailable)
    assert recipe["neural_prior_confidence"] == 0.99
    assert recipe["confidence"] == 0.94
    assert TriangleRouter.dispatch_strategy(recipe["confidence"]) == :review
    assert pattern["neural_dispatch_guard"] == "unavailable"
  end

  test "unavailable neural check does not promote a low-confidence recipe" do
    original = {:eliminate, %{"confidence" => 0.75}, @pattern}
    assert {:eliminate, recipe, _} = DispatchGuard.enforce(original, :unavailable)
    assert recipe["confidence"] == 0.75
    assert TriangleRouter.dispatch_strategy(recipe["confidence"]) == :report_only
  end

  test "malformed recipe confidence cannot become auto-executable" do
    original = {:eliminate, %{"confidence" => "high"}, @pattern}
    assert {:eliminate, recipe, _} = DispatchGuard.enforce(original, :unavailable)
    assert recipe["confidence"] == 0.0
  end

  test "neural review recommendation caps otherwise automatic recipe" do
    original = {:eliminate, %{"confidence" => 0.98}, @pattern}
    assert {:eliminate, recipe, _} = DispatchGuard.enforce(original, %{strategy: :review})
    assert TriangleRouter.dispatch_strategy(recipe["confidence"]) == :review
    assert recipe["neural_dispatch_guard"] == "review"
  end

  test "neural report-only recommendation demotes elimination to control" do
    original = {:eliminate, %{"confidence" => 0.99}, @pattern}
    assert {:control, pattern} = DispatchGuard.enforce(original, %{strategy: :report_only})
    assert pattern["neural_dispatch_guard"] == "report_only"
  end

  test "neural novelty always demotes to control" do
    original = {:eliminate, %{"confidence" => 0.99}, @pattern}
    assert {:control, pattern} =
             DispatchGuard.enforce(original, %{strategy: :auto_execute, is_novel: true})

    assert pattern["neural_dispatch_guard"] == "novel"
  end

  test "an unrecognised recommendation fails to review ceiling" do
    original = {:eliminate, %{"confidence" => 0.99}, @pattern}
    assert {:eliminate, recipe, _} = DispatchGuard.enforce(original, %{})
    assert TriangleRouter.dispatch_strategy(recipe["confidence"]) == :review
    assert recipe["neural_dispatch_guard"] == "invalid_recommendation"
  end

  test "neural auto_execute never raises a symbolic recipe confidence" do
    original = {:eliminate, %{"confidence" => 0.80}, @pattern}
    assert ^original = DispatchGuard.enforce(original, %{strategy: :auto_execute})
  end

  test "substitutions remain review-only when the coordinator is missing" do
    original = {:substitute, %{"confidence" => 0.99}, @pattern}
    assert {:substitute, recipe, pattern} = DispatchGuard.enforce(original, :unavailable)
    assert recipe["neural_dispatch_guard"] == "unavailable"
    assert pattern["neural_dispatch_guard"] == "unavailable"
  end

  test "a report-only recommendation demotes a substitution" do
    original = {:substitute, %{"confidence" => 0.99}, @pattern}
    assert {:control, _} = DispatchGuard.enforce(original, %{strategy: :report_only})
  end

  test "control remains control when neural check is unavailable" do
    assert {:control, pattern} = DispatchGuard.enforce({:control, @pattern}, :unavailable)
    assert pattern["neural_dispatch_guard"] == "unavailable"
  end

  @complete_auto_recommendation %{
    strategy: :auto_execute,
    confidence: 0.98,
    is_novel: false,
    architecture: :blackboard,
    networks_consulted: 8,
    annealing_clamped: false
  }

  test "a partial auto_execute recommendation cannot preserve automatic execution" do
    action = {:eliminate, %{"confidence" => 0.99}, @pattern}

    for recommendation <- [
          %{strategy: :auto_execute},
          %{strategy: :auto_execute, confidence: 0.99},
          Map.delete(@complete_auto_recommendation, :annealing_clamped),
          Map.put(@complete_auto_recommendation, :confidence, "0.98"),
          Map.put(@complete_auto_recommendation, :networks_consulted, 0)
        ] do
      assert {:eliminate, recipe, _} = DispatchGuard.enforce(action, recommendation)
      assert recipe["confidence"] <= 0.94
      assert recipe["neural_dispatch_guard"] == "invalid_recommendation"
    end
  end

  test "a fully formed auto_execute verdict can preserve a valid recipe" do
    action = {:eliminate, %{"confidence" => 0.99}, @pattern}
    assert ^action = DispatchGuard.enforce(action, @complete_auto_recommendation)
  end

  test "even a complete recommendation cannot promote malformed recipe confidence" do
    action = {:eliminate, %{"confidence" => "very high"}, @pattern}
    assert {:eliminate, recipe, _} =
             DispatchGuard.enforce(action, @complete_auto_recommendation)

    assert recipe["confidence"] == 0.0
    assert recipe["neural_prior_confidence"] == "very high"
    assert recipe["neural_dispatch_guard"] == "invalid_recipe_confidence"
  end

  test "review verdict preserves the original recipe confidence for evidence" do
    action = {:eliminate, %{"confidence" => 0.99}, @pattern}
    assert {:eliminate, recipe, _} =
             DispatchGuard.enforce(action, %{strategy: :review, confidence: 0.89})

    assert recipe["neural_prior_confidence"] == 0.99
    assert recipe["confidence"] == 0.94
  end

  test "neural recommendation uses the matched recipe ID, not stale registry metadata" do
    action =
      {:eliminate, %{"id" => "recipe-42", "confidence" => 0.99},
       Map.put(@pattern, "recipe_id", nil)}

    assert DispatchGuard.recommendation_context(action)["recipe_id"] == "recipe-42"
  end

  test "neural recommendation has the same context for substitution actions" do
    action = {:substitute, %{"id" => "recipe-7"}, @pattern}
    assert DispatchGuard.recommendation_context(action)["recipe_id"] == "recipe-7"
  end

  test "control-tier coordinator context keeps its pattern" do
    assert DispatchGuard.recommendation_context({:control, @pattern}) == @pattern
  end
end
