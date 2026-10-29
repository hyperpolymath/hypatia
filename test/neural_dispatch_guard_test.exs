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
end
