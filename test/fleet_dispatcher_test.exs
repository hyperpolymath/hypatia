# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.FleetDispatcherTest do
  use ExUnit.Case, async: true

  alias Hypatia.FleetDispatcher

  @sample_recipe %{
    "id" => "recipe-shell-quote-vars",
    "confidence" => 0.99,
    "description" => "Quote shell variables",
    "fix_script" => "fix-shell-quoting.sh",
    "action" => "shell"
  }

  @sample_pattern %{
    "id" => "PA009-shell-unquoted-var",
    "category" => "CommandInjection",
    "description" => "Unquoted shell variable",
    "severity" => "High",
    "routed_repo" => "test-repo",
    "repos_affected_list" => ["test-repo"]
  }

  describe "dispatch_routed_action/1" do
    test "dispatches eliminate action" do
      result =
        FleetDispatcher.dispatch_routed_action({:eliminate, @sample_recipe, @sample_pattern})

      # Fleet dispatcher logs dispatch; returns :ok or {:ok, _}
      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end

    test "dispatches substitute action" do
      recipe = %{
        "id" => "recipe-safe-command",
        "confidence" => 0.90,
        "proven_module" => "SafeCommand"
      }

      result =
        FleetDispatcher.dispatch_routed_action({:substitute, recipe, @sample_pattern})

      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end

    test "dispatches control action" do
      result = FleetDispatcher.dispatch_routed_action({:control, @sample_pattern})

      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end
  end

  describe "dispatch_finding/1 (legacy)" do
    test "dispatches by finding type" do
      finding = %{
        type: :fix_suggestion,
        repo: "test-repo",
        file: "scripts/deploy.sh",
        issue: "Unquoted variable",
        suggestion: "Add double quotes"
      }

      result = FleetDispatcher.dispatch_finding(finding)

      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end
  end

  # E0-19: the kind of match that chose a recipe caps the tier of its dispatch.
  # `eliminate_strategy/2` is the one place where the live dispatch path decides the tier. It
  # is tested directly because the Kin Gate holds every auto_execute action in a test (the
  # novelty network is untrained, E0-24), so a dispatch cannot show an auto_execute here.
  describe "the cap for a merged match, at the dispatch site (E0-19)" do
    @recipe %{"id" => "recipe-example-uncapped", "confidence" => 0.99}

    test "an exact match at confidence 0.99 is dispatched for auto_execute" do
      pattern = Map.put(@sample_pattern, "match_kind", "exact")
      assert FleetDispatcher.eliminate_strategy(@recipe, pattern) == :auto_execute
    end

    test "category, fuzzy, unknown and no kind at confidence 0.99 are review at most" do
      for kind <- ["category", "fuzzy", "unknown", nil] do
        pattern = if kind, do: Map.put(@sample_pattern, "match_kind", kind), else: @sample_pattern
        assert FleetDispatcher.eliminate_strategy(@recipe, pattern) == :review, inspect(kind)
      end
    end

    test "a dependabot fix, which no matcher chose, has no kind and is capped too" do
      alert_recipe = %{"id" => "r", "triangle_tier" => "eliminate", "confidence" => 0.99}
      pattern = Map.merge(@sample_pattern, %{"dependabot_recipe" => alert_recipe})

      assert FleetDispatcher.eliminate_strategy(%{"confidence" => 0.99}, pattern) == :review
    end

    test "a lower tier is kept: the cap only lowers" do
      pattern = Map.put(@sample_pattern, "match_kind", "exact")
      assert FleetDispatcher.eliminate_strategy(%{"confidence" => 0.9}, pattern) == :review
      assert FleetDispatcher.eliminate_strategy(%{"confidence" => 0.5}, pattern) == :report_only
    end
  end
end
