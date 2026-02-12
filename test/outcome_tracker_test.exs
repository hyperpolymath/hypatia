# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.OutcomeTrackerTest do
  use ExUnit.Case, async: true

  alias Hypatia.OutcomeTracker

  describe "record_outcome/4" do
    test "records a successful outcome" do
      result =
        OutcomeTracker.record_outcome(
          "recipe-shell-quote-vars",
          "test-repo",
          "scripts/test.sh",
          :success
        )

      assert {:ok, record} = result
      assert record["recipe_id"] == "recipe-shell-quote-vars"
      assert record["repo"] == "test-repo"
      assert record["outcome"] == "success"
    end

    test "records a failure outcome" do
      result =
        OutcomeTracker.record_outcome(
          "recipe-todo-to-fill",
          "test-repo",
          "src/main.rs",
          :failure
        )

      assert {:ok, record} = result
      assert record["outcome"] == "failure"
    end
  end

  describe "confidence_trend/1" do
    test "returns insufficient_data for recipe with few outcomes" do
      assert OutcomeTracker.confidence_trend("recipe-nonexistent") == :insufficient_data
    end
  end

  describe "update_recipe_confidence/1" do
    test "returns no_outcomes for recipe with no recorded outcomes" do
      assert OutcomeTracker.update_recipe_confidence("recipe-nonexistent") == :no_outcomes
    end
  end
end
