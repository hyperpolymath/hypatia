# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.OutcomeTrackerTest do
  use ExUnit.Case, async: true

  # OutcomeTracker reads JSONL files which can be slow with Jason.Decoder.
  # Allow 120s per test instead of the default 60s.
  @moduletag timeout: 120_000

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

  describe "trusted fleet evidence" do
    test "unverified or contradictory successes cannot increase recipe confidence" do
      rows = [
        %{"outcome" => "success"},
        %{"outcome" => "success", "verification" => "unverified"},
        %{"outcome" => "success", "verification" => "scan_failed"},
        %{"outcome" => "success", "verification" => "still_present"},
        %{"outcome" => "success", "verification" => "verified"},
        %{"outcome" => "failure", "verification" => "incomplete"},
        %{"outcome" => "false_positive", "verification" => "still_present"}
      ]

      assert OutcomeTracker.qualified_outcome_counts(rows) ==
               %{successes: 1, failures: 1, false_positives: 1, ignored: 4}
    end

    test "import provenance survives in the append-only destination" do
      source_id = "fleet-replay-test-" <> Integer.to_string(System.unique_integer([:positive]))
      refute MapSet.member?(OutcomeTracker.imported_fleet_ids(), source_id)

      assert {:ok, record} =
               OutcomeTracker.record_fleet_outcome(
                 "recipe-fleet-replay-test", "replay-repo", ".", :failure, source_id, "failed"
               )

      assert record["fleet_import_id"] == source_id
      assert record["origin"] == "gitbot-fleet"
      assert MapSet.member?(OutcomeTracker.imported_fleet_ids(), source_id)
    end

    test "fleet import insists on predicate evidence and matching exit-status outcomes" do
      row = %{"repo" => "example", "pattern" => "recipe-a", "outcome" => "success"}

      assert Hypatia.LearningScheduler.classify_fleet_record(row) == :skip
      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "verified", "before_hits" => 2, "after_hits" => 0})
             ) == {:ok, "recipe-a", "example", ".", :success, "verified"}
      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "verified", "before_hits" => 0, "after_hits" => 0})
             ) == :skip
      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.put(row, "verdict", "no_op")
             ) == :skip
      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "failed", "exit_code" => 1, "outcome" => "failure"})
             ) == {:ok, "recipe-a", "example", ".", :failure, "failed"}
      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "incomplete", "after_hits" => 2, "outcome" => "failure"})
             ) == {:ok, "recipe-a", "example", ".", :failure, "incomplete"}
      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "verified", "before_hits" => 1, "after_hits" => 0,
                                 "bot" => "hypatia"})
             ) == :skip
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
