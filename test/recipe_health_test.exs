# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for the closed-loop verification metric:
# OutcomeTracker.verification_rate/2 and OutcomeTracker.recipe_health/1.

defmodule Hypatia.RecipeHealthTest do
  # async: false because the outcome log is a shared on-disk resource;
  # writing test outcomes from concurrent tests would race each other.
  use ExUnit.Case, async: false

  alias Hypatia.OutcomeTracker

  @test_recipe_prefix "test-recipe-health-"

  setup do
    # Each test gets a unique recipe_id so its outcomes are isolated in
    # the shared outcomes log. We don't clean up — the verification_rate
    # aggregator filters by recipe_id so leftover records from a previous
    # run only affect their own recipe_id.
    recipe_id = @test_recipe_prefix <> Integer.to_string(System.unique_integer([:positive]))
    {:ok, recipe_id: recipe_id}
  end

  describe "verification_rate/2" do
    test "returns :no_outcomes for a recipe that has no records", %{recipe_id: recipe_id} do
      assert {:ok, :no_outcomes} = OutcomeTracker.verification_rate(recipe_id)
    end

    test "returns :insufficient_data below the threshold", %{recipe_id: recipe_id} do
      OutcomeTracker.record_outcome(recipe_id, "test-repo", "a.ex", :success, %{
        "verification" => "verified"
      })

      assert {:ok, %{rate: :insufficient_data, verifiable: 1, total: 1}} =
               OutcomeTracker.verification_rate(recipe_id, 5)
    end

    test "computes rate from verified/still_present ratio", %{recipe_id: recipe_id} do
      # 4 verified + 1 still_present = 5 verifiable, rate = 0.8
      for i <- 1..4 do
        OutcomeTracker.record_outcome(recipe_id, "r", "f#{i}", :success, %{
          "verification" => "verified"
        })
      end

      OutcomeTracker.record_outcome(recipe_id, "r", "f5", :success, %{
        "verification" => "still_present"
      })

      assert {:ok, %{rate: rate, verifiable: 5, verified: 4, still_present: 1}} =
               OutcomeTracker.verification_rate(recipe_id, 5)

      assert_in_delta(rate, 0.8, 0.001)
    end

    test "excludes scan_failed and unverified from the denominator", %{recipe_id: recipe_id} do
      # 5 verified + 100 scan_failed + 100 unverified -> rate is 1.0, not
      # diluted by environments where panic-attack wasn't available.
      for i <- 1..5 do
        OutcomeTracker.record_outcome(recipe_id, "r", "v#{i}", :success, %{
          "verification" => "verified"
        })
      end

      for i <- 1..3 do
        OutcomeTracker.record_outcome(recipe_id, "r", "sf#{i}", :success, %{
          "verification" => "scan_failed"
        })

        OutcomeTracker.record_outcome(recipe_id, "r", "u#{i}", :success, %{
          "verification" => "unverified"
        })
      end

      assert {:ok, %{rate: 1.0, verifiable: 5, scan_failed: 3, unverified: 3}} =
               OutcomeTracker.verification_rate(recipe_id, 5)
    end
  end

  describe "recipe_health/1" do
    test "returns at least the recipe we just recorded outcomes for", %{recipe_id: recipe_id} do
      for i <- 1..6 do
        OutcomeTracker.record_outcome(recipe_id, "r", "f#{i}", :success, %{
          "verification" => "verified"
        })
      end

      rows = OutcomeTracker.recipe_health(min_attempts: 5)
      ours = Enum.find(rows, &(&1.recipe_id == recipe_id))

      assert ours != nil
      assert ours.successes == 6
      assert ours.verification.verified == 6
      assert ours.verification.rate == 1.0
      assert ours.status == :healthy
    end

    test "tags quarantine_candidate when verification rate is below 0.30", %{recipe_id: recipe_id} do
      # 1 verified + 9 still_present = 10 verifiable, rate = 0.1
      OutcomeTracker.record_outcome(recipe_id, "r", "v1", :success, %{
        "verification" => "verified"
      })

      for i <- 1..9 do
        OutcomeTracker.record_outcome(recipe_id, "r", "sp#{i}", :success, %{
          "verification" => "still_present"
        })
      end

      rows = OutcomeTracker.recipe_health(min_attempts: 5)
      ours = Enum.find(rows, &(&1.recipe_id == recipe_id))

      assert ours.status == :quarantine_candidate
    end

    test "quarantined?/2 returns true when verification rate is below threshold", %{
      recipe_id: recipe_id
    } do
      # 1 verified + 9 still_present = 10 verifiable, rate = 0.10
      OutcomeTracker.record_outcome(recipe_id, "r", "v1", :success, %{
        "verification" => "verified"
      })

      for i <- 1..9 do
        OutcomeTracker.record_outcome(recipe_id, "r", "sp#{i}", :success, %{
          "verification" => "still_present"
        })
      end

      assert OutcomeTracker.quarantined?(recipe_id, threshold: 0.30, min_attempts: 5) == true
    end

    test "quarantined?/2 returns false on insufficient data (avoids chicken-and-egg)", %{
      recipe_id: recipe_id
    } do
      # Only 2 verifiable outcomes -- below min_attempts. The gate
      # should let the recipe through so it can earn more verification
      # data, not gate it on too few samples.
      OutcomeTracker.record_outcome(recipe_id, "r", "v1", :success, %{
        "verification" => "still_present"
      })

      OutcomeTracker.record_outcome(recipe_id, "r", "v2", :success, %{
        "verification" => "still_present"
      })

      assert OutcomeTracker.quarantined?(recipe_id, threshold: 0.30, min_attempts: 5) == false
    end

    test "quarantined?/2 honours HYPATIA_RECIPE_QUARANTINE_DISABLE env override", %{
      recipe_id: recipe_id
    } do
      # Set up data that WOULD quarantine, then disable via env.
      OutcomeTracker.record_outcome(recipe_id, "r", "v1", :success, %{
        "verification" => "verified"
      })

      for i <- 1..9 do
        OutcomeTracker.record_outcome(recipe_id, "r", "sp#{i}", :success, %{
          "verification" => "still_present"
        })
      end

      System.put_env("HYPATIA_RECIPE_QUARANTINE_DISABLE", "true")

      on_exit(fn -> System.delete_env("HYPATIA_RECIPE_QUARANTINE_DISABLE") end)

      assert OutcomeTracker.quarantined?(recipe_id, threshold: 0.30, min_attempts: 5) == false
    end

    test "tags degraded between quarantine and healthy", %{recipe_id: recipe_id} do
      # 3 verified + 7 still_present = 10 verifiable, rate = 0.3
      # → just at the quarantine threshold (0.30), so degraded (< 0.70).
      for i <- 1..3 do
        OutcomeTracker.record_outcome(recipe_id, "r", "v#{i}", :success, %{
          "verification" => "verified"
        })
      end

      for i <- 1..7 do
        OutcomeTracker.record_outcome(recipe_id, "r", "sp#{i}", :success, %{
          "verification" => "still_present"
        })
      end

      rows = OutcomeTracker.recipe_health(min_attempts: 5)
      ours = Enum.find(rows, &(&1.recipe_id == recipe_id))

      assert ours.status == :degraded
    end
  end
end
