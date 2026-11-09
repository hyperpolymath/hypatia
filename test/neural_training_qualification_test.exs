# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Neural.TrainingQualificationTest do
  use ExUnit.Case, async: true

  alias Hypatia.Neural.TrainingPipeline

  defp observation(index, outcome, verification) do
    %{
      "recipe_id" => "recipe-training-qualification-fixture",
      "timestamp" => "2026-01-01T00:00:#{String.pad_leading(Integer.to_string(index), 2, "0")}Z",
      "outcome" => outcome,
      "verification" => verification
    }
  end

  test "unverified and contradictory successes cannot alter a qualified trajectory" do
    qualified =
      for index <- 1..10 do
        if index <= 5,
          do: observation(index, "success", "verified"),
          else: observation(index, "failure", "failed")
      end

    unqualified =
      Enum.map(
        ["unverified", "scan_failed", "scan_skipped", "still_present", "incomplete", nil],
        &observation(0, "success", &1)
      ) ++ [observation(0, "success", "verified") |> Map.delete("verification")]

    expected = TrainingPipeline.build_esn_training_data(qualified)
    assert [%{count: 10, series: series}] = expected
    assert Enum.take(series, 5) == List.duplicate(1.0, 5)
    assert List.last(series) == 0.5

    assert TrainingPipeline.build_esn_training_data(unqualified ++ Enum.reverse(qualified)) ==
             expected
  end

  test "unqualified observations cannot satisfy the minimum sample count" do
    qualified = for index <- 1..9, do: observation(index, "success", "verified")
    unqualified = for index <- 10..30, do: observation(index, "success", "unverified")

    assert TrainingPipeline.build_esn_training_data(qualified ++ unqualified) == []
    assert TrainingPipeline.build_esn_training_data(unqualified) == []
  end

  test "qualified failures and false positives remain negative evidence" do
    rows =
      for index <- 1..10 do
        case rem(index, 3) do
          0 -> observation(index, "success", "verified")
          1 -> observation(index, "failure", "incomplete")
          2 -> observation(index, "false_positive", "still_present")
        end
      end

    assert [%{count: 10, series: series}] = TrainingPipeline.build_esn_training_data(rows)
    assert List.last(series) == 0.3
  end

  test "historical pattern and fixed_at fields retain their identity and time ordering" do
    rows =
      for index <- 1..10 do
        observation(index, if(index <= 5, do: "success", else: "failure"), "verified")
        |> Map.put("pattern", "historical-training-fixture")
        |> Map.put("fixed_at", observation(index, "success", "verified")["timestamp"])
        |> Map.drop(["recipe_id", "timestamp"])
      end

    assert [%{recipe_id: "historical-training-fixture", count: 10, series: series}] =
             TrainingPipeline.build_esn_training_data(Enum.reverse(rows))

    assert Enum.take(series, 5) == List.duplicate(1.0, 5)
    assert List.last(series) == 0.5
  end

  test "missing or invalid recipe identities and non-object JSON values are ignored" do
    invalid =
      for id <- [nil, "", "  ", 42, [], %{}], index <- 1..10 do
        observation(index, "success", "verified") |> Map.put("recipe_id", id)
      end

    missing =
      for index <- 1..10 do
        observation(index, "success", "verified") |> Map.delete("recipe_id")
      end

    assert TrainingPipeline.build_esn_training_data(invalid ++ missing ++ [nil, [], 42, "row"]) ==
             []
  end
end
