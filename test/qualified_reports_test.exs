# SPDX-License-Identifier: MPL-2.0
# E0-13: a report that shows a success count also shows the qualified count.
# Issue 984, acceptance A5: missing evidence is "a visible failure or unknown,
# never a silent success".
defmodule Hypatia.QualifiedReportsTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Hypatia.OutcomeTracker
  alias Hypatia.VCL.Query
  alias Hypatia.Watcher.AnomalyDetector

  @moduletag timeout: 120_000

  @recipe "recipe-e0-13"

  # 3 claimed successes, 1 of them verified. Plus 2 failure rows: 1 with
  # evidence ("failed") and 1 without ("unverified"). 5 rows in all.
  @rows [
    %{"outcome" => "success", "verification" => "verified"},
    %{"outcome" => "success", "verification" => "unverified"},
    %{"outcome" => "success", "verification" => "scan_failed"},
    %{"outcome" => "failure", "verification" => "failed"},
    %{"outcome" => "failure", "verification" => "unverified"}
  ]

  setup do
    original = Application.fetch_env(:hypatia, :verisimdb_data_path)

    root =
      System.tmp_dir!()
      |> Path.join("hypatia_qualified_reports_#{:erlang.unique_integer([:positive])}")

    outcomes = Path.join(root, "outcomes")
    File.mkdir_p!(outcomes)

    now = DateTime.utc_now() |> DateTime.to_iso8601()

    lines =
      for row <- @rows do
        row
        |> Map.merge(%{
          "recipe_id" => @recipe,
          "repo" => "owner/repo",
          "file" => "a.txt",
          "timestamp" => now,
          "bot" => "hypatia"
        })
        |> Jason.encode!()
      end

    File.write!(Path.join(outcomes, "2026-11.jsonl"), Enum.join(lines, "\n") <> "\n")
    Application.put_env(:hypatia, :verisimdb_data_path, root)

    on_exit(fn ->
      case original do
        {:ok, value} -> Application.put_env(:hypatia, :verisimdb_data_path, value)
        :error -> Application.delete_env(:hypatia, :verisimdb_data_path)
      end

      File.rm_rf!(root)
    end)

    %{root: root, now: now}
  end

  defp assert_adds_up(qualified, total) do
    assert qualified.successes + qualified.failures + qualified.false_positives +
             qualified.ignored == total
  end

  describe "OutcomeTracker.recipe_health/1" do
    test "shows claimed 3 and qualified 1 and keeps each existing key" do
      assert [row] = Enum.filter(OutcomeTracker.recipe_health(), &(&1.recipe_id == @recipe))

      # Existing keys keep their meaning.
      for key <- [
            :recipe_id,
            :dispatches,
            :successes,
            :failures,
            :false_positives,
            :success_rate,
            :verification,
            :status
          ] do
        assert Map.has_key?(row, key)
      end

      assert row.successes == 3
      assert row.failures == 2

      # New keys.
      assert row.claimed_successes == 3
      assert row.qualified.successes == 1
      assert row.qualified.failures == 1
      assert_adds_up(row.qualified, row.dispatches)
    end
  end

  describe "OutcomeTracker.verification_rate/2" do
    test "names the claimed denominator and keeps each existing key" do
      assert {:ok, report} = OutcomeTracker.verification_rate(@recipe, 1)

      for key <- [
            :verified,
            :still_present,
            :scan_failed,
            :unverified,
            :total,
            :rate,
            :verifiable
          ] do
        assert Map.has_key?(report, key)
      end

      assert report.total == 3
      assert report.claimed == 3
      assert report.verified == 1
    end
  end

  describe "VCL.Query reports" do
    test "recipe_effectiveness shows claimed 3 and qualified 1" do
      assert {:ok, report} = Query.recipe_effectiveness(@recipe)

      for key <- [:recipe_id, :total_attempts, :successes, :failures, :false_positives] do
        assert Map.has_key?(report, key)
      end

      assert report.successes == 3
      assert report.claimed_successes == 3
      assert report.qualified.successes == 1
      assert_adds_up(report.qualified, report.total_attempts)
    end

    test "outcome_timeline shows claimed 3 and qualified 1 for the day" do
      assert {:ok, [day]} = Query.outcome_timeline(@recipe)

      for key <- [:date, :successes, :failures, :false_positives, :total] do
        assert Map.has_key?(day, key)
      end

      assert day.successes == 3
      assert day.claimed_successes == 3
      assert day.qualified.successes == 1
      assert_adds_up(day.qualified, day.total)
    end

    test "pipeline_health shows the claimed and the qualified success count" do
      assert {:ok, health} = Query.pipeline_health()

      for key <- [:outcomes_recorded, :success_rate, :failure_count] do
        assert Map.has_key?(health, key)
      end

      assert health.claimed_successes == 3
      assert health.qualified.successes == 1
      assert_adds_up(health.qualified, health.outcomes_recorded)
    end
  end

  describe "mix hypatia.strategy_effectiveness" do
    test "each row shows claimed and qualified counts", %{root: root} do
      out =
        capture_io(fn ->
          Mix.Tasks.Hypatia.StrategyEffectiveness.run([
            "--format",
            "json",
            "--outcomes_path",
            Path.join(root, "outcomes"),
            "--metrics_path",
            Path.join(root, "metrics")
          ])
        end)

      json = out |> String.split("\n", trim: true) |> List.last() |> Jason.decode!()
      assert [row] = json["rows"]

      for key <- ["total", "successes", "failures", "false_positives", "verified", "success_rate"] do
        assert Map.has_key?(row, key)
      end

      assert row["successes"] == 3
      assert row["claimed_successes"] == 3
      assert row["qualified"]["successes"] == 1

      assert_adds_up(
        %{
          successes: row["qualified"]["successes"],
          failures: row["qualified"]["failures"],
          false_positives: row["qualified"]["false_positives"],
          ignored: row["qualified"]["ignored"]
        },
        row["total"]
      )
    end
  end

  describe "Watcher.AnomalyDetector" do
    setup do
      detector =
        start_supervised!(%{
          id: AnomalyDetector,
          start: {GenServer, :start_link, [AnomalyDetector, []]}
        })

      %{detector: detector}
    end

    defp deliver(detector, metadata) do
      send(detector, {:hypatia_event, [:hypatia, :outcome, :recorded], %{count: 1}, metadata, 0})
    end

    test "window/0 keeps its list of booleans; the new function shows 3 and 1", %{
      detector: detector
    } do
      for row <- @rows,
          do: deliver(detector, %{outcome: row["outcome"], verification: row["verification"]})

      # window/0 asserts a list of booleans (success = true), newest first.
      window = GenServer.call(detector, :window)
      assert window == [false, false, true, true, true]

      assert %{claimed: 3, qualified: 1, window: 5} =
               AnomalyDetector.claimed_and_qualified(detector)
    end

    test "an event with no verification value is a claim and no evidence", %{
      detector: detector
    } do
      deliver(detector, %{outcome: "success"})
      deliver(detector, %{"outcome" => "success"})

      assert %{claimed: 2, qualified: 0, window: 2} =
               AnomalyDetector.claimed_and_qualified(detector)
    end
  end
end
