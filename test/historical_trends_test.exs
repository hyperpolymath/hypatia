# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.HistoricalTrendsTest do
  use ExUnit.Case, async: true

  alias Hypatia.HistoricalTrends

  # HistoricalTrends resolves its scan_history_path at compile time via
  # Application.compile_env, matching the convention used elsewhere in
  # the codebase (see OutcomeTracker). Tests therefore operate on the
  # compiled default path and clean up after themselves using file
  # names that cannot collide with real scan cycles (far-future dates).

  @history_path Application.compile_env(:hypatia, :scan_history_path, "data/verisim/scan-history")

  describe "record_scan_snapshot/2" do
    test "counts weak points and categories across repos" do
      now = ~U[2099-11-15 12:00:00Z]
      unique = System.unique_integer([:positive])
      scan_id = "scan-test-summarize-#{unique}"

      scans = [
        %{
          repo: "alpha",
          scan: %{
            "weak_points" => [
              %{"category" => "injection", "severity" => "High"},
              %{"category" => "injection", "severity" => "Low"},
              %{"category" => "crypto", "severity" => "Medium"}
            ]
          }
        },
        %{repo: "beta", scan: %{"weak_points" => []}}
      ]

      {:ok, ^scan_id, record} =
        HistoricalTrends.record_scan_snapshot(scans, at: now, scan_id: scan_id)

      assert record["total_repos"] == 2
      assert record["total_weak_points"] == 3

      alpha = record["repos"]["alpha"]
      assert alpha["weak_point_count"] == 3
      assert alpha["categories"]["injection"] == 2
      assert alpha["categories"]["crypto"] == 1
      assert record["repos"]["beta"]["weak_point_count"] == 0

      cleanup_line_in_file(Path.join(@history_path, "2099-11-15.jsonl"), scan_id)
    end

    test "appends rather than overwriting" do
      now = ~U[2099-11-16 10:00:00Z]
      ids = ["scan-test-append-1-#{System.unique_integer([:positive])}",
             "scan-test-append-2-#{System.unique_integer([:positive])}"]

      for id <- ids do
        {:ok, _, _} =
          HistoricalTrends.record_scan_snapshot(
            [%{repo: "r", scan: %{"weak_points" => []}}],
            at: now,
            scan_id: id
          )
      end

      file = Path.join(@history_path, "2099-11-16.jsonl")

      try do
        lines =
          file
          |> File.stream!()
          |> Enum.filter(fn line -> Enum.any?(ids, &String.contains?(line, &1)) end)

        assert length(lines) == 2
      after
        for id <- ids, do: cleanup_line_in_file(file, id)
      end
    end
  end

  describe "weak_point_trend/3" do
    test "carries delta 0 for the first point; subsequent points report change" do
      file = Path.join(@history_path, "2099-12-31.jsonl")
      File.mkdir_p!(@history_path)
      File.rm_rf(file)

      rec_a = %{
        "scan_id" => "scan-A",
        "scanned_at" => "2099-12-31T10:00:00Z",
        "repos" => %{"deltatest" => %{"weak_point_count" => 3, "categories" => %{}}},
        "total_weak_points" => 3,
        "total_repos" => 1
      }

      rec_b = %{
        "scan_id" => "scan-B",
        "scanned_at" => "2099-12-31T18:00:00Z",
        "repos" => %{"deltatest" => %{"weak_point_count" => 7, "categories" => %{}}},
        "total_weak_points" => 7,
        "total_repos" => 1
      }

      File.write!(file, Jason.encode!(rec_a) <> "\n" <> Jason.encode!(rec_b) <> "\n")

      try do
        trend = HistoricalTrends.weak_point_trend("deltatest", ~D[2099-12-31], ~D[2099-12-31])

        assert length(trend) == 2
        [first, second] = trend
        assert first.count == 3
        assert first.delta == 0
        assert second.count == 7
        assert second.delta == 4
      after
        File.rm_rf(file)
      end
    end

    test "returns empty for repos with no recorded history" do
      assert HistoricalTrends.weak_point_trend("nonexistent-repo-x9q", ~D[1999-01-01], ~D[1999-12-31]) == []
    end
  end

  describe "detect_anomalies/1" do
    test "returns a list (possibly empty) for the default window" do
      assert HistoricalTrends.detect_anomalies() |> is_list()
    end

    test "does not flag repos with fewer than min_points samples" do
      # Arbitrarily large min_points so any real scan-history data in the
      # trailing window is filtered out and the result is deterministic.
      assert HistoricalTrends.detect_anomalies(min_points: 1_000_000) == []
    end
  end

  describe "category_frequency/2" do
    test "returns a list (possibly empty) for the default window" do
      assert HistoricalTrends.category_frequency("injection", 7) |> is_list()
    end
  end

  # ── helpers ──────────────────────────────────────────────────────────────

  defp cleanup_line_in_file(path, marker) do
    if File.exists?(path) do
      kept =
        path
        |> File.stream!()
        |> Enum.reject(&String.contains?(&1, marker))
        |> Enum.join()

      if kept == "" do
        File.rm(path)
      else
        File.write!(path, kept)
      end
    end
  end
end
