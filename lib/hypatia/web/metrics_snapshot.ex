# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.MetricsSnapshot do
  @moduledoc """
  Compact estate-level snapshot for the Ada TUI and external
  dashboards.

  The Ada TUI (`lib/tui/port.ex`) polls `GET /metrics/snapshot` every
  10 seconds and renders the returned counters. The shape it expects
  comes from the fallback map in `Hypatia.TUI.Port.fallback_snapshot/0`:

      %{
        "repos"        => integer,
        "weak_points"  => integer,
        "dispatched"   => integer,
        "outcomes"     => integer,
        "recipes"      => integer,
        "confidence"   => float,
        "status"       => "ok" | "degraded" | ...
      }

  Reads from the verisim-data flat-file store via VerisimConnector.
  Any failure returns a degraded snapshot rather than raising — the
  TUI must keep rendering even if data sources are slow / down.
  """

  require Logger

  def build do
    started = System.monotonic_time(:millisecond)

    snapshot = %{
      "repos" => safe_count(&repos_count/0),
      "weak_points" => safe_count(&weak_points_count/0),
      "dispatched" => safe_count(&dispatched_count/0),
      "outcomes" => safe_count(&outcomes_count/0),
      "recipes" => safe_count(&recipes_count/0),
      "confidence" => safe_confidence(),
      "status" => "ok",
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "duration_ms" => System.monotonic_time(:millisecond) - started
    }

    snapshot
  rescue
    e ->
      Logger.warning("MetricsSnapshot.build/0 degraded: #{Exception.message(e)}")
      degraded()
  catch
    _, _ -> degraded()
  end

  defp degraded do
    %{
      "repos" => 0,
      "weak_points" => 0,
      "dispatched" => 0,
      "outcomes" => 0,
      "recipes" => 0,
      "confidence" => 0.0,
      "status" => "degraded",
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp safe_count(fun) do
    fun.()
  rescue
    _ -> 0
  catch
    _, _ -> 0
  end

  defp repos_count do
    Hypatia.VerisimConnector.fetch_all_scans() |> length()
  end

  defp weak_points_count do
    Hypatia.VerisimConnector.fetch_all_scans()
    |> Enum.reduce(0, fn scan, acc ->
      acc + length(Map.get(scan, "weak_points", []))
    end)
  end

  defp dispatched_count do
    # All outcomes recorded = all dispatches that completed.
    Hypatia.VerisimConnector.fetch_all_outcomes() |> length()
  end

  defp outcomes_count do
    dispatched_count()
  end

  defp recipes_count do
    Hypatia.RecipeMatcher.all_recipes() |> length()
  end

  defp safe_confidence do
    Hypatia.RecipeMatcher.all_recipes()
    |> Enum.map(&Map.get(&1, "confidence", 0.0))
    |> case do
      [] -> 0.0
      list -> Enum.sum(list) / length(list)
    end
  rescue
    _ -> 0.0
  catch
    _, _ -> 0.0
  end
end
