# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.Persistence do
  @moduledoc """
  Periodic snapshot of `Hypatia.Watcher` state to `data/verisim/metrics/`.

  Phase 1's Watcher state is ephemeral — ETS tables die with the
  GenServer, and a restart loses the rolling-window history. Useful
  for live monitoring, useless for trend analysis ("did dispatch
  volume drop yesterday?", "is this recipe's verification rate
  trending down over weeks?").

  This module fixes that. Every `@snapshot_interval_ms` (default
  5 minutes) it appends a compact snapshot to
  `data/verisim/metrics/YYYY-MM-DD.jsonl`. The file is append-only,
  one JSON-encoded record per line, suitable for replay / VCL queries
  / external trend analysis.

  ## Snapshot record shape

      {
        "at_ms": 1779597642645,
        "at_iso": "2026-05-24T04:00:42.645Z",
        "uptime_seconds": 12345,
        "dropped_events": 0,
        "queue_depths": {"Hypatia.Watcher": 0, ...},
        "counts_m5":  {"hypatia.scan.complete": 3, ...},
        "counts_h1":  {"hypatia.scan.complete": 38, ...},
        "counts_d1":  {"hypatia.scan.complete": 412, ...},
        "recipe_health_summary": {
          "healthy": 12,
          "degraded": 2,
          "quarantine_candidate": 0,
          "insufficient_data": 8,
          "no_data": 0
        },
        "alert_count": N
      }

  Storing the recipe-health summary (counts per status), not every
  recipe row, keeps the per-snapshot size bounded. For per-recipe
  trends, query the outcomes log directly.

  ## Storage path

  Defaults to `<verisimdb_data_path>/metrics/`. Configurable via the
  `:hypatia, :metrics_path` application env. The path is created on
  init if it doesn't exist — append-only file IO can't auto-create
  intermediate directories.
  """

  use GenServer

  require Logger

  @snapshot_interval_ms 5 * 60 * 1000
  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

  # ─── Public ────────────────────────────────────────────────────────────

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Force an immediate snapshot. Returns the path that was (or would
  be) written to, plus the snapshot record. Useful in tests and for
  on-demand persistence from a mix task.
  """
  def snapshot_now do
    GenServer.call(__MODULE__, :snapshot_now, 10_000)
  end

  @doc """
  Path of the file the next snapshot will land in (today's UTC date).
  """
  def current_file do
    Path.join(metrics_dir(), today_filename())
  end

  # ─── GenServer ─────────────────────────────────────────────────────────

  @impl true
  def init(_opts) do
    ensure_dir!()
    Process.send_after(self(), :snapshot, @snapshot_interval_ms)
    {:ok, %{}}
  end

  @impl true
  def handle_info(:snapshot, state) do
    do_snapshot()
    Process.send_after(self(), :snapshot, @snapshot_interval_ms)
    {:noreply, state}
  end

  @impl true
  def handle_call(:snapshot_now, _from, state) do
    {path, record} = do_snapshot()
    {:reply, {:ok, path, record}, state}
  end

  # ─── Snapshot path ─────────────────────────────────────────────────────

  defp do_snapshot do
    snap = safe_watcher_snapshot()
    health = safe_recipe_health()
    alerts = safe_alerts_count()

    now_ms = System.system_time(:millisecond)

    record = %{
      at_ms: now_ms,
      at_iso: DateTime.from_unix!(now_ms, :millisecond) |> DateTime.to_iso8601(),
      uptime_seconds: Map.get(snap, :uptime_seconds, 0),
      dropped_events: Map.get(snap, :dropped_events, 0),
      queue_depths: Map.get(snap, :queue_depths, %{}),
      counts_m5: flatten_counts(get_in(snap, [:counts, :m5]) || %{}),
      counts_h1: flatten_counts(get_in(snap, [:counts, :h1]) || %{}),
      counts_d1: flatten_counts(get_in(snap, [:counts, :d1]) || %{}),
      recipe_health_summary: summarise_health(health),
      alert_count: alerts
    }

    path = current_file()

    case File.write(path, Jason.encode!(record) <> "\n", [:append, :utf8]) do
      :ok ->
        :ok

      {:error, reason} ->
        Logger.error(
          "Watcher.Persistence write failed at #{path}: #{inspect(reason)}. " <>
            "Trend data for this interval is lost."
        )
    end

    {path, record}
  end

  defp summarise_health(rows) do
    Enum.reduce(
      rows,
      %{healthy: 0, degraded: 0, quarantine_candidate: 0, insufficient_data: 0, no_data: 0, unverified: 0},
      fn r, acc -> Map.update(acc, r.status, 1, &(&1 + 1)) end
    )
  end

  defp flatten_counts(counts_map) do
    Map.new(counts_map, fn {k, v} -> {Enum.join(k, "."), v} end)
  end

  # ─── Safe accessors ────────────────────────────────────────────────────

  defp safe_watcher_snapshot do
    case Process.whereis(Hypatia.Watcher) do
      nil -> %{}
      _ -> Hypatia.Watcher.snapshot()
    end
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  defp safe_recipe_health do
    Hypatia.OutcomeTracker.recipe_health()
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  defp safe_alerts_count do
    case Process.whereis(Hypatia.Watcher.Alerts) do
      nil -> 0
      _ -> Hypatia.Watcher.Alerts.recent() |> length()
    end
  rescue
    _ -> 0
  catch
    _, _ -> 0
  end

  # ─── Paths ─────────────────────────────────────────────────────────────

  defp ensure_dir! do
    dir = metrics_dir()
    File.mkdir_p!(dir)
  end

  defp metrics_dir do
    Application.get_env(:hypatia, :metrics_path) ||
      Path.join(Path.expand(@verisimdb_data_path), "metrics")
  end

  defp today_filename do
    {{year, month, day}, _} = :calendar.universal_time()

    "#{year}-#{pad(month)}-#{pad(day)}.jsonl"
  end

  defp pad(n) when n < 10, do: "0#{n}"
  defp pad(n), do: "#{n}"
end
