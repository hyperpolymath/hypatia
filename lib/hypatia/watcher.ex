# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher do
  @moduledoc """
  Live-monitoring aggregator for the supervision tree.

  Subscribes to every event in `Hypatia.Telemetry.all_events/0` and
  maintains rolling-window counters in ETS so the CLI dashboard, JSON
  API, and (future) alerting layer can read live state without
  re-parsing JSONL or re-querying the outcomes log.

  ## State model

  Three ETS tables hold time-bucketed event counts:
    - `:hypatia_watcher_5m` — 5-minute window, 5-second buckets (60 buckets)
    - `:hypatia_watcher_1h` — 1-hour window, 1-minute buckets (60 buckets)
    - `:hypatia_watcher_1d` — 1-day window, 1-hour buckets (24 buckets)

  Each row is `{{event, bucket_ts}, count}`. A periodic tick prunes
  expired buckets so the tables don't grow unbounded.

  Also tracks:
    - GenServer message-queue depths via `:erlang.process_info(pid,
      :message_queue_len)` polled every 5s
    - Most-recent dispatch / outcome / quarantine event for each
      recipe_id (for drill-down)

  ## Back-pressure

  The watcher must NEVER block the producer. Telemetry handlers run
  in the *caller's* process, so they cast to the watcher; the
  watcher's mailbox is the only place events can pile up.
  `:hibernate_after` plus a drop-on-full counter (exposed as a
  metric itself) keep the watcher honest under load.

  ## Lifecycle

  Supervised by `Hypatia.Application`. On terminate, ETS tables die
  with the process — live state is ephemeral by design (Phase 1
  scope). Persistence to verisim-data is Phase 3 work.
  """

  use GenServer
  require Logger

  alias Hypatia.Telemetry

  @tables [
    {:hypatia_watcher_5m, 5_000, 60},
    {:hypatia_watcher_1h, 60_000, 60},
    {:hypatia_watcher_1d, 3_600_000, 24}
  ]

  @prune_interval_ms 30_000
  @queue_poll_interval_ms 5_000
  @handler_id "hypatia-watcher"
  # Drop telemetry events if our mailbox is over this. Keeps the
  # watcher from becoming a tarpit during sweep storms.
  @max_mailbox 1_000
  @recent_events_per_kind 50

  # ─── Public API ────────────────────────────────────────────────────────

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Snapshot of current state: counters across all three windows, queue
  depths, and the recent-event tail. JSON-serialisable.
  """
  def snapshot do
    GenServer.call(__MODULE__, :snapshot, 5_000)
  catch
    :exit, _ -> %{status: :unavailable}
  end

  @doc """
  Event counts in the given window (`:m5 | :h1 | :d1`) keyed by
  telemetry event name. Cheap — reads ETS directly without going
  through the GenServer.
  """
  def counts(window \\ :m5) do
    table = window_table(window)

    if :ets.info(table) == :undefined do
      %{}
    else
      table
      |> :ets.tab2list()
      |> Enum.reduce(%{}, fn {{event, _bucket}, count}, acc ->
        Map.update(acc, event, count, &(&1 + count))
      end)
    end
  end

  @doc """
  Most-recent N events of every kind, oldest first. Drilldown surface.
  """
  def recent_events do
    GenServer.call(__MODULE__, :recent_events, 5_000)
  catch
    :exit, _ -> []
  end

  @doc """
  Message-queue depth for every supervised GenServer, plus the
  watcher's own backpressure counters. Backs the "is anything stuck?"
  view in the dashboard.
  """
  def queue_depths do
    GenServer.call(__MODULE__, :queue_depths, 5_000)
  catch
    :exit, _ -> %{}
  end

  # ─── GenServer ─────────────────────────────────────────────────────────

  @impl true
  def init(_opts) do
    Enum.each(@tables, fn {name, _bucket_ms, _max_buckets} ->
      :ets.new(name, [:named_table, :public, :set, read_concurrency: true])
    end)

    attach_handler()

    Process.send_after(self(), :prune, @prune_interval_ms)
    Process.send_after(self(), :poll_queues, @queue_poll_interval_ms)

    state = %{
      recent: %{},
      queue_depths: %{},
      dropped_events: 0,
      started_at: DateTime.utc_now()
    }

    {:ok, state, :hibernate}
  end

  @impl true
  def handle_cast({:event, event, measurements, metadata}, state) do
    if mailbox_overloaded?() do
      {:noreply, %{state | dropped_events: state.dropped_events + 1}}
    else
      now = System.system_time(:millisecond)

      record_counts(event, now)
      state = record_recent(state, event, measurements, metadata, now)
      broadcast(event, measurements, metadata, now)

      {:noreply, state}
    end
  end

  @doc """
  Subscribe the calling process to all telemetry events from the
  watcher. Each event arrives as
  `{:hypatia_event, event, measurements, metadata, timestamp_ms}`.
  Subscription is automatically dropped when the caller dies.

  Optional `:events` filters delivery to a list of event names
  (lists like `[:hypatia, :scan, :complete]`).
  """
  def subscribe(opts \\ []) do
    filter = Keyword.get(opts, :events, :all)
    Registry.register(Hypatia.Watcher.PubSub, :events, filter)
    :ok
  end

  defp broadcast(event, measurements, metadata, ts) do
    # Registry dispatch runs in the caller's process; we're already
    # inside the watcher GenServer so any handler exception MUST be
    # caught — otherwise one misbehaving subscriber takes down the
    # whole watcher.
    Registry.dispatch(Hypatia.Watcher.PubSub, :events, fn entries ->
      Enum.each(entries, fn {pid, filter} ->
        if filter == :all or event in filter do
          send(pid, {:hypatia_event, event, measurements, metadata, ts})
        end
      end)
    end)
  rescue
    _ -> :ok
  catch
    _, _ -> :ok
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    {:reply,
     %{
       counts: %{
         m5: counts(:m5),
         h1: counts(:h1),
         d1: counts(:d1)
       },
       queue_depths: state.queue_depths,
       dropped_events: state.dropped_events,
       recent_by_kind: state.recent,
       uptime_seconds: DateTime.diff(DateTime.utc_now(), state.started_at),
       generated_at: DateTime.utc_now() |> DateTime.to_iso8601()
     }, state}
  end

  def handle_call(:recent_events, _from, state) do
    {:reply, state.recent, state}
  end

  def handle_call(:queue_depths, _from, state) do
    {:reply, state.queue_depths, state}
  end

  @impl true
  def handle_info(:prune, state) do
    Enum.each(@tables, fn {name, bucket_ms, max_buckets} ->
      prune_table(name, bucket_ms, max_buckets)
    end)

    Process.send_after(self(), :prune, @prune_interval_ms)
    {:noreply, state, :hibernate}
  end

  def handle_info(:poll_queues, state) do
    depths = collect_queue_depths()
    Process.send_after(self(), :poll_queues, @queue_poll_interval_ms)
    {:noreply, %{state | queue_depths: depths}}
  end

  @impl true
  def terminate(_reason, _state) do
    :telemetry.detach(@handler_id)
    :ok
  end

  # ─── Internals ─────────────────────────────────────────────────────────

  defp attach_handler do
    # Use a captured remote function (&__MODULE__.handle_event/4) rather
    # than an anonymous closure — telemetry warns about local fns
    # because they prevent hot-code-reloading of the handler. The
    # captured function casts back to the watcher so the producer
    # process never pays the watcher's processing cost.
    :telemetry.attach_many(
      @handler_id,
      Telemetry.all_events(),
      &__MODULE__.handle_event/4,
      nil
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, _config) do
    GenServer.cast(__MODULE__, {:event, event, measurements, metadata})
  end

  defp record_counts(event, now) do
    Enum.each(@tables, fn {name, bucket_ms, _max} ->
      bucket = bucket_for(now, bucket_ms)
      :ets.update_counter(name, {event, bucket}, 1, {{event, bucket}, 0})
    end)
  end

  defp record_recent(state, event, measurements, metadata, now) do
    entry = %{
      event: event,
      measurements: measurements,
      metadata: metadata,
      at: now
    }

    updated =
      Map.update(state.recent, event, [entry], fn existing ->
        [entry | existing] |> Enum.take(@recent_events_per_kind)
      end)

    %{state | recent: updated}
  end

  defp prune_table(name, bucket_ms, max_buckets) do
    cutoff = bucket_for(System.system_time(:millisecond), bucket_ms) - max_buckets * bucket_ms
    # :ets.select_delete via match_spec: {{_event, bucket}, _count} where bucket < cutoff
    :ets.select_delete(name, [
      {{{:"$1", :"$2"}, :"$3"}, [{:<, :"$2", cutoff}], [true]}
    ])
  end

  defp bucket_for(now_ms, bucket_ms), do: div(now_ms, bucket_ms) * bucket_ms

  defp window_table(:m5), do: :hypatia_watcher_5m
  defp window_table(:h1), do: :hypatia_watcher_1h
  defp window_table(:d1), do: :hypatia_watcher_1d

  defp mailbox_overloaded? do
    {:message_queue_len, n} = Process.info(self(), :message_queue_len)
    n > @max_mailbox
  end

  defp collect_queue_depths do
    # Walk the supervisor's children and probe each. Anything that
    # isn't a live process (transient / restarting) gets nil.
    case Process.whereis(Hypatia.Supervisor) do
      nil ->
        %{}

      sup_pid ->
        sup_pid
        |> Supervisor.which_children()
        |> Enum.reduce(%{}, fn
          {id, pid, _type, _modules}, acc when is_pid(pid) ->
            case Process.info(pid, :message_queue_len) do
              {:message_queue_len, len} -> Map.put(acc, inspect(id), len)
              _ -> Map.put(acc, inspect(id), nil)
            end

          {id, _, _, _}, acc ->
            Map.put(acc, inspect(id), nil)
        end)
    end
  end
end
