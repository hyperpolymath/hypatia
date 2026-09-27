# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Safety.RateLimiter do
  @moduledoc """
  Dispatch rate limiter for Hypatia.

  Prevents overwhelming fleet bots with too many dispatches.
  Uses a sliding window algorithm per bot.

  Limits:
  - Per-bot: max 50 dispatches per minute
  - Global: max 200 dispatches per minute
  - Burst: max 10 dispatches in 5 seconds

  When limits are hit, dispatches are queued and retried
  after the window slides.
  """

  use GenServer
  require Logger

  @per_bot_limit 50
  @per_bot_window_ms 60_000
  @global_limit 200
  @global_window_ms 60_000
  @burst_limit 10
  @burst_window_ms 5_000
  @drain_interval_ms 5_000

  defstruct [
    # bot_name => [timestamp_ms, ...]
    bot_windows: %{},
    # [timestamp_ms, ...]
    global_window: [],
    # Queued dispatches when rate limited
    queue: :queue.new(),
    total_dispatched: 0,
    total_queued: 0,
    total_rate_limited: 0
  ]

  # --- GenServer API ---

  def start_link(opts \\ []) do
    # `:name` lets a test own its own instance (started with `name:`, shut
    # down with the test process) instead of reaching for the app-level
    # shared one, whose state the rest of the suite touches and whose crash
    # resets it silently (#857). Production starts it unnamed -> __MODULE__.
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def init(_opts) do
    Process.send_after(self(), :drain_queue, @drain_interval_ms)
    {:ok, %__MODULE__{}}
  end

  # --- Public API ---

  @doc """
  Check if a dispatch is allowed for a bot. Returns :ok or {:rate_limited, retry_after_ms}.

  `server` defaults to the app-level instance; tests pass their own (#857).
  """
  def check(bot_name, server \\ __MODULE__) do
    GenServer.call(server, {:check, bot_name})
  end

  @doc "Record a dispatch (call after successful dispatch)"
  def record_dispatch(bot_name, server \\ __MODULE__) do
    GenServer.cast(server, {:record, bot_name})
  end

  @doc "Enqueue a dispatch for later if rate limited"
  def enqueue(dispatch_entry, server \\ __MODULE__) do
    GenServer.cast(server, {:enqueue, dispatch_entry})
  end

  @doc "Get rate limiter statistics"
  def stats(server \\ __MODULE__) do
    GenServer.call(server, :stats)
  end

  # --- Callbacks ---

  def handle_call({:check, bot_name}, _from, state) do
    now = System.system_time(:millisecond)

    # Clean windows
    bot_window =
      state.bot_windows
      |> Map.get(bot_name, [])
      |> Enum.filter(fn ts -> now - ts < @per_bot_window_ms end)

    global_window = Enum.filter(state.global_window, fn ts -> now - ts < @global_window_ms end)

    burst_window = Enum.filter(bot_window, fn ts -> now - ts < @burst_window_ms end)

    cond do
      length(burst_window) >= @burst_limit ->
        oldest = Enum.min(burst_window)
        retry_after = @burst_window_ms - (now - oldest)

        {:reply, {:rate_limited, :burst, retry_after},
         update_windows(state, bot_name, bot_window, global_window)}

      length(bot_window) >= @per_bot_limit ->
        oldest = Enum.min(bot_window)
        retry_after = @per_bot_window_ms - (now - oldest)

        {:reply, {:rate_limited, :per_bot, retry_after},
         update_windows(state, bot_name, bot_window, global_window)}

      length(global_window) >= @global_limit ->
        oldest = Enum.min(global_window)
        retry_after = @global_window_ms - (now - oldest)

        {:reply, {:rate_limited, :global, retry_after},
         update_windows(state, bot_name, bot_window, global_window)}

      true ->
        {:reply, :ok, update_windows(state, bot_name, bot_window, global_window)}
    end
  end

  def handle_call(:stats, _from, state) do
    stats = %{
      total_dispatched: state.total_dispatched,
      total_queued: state.total_queued,
      total_rate_limited: state.total_rate_limited,
      queue_size: :queue.len(state.queue),
      active_bots: map_size(state.bot_windows),
      global_window_size: length(state.global_window)
    }

    {:reply, stats, state}
  end

  def handle_cast({:record, bot_name}, state) do
    now = System.system_time(:millisecond)
    bot_window = Map.get(state.bot_windows, bot_name, [])

    {:noreply,
     %{
       state
       | bot_windows: Map.put(state.bot_windows, bot_name, [now | bot_window]),
         global_window: [now | state.global_window],
         total_dispatched: state.total_dispatched + 1
     }}
  end

  def handle_cast({:enqueue, entry}, state) do
    {:noreply,
     %{
       state
       | queue: :queue.in(entry, state.queue),
         total_queued: state.total_queued + 1,
         total_rate_limited: state.total_rate_limited + 1
     }}
  end

  def handle_info(:drain_queue, state) do
    state = drain_queued(state)
    Process.send_after(self(), :drain_queue, @drain_interval_ms)
    {:noreply, state}
  end

  # --- Internal ---

  defp update_windows(state, bot_name, bot_window, global_window) do
    %{
      state
      | bot_windows: Map.put(state.bot_windows, bot_name, bot_window),
        global_window: global_window
    }
  end

  defp drain_queued(state) do
    case :queue.out(state.queue) do
      {{:value, entry}, remaining_queue} ->
        bot = Map.get(entry, "bot", Map.get(entry, :bot, "unknown"))

        case check_internal(state, bot) do
          :ok ->
            # Dispatch via fleet dispatcher. A dispatch that raises must NOT
            # take the rate limiter down with it: the limiter is
            # app-supervised, so a crash here is silently restarted with
            # empty `%__MODULE__{}` state, resetting every bot's window and
            # all counters underneath the whole suite (#857). Drop the entry,
            # log the failure, keep the limiter alive.
            Logger.info("Rate limiter: draining queued dispatch to #{bot}")

            try do
              Hypatia.FleetDispatcher.dispatch_finding(entry)
            rescue
              e ->
                Logger.error(
                  "Rate limiter: queued dispatch to #{bot} raised " <>
                    Exception.format(:error, e, __STACKTRACE__) <>
                    " -- dropping the entry; the limiter stays up"
                )
            catch
              kind, reason ->
                Logger.error(
                  "Rate limiter: queued dispatch to #{bot} exited " <>
                    Exception.format(kind, reason, __STACKTRACE__) <>
                    " -- dropping the entry; the limiter stays up"
                )
            end

            drain_queued(%{state | queue: remaining_queue})

          {:rate_limited, _, _} ->
            # Still rate limited, put back
            state
        end

      {:empty, _} ->
        state
    end
  end

  # Mirrors handle_call({:check, ...}) INCLUDING the burst limit. It used to
  # check only the per-bot and global windows, so `drain_queued/1`'s "still
  # rate limited, put back" branch was dead: a burst-saturated bot was still
  # :ok here, the entry was dispatched instead of requeued, and a raising
  # dispatch then killed the limiter (#857).
  defp check_internal(state, bot_name) do
    now = System.system_time(:millisecond)

    bot_window =
      state.bot_windows
      |> Map.get(bot_name, [])
      |> Enum.filter(fn ts -> now - ts < @per_bot_window_ms end)

    global_window = Enum.filter(state.global_window, fn ts -> now - ts < @global_window_ms end)

    burst_window = Enum.filter(bot_window, fn ts -> now - ts < @burst_window_ms end)

    cond do
      length(burst_window) >= @burst_limit -> {:rate_limited, :burst, 0}
      length(bot_window) >= @per_bot_limit -> {:rate_limited, :per_bot, 0}
      length(global_window) >= @global_limit -> {:rate_limited, :global, 0}
      true -> :ok
    end
  end
end
