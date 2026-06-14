# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.Ticker do
  @moduledoc """
  The in-brain tick — runs one merge-orchestration cycle on an interval.

  This is the GenServer half of the scheduled trigger (the GitHub Actions cron
  is the other). It mirrors `Hypatia.LearningScheduler`: schedule a tick, run a
  guarded cycle, reschedule. A cycle failure is caught and recorded — a bad
  store / transient error must never crash the tick or its supervisor.

  **Off by default.** It is only added to the supervision tree when
  `config :hypatia, :merge_orchestration, enabled: true` (see
  `Hypatia.Application`). Cadence is `config … :interval_ms` (default 15 min);
  `:cycle_opts` are forwarded to `Scheduler.cycle/1` (e.g. `:store`, `:holder`).

  Because it only calls `Scheduler.cycle/1`, the brain stays token-free: the tick
  reads the store and writes the decision manifest; the `.git-private-farm`
  actuator (a separate, token-bearing process) performs the merges.
  """

  use GenServer
  require Logger

  alias Hypatia.MergeOrchestration.Scheduler

  @default_interval_ms 15 * 60 * 1_000
  @default_first_delay_ms 30_000

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  @doc "Run a cycle now (manual / test trigger), out of band of the interval."
  def run_now(server \\ __MODULE__), do: GenServer.cast(server, :tick_now)

  @doc "Current ticker state (last_run, runs, last_stats, errors)."
  def status(server \\ __MODULE__), do: GenServer.call(server, :status)

  # --- GenServer callbacks ---

  @impl true
  def init(opts) do
    state = %{
      interval_ms: Keyword.get(opts, :interval_ms, config(:interval_ms, @default_interval_ms)),
      cycle_opts: Keyword.get(opts, :cycle_opts, config(:cycle_opts, [])),
      last_run: nil,
      runs: 0,
      last_stats: nil,
      errors: []
    }

    Process.send_after(self(), :tick, Keyword.get(opts, :first_delay_ms, @default_first_delay_ms))
    Logger.info("MergeOrchestration.Ticker started; cadence #{div(state.interval_ms, 1000)}s")
    {:ok, state}
  end

  @impl true
  def handle_info(:tick, state) do
    state = do_cycle(state)
    Process.send_after(self(), :tick, state.interval_ms)
    {:noreply, state}
  end

  @impl true
  def handle_cast(:tick_now, state), do: {:noreply, do_cycle(state)}

  @impl true
  def handle_call(:status, _from, state), do: {:reply, state, state}

  # --- one guarded cycle (a failure must never crash the tick) ---

  @doc false
  def do_cycle(state) do
    result = Scheduler.cycle(state.cycle_opts)
    Logger.info("MergeOrchestration.Ticker: cycle #{inspect(result.stats)}")
    %{state | last_run: DateTime.utc_now(), runs: state.runs + 1, last_stats: result.stats}
  rescue
    e ->
      Logger.warning("MergeOrchestration.Ticker: cycle failed #{inspect(e)}")
      %{state | last_run: DateTime.utc_now(), errors: [inspect(e) | state.errors]}
  end

  defp config(key, default) do
    :hypatia
    |> Application.get_env(:merge_orchestration, [])
    |> Keyword.get(key, default)
  end
end
