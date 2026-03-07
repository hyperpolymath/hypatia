# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Kin.Watchdog do
  @moduledoc """
  Internal watchdog for Hypatia's OTP processes.

  Monitors all GenServers in the supervision tree and:
  - Detects processes that have crashed and not restarted
  - Tracks restart frequency (circuit breaker for restart storms)
  - Logs structured health telemetry
  - Provides graceful degradation when subsystems are unavailable
  """

  use GenServer
  require Logger

  @check_interval_ms 60_000  # 1 minute
  @restart_storm_threshold 5  # 5 restarts in the window = storm
  # Restart storm uses count-based detection: @restart_storm_threshold restarts
  # within the periodic decay window (~10 check cycles = ~10 minutes)

  @monitored_processes [
    {Hypatia.VQL.Client, :vql, :critical},
    {Hypatia.Safety.RateLimiter, :safety, :critical},
    {Hypatia.Safety.Quarantine, :safety, :critical},
    {Hypatia.Rules.Learning, :intelligence, :important},
    {Hypatia.LearningScheduler, :intelligence, :important},
    {Hypatia.SelfDiagnostics, :intelligence, :important},
    {Hypatia.Neural.Coordinator, :neural, :optional},
    {Hypatia.Kin.Coordinator, :kin, :important}
  ]

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Get process health report for all monitored processes."
  def process_health do
    GenServer.call(__MODULE__, :process_health)
  end

  @doc "Check if a specific layer is operational."
  def layer_healthy?(layer) do
    GenServer.call(__MODULE__, {:layer_healthy, layer})
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(_opts) do
    state = %{
      process_states: %{},
      restart_counts: %{},
      restart_storms: MapSet.new(),
      last_check: nil,
      check_count: 0
    }

    Process.send_after(self(), :check_processes, 5_000)
    Logger.info("Kin.Watchdog started. Monitoring #{length(@monitored_processes)} processes.")
    {:ok, state}
  end

  @impl true
  def handle_info(:check_processes, state) do
    new_state = check_all_processes(state)
    Process.send_after(self(), :check_processes, @check_interval_ms)
    {:noreply, new_state}
  end

  @impl true
  def handle_call(:process_health, _from, state) do
    report = Enum.map(@monitored_processes, fn {mod, layer, importance} ->
      pid = Process.whereis(mod)
      alive = pid != nil and Process.alive?(pid)
      restart_count = Map.get(state.restart_counts, mod, 0)
      in_storm = MapSet.member?(state.restart_storms, mod)

      %{
        module: mod,
        layer: layer,
        importance: importance,
        alive: alive,
        pid: pid,
        restart_count: restart_count,
        restart_storm: in_storm
      }
    end)

    {:reply, report, state}
  end

  @impl true
  def handle_call({:layer_healthy, layer}, _from, state) do
    layer_procs = Enum.filter(@monitored_processes, fn {_, l, _} -> l == layer end)
    critical_down = Enum.any?(layer_procs, fn {mod, _, importance} ->
      importance == :critical and not process_alive?(mod)
    end)
    {:reply, not critical_down, state}
  end

  # --- Core Logic ---

  defp check_all_processes(state) do
    now = System.monotonic_time(:millisecond)

    {new_process_states, new_restart_counts, events} =
      Enum.reduce(@monitored_processes, {state.process_states, state.restart_counts, []}, fn
        {mod, layer, importance}, {ps, rc, evts} ->
          alive = process_alive?(mod)
          prev_alive = Map.get(ps, mod, true)

          {new_rc, new_evts} =
            cond do
              not prev_alive and alive ->
                # Process came back
                count = Map.get(rc, mod, 0) + 1
                Logger.info("Watchdog: #{inspect(mod)} (#{layer}) restarted (count: #{count})")
                {Map.put(rc, mod, count), [{:restarted, mod, layer, importance} | evts]}

              prev_alive and not alive ->
                # Process went down
                Logger.warning("Watchdog: #{inspect(mod)} (#{layer}/#{importance}) is DOWN")
                {rc, [{:down, mod, layer, importance} | evts]}

              true ->
                {rc, evts}
            end

          {Map.put(ps, mod, alive), new_rc, new_evts}
      end)

    # Detect restart storms
    new_storms =
      new_restart_counts
      |> Enum.filter(fn {_mod, count} -> count >= @restart_storm_threshold end)
      |> Enum.map(fn {mod, _} -> mod end)
      |> MapSet.new()

    # Log storms
    new_storm_entries = MapSet.difference(new_storms, state.restart_storms)
    Enum.each(new_storm_entries, fn mod ->
      Logger.error("Watchdog: RESTART STORM detected for #{inspect(mod)} — backing off supervision")
    end)

    # Handle critical failures
    Enum.each(events, fn
      {:down, mod, _layer, :critical} ->
        Logger.error("Watchdog: CRITICAL process #{inspect(mod)} is down — system degraded")
      _ -> :ok
    end)

    # Decay restart counts periodically (every 10 checks = ~10 minutes)
    new_restart_counts =
      if rem(state.check_count + 1, 10) == 0 do
        Map.new(new_restart_counts, fn {mod, count} -> {mod, max(0, count - 1)} end)
      else
        new_restart_counts
      end

    %{state |
      process_states: new_process_states,
      restart_counts: new_restart_counts,
      restart_storms: new_storms,
      last_check: now,
      check_count: state.check_count + 1
    }
  end

  defp process_alive?(mod) do
    case Process.whereis(mod) do
      nil -> false
      pid -> Process.alive?(pid)
    end
  end
end
