# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Safety.Quarantine do
  @moduledoc """
  Bot quarantine system for Hypatia.

  Bots that consistently fail are quarantined — they receive no new
  dispatches until manually reviewed or the quarantine expires.

  Quarantine triggers:
  - 5+ consecutive failures
  - False positive rate > 30% (over last 20 outcomes)
  - Trust score drops below 0.2

  Quarantine levels:
  - :soft — bot still receives report_only dispatches
  - :hard — bot receives nothing, all dispatches rerouted
  - :permanent — requires manual intervention to lift

  Auto-release:
  - :soft quarantine expires after 24 hours
  - :hard quarantine expires after 72 hours
  - :permanent never expires automatically
  """

  use GenServer
  require Logger

  @consecutive_failure_threshold 5
  @fp_rate_threshold 0.3
  @fp_window 20  # Last N outcomes
  # Future: minimum trust level for quarantined bots — @trust_floor 0.2
  @soft_duration_ms 24 * 60 * 60 * 1_000
  @hard_duration_ms 72 * 60 * 60 * 1_000
  @check_interval_ms 15 * 60 * 1_000  # Check every 15 min

  defstruct [
    quarantined: %{},  # bot_name => %{level, reason, since, until, outcomes}
    bot_outcomes: %{}, # bot_name => [{outcome, timestamp}, ...]
    reroute_rules: %{} # quarantined_bot => replacement_bot
  ]

  # --- GenServer API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    Process.send_after(self(), :check_expirations, @check_interval_ms)
    {:ok, %__MODULE__{}}
  end

  # --- Public API ---

  @doc "Check if a bot is quarantined. Returns :ok or {:quarantined, level, reason}"
  def check(bot_name) do
    GenServer.call(__MODULE__, {:check, bot_name})
  end

  @doc "Record a bot outcome (used to trigger auto-quarantine)"
  def record_outcome(bot_name, outcome) do
    GenServer.cast(__MODULE__, {:outcome, bot_name, outcome})
  end

  @doc "Manually quarantine a bot"
  def quarantine(bot_name, level, reason) do
    GenServer.cast(__MODULE__, {:quarantine, bot_name, level, reason})
  end

  @doc "Manually release a bot from quarantine"
  def release(bot_name) do
    GenServer.cast(__MODULE__, {:release, bot_name})
  end

  @doc "Get all quarantined bots"
  def list_quarantined do
    GenServer.call(__MODULE__, :list)
  end

  @doc "Get reroute target for a quarantined bot"
  def reroute_target(bot_name) do
    GenServer.call(__MODULE__, {:reroute, bot_name})
  end

  @doc "Set reroute rule: dispatches for quarantined bot go to replacement"
  def set_reroute(quarantined_bot, replacement_bot) do
    GenServer.cast(__MODULE__, {:set_reroute, quarantined_bot, replacement_bot})
  end

  # --- Callbacks ---

  def handle_call({:check, bot_name}, _from, state) do
    case Map.get(state.quarantined, bot_name) do
      nil -> {:reply, :ok, state}
      q -> {:reply, {:quarantined, q.level, q.reason}, state}
    end
  end

  def handle_call(:list, _from, state) do
    {:reply, state.quarantined, state}
  end

  def handle_call({:reroute, bot_name}, _from, state) do
    {:reply, Map.get(state.reroute_rules, bot_name), state}
  end

  def handle_cast({:outcome, bot_name, outcome}, state) do
    now = System.system_time(:millisecond)
    outcomes = Map.get(state.bot_outcomes, bot_name, [])
    updated = [{outcome, now} | outcomes] |> Enum.take(100)

    state = %{state | bot_outcomes: Map.put(state.bot_outcomes, bot_name, updated)}

    # Check auto-quarantine conditions
    state = check_auto_quarantine(state, bot_name, updated)

    {:noreply, state}
  end

  def handle_cast({:quarantine, bot_name, level, reason}, state) do
    now = System.system_time(:millisecond)
    duration = case level do
      :soft -> @soft_duration_ms
      :hard -> @hard_duration_ms
      :permanent -> nil
    end

    q = %{
      level: level,
      reason: reason,
      since: now,
      until: if(duration, do: now + duration, else: nil)
    }

    Logger.warning("Bot #{bot_name} QUARANTINED (#{level}): #{reason}")

    # Persist to ArangoDB if available
    if Process.whereis(Hypatia.Data.ArangoDB) do
      Hypatia.Data.ArangoDB.upsert("bots", bot_name, %{
        "quarantined" => true,
        "quarantine_reason" => reason,
        "quarantine_until" => q.until
      })
    end

    {:noreply, %{state | quarantined: Map.put(state.quarantined, bot_name, q)}}
  end

  def handle_cast({:release, bot_name}, state) do
    Logger.info("Bot #{bot_name} released from quarantine")

    if Process.whereis(Hypatia.Data.ArangoDB) do
      Hypatia.Data.ArangoDB.upsert("bots", bot_name, %{
        "quarantined" => false,
        "quarantine_reason" => nil,
        "quarantine_until" => nil
      })
    end

    {:noreply, %{state |
      quarantined: Map.delete(state.quarantined, bot_name),
      reroute_rules: Map.delete(state.reroute_rules, bot_name)
    }}
  end

  def handle_cast({:set_reroute, from, to}, state) do
    {:noreply, %{state | reroute_rules: Map.put(state.reroute_rules, from, to)}}
  end

  def handle_info(:check_expirations, state) do
    now = System.system_time(:millisecond)

    expired = state.quarantined
    |> Enum.filter(fn {_bot, q} -> q.until != nil and now >= q.until end)
    |> Enum.map(fn {bot, _q} -> bot end)

    Enum.each(expired, fn bot ->
      Logger.info("Bot #{bot} auto-released from quarantine (expired)")
    end)

    updated = Map.drop(state.quarantined, expired)
    reroutes = Map.drop(state.reroute_rules, expired)

    Process.send_after(self(), :check_expirations, @check_interval_ms)
    {:noreply, %{state | quarantined: updated, reroute_rules: reroutes}}
  end

  # --- Auto-Quarantine Logic ---

  defp check_auto_quarantine(state, bot_name, outcomes) do
    # Already quarantined? Skip
    if Map.has_key?(state.quarantined, bot_name) do
      state
    else
      cond do
        consecutive_failures(outcomes) >= @consecutive_failure_threshold ->
          do_auto_quarantine(state, bot_name, :hard,
            "#{@consecutive_failure_threshold} consecutive failures")

        false_positive_rate(outcomes) > @fp_rate_threshold ->
          do_auto_quarantine(state, bot_name, :soft,
            "False positive rate #{Float.round(false_positive_rate(outcomes) * 100, 1)}% > #{@fp_rate_threshold * 100}%")

        true ->
          state
      end
    end
  end

  defp do_auto_quarantine(state, bot_name, level, reason) do
    now = System.system_time(:millisecond)
    duration = case level do
      :soft -> @soft_duration_ms
      :hard -> @hard_duration_ms
      _ -> nil
    end

    q = %{level: level, reason: reason, since: now, until: if(duration, do: now + duration, else: nil)}
    Logger.warning("Bot #{bot_name} AUTO-QUARANTINED (#{level}): #{reason}")

    if Process.whereis(Hypatia.Data.ArangoDB) do
      Hypatia.Data.ArangoDB.record_anomaly(
        %{"bot" => bot_name, "action" => "auto_quarantine"},
        0.0, :quarantine, :warning)
    end

    %{state | quarantined: Map.put(state.quarantined, bot_name, q)}
  end

  defp consecutive_failures(outcomes) do
    outcomes
    |> Enum.take_while(fn {outcome, _ts} -> outcome in [:failure, "failure"] end)
    |> length()
  end

  defp false_positive_rate(outcomes) do
    recent = Enum.take(outcomes, @fp_window)
    if length(recent) < 5, do: 0.0, else: do_fp_rate(recent)
  end

  defp do_fp_rate(recent) do
    fps = Enum.count(recent, fn {o, _} -> o in [:false_positive, "false_positive"] end)
    fps / length(recent)
  end
end
