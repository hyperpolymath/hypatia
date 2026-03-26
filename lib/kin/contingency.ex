# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Kin.Contingency do
  @moduledoc """
  Kin Contingency — emergency procedures and graceful degradation.

  Handles system-wide failure scenarios:

  - **Full shutdown**: stop all bot operations immediately (panic button)
  - **Selective disable**: disable a specific bot or capability
  - **Drain mode**: let in-flight operations complete, accept no new ones
  - **Rollback wave**: coordinate reverting a batch of fixes across repos
  - **Isolation**: quarantine a misbehaving bot from the ecosystem

  ## Emergency levels

  1. `:advisory` — log warning, continue normally
  2. `:caution` — hold all auto-execute actions, allow review-tier
  3. `:freeze` — hold ALL actions, nothing proceeds without human approval
  4. `:shutdown` — immediately stop all bot operations
  """

  use GenServer
  require Logger

  @emergency_file "EMERGENCY.json"

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Get current emergency level."
  def level do
    GenServer.call(__MODULE__, :level)
  end

  @doc "Set emergency level. Requires reason."
  def set_level(new_level, reason) when new_level in [:normal, :advisory, :caution, :freeze, :shutdown] do
    GenServer.call(__MODULE__, {:set_level, new_level, reason})
  end

  @doc "Check if an action is permitted under current emergency level."
  def action_permitted?(dispatch_tier) do
    GenServer.call(__MODULE__, {:permitted, dispatch_tier})
  end

  @doc "Isolate a specific bot — all its actions are held."
  def isolate_bot(bot_id, reason) do
    GenServer.call(__MODULE__, {:isolate, bot_id, reason})
  end

  @doc "Restore a previously isolated bot."
  def restore_bot(bot_id) do
    GenServer.call(__MODULE__, {:restore, bot_id})
  end

  @doc "Get list of isolated bots."
  def isolated_bots do
    GenServer.call(__MODULE__, :isolated)
  end

  @doc "Check if a specific bot is isolated."
  def bot_isolated?(bot_id) do
    GenServer.call(__MODULE__, {:bot_isolated, bot_id})
  end

  @doc "Initiate a rollback wave across multiple repos."
  def rollback_wave(repo_list, reason) do
    GenServer.call(__MODULE__, {:rollback_wave, repo_list, reason})
  end

  @doc """
  Check whether the contingency system is running and available.

  Returns `true` if the GenServer is alive and responding, `false` otherwise.
  Used by the dispatch channel to report system health status.
  """
  def available? do
    case GenServer.whereis(__MODULE__) do
      nil -> false
      pid ->
        Process.alive?(pid) and
          match?({:ok, _}, safe_call(:level))
    end
  end

  @doc "Get contingency event log."
  def event_log do
    GenServer.call(__MODULE__, :event_log)
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(_opts) do
    state = %{
      level: :normal,
      level_reason: nil,
      level_set_at: nil,
      isolated_bots: %{},       # bot_id => %{reason, isolated_at}
      event_log: [],
      rollback_history: []
    }

    # Check for persisted emergency state
    state = load_persisted_state(state)

    if state.level != :normal do
      Logger.warning("Contingency: loaded persisted emergency level :#{state.level} — #{state.level_reason}")
    end

    Logger.info("Kin.Contingency started — emergency level: :#{state.level}")
    {:ok, state}
  end

  @impl true
  def handle_call(:level, _from, state) do
    {:reply, state.level, state}
  end

  @impl true
  def handle_call({:set_level, new_level, reason}, _from, state) do
    old_level = state.level

    if new_level != old_level do
      Logger.warning("Contingency: level changed :#{old_level} -> :#{new_level} — #{reason}")
    end

    new_state = %{state |
      level: new_level,
      level_reason: reason,
      level_set_at: DateTime.utc_now()
    }
    |> log_event(:level_change, %{from: old_level, to: new_level, reason: reason})

    persist_state(new_state)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:permitted, dispatch_tier}, _from, state) do
    permitted = case state.level do
      :normal -> true
      :advisory -> true
      :caution -> dispatch_tier != :auto_execute
      :freeze -> false
      :shutdown -> false
    end
    {:reply, permitted, state}
  end

  @impl true
  def handle_call({:isolate, bot_id, reason}, _from, state) do
    Logger.warning("Contingency: ISOLATING bot #{bot_id} — #{reason}")
    entry = %{reason: reason, isolated_at: DateTime.utc_now()}
    new_isolated = Map.put(state.isolated_bots, bot_id, entry)
    new_state = %{state | isolated_bots: new_isolated}
    |> log_event(:bot_isolated, %{bot_id: bot_id, reason: reason})

    persist_state(new_state)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:restore, bot_id}, _from, state) do
    if Map.has_key?(state.isolated_bots, bot_id) do
      Logger.info("Contingency: RESTORING bot #{bot_id}")
      new_isolated = Map.delete(state.isolated_bots, bot_id)
      new_state = %{state | isolated_bots: new_isolated}
      |> log_event(:bot_restored, %{bot_id: bot_id})

      persist_state(new_state)
      {:reply, :ok, new_state}
    else
      {:reply, {:error, :not_isolated}, state}
    end
  end

  @impl true
  def handle_call(:isolated, _from, state) do
    {:reply, state.isolated_bots, state}
  end

  @impl true
  def handle_call({:bot_isolated, bot_id}, _from, state) do
    {:reply, Map.has_key?(state.isolated_bots, bot_id), state}
  end

  @impl true
  def handle_call({:rollback_wave, repo_list, reason}, _from, state) do
    Logger.error("Contingency: ROLLBACK WAVE initiated for #{length(repo_list)} repos — #{reason}")

    results = Enum.map(repo_list, fn repo ->
      result = attempt_rollback(repo)
      {repo, result}
    end)

    entry = %{
      at: DateTime.utc_now(),
      repos: repo_list,
      reason: reason,
      results: results
    }

    new_state = %{state | rollback_history: Enum.take([entry | state.rollback_history], 50)}
    |> log_event(:rollback_wave, %{repos: length(repo_list), reason: reason})

    {:reply, results, new_state}
  end

  @impl true
  def handle_call(:event_log, _from, state) do
    {:reply, Enum.take(state.event_log, 100), state}
  end

  # --- Internal ---

  defp attempt_rollback(repo) do
    # Delegate to the batch rollback system
    try do
      Hypatia.Safety.BatchRollback.rollback(repo, "contingency wave")
    rescue
      e -> {:error, Exception.message(e)}
    catch
      :exit, reason -> {:error, inspect(reason)}
    end
  end

  defp log_event(state, event_type, data) do
    entry = Map.merge(data, %{
      event: event_type,
      at: DateTime.utc_now()
    })
    %{state | event_log: Enum.take([entry | state.event_log], 500)}
  end

  defp persist_state(state) do
    kin_dir = Hypatia.Kin.Protocol.kin_dir()
    File.mkdir_p!(kin_dir)
    path = Path.join(kin_dir, @emergency_file)

    data = %{
      "level" => to_string(state.level),
      "reason" => state.level_reason,
      "set_at" => state.level_set_at && DateTime.to_iso8601(state.level_set_at),
      "isolated_bots" => Enum.map(state.isolated_bots, fn {id, info} ->
        %{"bot_id" => id, "reason" => info.reason, "isolated_at" => DateTime.to_iso8601(info.isolated_at)}
      end)
    }

    case Jason.encode(data, pretty: true) do
      {:ok, json} -> File.write!(path, json <> "\n")
      _ -> :ok
    end
  end

  defp load_persisted_state(state) do
    path = Path.join(Hypatia.Kin.Protocol.kin_dir(), @emergency_file)

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} ->
            level = String.to_existing_atom(Map.get(data, "level", "normal"))
            isolated =
              data
              |> Map.get("isolated_bots", [])
              |> Enum.map(fn entry ->
                {entry["bot_id"], %{
                  reason: entry["reason"],
                  isolated_at: parse_dt(entry["isolated_at"])
                }}
              end)
              |> Map.new()

            %{state |
              level: level,
              level_reason: Map.get(data, "reason"),
              level_set_at: parse_dt(Map.get(data, "set_at")),
              isolated_bots: isolated
            }
          _ -> state
        end
      _ -> state
    end
  end

  defp parse_dt(nil), do: nil
  defp parse_dt(ts) do
    case DateTime.from_iso8601(ts) do
      {:ok, dt, _} -> dt
      _ -> nil
    end
  end

  defp safe_call(msg) do
    try do
      {:ok, GenServer.call(__MODULE__, msg, 2_000)}
    catch
      :exit, _ -> :error
    end
  end
end
