# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Kin.Coordinator do
  @moduledoc """
  Kin Coordinator — higher-order orchestration of the Hypatia ecosystem.

  Maintains awareness of all kin siblings (panic-attacker, gitbot-fleet, auto-fix)
  via heartbeat polling. Provides:

  - **Kin graph**: who depends on whom, what capabilities are available
  - **Health aggregation**: unified view of entire ecosystem health
  - **Degraded mode**: adapts behaviour when siblings are down
  - **Pipeline gates**: enforces scan -> process -> dispatch ordering
  - **Self-healing**: attempts restart of failed siblings via systemd
  - **Escalation**: notifies user when components are persistently down
  """

  use GenServer
  require Logger

  alias Hypatia.Kin.Protocol

  @poll_interval_ms 5 * 60 * 1_000  # 5 minutes
  @initial_delay_ms 15_000  # 15 seconds after boot

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Get the full kin graph: all siblings with their status."
  def kin_graph do
    GenServer.call(__MODULE__, :kin_graph)
  end

  @doc "Get ecosystem health summary."
  def ecosystem_health do
    GenServer.call(__MODULE__, :ecosystem_health)
  end

  @doc "Check if a specific capability is available in the ecosystem."
  def capability_available?(capability) do
    GenServer.call(__MODULE__, {:capability_available, capability})
  end

  @doc "Check if the pipeline is ready for dispatch (scanner healthy, fleet reachable)."
  def dispatch_ready? do
    GenServer.call(__MODULE__, :dispatch_ready)
  end

  @doc "Get degraded mode status — what's working, what's falling back."
  def degraded_status do
    GenServer.call(__MODULE__, :degraded_status)
  end

  @doc "Force an immediate kin poll cycle."
  def force_poll do
    GenServer.cast(__MODULE__, :force_poll)
  end

  @doc "Record that a kin sibling just reported in (called by heartbeat writers)."
  def sibling_checked_in(kin_id, status_data) do
    GenServer.cast(__MODULE__, {:sibling_checkin, kin_id, status_data})
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(_opts) do
    state = %{
      kin_states: %{},
      last_poll: nil,
      ecosystem_status: :unknown,
      degraded_capabilities: [],
      poll_count: 0,
      escalation_history: [],
      self_healing_log: []
    }

    Process.send_after(self(), :poll_kin, @initial_delay_ms)
    Logger.info("Kin.Coordinator started. Polling siblings every #{div(@poll_interval_ms, 60_000)} min.")
    {:ok, state}
  end

  @impl true
  def handle_info(:poll_kin, state) do
    new_state = poll_all_siblings(state)
    Process.send_after(self(), :poll_kin, @poll_interval_ms)
    {:noreply, new_state}
  end

  @impl true
  def handle_call(:kin_graph, _from, state) do
    graph = build_kin_graph(state)
    {:reply, graph, state}
  end

  @impl true
  def handle_call(:ecosystem_health, _from, state) do
    health = %{
      status: state.ecosystem_status,
      siblings: map_sibling_summaries(state),
      degraded_capabilities: state.degraded_capabilities,
      last_poll: state.last_poll,
      poll_count: state.poll_count
    }
    {:reply, health, state}
  end

  @impl true
  def handle_call({:capability_available, capability}, _from, state) do
    cap_str = to_string(capability)
    available = not Enum.member?(state.degraded_capabilities, cap_str)
    {:reply, available, state}
  end

  @impl true
  def handle_call(:dispatch_ready, _from, state) do
    scanner_ok = sibling_healthy?(state, "panic-attacker")
    fleet_ok = sibling_healthy?(state, "gitbot-fleet")
    # Dispatch is ready if scanner has run recently and fleet is reachable
    # But we degrade gracefully: can dispatch from cached findings even if scanner is stale
    ready = scanner_ok or has_cached_findings?()
    {:reply, %{ready: ready, scanner: scanner_ok, fleet: fleet_ok, cached_fallback: not scanner_ok and ready}, state}
  end

  @impl true
  def handle_call(:degraded_status, _from, state) do
    degraded = Enum.map(state.kin_states, fn {kin_id, kin_state} ->
      case kin_state.status do
        :healthy -> nil
        status -> {kin_id, status, kin_state.fallback_mode}
      end
    end)
    |> Enum.reject(&is_nil/1)

    {:reply, degraded, state}
  end

  @impl true
  def handle_cast(:force_poll, state) do
    new_state = poll_all_siblings(state)
    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:sibling_checkin, kin_id, status_data}, state) do
    kin_state = %{
      status: :healthy,
      last_seen: DateTime.utc_now(),
      heartbeat: status_data,
      fallback_mode: nil,
      consecutive_failures: 0
    }
    new_kin_states = Map.put(state.kin_states, kin_id, kin_state)
    new_state = %{state | kin_states: new_kin_states}
    |> recompute_ecosystem_status()

    Logger.debug("Kin sibling #{kin_id} checked in: healthy")
    {:noreply, new_state}
  end

  # --- Core Logic ---

  defp poll_all_siblings(state) do
    now = DateTime.utc_now()

    kin_states =
      Protocol.known_kin()
      |> Enum.reject(fn {id, _} -> id == "hypatia" end)
      |> Enum.map(fn {kin_id, kin_meta} ->
        {kin_id, poll_sibling(kin_id, kin_meta, Map.get(state.kin_states, kin_id))}
      end)
      |> Map.new()

    # Write hypatia's own heartbeat
    write_self_heartbeat(kin_states)

    new_state = %{state |
      kin_states: kin_states,
      last_poll: now,
      poll_count: state.poll_count + 1
    }
    |> recompute_ecosystem_status()
    |> maybe_self_heal()
    |> maybe_escalate()

    if new_state.ecosystem_status != state.ecosystem_status do
      Logger.info("Ecosystem status changed: #{state.ecosystem_status} -> #{new_state.ecosystem_status}")
    end

    new_state
  end

  defp poll_sibling(kin_id, _kin_meta, prev_state) do
    case Protocol.read_heartbeat(kin_id) do
      {:ok, heartbeat} ->
        cond do
          Protocol.critically_stale?(heartbeat) ->
            %{
              status: :dead,
              last_seen: parse_timestamp(heartbeat),
              heartbeat: heartbeat,
              fallback_mode: fallback_for(kin_id),
              consecutive_failures: (prev_state && prev_state.consecutive_failures + 1) || 1
            }

          Protocol.stale?(heartbeat) ->
            %{
              status: :stale,
              last_seen: parse_timestamp(heartbeat),
              heartbeat: heartbeat,
              fallback_mode: fallback_for(kin_id),
              consecutive_failures: 0
            }

          Map.get(heartbeat, "status") == "error" ->
            %{
              status: :error,
              last_seen: parse_timestamp(heartbeat),
              heartbeat: heartbeat,
              fallback_mode: fallback_for(kin_id),
              consecutive_failures: (prev_state && prev_state.consecutive_failures + 1) || 1
            }

          true ->
            %{
              status: :healthy,
              last_seen: parse_timestamp(heartbeat),
              heartbeat: heartbeat,
              fallback_mode: nil,
              consecutive_failures: 0
            }
        end

      {:error, :no_heartbeat} ->
        %{
          status: :unknown,
          last_seen: nil,
          heartbeat: nil,
          fallback_mode: fallback_for(kin_id),
          consecutive_failures: (prev_state && prev_state.consecutive_failures + 1) || 0
        }

      {:error, _reason} ->
        %{
          status: :error,
          last_seen: nil,
          heartbeat: nil,
          fallback_mode: fallback_for(kin_id),
          consecutive_failures: (prev_state && prev_state.consecutive_failures + 1) || 1
        }
    end
  end

  defp fallback_for("panic-attacker"), do: :use_cached_findings
  defp fallback_for("gitbot-fleet"), do: :queue_to_pending
  defp fallback_for("auto-fix"), do: :manual_fix_only
  defp fallback_for(_), do: :none

  defp recompute_ecosystem_status(state) do
    statuses = Enum.map(state.kin_states, fn {_id, s} -> s.status end)

    ecosystem =
      cond do
        Enum.all?(statuses, &(&1 == :healthy)) -> :healthy
        Enum.any?(statuses, &(&1 == :dead)) -> :critical
        Enum.any?(statuses, &(&1 == :error)) -> :degraded
        Enum.any?(statuses, &(&1 in [:stale, :unknown])) -> :partial
        Enum.empty?(statuses) -> :initializing
        true -> :unknown
      end

    degraded_caps =
      state.kin_states
      |> Enum.filter(fn {_id, s} -> s.status not in [:healthy] end)
      |> Enum.flat_map(fn {kin_id, _s} ->
        case Map.get(Protocol.known_kin(), kin_id) do
          %{capabilities: caps} -> Enum.map(caps, &to_string/1)
          _ -> []
        end
      end)

    %{state | ecosystem_status: ecosystem, degraded_capabilities: degraded_caps}
  end

  defp maybe_self_heal(state) do
    healed =
      state.kin_states
      |> Enum.filter(fn {_id, s} -> s.status in [:dead, :error] and s.consecutive_failures >= 3 end)
      |> Enum.map(fn {kin_id, _s} ->
        result = attempt_heal(kin_id)
        {kin_id, result}
      end)

    if healed != [] do
      log_entries = Enum.map(healed, fn {id, result} ->
        Logger.info("Self-healing attempt for #{id}: #{inspect(result)}")
        %{kin_id: id, result: result, at: DateTime.utc_now()}
      end)

      %{state | self_healing_log: Enum.take(log_entries ++ state.self_healing_log, 50)}
    else
      state
    end
  end

  defp attempt_heal("auto-fix") do
    # Restart the systemd timer if it's dead
    case System.cmd("systemctl", ["--user", "is-active", "hypatia-autofix.timer"], stderr_to_stdout: true) do
      {output, 0} ->
        if String.trim(output) == "active", do: :already_active, else: restart_timer()
      _ ->
        restart_timer()
    end
  end

  defp attempt_heal(_kin_id) do
    # For CLI tools (panic-attacker, gitbot-fleet), we can't restart them —
    # they run on-demand. Just log and flag for escalation.
    :cannot_heal_cli_tool
  end

  defp restart_timer do
    case System.cmd("systemctl", ["--user", "restart", "hypatia-autofix.timer"], stderr_to_stdout: true) do
      {_, 0} -> :restarted
      {err, _} -> {:restart_failed, String.trim(err)}
    end
  end

  defp maybe_escalate(state) do
    # Escalate if any sibling has been failing for 5+ consecutive polls
    critical =
      state.kin_states
      |> Enum.filter(fn {_id, s} -> s.consecutive_failures >= 5 end)
      |> Enum.map(fn {kin_id, s} -> {kin_id, s.status, s.consecutive_failures} end)

    if critical != [] do
      Enum.each(critical, fn {kin_id, status, failures} ->
        Logger.error(
          "ESCALATION: Kin sibling #{kin_id} has been #{status} for #{failures} consecutive polls. " <>
          "Manual intervention may be required."
        )
      end)

      escalation_entries = Enum.map(critical, fn {kin_id, status, failures} ->
        %{kin_id: kin_id, status: status, failures: failures, at: DateTime.utc_now()}
      end)

      %{state | escalation_history: Enum.take(escalation_entries ++ state.escalation_history, 100)}
    else
      state
    end
  end

  defp write_self_heartbeat(kin_states) do
    sibling_summary =
      Enum.map(kin_states, fn {id, s} ->
        {id, %{"status" => to_string(s.status), "last_seen" => s.last_seen && DateTime.to_iso8601(s.last_seen)}}
      end)
      |> Map.new()

    Protocol.write_heartbeat(%{
      "status" => "healthy",
      "last_run" => %{
        "siblings_polled" => map_size(kin_states),
        "sibling_status" => sibling_summary
      },
      "errors" => []
    })
  end

  defp build_kin_graph(state) do
    nodes =
      Protocol.known_kin()
      |> Enum.map(fn {kin_id, meta} ->
        sibling_state = Map.get(state.kin_states, kin_id)
        status = if kin_id == "hypatia", do: :healthy, else: (sibling_state && sibling_state.status) || :unknown

        %{
          id: kin_id,
          role: meta.role,
          capabilities: meta.capabilities,
          status: status,
          fallback: sibling_state && sibling_state.fallback_mode
        }
      end)

    edges = [
      %{from: "panic-attacker", to: "hypatia", type: :feeds, label: "scan results"},
      %{from: "hypatia", to: "gitbot-fleet", type: :dispatches, label: "fix actions"},
      %{from: "auto-fix", to: "hypatia", type: :feeds, label: "fix outcomes"},
      %{from: "hypatia", to: "panic-attacker", type: :triggers, label: "scan requests"},
      %{from: "gitbot-fleet", to: "hypatia", type: :feeds, label: "dispatch outcomes"},
      %{from: "hypatia", to: "auto-fix", type: :schedules, label: "timer management"}
    ]

    %{nodes: nodes, edges: edges}
  end

  defp map_sibling_summaries(state) do
    Enum.map(state.kin_states, fn {kin_id, s} ->
      %{
        id: kin_id,
        status: s.status,
        last_seen: s.last_seen,
        age: s.heartbeat && Protocol.heartbeat_age(s.heartbeat),
        fallback: s.fallback_mode,
        failures: s.consecutive_failures
      }
    end)
  end

  defp sibling_healthy?(state, kin_id) do
    case Map.get(state.kin_states, kin_id) do
      %{status: :healthy} -> true
      _ -> false
    end
  end

  defp has_cached_findings? do
    scans_dir = Hypatia.Paths.scans()
    case File.ls(scans_dir) do
      {:ok, files} -> length(files) > 0
      _ -> false
    end
  end

  defp parse_timestamp(heartbeat) do
    case Map.get(heartbeat, "timestamp") do
      nil -> nil
      ts ->
        case DateTime.from_iso8601(ts) do
          {:ok, dt, _} -> dt
          _ -> nil
        end
    end
  end
end
