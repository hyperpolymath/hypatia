# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.TUI.Port do
  @moduledoc """
  Elixir Port wrapper for the Ada TUI binary.

  Launches `tui/obj/hypatia_tui` as an Erlang Port when `:tui_enabled`
  config is true (default false — the TUI is an optional operator tool,
  not required for pipeline operation).

  On each `:tick` (default every 10 s), fetches a live status snapshot
  from the Hypatia HTTP endpoint and sends it as a JSON line to the Ada
  process stdin. The Ada binary renders the dashboard and waits for the
  next snapshot.

  On supervisor shutdown, sends "EXIT\\n" to the Ada process before
  closing the Port so the Ada side gets a clean shutdown message.
  """

  use GenServer
  require Logger

  @default_tick_ms 10_000

  # ── Public API ────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def send_snapshot(snapshot) when is_map(snapshot) do
    GenServer.cast(__MODULE__, {:snapshot, snapshot})
  end

  # ── GenServer callbacks ───────────────────────────────────────────────

  @impl true
  def init(opts) do
    enabled = Application.get_env(:hypatia, :tui_enabled, false)
    tick_ms = Keyword.get(opts, :tick_ms, @default_tick_ms)

    state = %{port: nil, tick_ms: tick_ms, enabled: enabled}

    if enabled do
      {:ok, state, {:continue, :open_port}}
    else
      Logger.debug("TUI.Port: disabled (set :tui_enabled true to enable)")
      {:ok, state}
    end
  end

  @impl true
  def handle_continue(:open_port, state) do
    bin = resolve_bin()

    case File.exists?(bin) do
      true ->
        port =
          Port.open({:spawn_executable, bin}, [
            :binary,
            :use_stdio,
            :stderr_to_stdout,
            {:line, 4096}
          ])

        Logger.info("TUI.Port: opened Ada TUI at #{bin}")
        schedule_tick(state.tick_ms)
        {:noreply, %{state | port: port}}

      false ->
        Logger.warning("TUI.Port: Ada binary not found at #{bin} — build with `gprbuild -P tui/hypatia_tui.gpr`")
        {:noreply, state}
    end
  end

  @impl true
  def handle_cast({:snapshot, snapshot}, %{port: port} = state) when not is_nil(port) do
    send_json(port, snapshot)
    {:noreply, state}
  end

  def handle_cast({:snapshot, _}, state), do: {:noreply, state}

  @impl true
  def handle_info(:tick, %{port: port} = state) when not is_nil(port) do
    snapshot = build_snapshot()
    send_json(port, snapshot)
    schedule_tick(state.tick_ms)
    {:noreply, state}
  end

  def handle_info(:tick, state) do
    {:noreply, state}
  end

  # Port sends us output lines — log at debug level only.
  def handle_info({port, {:data, {:eol, line}}}, %{port: port} = state) do
    Logger.debug("TUI.Port ada: #{line}")
    {:noreply, state}
  end

  def handle_info({port, {:exit_status, code}}, %{port: port} = state) do
    Logger.info("TUI.Port: Ada TUI exited (status #{code})")
    {:noreply, %{state | port: nil}}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, %{port: port}) when not is_nil(port) do
    send(port, {self(), {:command, "EXIT\n"}})
    Port.close(port)
  end

  def terminate(_reason, _state), do: :ok

  # ── Helpers ───────────────────────────────────────────────────────────

  defp schedule_tick(ms), do: Process.send_after(self(), :tick, ms)

  defp send_json(port, map) do
    line = Jason.encode!(map) <> "\n"
    send(port, {self(), {:command, line}})
  end

  # Pull a live snapshot from Hypatia's own metrics. Falls back to zeros
  # if the HTTP endpoint is not reachable (TUI still shows the last render).
  defp build_snapshot do
    http_port = Application.get_env(:hypatia, :http_port, 9090)
    url = "http://127.0.0.1:#{http_port}/metrics/snapshot"

    case :httpc.request(:get, {String.to_charlist(url), []}, [{:timeout, 2000}], []) do
      {:ok, {{_, 200, _}, _, body}} ->
        case Jason.decode(to_string(body)) do
          {:ok, data} -> data
          _ -> fallback_snapshot()
        end

      _ ->
        fallback_snapshot()
    end
  end

  defp fallback_snapshot do
    %{
      "repos" => 0,
      "weak_points" => 0,
      "dispatched" => 0,
      "outcomes" => 0,
      "recipes" => 0,
      "confidence" => 0.0,
      "status" => "unreachable"
    }
  end

  defp resolve_bin do
    # Prefer the env override, then fall back to relative path from CWD.
    Application.get_env(:hypatia, :tui_bin_path) ||
      Path.join(File.cwd!(), "tui/obj/hypatia_tui")
  end
end
