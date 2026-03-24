# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Diagnostics.Monitor do
  @moduledoc """
  System-wide diagnostics monitor that proactively detects failures
  and alerts users before issues become critical.
  
  Features:
  - Component health monitoring
  - Performance drift detection
  - Automatic failure recovery attempts
  - User alerts via SSE
  - Persistent failure logging
  """

  use GenServer
  require Logger

  # Check interval (30 seconds)
  @check_interval 30_000

  # Failure thresholds
  @error_rate_threshold 0.1
  @buffer_warning_threshold 0.9

  # Recovery attempts
  @max_recovery_attempts 3

  # ====================================================================
  # Public API
  # ====================================================================

  @doc "Start the diagnostics monitor."
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc "Get current diagnostics status."
  def status do
    GenServer.call(__MODULE__, :status)
  end

  @doc "Force an immediate health check."
  def check_now do
    GenServer.cast(__MODULE__, :check_now)
  end

  # ====================================================================
  # GenServer Callbacks
  # ====================================================================

  @impl true
  def init(_) do
    schedule_checks()
    
    initial_state = %{
      components: [],
      failures: [],
      last_check: DateTime.utc_now(),
      recovery_attempts: %{},
      metrics_history: []
    }
    
    Logger.info("Diagnostics monitor started - checking every #{div(@check_interval, 1000)}s")
    {:ok, initial_state}
  end

  @impl true
  def handle_cast(:check_now, state) do
    {:noreply, do_health_check(state)}
  end

  @impl true
  def handle_info(:run_checks, state) do
    {:noreply, do_health_check(state)}
  end

  @impl true
  def handle_call(:status, _from, state) do
    {:reply, state, state}
  end

  # ====================================================================
  # Health Check Core
  # ====================================================================

  defp do_health_check(state) do
    try do
      components = check_all_components()
      failures = detect_failures(components)

      Process.send_after(self(), :run_checks, @check_interval)

      if failures != [] do
        notify_failures(failures)
        attempt_recoveries(failures, state)
      else
        %{state | components: components, failures: [], last_check: DateTime.utc_now()}
      end
    rescue
      error ->
        Logger.error("Diagnostics monitor error: #{inspect(error)}")
        Process.send_after(self(), :run_checks, @check_interval)
        state
    end
  end

  defp schedule_checks() do
    Process.send_after(self(), :run_checks, @check_interval)
  end

  # ====================================================================
  # Component Checks
  # ====================================================================

  defp check_all_components() do
    [
      check_pipeline(),
      check_learning(),
      check_neural(),
      check_kin(),
      check_diagnostics()
    ]
  end

  defp check_pipeline() do
    try do
      case Hypatia.Dispatch.Pipeline.get_metrics() do
        nil -> {:error, :pipeline_unresponsive}
        metrics -> {:ok, {:pipeline, metrics}}
      end
    rescue
      _ -> {:error, :pipeline_crashed}
    catch
      :exit, _ -> {:error, :pipeline_crashed}
    end
  end

  defp check_learning() do
    try do
      case GenServer.call(Hypatia.Rules.Learning, :get_stats, 1000) do
        nil -> {:error, :learning_unresponsive}
        stats -> {:ok, {:learning, stats}}
      end
    rescue
      _ -> {:error, :learning_crashed}
    end
  end

  defp check_neural() do
    try do
      case GenServer.call(Hypatia.Neural.Coordinator, :status, 1000) do
        nil -> {:error, :neural_unresponsive}
        status -> {:ok, {:neural, status}}
      end
    catch
      # Training cycles can take minutes — a timeout means busy, not crashed
      :exit, {:timeout, _} -> {:ok, {:neural, :training_in_progress}}
      :exit, _ -> {:error, :neural_crashed}
    rescue
      _ -> {:error, :neural_crashed}
    end
  end

  defp check_kin() do
    try do
      case GenServer.call(Hypatia.Kin.Coordinator, :ecosystem_health, 1000) do
        nil -> {:error, :kin_unresponsive}
        status -> {:ok, {:kin, status}}
      end
    rescue
      _ -> {:error, :kin_crashed}
    end
  end

  defp check_diagnostics() do
    {:ok, {:diagnostics, %{status: :healthy}}}
  end

  # ====================================================================
  # Failure Detection
  # ====================================================================

  defp detect_failures(components) do
    Enum.filter(components, fn
      # Hard failures
      {:error, reason} -> {:failure, reason}
      
      # Performance issues
      {:ok, {_, metrics}} when is_map(metrics) ->
        cond do
          Map.get(metrics, :dropped, 0) > Map.get(metrics, :dispatched, 1) * @error_rate_threshold ->
            {:failure, :high_error_rate}
          
          Map.get(metrics, :buffered, 0) > Map.get(metrics, :max_buffer, 1) * @buffer_warning_threshold ->
            {:failure, :buffer_overflow}
          
          true -> nil
        end
      
      _ -> nil
    end)
  end

  # ====================================================================
  # Failure Notification
  # ====================================================================

  defp notify_failures(failures) do
    Enum.each(failures, fn {:failure, reason} ->
      # Broadcast to users via SSE
      broadcast_alert(reason)
      
      # Log for persistence
      Logger.error("DIAGNOSTICS ALERT: #{inspect(reason)}")
      
      # Attempt recovery
      attempt_recovery(reason)
    end)
  end

  defp broadcast_alert(reason) do
    alert_type = case reason do
      :pipeline_unresponsive -> :critical
      :pipeline_crashed -> :critical
      :high_error_rate -> :warning
      :buffer_overflow -> :warning
      _ -> :error
    end

    Hypatia.Web.DispatchChannel.broadcast_event(%{
      type: :system_alert,
      data: %{
        severity: alert_type,
        component: "SystemDiagnostics",
        issue: "Component failure detected",
        details: inspect(reason),
        timestamp: DateTime.utc_now(),
        action: "Please check system logs and consider restarting affected components"
      }
    })
  end

  # ====================================================================
  # Automatic Recovery
  # ====================================================================

  defp attempt_recovery(reason) do
    case reason do
      :pipeline_unresponsive ->
        recover_pipeline()
      
      :pipeline_crashed ->
        recover_pipeline()
      
      :learning_unresponsive ->
        recover_learning()
      
      :learning_crashed ->
        recover_learning()
      
      :neural_unresponsive ->
        recover_neural()
      
      :neural_crashed ->
        recover_neural()
      
      _ ->
        :ok  # No automatic recovery for this failure type
    end
  end

  defp recover_pipeline() do
    Logger.info("Attempting pipeline recovery...")
    
    # Try graceful flush first
    case Hypatia.Dispatch.Pipeline.flush_buffer() do
      :ok ->
        Logger.info("Pipeline recovered via flush")
        :ok
      
      _ ->
        # If flush fails, try restart
        Logger.info("Pipeline flush failed, attempting restart...")
        
        case GenServer.stop(Hypatia.Dispatch.Pipeline, :normal, 5000) do
          :ok ->
            case Hypatia.Dispatch.Pipeline.start_link() do
              {:ok, _} ->
                Logger.info("Pipeline restarted successfully")
                :ok
              _ ->
                Logger.error("Pipeline restart failed")
                :error
            end
          _ ->
            Logger.error("Pipeline stop failed")
            :error
        end
    end
  end

  defp recover_learning() do
    Logger.info("Attempting learning system recovery...")

    try do
      _stats = GenServer.call(Hypatia.Rules.Learning, :get_stats, 1000)
      # If it responds, it's healthy
      :ok
    catch
      :exit, _ ->
        Logger.info("Learning system unresponsive, attempting restart...")

        try do
          GenServer.stop(Hypatia.Rules.Learning, :normal, 5000)
          case Hypatia.Rules.Learning.start_link() do
            {:ok, _} ->
              Logger.info("Learning system restarted successfully")
              :ok
            _ ->
              Logger.error("Learning system restart failed")
              :error
          end
        catch
          :exit, _ ->
            Logger.error("Learning system stop failed")
            :error
        end
    end
  end

  defp recover_neural() do
    Logger.info("Attempting neural coordinator recovery...")

    try do
      case GenServer.call(Hypatia.Neural.Coordinator, :health_report, 2000) do
        %{} ->
          Logger.info("Neural coordinator responding — healthy")
          :ok

        _ ->
          Logger.warning("Neural coordinator returned unexpected status")
          :ok
      end
    catch
      :exit, _ ->
        Logger.info("Neural coordinator unresponsive, attempting restart...")

        try do
          GenServer.stop(Hypatia.Neural.Coordinator, :normal, 5000)
          case Hypatia.Neural.Coordinator.start_link() do
            {:ok, _} ->
              Logger.info("Neural coordinator restarted successfully")
              :ok
            _ ->
              Logger.error("Neural coordinator restart failed")
              :error
          end
        catch
          :exit, _ ->
            Logger.error("Neural coordinator stop/restart failed")
            :error
        end
    end
  end

  defp attempt_recoveries(failures, state) do
    Enum.reduce(failures, state, fn {:failure, reason}, acc ->
      _recovery_result = attempt_recovery(reason)
      
      # Track recovery attempts
      attempt_count = Map.get(acc.recovery_attempts, reason, 0) + 1
      attempts = Map.put(acc.recovery_attempts, reason, attempt_count)
      
      # Give up after max attempts
      if attempt_count >= @max_recovery_attempts do
        Logger.warning("Max recovery attempts reached for: #{inspect(reason)}")
      end
      
      %{acc | recovery_attempts: attempts}
    end)
  end

end

# ====================================================================
# Application Integration
# ====================================================================

# Add to lib/application.ex:
# children = [
#   Hypatia.Diagnostics.Monitor,  # Add this line
#   ... other children
# ]