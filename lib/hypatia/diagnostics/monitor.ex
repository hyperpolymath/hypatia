# SPDX-License-Identifier: MPL-2.0
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
      observed = record_observations(state, components)
      schedule_checks()
      finish_observations(observed, &notify_failures/1, &attempt_recoveries/2)
    rescue
      error ->
        Logger.error("Diagnostics monitor error: #{inspect(error)}")
        schedule_checks()
        state
    end
  end

  # Periodic checks are config-gated so the test env can disable them:
  # a mid-suite tick whose recovery path GenServer.stops supervised
  # children (Pipeline/Learning/Coordinator) is invisible order-dependent
  # test breakage. check_now/0 still works when scheduling is off.
  defp schedule_checks() do
    if Application.get_env(:hypatia, :diagnostics_periodic_checks, true) do
      Process.send_after(self(), :run_checks, @check_interval)
    end
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

  # Probe the neural coordinator without treating a long-running training
  # cycle as a crash. Other exits and exceptions report a failed health check.
  @doc false
  def check_neural(server \\ Hypatia.Neural.Coordinator, timeout \\ 1000) do
    try do
      source = Hypatia.Neural.SourceStore.current()
      status = GenServer.call(server, {:status, source}, timeout)
      classify_neural_status(status)
    rescue
      _ -> {:error, :neural_crashed}
    catch
      # A busy process has not supplied a current-source health receipt.
      # Report uncertainty without granting automatic restart authority.
      :exit, {:timeout, _} -> {:error, {:neural_source_unavailable, :busy_or_unverified}}
      :exit, _ -> {:error, :neural_crashed}
    end
  end

  @doc false
  def classify_neural_status({:ok, status}) do
    if Hypatia.Neural.SourceStore.ready_report?(status) and Map.get(status, :status) == :running do
      {:ok, {:neural, status}}
    else
      {:error, {:neural_source_unavailable, status}}
    end
  end

  def classify_neural_status(other), do: {:error, {:neural_source_unavailable, other}}

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

  @doc false
  def detect_failures(components) do
    Enum.flat_map(components, fn
      # Hard failures
      {:error, reason} ->
        [{:failure, reason}]

      # Performance issues
      {:ok, {_, metrics}} when is_map(metrics) ->
        cond do
          Map.get(metrics, :dropped, 0) > Map.get(metrics, :dispatched, 1) * @error_rate_threshold ->
            [{:failure, :high_error_rate}]

          Map.get(metrics, :buffered, 0) >
              Map.get(metrics, :max_buffer, 1) * @buffer_warning_threshold ->
            [{:failure, :buffer_overflow}]

          true ->
            []
        end

      _ ->
        []
    end)
  end

  @doc false
  def record_observations(state, components) do
    %{state | components: components, failures: detect_failures(components), last_check: DateTime.utc_now()}
  end

  # Callbacks are explicit so regression tests can exercise notification
  # failure without contacting channels or invoking any recovery action.
  @doc false
  def finish_observations(observed, notify, recover) do
    if observed.failures == [] do
      observed
    else
      try do
        notify.(observed.failures)

        recoverable =
          Enum.reject(observed.failures, fn {:failure, reason} ->
            source_lifecycle_failure?(reason)
          end)

        if recoverable == [], do: observed, else: recover.(recoverable, observed)
      rescue
        error ->
          Logger.error("Diagnostics notification/recovery failed: #{inspect(error)}")
          observed
      catch
        :exit, reason ->
          Logger.error("Diagnostics notification/recovery exited: #{inspect(reason)}")
          observed
      end
    end
  end

  @doc false
  def source_lifecycle_failure?({:neural_source_unavailable, _}), do: true
  def source_lifecycle_failure?(_), do: false

  # ====================================================================
  # Failure Notification
  # ====================================================================

  defp notify_failures(failures) do
    Enum.each(failures, fn {:failure, reason} ->
      # Broadcast to users via SSE
      broadcast_alert(reason)

      # Log for persistence
      Logger.error("DIAGNOSTICS ALERT: #{inspect(reason)}")

      # Notification has no recovery side effect. finish_observations filters
      # source refusals before the sole attempt_recoveries path.
    end)
  end

  defp broadcast_alert(reason) do
    alert_type =
      case reason do
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
        # No automatic recovery for this failure type
        :ok
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
      source = Hypatia.Neural.SourceStore.current()
      report = GenServer.call(Hypatia.Neural.Coordinator, {:health_report, source}, 2000)

      if Hypatia.Neural.SourceStore.ready_report?(report) do
        Logger.info("Neural coordinator responding with its current source")
        :ok
      else
        # A lifecycle refusal requires an explicit operator decision. Do not
        # restart or relabel the cohort to turn that refusal into availability.
        Logger.warning("Neural recovery refused; inspect source lifecycle: #{inspect(report)}")
        :error
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
      if source_lifecycle_failure?(reason) do
        acc
      else
        _recovery_result = attempt_recovery(reason)

        # Preserve the existing recovery authority and accounting for other
        # failure types; source lifecycle refusals never enter this branch.
        attempt_count = Map.get(acc.recovery_attempts, reason, 0) + 1
        attempts = Map.put(acc.recovery_attempts, reason, attempt_count)

        if attempt_count >= @max_recovery_attempts do
          Logger.warning("Max recovery attempts reached for: #{inspect(reason)}")
        end

        %{acc | recovery_attempts: attempts}
      end
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
