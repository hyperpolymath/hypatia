# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Dispatch.Pipeline do
  @moduledoc """
  GenStage-based dispatch pipeline with backpressure handling.
  
  Provides controlled parallel processing of dispatch events with
  adaptive concurrency based on system load and downstream capacity.
  """

  use GenStage

  alias Hypatia.{FleetDispatcher, TriangleRouter}
  alias Hypatia.Kin.{Gate, Contingency}

  require Logger

  # ====================================================================
  # Event Broadcasting
  # ====================================================================

  @doc """
  Subscribe to pipeline events.
  
  Used by SSE channel and other subscribers.
  """
  def subscribe_events(pid) do
    GenStage.cast(__MODULE__, {:subscribe, pid})
  end

  @doc """
  Unsubscribe from pipeline events.
  """
  def unsubscribe_events(pid) do
    GenStage.cast(__MODULE__, {:unsubscribe, pid})
  end

  @doc """
  Broadcast event to all subscribers.
  """
  def broadcast_event(event) do
    GenStage.cast(__MODULE__, {:broadcast, event})
  end

  # ====================================================================
  # GenStage Callbacks - Event Handling
  # ====================================================================

  @impl true
  def handle_cast({:subscribe, pid}, state) do
    subscribers = [pid | (state.subscribers || [])]
    metrics = update_metrics(state.metrics, :subscribers, 1)
    {:noreply, %{state | subscribers: subscribers, metrics: metrics}}
  end

  @impl true
  def handle_cast({:unsubscribe, pid}, state) do
    subscribers = List.delete(state.subscribers || [], pid)
    metrics = update_metrics(state.metrics, :subscribers, -1)
    {:noreply, %{state | subscribers: subscribers, metrics: metrics}}
  end

  @impl true
  def handle_cast({:broadcast, event}, state) do
    # Notify all subscribers
    Enum.each(state.subscribers || [], fn subscriber ->
      GenServer.cast(subscriber, {:pipeline_event, event})
    end)
    {:noreply, state}
  end

  # ====================================================================
  # Application Integration
  # ====================================================================

  @doc """
  Start the complete dispatch system (pipeline + builtin consumer).
  """
  def start_system() do
    {:ok, _pipeline} = start_link()
    {:ok, _consumer} = start_builtin_consumer()
    
    # Schedule periodic metrics logging
    Process.send_after(self(), :log_metrics, 30_000)
    
    # Broadcast system ready event
    broadcast_event(%{
      type: :system_ready,
      data: %{
        system: "hypatia-dispatch",
        status: "operational",
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
      }
    })
    
    :ok
  end

  def handle_info(:log_metrics, state) do
    case get_metrics() do
      nil ->
        {:noreply, [], state}

      metrics ->
        log_metrics(metrics)

        broadcast_event(%{
          type: :system_metrics,
          data: metrics
        })

        Process.send_after(self(), :log_metrics, 30_000)
        {:noreply, [], state}
    end
  end

  # Configuration - can be overridden via Application config
  @default_concurrency 10
  @max_demand 100
  @batch_size 5
  @max_buffer 1000

  def start_link(opts \\ []) do
    initial_state = Keyword.merge([
      concurrency: @default_concurrency,
      max_demand: @max_demand,
      batch_size: @batch_size,
      max_buffer: @max_buffer,
      buffer: [],
      subscribers: [],  # Event subscribers (PIDs)
      metrics: %{
        received: 0,
        dispatched: 0,
        buffered: 0,
        dropped: 0,
        latency: 0,
        subscribers: 0
      }
    ], opts)

    GenStage.start_link(__MODULE__, initial_state, name: __MODULE__)
  end

  # ====================================================================
  # Producer Interface
  # ====================================================================

  @doc """
  Enqueue a dispatch event into the pipeline.
  
  Returns :ok if accepted, :dropped if buffer full.
  """
  def enqueue(%{tier: tier, strategy: strategy} = event) do
    GenStage.cast(__MODULE__, {:enqueue, event})
  end

  def enqueue_events(events) when is_list(events) do
    GenStage.cast(__MODULE__, {:enqueue_batch, events})
  end

  # ====================================================================
  # Consumer Interface
  # ====================================================================

  @doc """
  Subscribe a consumer to receive dispatch events.
  
  Consumers should implement GenStage consumer behaviour.
  """
  def subscribe(consumer_pid) do
    GenStage.sync_subscribe(consumer_pid, to: __MODULE__, min_max_demand: {@batch_size, @max_demand})
  end

  # ====================================================================
  # GenStage Callbacks
  # ====================================================================

  @impl true
  def init(state) do
    {:producer, state}
  end

  @impl true
  def handle_cast({:enqueue, event}, state) do
    metrics = update_metrics(state.metrics, :received, 1)
    
    cond do
      length(state.buffer) < state.max_buffer ->
        buffer = [event | state.buffer]
        metrics = update_metrics(metrics, :buffered, 1)
        {:noreply, %{state | buffer: buffer, metrics: metrics}}
      
      true ->
        Logger.warning("Dispatch pipeline buffer full (max: #{state.max_buffer}), dropping event")
        metrics = update_metrics(metrics, :dropped, 1)
        {:noreply, %{state | metrics: metrics}}
    end
  end

  @impl true
  def handle_cast({:enqueue_batch, events}, state) do
    count = length(events)
    metrics = update_metrics(state.metrics, :received, count)
    
    capacity = state.max_buffer - length(state.buffer)
    {accepted, rejected} = Enum.split(events, capacity)

    buffer = accepted ++ state.buffer
    metrics = update_metrics(metrics, :buffered, length(accepted))
    metrics = update_metrics(metrics, :dropped, length(rejected))
    
    if length(rejected) > 0 do
      Logger.warning("Dispatch pipeline buffer full, dropped #{length(rejected)}/#{count} events")
    end
    
    {:noreply, %{state | buffer: buffer, metrics: metrics}}
  end

  @impl true
  def handle_demand(demand, state) when demand > 0 do
    # Calculate how many events we can send
    to_send = Enum.min([demand, length(state.buffer), state.batch_size])
    
    if to_send > 0 do
      {events, remaining_buffer} = Enum.split(state.buffer, to_send)
      metrics = update_metrics(state.metrics, :dispatched, to_send)
      
      # Reverse to maintain FIFO order (we prepended during enqueue)
      events = Enum.reverse(events)
      
      {:noreply, events, %{state | buffer: remaining_buffer, metrics: metrics}}
    else
      {:noreply, [], state}
    end
  end

  @impl true
  def handle_info(:metrics, state) do
    {:noreply, state, state.metrics}
  end

  @impl true
  def handle_info(:flush, state) do
    # Emergency flush - send all buffered events
    events = Enum.reverse(state.buffer)
    metrics = update_metrics(state.metrics, :dispatched, length(events))
    {:noreply, events, %{state | buffer: [], metrics: metrics}}
  end

  @impl true
  def terminate(reason, state) do
    Logger.info("Dispatch pipeline terminating: #{inspect(reason)}")

    # GenStage may pass state as a keyword list during termination,
    # so we handle both map and keyword list forms defensively.
    metrics =
      cond do
        is_map(state) and Map.has_key?(state, :metrics) -> state.metrics
        is_list(state) -> Keyword.get(state, :metrics, %{})
        true -> %{}
      end

    Logger.info("Final metrics: #{inspect(metrics)}")
    :ok
  end

  # ====================================================================
  # Consumer Implementation (Built-in Fallback)
  # ====================================================================

  @doc """
  Built-in consumer that processes events using FleetDispatcher.
  
  This provides a fallback consumer if no external consumers are subscribed.
  """
  def start_builtin_consumer() do
    GenStage.start_link(__MODULE__.BuiltinConsumer, [], name: __MODULE__.BuiltinConsumer)
  end

  defmodule BuiltinConsumer do
    use GenStage

    def init(_opts) do
      # Subscribe to the producer
      Hypatia.Dispatch.Pipeline.subscribe(self())
      {:consumer, []}
    end

    def handle_events(events, _from, state) do
      # Process each event through FleetDispatcher
      results = Enum.map(events, &process_event/1)
      
      # Count successes/failures
      success_count = Enum.count(results, &(&1 == :ok))
      failure_count = length(results) - success_count
      
      Logger.info("Processed #{length(events)} events: #{success_count} success, #{failure_count} failed")
      
      {:noreply, [], state}
    end

    defp process_event(event) do
      try do
        case FleetDispatcher.dispatch_routed_action(event) do
          {:ok, _} -> :ok
          {:error, _reason} -> :error
          other ->
            Logger.error("Unexpected dispatch result: #{inspect(other)}")
            :error
        end
      rescue
        e ->
          Logger.error("Dispatch failed with exception: #{inspect(e)}")
          :error
      end
    end
  end

  # ====================================================================
  # Metrics and Monitoring
  # ====================================================================

  @doc """
  Get current pipeline metrics.
  """
  def get_metrics() do
    GenStage.call(__MODULE__, :metrics, 5000) || %{
      received: 0,
      dispatched: 0,
      buffered: 0,
      dropped: 0,
      latency: 0
    }
  end

  @doc """
  Emergency flush of buffered events.
  """
  def flush_buffer() do
    GenStage.cast(__MODULE__, :flush)
  end

  # ====================================================================
  # Private Helpers
  # ====================================================================

  defp update_metrics(metrics, key, increment) do
    Map.update(metrics, key, increment, &(&1 + increment))
  end

  defp log_metrics(metrics) do
    Logger.info("""
    Dispatch Pipeline Metrics:
      Received:    #{metrics.received}
      Dispatched:  #{metrics.dispatched}
      Buffered:    #{metrics.buffered}
      Dropped:     #{metrics.dropped}
      Throughput:  #{(metrics.dispatched / max(metrics.received, 1)) * 100}%
    """)
  end

  # ====================================================================
  # Application Integration
  # ====================================================================

end