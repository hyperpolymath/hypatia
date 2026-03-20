# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Web.DispatchChannel do
  @moduledoc """
  Server-Sent Events (SSE) channel for real-time dispatch monitoring.
  
  Provides event streaming for:
  - Dispatch status updates
  - Bot health changes
  - System metrics
  - Error notifications
  """

  use Phoenix.Channel

  alias Hypatia.{Dispatch.Pipeline, FleetDispatcher}
  alias Hypatia.Kin.{Contingency, Gate, Coordinator}

  # Client-side channel name
  @channel_name "dispatch:events"

  # Event types
  @event_types %{
    dispatch_started: "dispatch_started",
    dispatch_completed: "dispatch_completed",
    dispatch_failed: "dispatch_failed",
    bot_health_changed: "bot_health_changed",
    system_metrics: "system_metrics",
    error: "error"
  }

  # ====================================================================
  # Channel Lifecycle
  # ====================================================================

  @doc """
  Join the dispatch events channel.
  
  Subscribes to real-time dispatch events with optional filters.
  """
  def join("dispatch:events", _params, socket) do
    # Authenticate (no auth for now, but ready for future)
    # if authorized?(socket), do: :ok, else: :error

    # Subscribe to pipeline events
    Pipeline.subscribe_events(self())
    
    # Send initial system status
    push_system_status(socket)
    
    {:ok, socket}
  end

  def join(_channel, _params, _socket) do
    {:error, %{reason: "unknown channel"}}
  end

  @doc """
  Handle channel termination.
  """
  def terminate(_reason, socket) do
    # Unsubscribe from events
    Pipeline.unsubscribe_events(self())
    :ok
  end

  # ====================================================================
  # Event Handlers
  # ====================================================================

  @doc """
  Handle incoming dispatch events from the pipeline.
  """
  def handle_in("subscribe", %{"types" => event_types}, socket) do
    # Filter subscription to specific event types
    {:ok, %{socket | private: %{filtered_types: event_types}}}
  end

  @doc """
  Handle dispatch events from the pipeline.
  """
  def handle_event("dispatch", %{"event" => event}, socket) do
    # Filter if client specified event types
    if should_deliver?(event, socket), do: broadcast_event(event)
    {:noreply, socket}
  end

  @doc """
  Handle health check requests.
  """
  def handle_in("health_check", _params, socket) do
    push_system_status(socket)
    {:reply, :ok, socket}
  end

  # ====================================================================
  # Event Broadcasting
  # ====================================================================

  @doc """
  Broadcast an event to all subscribers.
  
  Called by dispatch pipeline when events occur.
  """
  def broadcast_event(%{type: type, data: data}) do
    event_payload = %{
      type: type,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      data: data
    }
    
    broadcast(@channel_name, "event", event_payload)
  end

  @doc """
  Broadcast dispatch started event.
  """
  def broadcast_dispatch_started(%{request_id: id, repo: repo, pattern_id: pattern} = event) do
    broadcast_event(%{
      type: @event_types.dispatch_started,
      data: Map.merge(%{
        request_id: id,
        repo: repo,
        pattern_id: pattern,
        status: "started"
      }, event)
    })
  end

  @doc """
  Broadcast dispatch completed event.
  """
  def broadcast_dispatch_completed(%{request_id: id, success: success} = event) do
    broadcast_event(%{
      type: if(success, do: @event_types.dispatch_completed, else: @event_types.dispatch_failed),
      data: Map.merge(%{
        request_id: id,
        success: success,
        status: if(success, do: "completed", else: "failed")
      }, event)
    })
  end

  @doc """
  Broadcast bot health change.
  """
  def broadcast_bot_health(bot_id, status, metrics) do
    broadcast_event(%{
      type: @event_types.bot_health_changed,
      data: %{
        bot_id: bot_id,
        status: status,
        metrics: metrics,
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
      }
    })
  end

  @doc """
  Broadcast system metrics.
  """
  def broadcast_system_metrics(metrics) do
    broadcast_event(%{
      type: @event_types.system_metrics,
      data: %{
        metrics: metrics,
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
      }
    })
  end

  # ====================================================================
  # Utility Functions
  # ====================================================================

  defp should_deliver?(event, socket) do
    case socket.assigns[:private] do
      %{filtered_types: types} when is_list(types) ->
        event_type = Map.get(event, "type") || @event_types.dispatch_started
        event_type in types
      
      _ ->
        true  # No filter, deliver all events
    end
  end

  defp push_system_status(socket) do
    metrics = Pipeline.get_metrics() || %{
      received: 0,
      dispatched: 0,
      buffered: 0,
      dropped: 0
    }
    
    health = if Contingency.available?(), do: "healthy", else: "degraded"
    
    status = %{
      system: "hypatia-dispatch",
      health: health,
      metrics: metrics,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      version: Application.spec(:hypatia, :vsn) || "unknown"
    }
    
    push(socket, "system_status", status)
  end

  # ====================================================================
  # Pipeline Integration
  # ====================================================================

  @doc """
  Callback from dispatch pipeline when event occurs.
  """
  def handle_pipeline_event(%{type: type, data: data}) do
    case type do
      :dispatch_started -> broadcast_dispatch_started(data)
      :dispatch_completed -> broadcast_dispatch_completed(data)
      :dispatch_failed -> broadcast_dispatch_completed(Map.put(data, :success, false))
      :bot_health -> apply(&broadcast_bot_health/3, data)
      :system_metrics -> broadcast_system_metrics(data)
      _ -> :ok
    end
  end

end

# ====================================================================
# Router Integration
# ====================================================================

# Add to your router.ex:
#
# scope "/api/v1/sse" do
#   get "/dispatch", Hypatia.Web.DispatchChannel, :sse_connection
#   get "/health", Hypatia.Web.DispatchChannel, :health_check
# end

# ====================================================================
# JavaScript Client Example
# ====================================================================

# const eventSource = new EventSource('/api/v1/sse/dispatch');
# 
# eventSource.onmessage = (event) => {
#   const data = JSON.parse(event.data);
#   console.log('Dispatch event:', data);
# };
# 
# eventSource.onerror = (error) => {
#   console.error('SSE error:', error);
# };

# ====================================================================
# Usage Examples
# ====================================================================

# Subscribe to all events:
# IEEx> Hypatia.Web.DispatchChannel.broadcast_event(%{type: "test", data: %{key: "value"}})
#
# Subscribe to specific events:
# {"event": "subscribe", "types": ["dispatch_completed", "dispatch_failed"]}
#
# Health check:
# {"event": "health_check"}

# ====================================================================
# Protocol Comparison
# ====================================================================

# GraphQL: Best for complex queries, flexible data fetching
# gRPC: Best for high-volume, performance-critical operations
# SSE: Best for real-time event streaming, live updates

# Together they provide comprehensive API coverage for all use cases.