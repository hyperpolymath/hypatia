# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Hypatia.Web.GroovePlug — HTTP handler for groove discovery endpoints.
#
# Handles the /.well-known/groove path that Gossamer (and other
# groove-aware systems) probe to discover Hypatia's capabilities.
#
# Routes:
#   GET  /.well-known/groove         → JSON capability manifest (static)
#   POST /.well-known/groove/message → Receive message from consumer
#   GET  /.well-known/groove/recv    → Drain pending messages for consumer
#
# This plug is designed to be inserted early in the pipeline (before
# the router) so that groove discovery works regardless of other
# middleware configuration.
#
# The manifest advertises Hypatia's neurosymbolic scanning capability
# and declares its dependencies on octad-storage and static-analysis
# services for cross-ecosystem orchestration.

defmodule Hypatia.Web.GroovePlug do
  @moduledoc """
  Plug for groove discovery endpoints on port 9090.

  Serves the standard `/.well-known/groove` manifest that describes
  Hypatia's neurosymbolic scanning capabilities. Gossamer, BoJ, and
  other groove-aware consumers probe this endpoint to discover what
  Hypatia offers and what it consumes.

  ## Manifest fields

  - `service_id` — Unique identifier for this service (`"hypatia"`)
  - `groove_version` — Protocol version (`"1"`)
  - `service_version` — Semantic version of this service
  - `capabilities` — Map of capability names to their configuration (object, not array)
  - `consumes` — Capability IDs this service would benefit from
  - `endpoints` — Named endpoints this service exposes
  - `health` — Health check endpoint path
  - `applicability` — Scale levels supported

  ## Message passing

  The `/message` and `/recv` sub-paths support lightweight
  inter-service messaging without requiring a full message broker.
  Messages are held in an in-memory queue (ETS table) and drained
  on poll.
  """

  import Plug.Conn

  @behaviour Plug

  # -------------------------------------------------------------------
  # Manifest — static JSON describing Hypatia's groove surface
  # -------------------------------------------------------------------

  @groove_manifest %{
    groove_version: "1",
    service_id: "hypatia",
    service_version: "0.1.0",
    capabilities: %{
      neurosymbolic_scanning: %{
        type: "scanning",
        description:
          "Neurosymbolic CI/CD intelligence with rule-based scanning and VCL integration",
        protocol: "http",
        endpoint: "/api/v1/scan",
        requires_auth: false,
        panel_compatible: true
      }
    },
    consumes: ["octad-storage", "static-analysis"],
    endpoints: %{
      api: "http://localhost:9090/api/v1",
      health: "http://localhost:9090/health"
    },
    health: "/health",
    applicability: ["individual", "team"]
  }

  # Pre-encode the manifest at compile time so we never re-encode on
  # every request. Jason.encode!/1 is deterministic for maps with atom
  # keys, so the output is stable across compilations.
  @groove_manifest_json Jason.encode!(@groove_manifest)

  # -------------------------------------------------------------------
  # ETS table for the lightweight message queue
  # -------------------------------------------------------------------

  @message_table :hypatia_groove_messages

  @doc """
  Returns the groove manifest as an Elixir map.

  Useful for programmatic access from other Hypatia modules (e.g.
  the neural coordinator or fleet dispatcher) without going through
  HTTP.
  """
  @spec manifest() :: map()
  def manifest, do: @groove_manifest

  @doc """
  Returns the groove manifest as a pre-encoded JSON string.
  """
  @spec manifest_json() :: String.t()
  def manifest_json, do: @groove_manifest_json

  @doc """
  Push a message into the groove message queue.

  Messages are stored in an ETS table and drained by consumers via
  `pop_messages/0`. The table is created lazily on first write.
  """
  @spec push_message(map()) :: :ok
  def push_message(msg) when is_map(msg) do
    ensure_table()
    :ets.insert(@message_table, {System.monotonic_time(), msg})
    :ok
  end

  @doc """
  Drain all pending messages from the groove message queue.

  Returns a list of maps in insertion order (oldest first). The
  queue is emptied after this call.
  """
  @spec pop_messages() :: [map()]
  def pop_messages do
    ensure_table()

    messages =
      @message_table
      |> :ets.tab2list()
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(&elem(&1, 1))

    :ets.delete_all_objects(@message_table)
    messages
  end

  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @impl true
  def init(opts), do: opts

  @impl true
  # GET /.well-known/groove — Return the capability manifest.
  def call(
        %Plug.Conn{method: "GET", path_info: [".well-known", "groove"]} = conn,
        _opts
      ) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, @groove_manifest_json)
    |> halt()
  end

  # POST /.well-known/groove/message — Receive a message from a consumer.
  #
  # Plug.Parsers may or may not have consumed the body depending on
  # pipeline ordering, so we handle both parsed and raw bodies.
  def call(
        %Plug.Conn{method: "POST", path_info: [".well-known", "groove", "message"]} = conn,
        _opts
      ) do
    message =
      case conn.body_params do
        %Plug.Conn.Unfetched{} ->
          # Body not yet parsed (e.g. raw HTTP/1.0 from Zig groove client).
          case Plug.Conn.read_body(conn) do
            {:ok, body, _conn} -> Jason.decode(body)
            _ -> {:error, :no_body}
          end

        %{"_json" => json} when is_map(json) ->
          {:ok, json}

        params when is_map(params) and map_size(params) > 0 ->
          {:ok, params}

        _ ->
          {:error, :empty}
      end

    case message do
      {:ok, msg} ->
        push_message(msg)

        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, ~s({"ok":true}))
        |> halt()

      {:error, _reason} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(400, ~s({"ok":false,"error":"invalid JSON"}))
        |> halt()
    end
  end

  # GET /.well-known/groove/recv — Drain pending messages.
  def call(
        %Plug.Conn{method: "GET", path_info: [".well-known", "groove", "recv"]} = conn,
        _opts
      ) do
    messages =
      try do
        pop_messages()
      catch
        :exit, _ -> []
      end

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(messages))
    |> halt()
  end

  # Pass through everything else — this plug only handles groove paths.
  def call(conn, _opts), do: conn

  # -------------------------------------------------------------------
  # Internal helpers
  # -------------------------------------------------------------------

  # Lazily create the ETS table if it doesn't exist yet. This avoids
  # requiring the table to be created in Application.start/2.
  defp ensure_table do
    case :ets.whereis(@message_table) do
      :undefined ->
        :ets.new(@message_table, [:named_table, :public, :ordered_set])

      _ref ->
        :ok
    end
  end
end
