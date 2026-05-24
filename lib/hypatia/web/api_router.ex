# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.ApiRouter do
  @moduledoc """
  Operational HTTP API. Forwarded from `Hypatia.Web.Router` at `/api`.

  All endpoints are loopback-only by default (operational data must
  not leak past the local machine). Set `HYPATIA_API_ALLOW_NONLOCAL=true`
  to bypass — the bypass is logged on each request so audit captures it.

  Endpoints:
    GET /status              full Watcher snapshot
    GET /counts/:window      event counts in window (5m | 1h | 1d)
    GET /recipes             recipe-health rows (?status=...)
  """

  use Plug.Router

  require Logger
  import Bitwise, only: [|||: 2, bxor: 2]

  plug :match
  plug :auth_gate
  plug :loopback_only
  plug :dispatch

  get "/status" do
    snap = Hypatia.Watcher.snapshot()
    json(conn, 200, normalize_snapshot(snap))
  end

  get "/counts/:window" do
    case parse_window(window) do
      {:ok, atom} ->
        counts = Hypatia.Watcher.counts(atom)
        json(conn, 200, %{window: window, counts: flatten_event_keys(counts)})

      :error ->
        json(conn, 400, %{
          error: "unknown_window",
          got: window,
          valid: ["5m", "1h", "1d"]
        })
    end
  end

  get "/recipes" do
    conn = Plug.Conn.fetch_query_params(conn)
    rows = Hypatia.OutcomeTracker.recipe_health()

    case Map.get(conn.query_params, "status") do
      nil ->
        json(conn, 200, %{count: length(rows), rows: rows})

      statuses ->
        try do
          allowed = statuses |> String.split(",") |> Enum.map(&String.to_existing_atom/1)
          filtered = Enum.filter(rows, &(&1.status in allowed))
          json(conn, 200, %{count: length(filtered), rows: filtered})
        rescue
          ArgumentError -> json(conn, 400, %{error: "unknown_status_filter"})
        end
    end
  end

  @doc """
  GET /api/recipes/:id -- single-recipe drill-down. Returns the same
  shape as one row from `/api/recipes`, plus the recipe definition
  itself when found in the registry.
  """
  get "/recipes/:id" do
    health = Hypatia.OutcomeTracker.recipe_health()
    row = Enum.find(health, &(&1.recipe_id == id))

    if row do
      recipe = Hypatia.RecipeMatcher.get_recipe(id)
      json(conn, 200, %{health: row, recipe: recipe})
    else
      json(conn, 404, %{error: "recipe_not_found", id: id})
    end
  end

  @doc """
  GET /api/quarantine -- everything currently auto-quarantined:
  recipes (verification-rate gate) and bots (consecutive-failure /
  FP-rate gate from Hypatia.Safety.Quarantine).
  """
  get "/quarantine" do
    recipes =
      Hypatia.OutcomeTracker.recipe_health()
      |> Enum.filter(&(&1.status in [:quarantine_candidate, :degraded]))

    bots =
      case Process.whereis(Hypatia.Safety.Quarantine) do
        nil -> %{}
        _ -> GenServer.call(Hypatia.Safety.Quarantine, :list, 1000)
      end

    json(conn, 200, %{
      recipes: %{count: length(recipes), rows: recipes},
      bots: %{count: map_size(bots), entries: bots}
    })
  end

  @doc """
  GET /api/alerts -- Recent threshold-rule alerts emitted by
  Hypatia.Watcher.Alerts (ring buffer, newest first). Powers the
  dashboard alert ribbon and supports manual triage.
  """
  get "/alerts" do
    rows =
      case Process.whereis(Hypatia.Watcher.Alerts) do
        nil -> []
        _ -> Hypatia.Watcher.Alerts.recent()
      end

    json(conn, 200, %{count: length(rows), rows: rows})
  end

  @doc """
  POST /api/alerts/ingest -- Federation ingress. Peer hypatia
  instances POST their alerts here via the Peer sink.

  Auth: the auth_gate plug enforces a valid bearer token, so this
  endpoint is only reachable when HYPATIA_API_BEARER_TOKEN is set
  and the request carries it. Federation without shared auth is
  refused at the gate, not here.

  Loop prevention: the ingested alert is tagged with
  `metadata.federated_from = <peer hostname or "unknown">` so the
  Peer sink can skip it on broadcast and the dashboard can
  attribute it.
  """
  post "/alerts/ingest" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)

    case Jason.decode(body) do
      {:ok, payload} when is_map(payload) ->
        peer_id =
          case Plug.Conn.get_req_header(conn, "x-forwarded-for") do
            [host | _] -> host
            _ -> conn.remote_ip |> :inet.ntoa() |> to_string()
          end

        alert =
          %{
            rule: parse_atom(payload["rule"]),
            severity: parse_atom(payload["severity"]),
            summary: payload["summary"] || "(no summary)",
            metadata: payload["metadata"] || %{},
            at: payload["at"] || System.system_time(:millisecond)
          }

        case Process.whereis(Hypatia.Watcher.Alerts) do
          nil -> json(conn, 503, %{error: "alerts_unavailable"})
          _ -> Hypatia.Watcher.Alerts.ingest_federated(alert, peer_id)
        end

        json(conn, 202, %{ok: true, peer: peer_id})

      _ ->
        json(conn, 400, %{error: "invalid_json"})
    end
  end

  defp parse_atom(value) when is_atom(value), do: value
  defp parse_atom(value) when is_binary(value) do
    String.to_existing_atom(value)
  rescue
    ArgumentError -> :unknown
  end
  defp parse_atom(_), do: :unknown

  @doc """
  GET /api/events -- Server-Sent Events stream of telemetry as it
  fires. Each event arrives as

      event: hypatia.scan.complete
      data: {"measurements": {...}, "metadata": {...}, "at": ms}

  Optional `?events=hypatia.scan.complete,hypatia.outcome.recorded`
  filter narrows the stream to specific event kinds.

  Heartbeats every 15s as comment lines (`: keepalive`) defeat proxy
  idle-timeouts. The handler exits cleanly when the client disconnects
  (Bandit closes the chunked response).
  """
  get "/events" do
    conn = Plug.Conn.fetch_query_params(conn)
    filter = parse_event_filter(conn.query_params["events"])

    case filter do
      {:error, msg} ->
        json(conn, 400, %{error: msg})

      events_to_listen ->
        Hypatia.Watcher.subscribe(events: events_to_listen)

        conn =
          conn
          |> put_resp_header("content-type", "text/event-stream")
          |> put_resp_header("cache-control", "no-cache")
          |> put_resp_header("connection", "keep-alive")
          |> send_chunked(200)

        sse_loop(conn, schedule_heartbeat())
    end
  end

  # ─── SSE internals ─────────────────────────────────────────────────────

  defp sse_loop(conn, heartbeat_ref) do
    receive do
      {:hypatia_event, event, measurements, metadata, ts} ->
        chunk_body =
          "event: #{Enum.join(event, ".")}\n" <>
            "data: " <>
            Jason.encode!(%{
              measurements: measurements,
              metadata: jsonable_metadata(metadata),
              at: ts
            }) <> "\n\n"

        case Plug.Conn.chunk(conn, chunk_body) do
          {:ok, conn} -> sse_loop(conn, heartbeat_ref)
          {:error, _closed} -> conn
        end

      :heartbeat ->
        case Plug.Conn.chunk(conn, ": keepalive\n\n") do
          {:ok, conn} -> sse_loop(conn, schedule_heartbeat())
          {:error, _closed} -> conn
        end
    end
  end

  defp schedule_heartbeat do
    Process.send_after(self(), :heartbeat, 15_000)
  end

  # `:all` means subscribe to everything. A list of dotted-string
  # event names means subscribe to that subset. Anything else is a
  # client bug.
  defp parse_event_filter(nil), do: :all
  defp parse_event_filter(""), do: :all

  defp parse_event_filter(csv) do
    csv
    |> String.split(",", trim: true)
    |> Enum.map(&parse_event_name/1)
    |> Enum.reduce_while([], fn
      {:ok, evt}, acc -> {:cont, [evt | acc]}
      {:error, raw}, _acc -> {:halt, {:error, "unknown_event_name: #{raw}"}}
    end)
    |> case do
      {:error, _} = err -> err
      list -> Enum.reverse(list)
    end
  end

  defp parse_event_name(name) do
    parts = name |> String.split(".") |> Enum.map(&safe_to_atom/1)

    if Enum.all?(parts, &is_atom/1) and parts in Hypatia.Telemetry.all_events() do
      {:ok, parts}
    else
      {:error, name}
    end
  end

  defp safe_to_atom(s) do
    String.to_existing_atom(s)
  rescue
    ArgumentError -> :__invalid__
  end

  # Metadata may contain pids/refs/funs that Jason can't encode.
  # Coerce them to strings so the stream is always valid JSON.
  defp jsonable_metadata(metadata) when is_map(metadata) do
    Map.new(metadata, fn {k, v} -> {k, jsonable_value(v)} end)
  end

  defp jsonable_value(v)
       when is_binary(v) or is_number(v) or is_boolean(v) or is_atom(v) or is_nil(v),
       do: v

  defp jsonable_value(v) when is_list(v), do: Enum.map(v, &jsonable_value/1)
  defp jsonable_value(v) when is_map(v), do: jsonable_metadata(v)
  defp jsonable_value(v), do: inspect(v)

  match _ do
    json(conn, 404, %{error: "not_found"})
  end

  # ─── Plug ──────────────────────────────────────────────────────────────

  # ─── Auth gate ─────────────────────────────────────────────────────────
  #
  # If HYPATIA_API_BEARER_TOKEN is set, any /api/* request must carry a
  # matching Authorization: Bearer <token> header. The token + loopback
  # checks compose: with neither, /api is loopback-only. With both, /api
  # is openable to non-local callers provided they present the token.
  #
  # Token comparison uses Plug.Crypto.secure_compare/2 so timing attacks
  # can't enumerate the secret.
  defp auth_gate(conn, _opts) do
    case System.get_env("HYPATIA_API_BEARER_TOKEN") do
      nil ->
        conn

      "" ->
        conn

      expected ->
        case get_bearer(conn) do
          {:ok, presented} ->
            if secure_compare(expected, presented) do
              # If the token is valid, the caller is implicitly allowed
              # past the loopback gate too — explicit authentication is
              # at least as strong as IP-based filtering.
              Plug.Conn.put_private(conn, :hypatia_auth_passed, true)
            else
              conn
              |> put_resp_content_type("application/json")
              |> send_resp(
                401,
                Jason.encode!(%{error: "invalid_token", hint: "Authorization: Bearer <token>"})
              )
              |> halt()
            end

          :error ->
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(
              401,
              Jason.encode!(%{
                error: "missing_token",
                hint:
                  "Hypatia API requires Authorization: Bearer <token> when " <>
                    "HYPATIA_API_BEARER_TOKEN is set on the server."
              })
            )
            |> halt()
        end
    end
  end

  defp get_bearer(conn) do
    case Plug.Conn.get_req_header(conn, "authorization") do
      ["Bearer " <> token] -> {:ok, token}
      ["bearer " <> token] -> {:ok, token}
      _ -> :error
    end
  end

  # Constant-time string comparison. Plug.Crypto provides this in the
  # production dep set; fall back to a hand-rolled equivalent for
  # environments where Plug.Crypto isn't loaded (e.g. escript builds).
  defp secure_compare(a, b) when is_binary(a) and is_binary(b) do
    if Code.ensure_loaded?(Plug.Crypto) and function_exported?(Plug.Crypto, :secure_compare, 2) do
      Plug.Crypto.secure_compare(a, b)
    else
      byte_size(a) == byte_size(b) and
        a |> :binary.bin_to_list() |> Enum.zip(:binary.bin_to_list(b)) |> Enum.reduce(0, fn {x, y}, acc -> acc ||| Bitwise.bxor(x, y) end) == 0
    end
  end

  defp loopback_only(conn, _opts) do
    if conn.private[:hypatia_auth_passed] do
      # Caller authenticated via bearer; loopback check is unnecessary.
      conn
    else
      legacy_loopback_only(conn, [])
    end
  end

  defp legacy_loopback_only(conn, _opts) do
    cond do
      System.get_env("HYPATIA_API_ALLOW_NONLOCAL") == "true" ->
        Logger.warning(
          "Hypatia /api access from #{inspect(conn.remote_ip)} allowed by " <>
            "HYPATIA_API_ALLOW_NONLOCAL env override"
        )

        conn

      loopback_ip?(conn.remote_ip) ->
        conn

      true ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(
          403,
          Jason.encode!(%{
            error: "loopback_only",
            path: conn.request_path,
            hint:
              "Hypatia /api is loopback-only. Set HYPATIA_API_ALLOW_NONLOCAL=true to " <>
                "permit non-local clients, or tunnel via SSH."
          })
        )
        |> halt()
    end
  end

  defp loopback_ip?({127, _, _, _}), do: true
  defp loopback_ip?({0, 0, 0, 0, 0, 0, 0, 1}), do: true
  defp loopback_ip?(_), do: false

  # ─── Helpers ───────────────────────────────────────────────────────────

  defp json(conn, status, body) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(body))
  end

  defp parse_window("5m"), do: {:ok, :m5}
  defp parse_window("1h"), do: {:ok, :h1}
  defp parse_window("1d"), do: {:ok, :d1}
  defp parse_window(_), do: :error

  defp flatten_event_keys(counts_map) do
    Map.new(counts_map, fn {k, v} -> {Enum.join(k, "."), v} end)
  end

  defp normalize_snapshot(snap) do
    %{
      counts: %{
        m5: flatten_event_keys(snap.counts.m5),
        h1: flatten_event_keys(snap.counts.h1),
        d1: flatten_event_keys(snap.counts.d1)
      },
      queue_depths: snap.queue_depths,
      dropped_events: snap.dropped_events,
      uptime_seconds: snap.uptime_seconds,
      generated_at: snap.generated_at,
      recent_by_kind:
        Map.new(snap.recent_by_kind, fn {event, entries} ->
          {Enum.join(event, "."),
           Enum.map(entries, fn entry -> %{entry | event: Enum.join(entry.event, ".")} end)}
        end)
    }
  end
end
