# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.GraphQL do
  @moduledoc """
  GraphQL-shaped endpoint over the existing Watcher / OutcomeTracker
  surface. Intentionally minimal — supports a fixed set of named
  queries against documented fields without the full Absinthe
  machinery (which would add a runtime dep and significant boot
  surface for the modest gain over the REST surface).

  ## Supported queries

  Request body shape (POST /graphql with Content-Type: application/json):

      { "query": "{ <field>(<args>) { <subfield> ... } }" }

  Supported top-level fields:

      status                  → Hypatia.Watcher.snapshot/0
      health                  → quick liveness check
      recipes(status: String) → Hypatia.OutcomeTracker.recipe_health/1
      recipe(id: String!)     → single recipe drill-down
      alerts                  → Hypatia.Watcher.Alerts.recent/0
      quarantine              → recipes + bots auto-quarantined

  ## Response shape

  Follows the GraphQL response envelope:

      { "data": { ... }, "errors": [ ... ] }

  ## What this is NOT

  * Not a full Absinthe-backed implementation. No introspection
    (`__schema`, `__type`), no resolvers-of-resolvers, no schema
    federation.
  * Not a typed schema. Field selection is parsed loosely — every
    field you ask for is returned if it exists in the source map.
  * Not a query language. Inline strings, variables, fragments,
    aliases, and directives are NOT supported.

  ## When to use this vs Absinthe

  This module gives GraphQL-shape JSON for IDE plugins / dashboards
  that prefer the format. When the project genuinely needs full
  schema introspection or typed federation, swap in Absinthe — the
  endpoint URL (POST /graphql) can stay the same.
  """

  import Plug.Conn

  require Logger

  def init(opts), do: opts

  def call(%Plug.Conn{method: "POST"} = conn, _opts) do
    {:ok, body, conn} = read_body(conn)

    case Jason.decode(body) do
      {:ok, %{"query" => query}} when is_binary(query) ->
        result = execute(query)

        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(result))

      _ ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(
          400,
          Jason.encode!(%{
            errors: [%{message: "request body must be JSON with a 'query' string"}]
          })
        )
    end
  end

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(
      405,
      Jason.encode!(%{errors: [%{message: "method_not_allowed", allowed: "POST"}]})
    )
  end

  @doc """
  Execute a GraphQL-shaped query string. Public so tests can call it
  without going through the Plug pipeline.

  Returns a map with `"data"` and `"errors"` keys per the GraphQL
  spec, even on failure (so consumers always get a parseable shape).
  """
  def execute(query) when is_binary(query) do
    case parse_query(query) do
      {:ok, fields} ->
        data =
          Enum.reduce(fields, %{}, fn {name, args, _subfields}, acc ->
            Map.put(acc, name, resolve(name, args))
          end)

        %{"data" => data}

      {:error, msg} ->
        %{"data" => nil, "errors" => [%{"message" => msg}]}
    end
  end

  # ── Resolvers ────────────────────────────────────────────────────────

  defp resolve("status", _args) do
    case Process.whereis(Hypatia.Watcher) do
      nil -> %{"status" => "unavailable"}
      _ -> normalize(Hypatia.Watcher.snapshot())
    end
  end

  defp resolve("health", _args) do
    %{
      "status" => "ok",
      "service" => "hypatia",
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp resolve("recipes", args) do
    rows = Hypatia.OutcomeTracker.recipe_health()

    rows =
      case Map.get(args, "status") do
        nil ->
          rows

        statuses when is_binary(statuses) ->
          allowed =
            statuses
            |> String.split(",", trim: true)
            |> Enum.map(fn s ->
              try do
                String.to_existing_atom(s)
              rescue
                ArgumentError -> :__invalid__
              end
            end)

          Enum.filter(rows, &(&1.status in allowed))

        _ ->
          rows
      end

    %{"count" => length(rows), "rows" => Enum.map(rows, &normalize/1)}
  end

  defp resolve("recipe", args) do
    case Map.get(args, "id") do
      nil ->
        %{"error" => "missing_argument", "argument" => "id"}

      id ->
        health = Hypatia.OutcomeTracker.recipe_health()
        row = Enum.find(health, &(&1.recipe_id == id))

        if row do
          recipe = Hypatia.RecipeMatcher.get_recipe(id)
          %{"health" => normalize(row), "recipe" => recipe}
        else
          %{"error" => "recipe_not_found", "id" => id}
        end
    end
  end

  defp resolve("alerts", _args) do
    rows =
      case Process.whereis(Hypatia.Watcher.Alerts) do
        nil -> []
        _ -> Hypatia.Watcher.Alerts.recent()
      end

    %{"count" => length(rows), "rows" => Enum.map(rows, &normalize/1)}
  end

  defp resolve("quarantine", _args) do
    recipes =
      Hypatia.OutcomeTracker.recipe_health()
      |> Enum.filter(&(&1.status in [:quarantine_candidate, :degraded]))

    bots =
      case Process.whereis(Hypatia.Safety.Quarantine) do
        nil -> %{}
        _ -> GenServer.call(Hypatia.Safety.Quarantine, :list, 1000)
      end

    %{
      "recipes" => %{"count" => length(recipes), "rows" => Enum.map(recipes, &normalize/1)},
      "bots" => %{"count" => map_size(bots), "entries" => normalize(bots)}
    }
  end

  defp resolve(unknown, _args) do
    %{"error" => "unknown_field", "field" => unknown}
  end

  # ── Query parser (minimal) ───────────────────────────────────────────

  # Accepts forms like:
  #   "{ status }"
  #   "{ status alerts }"
  #   "{ recipes(status: \"degraded\") }"
  #   "{ recipe(id: \"recipe-foo\") }"
  # Loose tokenisation — enough for the small surface this module exposes.
  defp parse_query(query) do
    # Strip surrounding braces and whitespace.
    inner =
      query
      |> String.trim()
      |> String.replace_leading("query", "")
      |> String.trim()
      |> String.trim_leading("{")
      |> String.trim_trailing("}")
      |> String.trim()

    if inner == "" do
      {:error, "empty_query"}
    else
      case parse_fields(inner, []) do
        {:ok, []} -> {:error, "no_fields_selected"}
        {:ok, fields} -> {:ok, fields}
        {:error, msg} -> {:error, msg}
      end
    end
  end

  defp parse_fields("", acc), do: {:ok, Enum.reverse(acc)}

  defp parse_fields(remaining, acc) do
    case parse_one_field(String.trim(remaining)) do
      {:ok, field, rest} -> parse_fields(String.trim(rest), [field | acc])
      :done -> {:ok, Enum.reverse(acc)}
      {:error, msg} -> {:error, msg}
    end
  end

  defp parse_one_field("") do
    :done
  end

  defp parse_one_field(input) do
    # name [( args... )] [{ subfields }] [trailing comma]
    case Regex.run(~r/^([a-zA-Z_][a-zA-Z0-9_]*)\s*/, input) do
      [match, name] ->
        rest = String.slice(input, String.length(match)..-1//1)

        {args, rest} = parse_args(String.trim(rest))
        # Subfields are ignored for selection — every field returns its
        # full shape. Keep the parse robust against their presence.
        {_subfields, rest} = skip_subfield_block(String.trim(rest))

        rest = rest |> String.trim_leading() |> String.trim_leading(",")

        {:ok, {name, args, []}, rest}

      _ ->
        {:error, "expected_field_name_at: #{String.slice(input, 0..20)}"}
    end
  end

  defp parse_args("(" <> rest) do
    case Regex.run(~r/^([^)]*)\)/, rest) do
      [match, inside] ->
        remaining = String.slice(rest, String.length(match)..-1//1)
        {parse_arg_list(inside), remaining}

      _ ->
        {%{}, rest}
    end
  end

  defp parse_args(rest), do: {%{}, rest}

  defp parse_arg_list(inside) do
    inside
    |> String.split(",")
    |> Enum.reduce(%{}, fn token, acc ->
      case Regex.run(~r/^\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:\s*(.+?)\s*$/, token) do
        [_, key, value] ->
          Map.put(acc, key, strip_quotes(value))

        _ ->
          acc
      end
    end)
  end

  defp strip_quotes(<<"\"", rest::binary>>) do
    rest |> String.trim_trailing("\"")
  end

  defp strip_quotes(other), do: other

  defp skip_subfield_block("{" <> rest) do
    case Regex.run(~r/^([^}]*)\}/, rest) do
      [match, inside] ->
        remaining = String.slice(rest, String.length(match)..-1//1)
        {inside, remaining}

      _ ->
        {"", rest}
    end
  end

  defp skip_subfield_block(rest), do: {nil, rest}

  # ── Helpers ──────────────────────────────────────────────────────────

  # Normalise atoms / structs / nested maps into JSON-encodable forms.
  defp normalize(v) when is_binary(v) or is_number(v) or is_boolean(v) or is_nil(v), do: v
  defp normalize(v) when is_atom(v), do: Atom.to_string(v)
  defp normalize(v) when is_list(v), do: Enum.map(v, &normalize/1)

  defp normalize(v) when is_map(v) do
    Map.new(v, fn {k, val} -> {stringify_key(k), normalize(val)} end)
  end

  defp normalize(v), do: inspect(v)

  # Map keys come in three shapes that need flat string conversion:
  #   atoms        :uptime_seconds         → "uptime_seconds"
  #   strings      "uptime_seconds"        → "uptime_seconds"
  #   atom lists   [:hypatia, :scan, ...]  → "hypatia.scan.complete"
  # The third case comes from Watcher counters keyed by telemetry
  # event names, and breaks to_string/1 outright (List.to_string
  # requires charlists).
  defp stringify_key(k) when is_binary(k), do: k
  defp stringify_key(k) when is_atom(k), do: Atom.to_string(k)

  defp stringify_key(k) when is_list(k) do
    if Enum.all?(k, &is_atom/1) do
      Enum.map_join(k, ".", &Atom.to_string/1)
    else
      inspect(k)
    end
  end

  defp stringify_key(k), do: inspect(k)
end
