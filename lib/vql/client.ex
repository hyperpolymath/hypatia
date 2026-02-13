# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.Client do
  @moduledoc """
  VQL Client for Hypatia.

  Provides VQL query parsing and execution against the federated data layer
  (verisimdb-data flat files + ArangoDB graph queries). Uses a built-in Elixir
  parser derived from VeriSim's VQLBridge, so no external Deno/Node process needed.

  ## Architecture

      VQL String ──► Client.parse/1 (built-in Elixir parser)
                        │
                        ▼
                     Parsed AST
                        │
                  ┌─────┴─────┐
                  ▼           ▼
           FileExecutor   ArangoDB
           (flat files)   (graph queries)
                  └─────┬─────┘
                        ▼
                  Merged Results

  ## Usage

      {:ok, ast} = Hypatia.VQL.Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 10")
      {:ok, results} = Hypatia.VQL.Client.execute(ast)

      # Or combined:
      {:ok, results} = Hypatia.VQL.Client.query("SELECT DOCUMENT FROM STORE scans")
  """

  use GenServer
  require Logger

  @default_timeout 10_000

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Parse a VQL query string into an AST."
  def parse(query_string) do
    GenServer.call(__MODULE__, {:parse, query_string}, @default_timeout)
  end

  @doc "Execute a pre-parsed VQL AST."
  def execute(ast, opts \\ []) do
    GenServer.call(__MODULE__, {:execute, ast, opts}, @default_timeout)
  end

  @doc "Parse and execute a VQL query in one call."
  def query(query_string, opts \\ []) do
    GenServer.call(__MODULE__, {:query, query_string, opts}, @default_timeout)
  end

  @doc "Get query statistics and cache info."
  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    state = %{
      query_count: 0,
      cache: %{},
      cache_ttl: 60_000,
      last_cache_clean: System.monotonic_time(:millisecond)
    }

    Logger.info("VQL Client initialized (built-in Elixir parser)")
    {:ok, state}
  end

  @impl true
  def handle_call({:parse, query_string}, _from, state) do
    result = parse_vql(query_string)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:execute, ast, opts}, _from, state) do
    result = Hypatia.VQL.FileExecutor.execute(ast, opts)
    {:reply, result, %{state | query_count: state.query_count + 1}}
  end

  @impl true
  def handle_call({:query, query_string, opts}, _from, state) do
    # Check cache first
    cache_key = :erlang.phash2({query_string, opts})
    now = System.monotonic_time(:millisecond)

    case Map.get(state.cache, cache_key) do
      {result, ts} when now - ts < state.cache_ttl ->
        {:reply, result, state}

      _ ->
        result =
          with {:ok, ast} <- parse_vql(query_string) do
            Hypatia.VQL.FileExecutor.execute(ast, opts)
          end

        cache = maybe_clean_cache(state.cache, now, state.last_cache_clean, state.cache_ttl)
        cache = Map.put(cache, cache_key, {result, now})

        {:reply, result, %{state |
          query_count: state.query_count + 1,
          cache: cache,
          last_cache_clean: if(cache != state.cache, do: now, else: state.last_cache_clean)
        }}
    end
  end

  @impl true
  def handle_call(:stats, _from, state) do
    {:reply, %{
      query_count: state.query_count,
      cache_size: map_size(state.cache),
      cache_ttl_ms: state.cache_ttl
    }, state}
  end

  # ---------------------------------------------------------------------------
  # Built-in VQL Parser (derived from VeriSim.Query.VQLBridge)
  # ---------------------------------------------------------------------------

  defp parse_vql(query_string) do
    query_string = String.trim(query_string)

    with {:ok, tokens} <- tokenize(query_string),
         {:ok, ast} <- parse_tokens(tokens) do
      {:ok, ast}
    end
  end

  defp tokenize(input) do
    # Handle quoted strings specially
    {tokens, _} = tokenize_chars(String.graphemes(input), [], [], false)
    {:ok, tokens}
  end

  defp tokenize_chars([], current, tokens, _in_quotes) do
    token = IO.iodata_to_binary(Enum.reverse(current))
    final = if token == "", do: tokens, else: [token | tokens]
    {Enum.reverse(final), []}
  end

  defp tokenize_chars(["\"" | rest], current, tokens, false) do
    token = IO.iodata_to_binary(Enum.reverse(current))
    tokens = if token == "", do: tokens, else: [token | tokens]
    tokenize_chars(rest, [], tokens, true)
  end

  defp tokenize_chars(["\"" | rest], current, tokens, true) do
    quoted = IO.iodata_to_binary(Enum.reverse(current))
    tokenize_chars(rest, [], [{:quoted, quoted} | tokens], false)
  end

  defp tokenize_chars([" " | rest], current, tokens, false) do
    token = IO.iodata_to_binary(Enum.reverse(current))
    tokens = if token == "", do: tokens, else: [token | tokens]
    tokenize_chars(rest, [], tokens, false)
  end

  defp tokenize_chars(["," | rest], current, tokens, false) do
    token = IO.iodata_to_binary(Enum.reverse(current))
    tokens = if token == "", do: tokens, else: [token | tokens]
    tokenize_chars(rest, [], tokens, false)
  end

  defp tokenize_chars([char | rest], current, tokens, in_quotes) do
    tokenize_chars(rest, [char | current], tokens, in_quotes)
  end

  defp parse_tokens(tokens) do
    with {:ok, modalities, rest} <- parse_select(tokens),
         {:ok, source, rest} <- parse_from(rest),
         {:ok, where_clause, rest} <- parse_where(rest),
         {:ok, proof, rest} <- parse_proof(rest),
         {:ok, limit, rest} <- parse_limit(rest),
         {:ok, offset, _rest} <- parse_offset(rest) do
      {:ok, %{
        modalities: modalities,
        source: source,
        where: where_clause,
        proof: proof,
        limit: limit,
        offset: offset
      }}
    end
  end

  defp parse_select(["SELECT" | rest]) do
    {modalities, rest} = take_modalities(rest, [])
    if modalities == [] do
      {:error, "Expected at least one modality after SELECT"}
    else
      {:ok, modalities, rest}
    end
  end

  defp parse_select(_), do: {:error, "Expected SELECT"}

  defp take_modalities(["GRAPH" | rest], acc), do: take_modalities(strip_comma(rest), [:graph | acc])
  defp take_modalities(["VECTOR" | rest], acc), do: take_modalities(strip_comma(rest), [:vector | acc])
  defp take_modalities(["TENSOR" | rest], acc), do: take_modalities(strip_comma(rest), [:tensor | acc])
  defp take_modalities(["SEMANTIC" | rest], acc), do: take_modalities(strip_comma(rest), [:semantic | acc])
  defp take_modalities(["DOCUMENT" | rest], acc), do: take_modalities(strip_comma(rest), [:document | acc])
  defp take_modalities(["TEMPORAL" | rest], acc), do: take_modalities(strip_comma(rest), [:temporal | acc])
  defp take_modalities(["*" | rest], acc), do: take_modalities(strip_comma(rest), [:all | acc])
  defp take_modalities(rest, acc), do: {Enum.reverse(acc), rest}

  defp strip_comma(["," <> token | rest]) when token != "", do: [token | rest]
  defp strip_comma(["," | rest]), do: rest
  defp strip_comma(rest), do: rest

  defp parse_from(["FROM", "STORE", store_id | rest]) do
    {:ok, {:store, normalize_token(store_id)}, rest}
  end

  defp parse_from(["FROM", "FEDERATION", pattern | rest]) do
    {drift_policy, rest} = parse_drift_policy(rest)
    {:ok, {:federation, normalize_token(pattern), drift_policy}, rest}
  end

  defp parse_from(["FROM", "HEXAD", uuid | rest]) do
    {:ok, {:hexad, normalize_token(uuid)}, rest}
  end

  defp parse_from(_), do: {:error, "Expected FROM clause (STORE, FEDERATION, or HEXAD)"}

  defp parse_drift_policy(["WITH", "DRIFT", policy | rest]) do
    drift = case String.upcase(policy) do
      "STRICT" -> :strict
      "REPAIR" -> :repair
      "TOLERATE" -> :tolerate
      "LATEST" -> :latest
      _ -> nil
    end
    {drift, rest}
  end

  defp parse_drift_policy(rest), do: {nil, rest}

  defp parse_where(["WHERE" | rest]) do
    {condition_tokens, rest} = Enum.split_while(rest, fn token ->
      is_tuple(token) or token not in ["PROOF", "LIMIT", "OFFSET"]
    end)

    condition = if condition_tokens == [] do
      nil
    else
      parse_conditions(condition_tokens)
    end

    {:ok, condition, rest}
  end

  defp parse_where(rest), do: {:ok, nil, rest}

  defp parse_conditions(tokens) do
    # Parse WHERE conditions into structured filter
    parse_condition_expr(tokens, [])
  end

  defp parse_condition_expr([], acc), do: finalize_conditions(acc)

  defp parse_condition_expr(["FIELD", field, op | rest], acc) do
    {value, rest} = extract_value(rest)
    condition = {:field, normalize_token(field), normalize_op(op), value}
    parse_condition_expr(skip_conjunction(rest), [condition | acc])
  end

  defp parse_condition_expr(["FULLTEXT", "CONTAINS" | rest], acc) do
    {value, rest} = extract_value(rest)
    condition = {:fulltext, :contains, value}
    parse_condition_expr(skip_conjunction(rest), [condition | acc])
  end

  defp parse_condition_expr(["FULLTEXT", "MATCHES" | rest], acc) do
    {value, rest} = extract_value(rest)
    condition = {:fulltext, :matches, value}
    parse_condition_expr(skip_conjunction(rest), [condition | acc])
  end

  defp parse_condition_expr([_ | rest], acc) do
    # Skip unrecognized tokens
    parse_condition_expr(rest, acc)
  end

  defp extract_value([{:quoted, val} | rest]), do: {val, rest}
  defp extract_value([val | rest]), do: {normalize_token(val), rest}
  defp extract_value([]), do: {"", []}

  defp normalize_op("=="), do: :eq
  defp normalize_op("!="), do: :neq
  defp normalize_op(">="), do: :gte
  defp normalize_op("<="), do: :lte
  defp normalize_op(">"), do: :gt
  defp normalize_op("<"), do: :lt
  defp normalize_op("CONTAINS"), do: :contains
  defp normalize_op("LIKE"), do: :like
  defp normalize_op("MATCHES"), do: :matches
  defp normalize_op(op), do: {:raw, op}

  defp normalize_token(token) when is_binary(token), do: token
  defp normalize_token({:quoted, val}), do: val

  defp skip_conjunction(["AND" | rest]), do: rest
  defp skip_conjunction(["OR" | rest]), do: rest
  defp skip_conjunction(rest), do: rest

  defp finalize_conditions([]), do: nil
  defp finalize_conditions([single]), do: single
  defp finalize_conditions(many), do: {:and, Enum.reverse(many)}

  defp parse_proof(["PROOF" | rest]) do
    {proof_tokens, rest} = Enum.split_while(rest, fn token ->
      is_tuple(token) or token not in ["LIMIT", "OFFSET"]
    end)
    proof_raw = proof_tokens |> Enum.map(fn
      {:quoted, v} -> "\"#{v}\""
      t -> t
    end) |> Enum.join(" ")
    {:ok, %{raw: proof_raw}, rest}
  end

  defp parse_proof(rest), do: {:ok, nil, rest}

  defp parse_limit(["LIMIT", n | rest]) do
    case Integer.parse(n) do
      {limit, _} -> {:ok, limit, rest}
      :error -> {:error, "Invalid LIMIT value: #{n}"}
    end
  end

  defp parse_limit(rest), do: {:ok, nil, rest}

  defp parse_offset(["OFFSET", n | rest]) do
    case Integer.parse(n) do
      {offset, _} -> {:ok, offset, rest}
      :error -> {:error, "Invalid OFFSET value: #{n}"}
    end
  end

  defp parse_offset(rest), do: {:ok, nil, rest}

  defp maybe_clean_cache(cache, now, last_clean, ttl) do
    if now - last_clean > ttl * 2 do
      cache
      |> Enum.reject(fn {_k, {_v, ts}} -> now - ts > ttl end)
      |> Map.new()
    else
      cache
    end
  end
end
