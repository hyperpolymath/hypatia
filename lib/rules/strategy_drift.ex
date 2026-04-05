# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.StrategyDrift do
  @moduledoc """
  N4: Detects strategy-shift events and flags previously-failed attempts
  that should be re-queued with the new top prover.

  The learning loop's strategy endpoint returns the current best prover
  per obligation_class. When that top recommendation changes for a
  class, attempts that previously failed with the old top prover may now
  succeed with the new one — they're worth re-queuing.

  This module tracks the last-seen top prover per class in an ETS
  table (`:hypatia_strategy_drift`) and compares against the current
  `/strategy` result. When a shift is detected it emits a
  `{:shift, class, old_prover, new_prover, candidates}` event where
  `candidates` is the list of `attempt_id`s that failed with the old
  prover on that class.

  Consuming callers (typically `LearningScheduler`) can use the
  candidate list to re-enqueue proof obligations via echidnabot's
  `submitProofObligation` GraphQL mutation.

  ## Sketch of the full loop

      1. N-minute tick in LearningScheduler
      2. For each obligation_class in mv_prover_success_by_class:
         a. Call recommend_with_novelty/2 → top prover
         b. Compare against ETS `:hypatia_strategy_drift[class]`
         c. If shifted:
            - fetch attempts that failed with old_prover, class
            - emit shift event with the candidate list
            - update ETS to new prover
      3. Caller re-queues each candidate via echidnabot API

  ## Data shape of ETS table

  `:hypatia_strategy_drift` is a named public ETS set:
    { class :: binary,
      current_top_prover :: binary,
      since :: DateTime.t(),
      last_checked :: DateTime.t() }
  """

  require Logger
  alias Hypatia.Rules.ProofStrategySelection

  @table :hypatia_strategy_drift
  @default_base_url "http://localhost:8080"

  # ── Public API ─────────────────────────────────────────────────────────

  @doc """
  Initialise the ETS table. Safe to call multiple times.
  """
  def init_table do
    case :ets.whereis(@table) do
      :undefined ->
        :ets.new(@table, [:named_table, :public, :set])
        :ok

      _ref ->
        :ok
    end
  end

  @doc """
  Check a single class for strategy shift. Returns one of:

    * `{:no_shift, class, top_prover}` — same top prover as last time (or
      first observation)
    * `{:shift, class, old_top, new_top, failed_attempt_ids}` — top prover
      changed; failed_attempt_ids are candidates for re-queueing
    * `{:error, reason}` — couldn't reach strategy endpoint

  Pass `:hypatia_strategy_drift` or set up the ETS table via `init_table/0`
  before calling.
  """
  def check_shift(class, opts \\ []) when is_binary(class) do
    init_table()
    base_url = Keyword.get(opts, :base_url, @default_base_url)

    case ProofStrategySelection.recommend_with_novelty(class, base_url: base_url) do
      {:ok, []} ->
        {:no_shift, class, nil}

      {:ok, [top | _]} ->
        new_top = Map.get(top, "prover")

        case :ets.lookup(@table, class) do
          [] ->
            # First observation
            now = DateTime.utc_now()
            :ets.insert(@table, {class, new_top, now, now})
            {:no_shift, class, new_top}

          [{^class, ^new_top, _since, _}] ->
            # Unchanged — update last_checked
            :ets.update_element(@table, class, {4, DateTime.utc_now()})
            {:no_shift, class, new_top}

          [{^class, old_top, _since, _}] when old_top != new_top ->
            # Shift detected
            candidates = fetch_failed_attempts(class, old_top, opts)
            now = DateTime.utc_now()
            :ets.insert(@table, {class, new_top, now, now})
            {:shift, class, old_top, new_top, candidates}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Sweep all obligation classes with known-meaningful sample sizes.
  Returns a list of shift events.
  """
  def check_all_shifts(opts \\ []) do
    classes = ~w(safety linearity termination equiv correctness confluence
                 totality invariant refinement model-check other)

    classes
    |> Enum.map(&check_shift(&1, opts))
    |> Enum.filter(&match?({:shift, _, _, _, _}, &1))
  end

  @doc """
  Clear the drift-tracking ETS table. Useful for tests and for forcing
  a re-baseline across all classes.
  """
  def reset do
    init_table()
    :ets.delete_all_objects(@table)
    :ok
  end

  @doc """
  Return the current tracked top prover for every class, plus its age.
  Used by dashboards and diagnostics.
  """
  def snapshot do
    init_table()

    :ets.tab2list(@table)
    |> Enum.map(fn {class, top, since, last_checked} ->
      %{
        class: class,
        top_prover: top,
        since: since,
        last_checked: last_checked
      }
    end)
  end

  # ── Internals ───────────────────────────────────────────────────────────

  defp fetch_failed_attempts(class, prover, opts) do
    # ClickHouse query: attempt_ids where outcome='failure' for (class, prover).
    # We query verisim-api's raw SQL endpoint — if it doesn't exist we return [].
    base_url = Keyword.get(opts, :base_url, @default_base_url)
    timeout_ms = Keyword.get(opts, :timeout, 5_000)

    # Use the /certificates endpoint with evidence_limit to pull failed rows.
    # This is an approximation — the certs view returns the most recent N
    # attempts per (class, prover), not specifically the failures. For a
    # proper implementation, a /failures endpoint should be added.
    url =
      base_url <>
        "/api/v1/proof_attempts/certificates?class=" <>
        URI.encode_www_form(class) <> "&prover=" <> URI.encode_www_form(prover) <>
        "&evidence_limit=100"

    case http_get(url, timeout_ms) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"proven" => rows}} when is_list(rows) ->
            rows
            |> Enum.flat_map(fn row -> Map.get(row, "evidence", []) end)
            |> Enum.filter(fn ev -> Map.get(ev, "outcome") == "failure" end)
            |> Enum.map(fn ev -> Map.get(ev, "attempt_id") end)
            |> Enum.reject(&is_nil/1)

          _ ->
            []
        end

      {:error, _} ->
        []
    end
  end

  defp http_get(url, timeout_ms) do
    Application.ensure_all_started(:inets)
    Application.ensure_all_started(:ssl)

    case :httpc.request(
           :get,
           {String.to_charlist(url), [{~c"accept", ~c"application/json"}]},
           [{:timeout, timeout_ms}],
           []
         ) do
      {:ok, {{_, status, _}, _headers, body}} when status in 200..299 ->
        {:ok, List.to_string(body)}

      {:ok, {{_, status, _}, _, _}} ->
        {:error, {:http_status, status}}

      {:error, reason} ->
        {:error, {:transport, reason}}
    end
  end
end
