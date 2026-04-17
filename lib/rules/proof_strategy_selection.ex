# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ProofStrategySelection do
  @moduledoc """
  Prover-strategy selection from VeriSimDB historical outcomes.

  Closes the proof learning loop: echidna writes every attempt to
  VeriSimDB's `proof_attempts` table; the `mv_prover_success_by_class`
  materialised view aggregates success rates per (obligation_class,
  prover_used); this module queries that view to recommend the prover
  most likely to succeed for a given obligation class.

  Called by `Hypatia.FleetDispatcher` before dispatching a
  :proof_obligation finding, so echidnabot can be hinted toward the
  historically-best prover rather than defaulting to Lean.

  Data flow:

      echidna attempt → verisim-api POST /api/v1/proof_attempts
                      → ClickHouse proof_attempts table
                      → mv_prover_success_by_class (auto-maintained)
                      → GET /api/v1/proof_attempts/strategy?class=X
                      → this module's recommend/2
                      → fleet_dispatcher routes with prover hint
                      → echidnabot runs recommended prover first

  Graceful degradation: if VeriSimDB is unreachable, `recommend/2`
  returns `{:error, reason}`. The caller should fall back to the
  configured default prover rather than failing the dispatch.

  Rule IDs: PS001-PS010
  """

  require Logger

  @default_limit 5
  @default_timeout_ms 5_000
  @verisim_url_env "HYPATIA_VERISIM_URL"
  @default_verisim_url "http://localhost:8080"

  # Tier-1 provers for novelty gating: when an obligation_class has no
  # historical data, route to a Tier-1 prover from the same family rather
  # than letting the strategy endpoint pick from sparse/no evidence.
  # Mirrors echidna's ProverKind tier assignments (src/rust/provers/mod.rs).
  @tier1_provers ~w(coq lean agda isabelle z3 cvc5 idris2)

  # Quarantine thresholds: a prover is auto-disabled for a given class
  # when its success_rate drops below :quarantine_rate after at least
  # :quarantine_min_attempts invocations. The quarantined pair is
  # filtered out of recommendations until fresh evidence rehabilitates it.
  @quarantine_rate 0.10
  @quarantine_min_attempts 50

  # Tiny sample tolerance: the minimum-attempts threshold is baked into
  # quarantined?/1 via @quarantine_min_attempts; this constant remained
  # as a no-op after refactoring (kept documented here for future use).

  @doc """
  PS001: Recommend provers for a given obligation class.

  Returns `{:ok, recommendations}` where recommendations is a list of
  maps:

      [
        %{
          "prover" => "coq",
          "success_rate" => 0.95,
          "avg_duration_ms" => 4521,
          "total_attempts" => 20
        },
        ...
      ]

  Options:
    - `:limit` (default 5) -- maximum recommendations to return
    - `:timeout` (default 5000ms) -- HTTP request timeout
    - `:base_url` -- override VeriSimDB URL (else HYPATIA_VERISIM_URL env or default)

  Returns `{:error, :not_configured}` if VeriSimDB URL is missing,
  `{:error, {:http_status, code}}` on non-2xx response,
  `{:error, {:transport, reason}}` on network failure,
  `{:error, {:decode, reason}}` on malformed JSON.
  """
  def recommend(obligation_class, opts \\ []) when is_binary(obligation_class) do
    limit = Keyword.get(opts, :limit, @default_limit)
    timeout_ms = Keyword.get(opts, :timeout, @default_timeout_ms)
    base_url = resolve_base_url(opts)

    path = "/api/v1/proof_attempts/strategy?class=" <> uri_encode(obligation_class)
          <> "&limit=" <> Integer.to_string(limit)

    url = base_url <> path

    case http_get(url, timeout_ms) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"recommendations" => recs}} when is_list(recs) ->
            {:ok, recs}

          {:ok, other} ->
            {:error, {:decode, {:unexpected_shape, other}}}

          {:error, reason} ->
            {:error, {:decode, reason}}
        end

      {:error, {:http_status, code}} ->
        Logger.warning("ProofStrategySelection: VeriSimDB returned #{code} for class=#{obligation_class}")
        {:error, {:http_status, code}}

      {:error, reason} ->
        {:error, {:transport, reason}}
    end
  end

  @doc """
  PS003 (V4e): Recommend provers, preferring PROVEN-certified pairs.

  Returns the same shape as `recommend/2` but with each recommendation
  annotated with a `"cert_status"` field (`"proven" | "pending"`), then
  re-sorted so that PROVEN certs surface above uncertified pairs at
  equal success rate.

  Sort order:
    1. cert_status  (proven > pending)
    2. success_rate (desc)
    3. avg_duration_ms (asc)

  Consults `GET /api/v1/proof_attempts/certificates?class=X` for the
  given class and joins by prover name. If the certs endpoint is
  unreachable, falls back to `recommend/2` behaviour (no cert-based
  reordering).

  Example:

      # Before cert preference (raw strategy order by rate):
      [%{"prover" => "coq",  "success_rate" => 0.95, "cert_status" => "pending"},
       %{"prover" => "z3",   "success_rate" => 0.95, "cert_status" => "proven"},
       %{"prover" => "lean", "success_rate" => 0.90, "cert_status" => "pending"}]

      # After cert preference (PROVEN outranks PENDING at tie):
      [%{"prover" => "z3",   "success_rate" => 0.95, "cert_status" => "proven"},
       %{"prover" => "coq",  "success_rate" => 0.95, "cert_status" => "pending"},
       %{"prover" => "lean", "success_rate" => 0.90, "cert_status" => "pending"}]
  """
  def recommend_with_certs(obligation_class, opts \\ []) when is_binary(obligation_class) do
    with {:ok, recs} <- recommend(obligation_class, opts),
         {:ok, certs_by_prover} <- fetch_certs(obligation_class, opts) do
      annotated =
        Enum.map(recs, fn rec ->
          prover = Map.get(rec, "prover")
          status = Map.get(certs_by_prover, prover, "pending")
          Map.put(rec, "cert_status", status)
        end)

      sorted =
        Enum.sort_by(
          annotated,
          fn rec ->
            {
              -cert_rank(Map.get(rec, "cert_status")),
              -(Map.get(rec, "success_rate") || 0.0),
              Map.get(rec, "avg_duration_ms") || 0.0
            }
          end
        )

      {:ok, sorted}
    else
      {:error, {:certs_unavailable, _}} ->
        # Fall back to plain recommendations without cert annotations.
        recommend(obligation_class, opts)

      other ->
        other
    end
  end

  defp cert_rank("proven"), do: 1
  defp cert_rank(_), do: 0

  defp fetch_certs(obligation_class, opts) do
    timeout_ms = Keyword.get(opts, :timeout, @default_timeout_ms)
    base_url = resolve_base_url(opts)

    url =
      base_url <>
        "/api/v1/proof_attempts/certificates?class=" <>
        uri_encode(obligation_class)

    case http_get(url, timeout_ms) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"proven" => proven}} when is_list(proven) ->
            by_prover =
              Map.new(proven, fn row ->
                {Map.get(row, "prover_used"), Map.get(row, "status", "pending")}
              end)

            {:ok, by_prover}

          _ ->
            {:error, {:certs_unavailable, :malformed_response}}
        end

      {:error, reason} ->
        {:error, {:certs_unavailable, reason}}
    end
  end

  @doc """
  PS004 (N1): Recommend with novelty gating + quarantine filtering.

  Wraps `recommend/2` with two additional policies:

    * *Novelty gating*: if the obligation_class has no historical data
      (empty recommendations from /strategy), return a Tier-1 fallback
      prover list rather than an empty result. Caller can exploit this
      to force exploration on novel classes.

    * *Quarantine*: filters out (class, prover) pairs whose success
      rate has dropped below #{@quarantine_rate * 100}% after
      ≥#{@quarantine_min_attempts} attempts. Prevents the dispatcher
      from repeatedly routing obligations to a prover that's clearly
      underperforming for that class.

  Returns `{:ok, recommendations}` where recommendations is the same
  shape as `recommend/2`, plus a `"tier"` field (`"novelty"` when the
  result is Tier-1 fallback, `"learned"` when derived from real data).

  ## Novelty fallback behaviour

  When `recommend/2` returns `{:ok, []}`, this function returns seven
  Tier-1 provers in a stable order, each with
  `{"total_attempts" => 0, "success_rate" => nil, "tier" => "novelty"}`.
  The caller should interpret absence of data as "explore" rather than
  "fail", and pick the first prover the echidnabot can dispatch to.
  """
  def recommend_with_novelty(obligation_class, opts \\ []) when is_binary(obligation_class) do
    case recommend(obligation_class, opts) do
      {:ok, []} ->
        {:ok, novelty_fallback()}

      {:ok, recs} ->
        learned = Enum.map(recs, &Map.put(&1, "tier", "learned"))
        filtered = Enum.reject(learned, &quarantined?/1)

        case filtered do
          [] ->
            # All provers quarantined -- fall back to novelty exploration.
            {:ok, novelty_fallback()}

          kept ->
            {:ok, kept}
        end

      {:error, _} = err ->
        err
    end
  end

  @doc """
  PS005 (N2): Returns `true` when the given recommendation row meets
  the quarantine criteria. Exposed for unit tests and external callers
  that want to apply the same policy.
  """
  def quarantined?(rec) when is_map(rec) do
    n = Map.get(rec, "total_attempts") || 0
    rate = Map.get(rec, "success_rate") || 0.0

    n >= @quarantine_min_attempts and rate < @quarantine_rate
  end

  defp novelty_fallback do
    Enum.map(@tier1_provers, fn prover ->
      %{
        "prover" => prover,
        "success_rate" => nil,
        "avg_duration_ms" => nil,
        "total_attempts" => 0,
        "tier" => "novelty"
      }
    end)
  end

  @doc """
  PS002: Classify a free-text claim into an obligation class.

  Heuristic keyword matcher. Returns one of the canonical classes used
  by echidna's verisim_bridge and the `obligation_class` enum in the
  proof_attempts table:

    - "linearity"   -- linear/affine/ownership claims
    - "termination" -- halting, totality, structural recursion
    - "equiv"       -- equivalence, behaviour preservation, bisimulation
    - "safety"      -- invariants, non-interference, memory safety
    - "unknown"     -- nothing matched

  Intentionally simple: exact classification is echidna's job, this is
  just a fast-path for strategy lookup.
  """
  def classify_obligation(claim) when is_binary(claim) do
    lower = String.downcase(claim)

    cond do
      String.contains?(lower, ["linear", "affine", "ownership", "borrow"]) ->
        "linearity"

      String.contains?(lower, ["terminat", "halt", "total", "recursi", "well-founded"]) ->
        "termination"

      String.contains?(lower, ["equiv", "bisimul", "preserves behavior", "preserves behaviour", "refine"]) ->
        "equiv"

      String.contains?(lower, ["safe", "invariant", "non-interference", "memory-safe", "crash-free"]) ->
        "safety"

      true ->
        "unknown"
    end
  end

  @doc """
  PS003: Format recommendations into a human-readable hint string.

  Used when surfacing strategy recommendations in log lines, dispatch
  manifests, and PR comments.
  """
  def explain_strategy([]), do: "no historical data available"

  def explain_strategy([top | _] = recommendations) do
    top_line =
      "recommend #{Map.get(top, "prover")} " <>
        "(#{format_pct(Map.get(top, "success_rate", 0.0))} success " <>
        "over #{Map.get(top, "total_attempts", 0)} attempts)"

    rest_line =
      recommendations
      |> Enum.drop(1)
      |> Enum.take(2)
      |> Enum.map(fn r ->
        "#{Map.get(r, "prover")}:#{format_pct(Map.get(r, "success_rate", 0.0))}"
      end)
      |> case do
        [] -> ""
        others -> "; fallbacks: " <> Enum.join(others, ", ")
      end

    top_line <> rest_line
  end

  # ─── internals ──────────────────────────────────────────────────────

  defp resolve_base_url(opts) do
    case Keyword.get(opts, :base_url) do
      nil -> System.get_env(@verisim_url_env) || @default_verisim_url
      url -> url
    end
  end

  defp http_get(url, timeout_ms) do
    request = {String.to_charlist(url), [{~c"accept", ~c"application/json"}]}

    case :httpc.request(:get, request, [{:timeout, timeout_ms}], []) do
      {:ok, {{_version, status, _reason}, _headers, body}} when status in 200..299 ->
        {:ok, List.to_string(body)}

      {:ok, {{_version, status, _reason}, _headers, _body}} ->
        {:error, {:http_status, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp uri_encode(s) when is_binary(s), do: URI.encode_www_form(s)

  defp format_pct(rate) when is_float(rate) do
    :erlang.float_to_binary(rate * 100.0, [{:decimals, 1}]) <> "%"
  end

  defp format_pct(_), do: "?%"
end
