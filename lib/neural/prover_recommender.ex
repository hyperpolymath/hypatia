# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Neural.ProverRecommender do
  @moduledoc """
  RBF-backed prover recommendation learned from VeriSimDB proof_attempts.

  Pulls historical proof_attempts rows from verisim-api, extracts feature
  vectors for each (obligation, prover) pair, trains a Radial Basis
  Function network that maps features → success probability, then
  recommends a prover for new obligations by comparing their feature
  vector against the learned centres.

  This is the first module in Hypatia's neural subsystem that consumes
  *real* post-V4 training data -- previous networks either used synthetic
  fixtures or trained on the 'findings' data shape. Here the RBF is
  trained directly on the same aggregates that feed the PROVEN/SANCTIFY
  certificate pipeline.

  Feature extraction (6 dimensions):
    1. class_score     -- obligation_class mapped to a scalar (one value per class)
    2. prover_score    -- prover mapped to a scalar
    3. claim_length    -- normalized to [0,1] by 2000-char cap
    4. confidence      -- stored confidence from the attempt row
    5. duration_log    -- log10(duration_ms + 1) / 5.0  (≈0 for instant, ≈1 for ~100s)
    6. retry_flag      -- 1.0 if parent_attempt_id is non-null, else 0.0

  Target: outcome mapped to a probability:
    success → 1.0   failure → 0.0   timeout → 0.25   unknown → 0.5
  """

  require Logger
  alias Hypatia.Neural.RadialNeuralNetwork

  @verisim_base_url System.get_env("VERISIM_URL") || "http://127.0.0.1:8080"
  @default_limit 500

  # Persistent-term key for the live model snapshot.
  @models_key {__MODULE__, :live_models}

  # --- Public API -----------------------------------------------------------

  @doc """
  Atomically replace the live model snapshot.

  Called by `LearningScheduler` after each successful retraining cycle.
  Uses `:persistent_term` for lock-free reads from any process. The
  previous snapshot is garbage-collected by the BEAM on the next GC cycle.

  Returns `:ok`.
  """
  @spec update_models(map()) :: :ok
  def update_models(models) when is_map(models) do
    :persistent_term.put(@models_key, models)
    :ok
  end

  @doc """
  Retrieve the most recently trained model snapshot, or `nil` if no
  training has completed yet.
  """
  @spec current_models() :: map() | nil
  def current_models do
    :persistent_term.get(@models_key, nil)
  end

  @doc """
  Train one RBF network per obligation_class from the most recent N
  proof_attempts. Returns a map `%{class => rbf}` indexed by obligation
  class, plus a global fallback model trained on all rows.
  """
  def train_from_verisim(opts \\ []) do
    limit = Keyword.get(opts, :limit, @default_limit)
    base_url = Keyword.get(opts, :base_url, @verisim_base_url)

    case fetch_attempts(limit, base_url) do
      {:ok, attempts} ->
        {vectors, targets, classes} = prepare_training_data(attempts)
        global = RadialNeuralNetwork.init(num_centers: 10)

        global_trained = RadialNeuralNetwork.train(global, vectors, targets)

        per_class = train_per_class(attempts, classes)
        {:ok, %{global: global_trained, per_class: per_class, sample_size: length(attempts)}}

      {:error, reason} ->
        Logger.warning("ProverRecommender: fetch_attempts failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Recommend a prover for a new obligation.

  `models` is the map returned by `train_from_verisim/1`. `obligation` is a
  map with keys `:obligation_class`, `:claim`, `:known_provers` (list of
  candidate prover atoms/strings). Returns
    `{:ok, {prover, predicted_success_rate, confidence}}`
  or `:no_data` if no relevant training has been seen.
  """
  def recommend(models, obligation) do
    candidates = Map.get(obligation, :known_provers, all_known_provers())
    class_rbf = Map.get(models.per_class, obligation.obligation_class, models.global)

    scores =
      for prover <- candidates do
        features = attempt_features(obligation, to_string(prover))
        {prediction, confidence} = RadialNeuralNetwork.predict(class_rbf, features)
        {prover, prediction, confidence}
      end

    # Highest predicted success rate wins; tie-break by confidence.
    best = Enum.max_by(scores, fn {_, pred, conf} -> {pred, conf} end, fn -> nil end)

    case best do
      nil -> :no_data
      triple -> {:ok, triple}
    end
  end

  # --- verisim-api bridge ---------------------------------------------------

  defp fetch_attempts(limit, base_url \\ nil) do
    resolved_url = base_url || @verisim_base_url
    url = "#{resolved_url}/api/v1/proof_attempts?limit=#{limit}"
    # verisim-api /proof_attempts GET doesn't exist yet -- fall back to ClickHouse
    # strategy endpoint aggregates when the row-level endpoint is absent.
    case http_get(url) do
      {:ok, body} -> Jason.decode(body)
      {:error, _} -> fetch_attempts_via_clickhouse(limit, resolved_url)
    end
  end

  defp fetch_attempts_via_clickhouse(limit, base_url \\ nil) do
    resolved_url = base_url || @verisim_base_url
    # ClickHouse HTTP: reach it by probing each active class's strategy endpoint
    # and folding the recommendations back into synthetic attempt rows.
    classes = ~w(safety linearity termination equiv correctness confluence
                 totality invariant refinement model-check other)

    attempts =
      Enum.flat_map(classes, fn class ->
        url = "#{resolved_url}/api/v1/proof_attempts/strategy?class=#{class}&limit=20"

        case http_get(url) do
          {:ok, body} ->
            case Jason.decode(body) do
              {:ok, %{"recommendations" => recs}} ->
                Enum.flat_map(recs, fn rec ->
                  # Unfold aggregate into weighted synthetic rows for training.
                  n = trunc(min(rec["total_attempts"], limit))
                  successes = round(rec["success_rate"] * n)

                  for i <- 1..n,
                      do: %{
                        "obligation_class" => class,
                        "prover_used" => rec["prover"],
                        "outcome" => if(i <= successes, do: "success", else: "failure"),
                        "duration_ms" => trunc(rec["avg_duration_ms"]),
                        "confidence" => 0.5,
                        "claim" => "",
                        "parent_attempt_id" => nil
                      }
                end)

              _ ->
                []
            end

          _ ->
            []
        end
      end)

    {:ok, attempts}
  end

  defp http_get(url) do
    # Hypatia already depends on :req via mix.exs; if unavailable fall back
    # to :httpc which is in stdlib.
    with {:ok, _} <- Application.ensure_all_started(:inets),
         {:ok, _} <- Application.ensure_all_started(:ssl) do
      case :httpc.request(:get, {String.to_charlist(url), []}, [], []) do
        {:ok, {{_, 200, _}, _headers, body}} -> {:ok, to_string(body)}
        {:ok, {{_, code, _}, _, body}} -> {:error, {code, to_string(body)}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  # --- Feature extraction ---------------------------------------------------

  defp prepare_training_data(attempts) do
    vectors =
      Enum.map(attempts, fn a ->
        attempt_features(
          %{
            obligation_class: Map.get(a, "obligation_class"),
            claim: Map.get(a, "claim", ""),
            confidence: Map.get(a, "confidence", 0.5),
            duration_ms: Map.get(a, "duration_ms", 0),
            parent_attempt_id: Map.get(a, "parent_attempt_id")
          },
          Map.get(a, "prover_used", "other")
        )
      end)

    targets =
      Enum.map(attempts, fn a ->
        outcome_to_target(Map.get(a, "outcome", "unknown"))
      end)

    classes = attempts |> Enum.map(& &1["obligation_class"]) |> Enum.uniq()
    {vectors, targets, classes}
  end

  defp train_per_class(attempts, classes) do
    Enum.reduce(classes, %{}, fn class, acc ->
      class_attempts = Enum.filter(attempts, &(&1["obligation_class"] == class))

      if length(class_attempts) >= 5 do
        {vectors, targets, _} = prepare_training_data(class_attempts)
        rbf = RadialNeuralNetwork.init(num_centers: min(5, div(length(class_attempts), 2)))
        trained = RadialNeuralNetwork.train(rbf, vectors, targets)
        if trained.trained, do: Map.put(acc, class, trained), else: acc
      else
        acc
      end
    end)
  end

  @doc false
  def attempt_features(obligation, prover) do
    [
      class_score(Map.get(obligation, :obligation_class, "other")),
      prover_score(prover),
      min(String.length(Map.get(obligation, :claim, "") || ""), 2000) / 2000.0,
      Map.get(obligation, :confidence, 0.5),
      :math.log10(Map.get(obligation, :duration_ms, 0) + 1) / 5.0,
      if(Map.get(obligation, :parent_attempt_id), do: 1.0, else: 0.0)
    ]
  end

  defp outcome_to_target("success"), do: 1.0
  defp outcome_to_target("failure"), do: 0.0
  defp outcome_to_target("timeout"), do: 0.25
  defp outcome_to_target(_), do: 0.5

  # --- Categorical embeddings (stable scalars in [0,1]) --------------------

  defp class_score(class) do
    classes = ~w(safety linearity termination equiv correctness confluence
                 totality invariant refinement model-check other)

    case Enum.find_index(classes, &(&1 == class)) do
      nil -> 1.0
      i -> (i + 1) / (length(classes) + 1)
    end
  end

  defp prover_score(prover) do
    provers =
      ~w(z3 cvc5 altergo coq lean agda idris2 isabelle fstar dafny why3
         metamath hol_light mizar hol4 pvs acl2 tlaps vampire eprover spass
         cadical kissat minisat twelf nuprl minlog imandra alloy spin cbmc
         seahorn dreal framac key abc glpk scip minizinc chuffed ortools
         typedwasm other)

    case Enum.find_index(provers, &(&1 == prover)) do
      nil -> 1.0
      i -> (i + 1) / (length(provers) + 1)
    end
  end

  defp all_known_provers do
    ~w(z3 cvc5 altergo coq lean agda idris2 vampire cadical metamath dafny
       why3)a
  end
end
