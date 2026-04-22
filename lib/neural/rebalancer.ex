# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Neural.Rebalancer do
  @moduledoc """
  Synthetic data augmentation for the ESN and RBF training sets.

  Recorded outcomes are ~99% success / ~0.3% failure / ~9% false-positive
  (6,280 / 18 / 666 out of the first 6,964 records — see
  `data/verisim/outcomes/`). Too imbalanced for either network to learn
  a useful decision boundary: ESN sees a near-flat trajectory; RBF's
  Gaussian centres cluster in a narrow region of the 8-D feature
  space with nothing in the rest of the space.

  This module keeps training honest without waiting for real failure
  data to catch up. Two operations:

    * `augment_esn_series/2` — inject synthetic regression events into
      a confidence time series. Simulates recipes that temporarily
      degrade (SHA drift, quoting regression, ORM escape mis-detection
      — the observed real-world failure modes).
    * `synthetic_rbf_examples/1` / `rebalance_rbf/2` — generate
      feature vectors across the 8-D space and label them with a
      rule-based target (severity × fix-rate → triangle tier) so the
      RBF has something to interpolate to when it sees a finding
      outside the observed region.

  Both are deterministic if a `:seed` option is passed. Defaults are
  set so a single call produces repeatable output within one BEAM run.
  """

  @doc """
  Inject synthetic regression events into a confidence time series.

  `series` is a list of floats in `[0.0, 1.0]`. Each point has
  probability `:rate` of becoming a "dip" — a point whose value drops
  by up to `:dip_depth` × `:rand.uniform()`. Values are floored at 0.0.

  Options:

    * `:rate` — dip probability per point (default `0.05`)
    * `:dip_depth` — maximum dip magnitude (default `0.4`)
    * `:seed` — seed for `:rand.seed(:exsss, ...)`. Defaults to
      `:erlang.system_time(:millisecond)` so every training run gets
      different augmentation. Pin an integer for deterministic tests.

  Returns a list of the same length.
  """
  def augment_esn_series(series, opts \\ []) when is_list(series) do
    rate = Keyword.get(opts, :rate, 0.05)
    dip_depth = Keyword.get(opts, :dip_depth, 0.4)
    seed_key = Keyword.get(opts, :seed, default_seed())

    seed_rand(seed_key)

    Enum.map(series, fn v ->
      if is_number(v) do
        if :rand.uniform() < rate do
          max(0.0, v - dip_depth * :rand.uniform())
        else
          v
        end
      else
        v
      end
    end)
  end

  @doc """
  Generate synthetic RBF training examples spread across the 8-D
  feature space. Returns `{vectors, targets}`.

  Vector fields (index):

    0. severity
    1. confidence
    2. complexity
    3. frequency
    4. fix_rate
    5. category
    6. age
    7. affected_files

  All in `[0.0, 1.0]`, matching `RadialNeuralNetwork.finding_to_vector/1`.

  The rule-based target (`severity_x_fix_rate → triangle_tier`) mirrors
  the safety-triangle routing heuristic: high-severity low-fix-rate
  patterns land at 1.0 (`:eliminate`), mid-spectrum at 0.5
  (`:substitute`), everything else at 0.0 (`:control`). This lets the
  RBF interpolate toward a reasonable target instead of extrapolating
  blindly.

  Options:

    * `:count` — number of synthetic examples (default `30`)
    * `:seed` — seed for `:rand.seed(:exsss, ...)`. Defaults to
      `:erlang.system_time(:millisecond)`. Pin an integer for
      deterministic tests.
  """
  def synthetic_rbf_examples(opts \\ []) do
    count = Keyword.get(opts, :count, 30)
    seed_key = Keyword.get(opts, :seed, default_seed())

    seed_rand(seed_key)

    {vectors, targets} =
      1..count
      |> Enum.map(fn _ ->
        v = random_vector()
        {v, target_for(v)}
      end)
      |> Enum.unzip()

    {vectors, targets}
  end

  @doc """
  Rebalance an RBF training set by appending synthetic examples.

  `existing` is `{vectors, targets}` from
  `TrainingPipeline.build_rbf_training_data/0`. Returns a new
  `{vectors, targets}` with synthetic examples concatenated.
  """
  def rebalance_rbf({vectors, targets}, opts \\ []) when is_list(vectors) and is_list(targets) do
    {syn_v, syn_t} = synthetic_rbf_examples(opts)
    {vectors ++ syn_v, targets ++ syn_t}
  end

  # ── helpers ──────────────────────────────────────────────────────────────

  defp random_vector do
    for _ <- 1..8, do: :rand.uniform()
  end

  # Target assignment rule. Mirrors the safety-triangle heuristic:
  #   high severity + low fix_rate   → eliminate (1.0)
  #   moderate severity or fix_rate  → substitute (0.5)
  #   everything else                → control (0.0)
  defp target_for(vector) do
    severity = Enum.at(vector, 0)
    fix_rate = Enum.at(vector, 4)

    cond do
      severity > 0.7 and fix_rate < 0.3 -> 1.0
      severity > 0.4 or fix_rate < 0.5 -> 0.5
      true -> 0.0
    end
  end

  defp seed_rand(seed_key) when is_integer(seed_key) do
    :rand.seed(:exsss, {seed_key, seed_key + 1, seed_key + 2})
  end

  defp seed_rand(_), do: seed_rand(default_seed())

  defp default_seed, do: :erlang.system_time(:millisecond)
end
