# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

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
  data to catch up. Three strategies, each additive:

    * **Strategy A** `augment_esn_series/2` — uniform random dips injected
      at a fixed per-point probability. Fast, deterministic with a seed,
      sufficient for mild imbalance correction.

    * **Strategy B** `adversarial_esn_series/2` — structured adversarial
      patterns (cliff collapse, slow recovery, confidence plateau) rather
      than uniform random dips. Stresses the ESN with the failure *shapes*
      observed in practice, not just magnitude.

    * **Strategy C** `corpus_esn_series/3` — reads the real failure records
      from `outcomes/*.jsonl` and injects dip events whose depth and
      duration are drawn from the empirical failure distribution. Keeps the
      synthetic signal grounded in what has actually gone wrong.

  All are deterministic if a `:seed` option is passed. Defaults use
  `:erlang.system_time(:millisecond)` so every training run varies.
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

  # ── Strategy B: adversarial structured patterns ──────────────────────────

  @doc """
  Strategy B: inject *structured* adversarial failure patterns into a
  confidence time series.

  Unlike Strategy A (uniform random dips), Strategy B injects whole
  failure *shapes* drawn from three observed archetypes:

    * `:cliff`    — sudden collapse to near-zero over 1-3 steps, then
                    stays low. Models a breaking change that invalidates
                    a recipe family.
    * `:recovery` — cliff followed by a gradual climb back toward the
                    pre-collapse level. Models a temporary regression
                    that gets patched.
    * `:plateau`  — confidence stalls at a mediocre level (0.4–0.6)
                    instead of converging upward. Models a recipe that
                    works sometimes but has a persistent edge-case miss.

  Options:

    * `:events`   — number of adversarial events to inject (default `3`)
    * `:seed`     — integer seed for reproducibility

  Returns a list of the same length as `series`.
  """
  def adversarial_esn_series(series, opts \\ []) when is_list(series) do
    events = Keyword.get(opts, :events, 3)
    seed_key = Keyword.get(opts, :seed, default_seed())

    seed_rand(seed_key)

    len = length(series)
    arr = List.to_tuple(series)

    # Pick `events` distinct insertion points, avoiding the first 5 steps
    # (let the ESN see at least a short warm-up sequence before each event).
    positions =
      if len <= 10 do
        []
      else
        1..events
        |> Enum.reduce([], fn _, acc ->
          pos = 5 + :rand.uniform(len - 6)
          if pos in acc, do: acc, else: [pos | acc]
        end)
        |> Enum.sort()
      end

    result =
      Enum.reduce(positions, arr, fn pos, a ->
        shape = Enum.at([:cliff, :recovery, :plateau], rem(:rand.uniform(100), 3))
        inject_shape(a, pos, shape, len)
      end)

    Tuple.to_list(result)
  end

  # ── Strategy C: corpus-grounded dips ─────────────────────────────────────

  @doc """
  Strategy C: build failure templates from real outcome records and inject
  them into a confidence time series.

  Reads all `outcomes/*.jsonl` files from `data_path` (defaults to
  `data/verisim`). For each real failure record, derives a dip template:
  depth is drawn from the empirical distribution of failure frequencies
  per recipe, and duration is mapped from the failure reason.

  Falls back to Strategy A if no failure records exist.

  Options:

    * `:data_path` — path to the verisim data directory
                     (default `"data/verisim"`)
    * `:max_events` — cap on injected events (default `5`)
    * `:seed`       — integer seed for reproducibility
  """
  def corpus_esn_series(series, failure_templates, opts \\ []) when is_list(series) do
    max_events = Keyword.get(opts, :max_events, 5)
    seed_key = Keyword.get(opts, :seed, default_seed())

    seed_rand(seed_key)

    templates = Enum.take(Enum.shuffle(failure_templates), max_events)

    len = length(series)
    arr = List.to_tuple(series)

    result =
      Enum.reduce(templates, arr, fn template, a ->
        pos = if len > 10, do: 5 + rem(:rand.uniform(1_000_000), len - 6), else: 0
        inject_corpus_dip(a, pos, template, len)
      end)

    Tuple.to_list(result)
  end

  @doc """
  Build failure templates from the real outcome corpus.

  Returns a (possibly empty) list of `%{depth: float, duration: integer}`
  maps, one per failure record found in `data_path/outcomes/*.jsonl`.
  Pass this list to `corpus_esn_series/3`.
  """
  def load_failure_templates(data_path \\ "data/verisim") do
    outcomes_dir = Path.join(Path.expand(data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn file ->
          path = Path.join(outcomes_dir, file)

          case File.read(path) do
            {:ok, content} ->
              content
              |> String.split("\n", trim: true)
              |> Enum.flat_map(fn line ->
                case Jason.decode(line) do
                  {:ok, %{"outcome" => outcome} = record}
                  when outcome in ["failure", "false_positive"] ->
                    [failure_template(record)]

                  _ ->
                    []
                end
              end)

            _ ->
              []
          end
        end)

      _ ->
        []
    end
  end

  # ── private helpers ───────────────────────────────────────────────────────

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

  # Inject an adversarial shape at `pos` into tuple `arr`.
  defp inject_shape(arr, pos, shape, len) do
    case shape do
      :cliff ->
        # Drop to 0.05–0.15 and stay there for 3–6 steps.
        depth = 0.05 + :rand.uniform() * 0.10
        duration = 3 + rem(:rand.uniform(100), 4)
        fill_range(arr, pos, min(pos + duration, len - 1), depth)

      :recovery ->
        # Cliff, then linear ramp back to original value over equal duration.
        depth = 0.05 + :rand.uniform() * 0.10
        cliff_end = min(pos + 2 + rem(:rand.uniform(100), 3), len - 1)
        original = elem(arr, pos)
        arr2 = fill_range(arr, pos, cliff_end, depth)
        recovery_end = min(cliff_end + (cliff_end - pos), len - 1)

        Enum.reduce(cliff_end..recovery_end//1, arr2, fn i, a ->
          progress = (i - cliff_end) / max(recovery_end - cliff_end, 1)
          put_elem(a, i, depth + progress * (original - depth))
        end)

      :plateau ->
        # Clamp values in a window to a mediocre band (0.40–0.58).
        plateau_val = 0.40 + :rand.uniform() * 0.18
        duration = 4 + rem(:rand.uniform(100), 6)
        end_pos = min(pos + duration, len - 1)

        Enum.reduce(pos..end_pos//1, arr, fn i, a ->
          put_elem(a, i, plateau_val)
        end)
    end
  end

  # Inject a corpus-derived dip: duration mapped from failure_reason.
  defp inject_corpus_dip(arr, pos, %{depth: depth, duration: duration}, len) do
    fill_range(arr, pos, min(pos + duration - 1, len - 1), depth)
  end

  defp fill_range(arr, from, to, value) do
    Enum.reduce(from..to//1, arr, fn i, a -> put_elem(a, i, value) end)
  end

  # Map a real failure record to a dip template.
  defp failure_template(record) do
    reason = Map.get(record, "failure_reason", "unknown")

    # Duration heuristic: some failure reasons cause longer outages.
    duration =
      case reason do
        "merge_conflict" -> 4
        "test_failure" -> 3
        "rate_limited" -> 2
        "timeout" -> 2
        _ -> 3
      end

    %{depth: 0.05 + :rand.uniform() * 0.20, duration: duration}
  end

  defp seed_rand(seed_key) when is_integer(seed_key) do
    :rand.seed(:exsss, {seed_key, seed_key + 1, seed_key + 2})
  end

  defp seed_rand(_), do: seed_rand(default_seed())

  defp default_seed, do: :erlang.system_time(:millisecond)
end
