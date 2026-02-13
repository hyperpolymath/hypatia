# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.MixtureOfExperts do
  @moduledoc """
  Mixture of Experts (MoE) for Hypatia.

  Routes findings to specialized expert networks based on finding characteristics.
  Each expert specializes in a domain (security, quality, sustainability, etc.)
  and the gating network learns which expert handles which pattern best.

  Architecture:
  - Gating network: softmax over expert weights given input features
  - Expert networks: per-domain confidence estimators
  - Sparse activation: top-k experts per input (efficiency)
  - Load balancing: auxiliary loss prevents expert collapse

  This enables:
  - Domain-specific confidence estimation (security vs quality vs style)
  - Efficient inference (only k experts activated per finding)
  - Specialization without manual routing rules
  - Continuous learning as new pattern types emerge
  """

  require Logger

  @top_k 2  # Activate top 2 experts per finding
  @load_balance_weight 0.01  # Auxiliary loss weight for balancing
  @learning_rate 0.01

  defstruct experts: %{}, gating_weights: %{}, expert_stats: %{}, last_trained: nil

  # --- Expert Definitions ---

  @expert_domains [
    :security,        # CVEs, vulnerabilities, secret exposure
    :quality,         # Code quality, complexity, duplication
    :sustainability,  # License compliance, dependency health
    :accessibility,   # A11y issues, WCAG compliance
    :performance,     # Performance anti-patterns, resource leaks
    :documentation,   # Missing docs, outdated docs
    :infrastructure   # CI/CD, container, deployment issues
  ]

  # --- Public API ---

  @doc "Initialize the MoE with default expert weights"
  def init do
    experts = Map.new(@expert_domains, fn domain ->
      {domain, init_expert(domain)}
    end)

    gating_weights = Map.new(@expert_domains, fn domain ->
      {domain, 1.0 / length(@expert_domains)}
    end)

    expert_stats = Map.new(@expert_domains, fn domain ->
      {domain, %{activations: 0, correct: 0, total: 0}}
    end)

    %__MODULE__{
      experts: experts,
      gating_weights: gating_weights,
      expert_stats: expert_stats
    }
  end

  @doc "Route a finding through the MoE and get aggregated confidence"
  def predict(%__MODULE__{} = moe, finding) do
    features = extract_features(finding)
    gate_scores = compute_gate(moe.gating_weights, features)

    # Top-k expert selection (sparse activation)
    top_experts = gate_scores
    |> Enum.sort_by(fn {_domain, score} -> -score end)
    |> Enum.take(@top_k)

    # Normalize top-k scores
    total = Enum.reduce(top_experts, 0.0, fn {_d, s}, acc -> acc + s end)
    normalized = Enum.map(top_experts, fn {d, s} -> {d, s / max(total, 0.001)} end)

    # Weighted expert predictions
    predictions = Enum.map(normalized, fn {domain, weight} ->
      expert = Map.get(moe.experts, domain)
      confidence = expert_predict(expert, features)
      {domain, weight, confidence}
    end)

    aggregated = Enum.reduce(predictions, 0.0, fn {_d, w, c}, acc -> acc + w * c end)
    selected_experts = Enum.map(predictions, fn {d, _w, _c} -> d end)

    {min(max(aggregated, 0.0), 1.0), selected_experts}
  end

  @doc "Train the MoE on an outcome (updates gating + expert weights)"
  def train(%__MODULE__{} = moe, finding, actual_outcome) do
    features = extract_features(finding)
    gate_scores = compute_gate(moe.gating_weights, features)

    top_experts = gate_scores
    |> Enum.sort_by(fn {_d, s} -> -s end)
    |> Enum.take(@top_k)

    target = if actual_outcome == :success, do: 1.0, else: 0.0

    # Update each activated expert
    updated_experts = Enum.reduce(top_experts, moe.experts, fn {domain, _score}, acc ->
      expert = Map.get(acc, domain)
      updated = train_expert(expert, features, target)
      Map.put(acc, domain, updated)
    end)

    # Update gating weights based on expert accuracy
    updated_gating = update_gating(moe.gating_weights, top_experts, features, target, moe.experts)

    # Update stats
    updated_stats = Enum.reduce(top_experts, moe.expert_stats, fn {domain, _score}, acc ->
      stats = Map.get(acc, domain, %{activations: 0, correct: 0, total: 0})
      correct_inc = if actual_outcome == :success, do: 1, else: 0
      Map.put(acc, domain, %{
        activations: stats.activations + 1,
        correct: stats.correct + correct_inc,
        total: stats.total + 1
      })
    end)

    # Load balancing: penalize over-activated experts
    balanced_gating = apply_load_balancing(updated_gating, updated_stats)

    %{moe |
      experts: updated_experts,
      gating_weights: balanced_gating,
      expert_stats: updated_stats,
      last_trained: DateTime.utc_now()
    }
  end

  @doc "Get expert utilization statistics"
  def expert_utilization(%__MODULE__{expert_stats: stats}) do
    total_activations = Enum.reduce(stats, 0, fn {_d, s}, acc -> acc + s.activations end)

    Enum.map(stats, fn {domain, s} ->
      utilization = if total_activations > 0, do: s.activations / total_activations, else: 0.0
      accuracy = if s.total > 0, do: s.correct / s.total, else: 0.0
      {domain, %{utilization: utilization, accuracy: accuracy, activations: s.activations}}
    end)
    |> Enum.sort_by(fn {_d, s} -> -s.utilization end)
  end

  # --- Gating Network ---

  defp compute_gate(gating_weights, features) do
    # Softmax over domain affinities
    domain_affinities = Map.new(gating_weights, fn {domain, base_weight} ->
      feature_affinity = domain_feature_affinity(domain, features)
      {domain, base_weight * (1.0 + feature_affinity)}
    end)

    # Softmax normalization
    max_val = domain_affinities |> Map.values() |> Enum.max()
    exp_scores = Map.new(domain_affinities, fn {d, v} -> {d, :math.exp(v - max_val)} end)
    sum_exp = exp_scores |> Map.values() |> Enum.sum()

    Map.new(exp_scores, fn {d, v} -> {d, v / max(sum_exp, 0.001)} end)
  end

  defp domain_feature_affinity(domain, features) do
    pattern = Map.get(features, :pattern_type, "")
    severity = Map.get(features, :severity_score, 0.5)

    case domain do
      :security -> if String.contains?(pattern, "cve") or String.contains?(pattern, "secret") or severity > 0.8, do: 2.0, else: 0.0
      :quality -> if String.contains?(pattern, "complexity") or String.contains?(pattern, "duplication"), do: 2.0, else: 0.0
      :sustainability -> if String.contains?(pattern, "license") or String.contains?(pattern, "dependency"), do: 2.0, else: 0.0
      :accessibility -> if String.contains?(pattern, "a11y") or String.contains?(pattern, "wcag"), do: 2.0, else: 0.0
      :performance -> if String.contains?(pattern, "perf") or String.contains?(pattern, "leak"), do: 2.0, else: 0.0
      :documentation -> if String.contains?(pattern, "doc") or String.contains?(pattern, "readme"), do: 2.0, else: 0.0
      :infrastructure -> if String.contains?(pattern, "ci") or String.contains?(pattern, "container"), do: 2.0, else: 0.0
      _ -> 0.0
    end
  end

  defp update_gating(gating_weights, top_experts, _features, target, experts) do
    Enum.reduce(top_experts, gating_weights, fn {domain, _score}, acc ->
      expert = Map.get(experts, domain)
      prediction = Map.get(expert, :last_prediction, 0.5)
      error = abs(target - prediction)

      # Increase gate weight for accurate experts, decrease for inaccurate
      adjustment = if error < 0.3, do: @learning_rate, else: -@learning_rate
      current = Map.get(acc, domain, 0.1)
      Map.put(acc, domain, max(current + adjustment, 0.01))
    end)
  end

  defp apply_load_balancing(gating_weights, stats) do
    total = Enum.reduce(stats, 0, fn {_d, s}, acc -> acc + s.activations end)
    if total == 0, do: gating_weights, else: do_load_balance(gating_weights, stats, total)
  end

  defp do_load_balance(gating_weights, stats, total) do
    ideal = 1.0 / max(map_size(gating_weights), 1)

    Map.new(gating_weights, fn {domain, weight} ->
      actual = Map.get(stats, domain, %{activations: 0}).activations / total
      imbalance = actual - ideal
      adjusted = weight - @load_balance_weight * imbalance
      {domain, max(adjusted, 0.01)}
    end)
  end

  # --- Expert Networks ---

  defp init_expert(domain) do
    %{
      domain: domain,
      weights: %{severity: 0.3, complexity: 0.2, frequency: 0.2, recency: 0.15, fix_rate: 0.15},
      bias: 0.5,
      last_prediction: 0.5
    }
  end

  defp expert_predict(expert, features) do
    weighted_sum = Enum.reduce(expert.weights, 0.0, fn {feature, weight}, acc ->
      value = Map.get(features, feature, 0.5)
      acc + weight * value
    end)

    sigmoid(weighted_sum + expert.bias)
  end

  defp train_expert(expert, features, target) do
    prediction = expert_predict(expert, features)
    error = target - prediction
    gradient = prediction * (1.0 - prediction)  # sigmoid derivative

    updated_weights = Map.new(expert.weights, fn {feature, weight} ->
      value = Map.get(features, feature, 0.5)
      {feature, weight + @learning_rate * error * gradient * value}
    end)

    updated_bias = expert.bias + @learning_rate * error * gradient

    %{expert | weights: updated_weights, bias: updated_bias, last_prediction: prediction}
  end

  # --- Feature Extraction ---

  defp extract_features(finding) do
    %{
      severity_score: severity_to_score(Map.get(finding, "severity", "medium")),
      complexity: Map.get(finding, "complexity", 0.5),
      frequency: Map.get(finding, "frequency", 0.5),
      recency: Map.get(finding, "recency", 0.5),
      fix_rate: Map.get(finding, "fix_rate", 0.5),
      pattern_type: String.downcase(Map.get(finding, "pattern", "") <> " " <> Map.get(finding, "category", ""))
    }
  end

  defp severity_to_score("critical"), do: 1.0
  defp severity_to_score("high"), do: 0.8
  defp severity_to_score("medium"), do: 0.5
  defp severity_to_score("low"), do: 0.3
  defp severity_to_score("info"), do: 0.1
  defp severity_to_score(_), do: 0.5

  defp sigmoid(x), do: 1.0 / (1.0 + :math.exp(-x))
end
