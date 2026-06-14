# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.Strategist do
  @moduledoc """
  The merge-decision head — Wave-0 artifact 1 (the decision contract) made
  executable.

  Pure: takes a PR context and returns the decision tuple
  `route × method × safety` honouring the contract invariants —
    * monotone veto (any veto ⇒ `:flag`),
    * the object/meta reflexivity guard (`:meta` ⇒ `:flag`),
    * the pool clamp (a pool only ever makes a decision *more* conservative).

  No I/O. The brain decides; it never executes. Confidence aggregation here is a
  simple mean over approving attestations; the competence-weighted Graph-of-Trust
  version is `Kin.Council` (the next increment).
  """

  @auto_threshold 0.95
  @review_threshold 0.85

  # Safety ordering, most-conservative first: flag < review < arm_auto.
  @order %{flag: 0, review: 1, arm_auto: 2}

  @doc "Decide the merge action for a PR context map."
  def decide(ctx) do
    {safety, clamped_by} = decide_safety(ctx)

    %{
      pr: ctx.pr,
      change_class: ctx.change_class,
      change_level: ctx.change_level,
      route: decide_route(ctx),
      method: decide_method(ctx),
      safety: safety,
      pool: ctx.pool,
      attestations: Map.get(ctx, :attestations, []),
      vetoes: Map.get(ctx, :vetoes, []),
      clamped_by: clamped_by,
      rationale: rationale(ctx, safety, clamped_by)
    }
  end

  # --- Safety axis: confidence + veto + meta, clamped by pool (monotone) ---
  defp decide_safety(ctx) do
    cond do
      Map.get(ctx, :vetoes, []) != [] ->
        {:flag, "veto:" <> veto_reason(ctx)}

      ctx.change_level == :meta ->
        {:flag, "meta"}

      true ->
        ctx
        |> aggregate_confidence()
        |> confidence_to_safety()
        |> clamp_to_pool(ctx.pool)
    end
  end

  defp confidence_to_safety(conf) when conf >= @auto_threshold, do: :arm_auto
  defp confidence_to_safety(conf) when conf >= @review_threshold, do: :review
  defp confidence_to_safety(_), do: :flag

  # Pool caps the decision; never raises it.
  defp clamp_to_pool(base, pool) do
    cap = pool_cap(pool)
    safer = min_safety(base, cap)
    {safer, if(safer != base, do: "pool-cap:#{pool}", else: nil)}
  end

  defp pool_cap(:p0), do: :flag
  defp pool_cap(:p1), do: :review
  defp pool_cap(:p2), do: :arm_auto
  defp pool_cap(:p3), do: :arm_auto
  defp pool_cap(:mass_squash), do: :arm_auto

  defp min_safety(a, b), do: if(@order[a] <= @order[b], do: a, else: b)

  # --- Method axis: repo policy default ⊕ commit hygiene (never confidence) ---
  defp decide_method(ctx) do
    cond do
      ctx.change_class in [:chore, :bump] -> :squash
      ctx.change_class == :proof -> :merge_commit
      Map.get(ctx, :commits_atomic_green, false) -> :rebase
      true -> :squash
    end
  end

  # --- Route axis: change-class ⇒ authoritative sensor ---
  defp decide_route(ctx) do
    authority =
      cond do
        Map.get(ctx, :license_touch, false) -> nil
        ctx.change_class == :proof -> "echidnabot"
        ctx.change_class == :bump -> "patch-bridge"
        ctx.change_class == :security -> "panicbot"
        ctx.change_class in [:chore, :docs, :refactor] -> "robot-repo-automaton"
        true -> "rhodibot"
      end

    %{authority_bot: authority, contributing_bots: Map.get(ctx, :contributing_bots, ["ci"])}
  end

  # --- Confidence aggregation (Kin.Council stub: mean of approve attestations) ---
  defp aggregate_confidence(ctx) do
    case ctx |> Map.get(:attestations, []) |> Enum.filter(&(&1.verdict == :approve)) |> Enum.map(& &1.confidence) do
      [] -> 0.0
      cs -> Enum.sum(cs) / length(cs)
    end
  end

  defp veto_reason(ctx) do
    case Map.get(ctx, :vetoes, []) do
      [%{reason: r} | _] -> r
      _ -> "unspecified"
    end
  end

  defp rationale(ctx, safety, clamped_by) do
    base = "#{ctx.change_class}/#{ctx.change_level} -> #{safety}"
    if clamped_by, do: base <> " (#{clamped_by})", else: base
  end
end
