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

  Confidence and bot-vetoes come from `KinCouncil.aggregate/2` over the PR's
  attestations (artifact 3); a license/SPDX touch adds a symbolic owner-only
  veto. No I/O — the brain decides; it never executes.
  """

  alias Hypatia.MergeOrchestration.KinCouncil

  @auto_threshold 0.95
  @review_threshold 0.85

  # Safety ordering, most-conservative first: flag < review < arm_auto.
  @order %{flag: 0, review: 1, arm_auto: 2}

  @doc "Decide the merge action for a PR context map."
  def decide(ctx) do
    council = KinCouncil.aggregate(Map.get(ctx, :attestations, []), weight: Map.get(ctx, :weight, &default_weight/1))
    vetoes = council.vetoes ++ symbolic_vetoes(ctx)
    {safety, clamped_by} = decide_safety(ctx, council.confidence, vetoes)

    %{
      pr: ctx.pr,
      change_class: ctx.change_class,
      change_level: ctx.change_level,
      route: decide_route(ctx),
      method: decide_method(ctx),
      safety: safety,
      pool: ctx.pool,
      confidence: council.confidence,
      attestations: Map.get(ctx, :attestations, []),
      vetoes: vetoes,
      clamped_by: clamped_by,
      rationale: rationale(ctx, safety, clamped_by)
    }
  end

  # --- Safety axis: confidence + veto + meta, clamped by pool (monotone) ---
  defp decide_safety(ctx, confidence, vetoes) do
    cond do
      vetoes != [] -> {:flag, "veto:" <> first_reason(vetoes)}
      ctx.change_level == :meta -> {:flag, "meta"}
      true -> confidence |> confidence_to_safety() |> clamp_to_pool(ctx.pool)
    end
  end

  defp confidence_to_safety(conf) when conf >= @auto_threshold, do: :arm_auto
  defp confidence_to_safety(conf) when conf >= @review_threshold, do: :review
  defp confidence_to_safety(_), do: :flag

  defp clamp_to_pool(base, pool) do
    safer = min_safety(base, pool_cap(pool))
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

  # license/SPDX is a symbolic, owner-only veto (not a bot attestation).
  defp symbolic_vetoes(ctx) do
    if Map.get(ctx, :license_touch, false),
      do: [%{bot: "policy-gate", reason: "license/SPDX -- owner-only"}],
      else: []
  end

  defp default_weight(_attestation), do: 1.0

  defp first_reason([%{reason: r} | _]), do: r
  defp first_reason(_), do: "unspecified"

  defp rationale(ctx, safety, clamped_by) do
    base = "#{ctx.change_class}/#{ctx.change_level} -> #{safety}"
    if clamped_by, do: base <> " (#{clamped_by})", else: base
  end
end
