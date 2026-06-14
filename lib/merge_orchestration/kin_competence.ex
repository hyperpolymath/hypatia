# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.KinCompetence do
  @moduledoc """
  Competence weighting for the Kin Council — Graph-of-Trust × MoE domain gating,
  made into the `(attestation -> float)` weight `KinCouncil.aggregate/2` already
  accepts.

  The council was uniform (every approving bot counted `1.0`). This grounds the
  weight in two signals the estate already computes:

    * **trust** — `Neural.GraphOfTrust.trust_score/2` per bot (PageRank over the
      outcome graph): how much has this bot *earned* by being right before;
    * **domain competence** — the MoE-style specialist gate: a bot's vote on a
      change counts in full when it is the **route authority** for that
      change-class (echidnabot⇄proof, patch-bridge⇄bump, panicbot⇄security,
      robot-repo-automaton⇄chore/docs/refactor, rhodibot⇄structure) and is
      discounted (`generalist`, default `0.5`) otherwise.

  `weight = trust(bot) × competence(bot, domain)`. A trust of `0.0` yields weight
  `0.0`, which `KinCouncil` reads as *recuse* — an untrusted bot's **approval**
  stops counting, while its **veto** still flags (vetoes are monotone, never
  weighted). The functions are pure over a plain trust *snapshot*, so
  `Strategist`/`KinCouncil` stay I/O-free; the operational layer fetches the
  snapshot from the GoT GenServer and threads it in via `ctx[:weight]`.
  """

  alias Hypatia.MergeOrchestration.Strategist

  @generalist 0.5

  @doc """
  Build the council weight fn. `opts`:
    * `:trust` — `%{bot => trust_float}` (unknown bot ⇒ neutral `1.0`);
    * `:authority` — the specialist bot string for this change (or `nil`);
    * `:generalist` — competence for a non-specialist approver (default `0.5`).
  """
  def weight_fn(opts \\ []) do
    trust = Keyword.get(opts, :trust, %{})
    authority = Keyword.get(opts, :authority, nil)
    generalist = Keyword.get(opts, :generalist, @generalist)

    fn attestation ->
      bot = attestation.bot
      t = Map.get(trust, bot, 1.0)
      competence = if not is_nil(authority) and bot == authority, do: 1.0, else: generalist
      t * competence
    end
  end

  @doc """
  Attach a competence-aware `:weight` to a decision `ctx` so `Strategist.decide/1`
  uses it. `trust` is the GoT snapshot `%{bot => score}`; the specialist is
  derived from the ctx's change-class via `Strategist.route_authority/1`.
  """
  def attach_weight(ctx, trust, opts \\ []) do
    Map.put(
      ctx,
      :weight,
      weight_fn([trust: trust, authority: Strategist.route_authority(ctx)] ++ opts)
    )
  end

  @doc """
  Snapshot a trust map for `bots` from a `Neural.GraphOfTrust` struct
  (`trust_score/2`). Thin glue over the neural layer; pure given the struct.
  """
  def trust_from_got(got, bots) when is_list(bots) do
    Map.new(bots, fn bot -> {bot, apply(got.__struct__, :trust_score, [got, bot])} end)
  end
end
