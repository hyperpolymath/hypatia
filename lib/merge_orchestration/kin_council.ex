# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.KinCouncil do
  @moduledoc """
  The competence-weighted veto council — the consensus mechanism made executable.

  NOT Byzantine: the bots are cooperative specialists, not adversaries, so we do
  not seek agreement-despite-traitors. Instead we *aggregate domain evidence
  weighted by competence*, with a *monotone safety veto* — any veto halts to
  `:flag` and is never out-voted. Integrity against tampering is bought
  separately, by signed attestations (artifact 3), not by quorum.

  `aggregate/2` folds a list of attestations into
  `%{confidence, vetoes, has_veto, contributing}`:

    * `confidence` — competence-weighted mean over the *approving* attestations
      (weight = Graph-of-Trust trust × MoE domain competence, via `opts[:weight]`);
    * `vetoes` — every `:veto` attestation, surfaced (monotone);
    * incompetent experts (weight 0) recuse — not counted, not out-voted.
  """

  @type attestation :: %{
          required(:bot) => String.t(),
          required(:verdict) => :approve | :hold | :veto,
          optional(:confidence) => float(),
          optional(:rationale) => String.t()
        }

  @doc """
  Fold attestations into an aggregate. `opts[:weight]` is `(attestation -> float)`;
  default is uniform `1.0`. A weight of `0.0` recuses that expert.
  """
  def aggregate(attestations, opts \\ []) when is_list(attestations) do
    weight = Keyword.get(opts, :weight, fn _ -> 1.0 end)

    vetoes =
      for a <- attestations, a.verdict == :veto do
        %{bot: a.bot, reason: Map.get(a, :rationale, "unspecified")}
      end

    weighted =
      for a <- attestations, a.verdict == :approve, w = weight.(a), w > 0.0 do
        {w, Map.get(a, :confidence, 0.0)}
      end

    confidence =
      case weighted do
        [] ->
          0.0

        pairs ->
          total = Enum.sum(for {w, _} <- pairs, do: w)
          Enum.sum(for {w, c} <- pairs, do: w * c) / total
      end

    %{
      confidence: confidence,
      vetoes: vetoes,
      has_veto: vetoes != [],
      contributing: attestations |> Enum.map(& &1.bot) |> Enum.uniq()
    }
  end

  @doc "Compose a competence weight from a trust score (Graph-of-Trust) and a domain competence (MoE gate)."
  def competence_weight(trust, competence) when is_number(trust) and is_number(competence),
    do: trust * competence
end
