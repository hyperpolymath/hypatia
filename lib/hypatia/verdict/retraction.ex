# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Verdict.Retraction do
  @moduledoc """
  Quarantine as retraction (HYP-01).

  Quarantining a bot or a rule retracts an assumption. Each evidence token a
  finding was issued on rests on assumptions: `{:rule, "module/id"}` for the
  rule that fired and, when the token names one, `{:bot, name}` for the bot
  that reported it. A confirming prover re-check rests on `{:prover, name}`.

  The rule is residual-evidence-types' `survives-retraction` /
  `survives-dependent` (Revision.agda, pinned in `scripts/fetch-siblings.sh`),
  transcribed in `src/Hypatia/ABI/Retraction.idr`:

    * a token survives exactly when none of the assumptions it rests on has
      been retracted (its support thins into what remains);
    * a finding survives while one of its tokens does, or while it rests on a
      confirming prover re-check whose prover is not retracted;
    * a surviving finding keeps only its surviving tokens, and its level is
      re-derived from them;
    * a finding with nothing left is retracted, never silently downgraded:
      it moves to the retracted list, with its original evidence kept for
      audit and the retracted assumptions it rested on named
      (`:retracted_by`, wire names from `name/1`).

  Pure: the caller supplies the retracted assumptions
  (`Hypatia.Safety.Quarantine.retracted_assumptions/0` reads them from the
  quarantine table).
  """

  alias Hypatia.Verdict

  @type assumption :: {:rule, String.t()} | {:bot, String.t()} | {:prover, String.t()}

  @doc """
  Wire name of an assumption (`"rule:<module>/<id>"`, `"bot:<name>"`,
  `"prover:<name>"`), as written into `:retracted_by`.
  """
  @spec name(assumption()) :: String.t()
  def name({kind, id}) when kind in [:rule, :bot, :prover], do: "#{kind}:#{id}"

  @doc """
  The assumption a quarantine key retracts: `"rule:<module>/<id>"` a rule,
  `"prover:<name>"` a prover, anything else the bot of that name (the
  quarantine table has always been keyed by bot name).
  """
  @spec parse(String.t()) :: assumption()
  def parse("rule:" <> rule), do: {:rule, rule}
  def parse("prover:" <> prover), do: {:prover, prover}
  def parse("bot:" <> bot), do: {:bot, bot}
  def parse(bot) when is_binary(bot), do: {:bot, bot}

  @doc "The assumptions an evidence token rests on."
  @spec token_support(map()) :: [assumption()]
  def token_support(token) do
    rule = [{:rule, to_string(Map.get(token, :rule, ""))}]

    case Map.get(token, :bot) do
      nil -> rule
      bot -> rule ++ [{:bot, to_string(bot)}]
    end
  end

  @doc "Does the token survive the retraction of `retracted`?"
  @spec token_survives?(map(), MapSet.t(assumption())) :: boolean()
  def token_survives?(token, retracted) do
    token |> token_support() |> Enum.all?(&(not MapSet.member?(retracted, &1)))
  end

  @doc """
  Split `findings` into those that survive the retraction of `retracted` and
  those that do not. Returns `{kept, retracted_findings}`.
  """
  @spec retract([map()], Enumerable.t()) :: {[map()], [map()]}
  def retract(findings, retracted) do
    retracted = MapSet.new(retracted)

    findings
    |> Enum.map(&retract_one(&1, retracted))
    |> Enum.split_with(&match?({:kept, _}, &1))
    |> then(fn {kept, gone} ->
      {Enum.map(kept, &elem(&1, 1)), Enum.map(gone, &elem(&1, 1))}
    end)
  end

  defp retract_one(finding, retracted) do
    evidence = Map.get(finding, :evidence) || []
    {alive, dead} = Enum.split_with(evidence, &token_survives?(&1, retracted))
    support = Map.get(finding, :support)
    recheck_retracted? = recheck_retracted?(support, retracted)
    # A re-check whose prover is retracted supports nothing any more: what
    # is left is the pattern match its tokens record.
    kept_support = if recheck_retracted?, do: %{kind: "pattern"}, else: support

    cond do
      dead == [] and not recheck_retracted? ->
        {:kept, finding}

      alive != [] or confirming_recheck?(kept_support) ->
        {:kept,
         finding
         |> Map.put(:support, kept_support)
         |> Map.put(:evidence, alive)
         |> Map.put(:retracted_evidence, dead)
         |> maybe_put(:retracted_support, recheck_retracted?, support)
         |> Map.put(:level, Verdict.level(kept_support, alive))}

      true ->
        by = retracted_by(dead, retracted) ++ prover_retracted(support, recheck_retracted?)
        {:retracted, Map.put(finding, :retracted_by, Enum.map(by, &name/1))}
    end
  end

  defp confirming_recheck?(%{kind: "mechanical", result: "confirms"}), do: true
  defp confirming_recheck?(_support), do: false

  # Only a confirming re-check supports the finding, so only its prover can
  # be retracted support; a refuting re-check never rested the finding on
  # anything, and naming its prover in :retracted_by would be false.
  defp recheck_retracted?(%{kind: "mechanical", result: "confirms", prover: prover}, retracted),
    do: MapSet.member?(retracted, {:prover, prover})

  defp recheck_retracted?(_support, _retracted), do: false

  defp prover_retracted(%{prover: prover}, true), do: [{:prover, prover}]
  defp prover_retracted(_support, false), do: []

  defp maybe_put(map, _key, false, _value), do: map
  defp maybe_put(map, key, true, value), do: Map.put(map, key, value)

  defp retracted_by(tokens, retracted) do
    tokens
    |> Enum.flat_map(&token_support/1)
    |> Enum.filter(&MapSet.member?(retracted, &1))
    |> Enum.uniq()
  end
end
