# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.Scheduler do
  @moduledoc """
  The scheduled trigger — closes the autonomy loop.

  A cron / CI / GenServer tick calls `cycle/1`, which assembles the *live*
  inputs the pure `Loop` doesn't: it snapshots Graph-of-Trust into the
  competence weighting, resolves the store path, and runs one `Loop.run/1`.

  The Mix task `hypatia.merge_orchestrate` is the canonical entry point:

      mix hypatia.merge_orchestrate --store data/verisim

  Trust source (`:got` opt):

    * `:auto` (default) — build a live `GraphOfTrust` snapshot, degrading to a
      uniform council if the neural layer / outcome data isn't available (a
      scheduled job must never crash the cron);
    * `nil` — force a uniform council;
    * a struct — use it directly.

  The `GraphOfTrust` call is late-bound (`apply/3`) so this module stays
  decoupled from the neural stack at compile time and the logic tests run
  dependency-free.
  """

  alias Hypatia.MergeOrchestration.{Loop, KinCompetence}
  require Logger

  # the fleet whose attestations the council weighs (the bots route_authority names)
  @fleet ~w(echidnabot patch-bridge panicbot robot-repo-automaton rhodibot sustainabot ci)
  @default_store "data/verisim"

  @doc "Run one merge-orchestration cycle. Returns the `Loop.run/1` summary (+ `:manifest_path`)."
  def cycle(opts \\ []) do
    store = Keyword.get(opts, :store) || System.get_env("MERGE_ORCH_STORE") || @default_store
    trust = Keyword.get_lazy(opts, :trust, fn -> trust_snapshot(opts) end)

    passthrough = Keyword.take(opts, [:holder, :now, :encode, :decode, :acquire])
    result = Loop.run([store: store, trust: trust] ++ passthrough)

    Logger.info("[merge-orchestration] cycle #{inspect(result.stats)} -> #{result.manifest_path}")
    result
  end

  defp trust_snapshot(opts) do
    bots = Keyword.get(opts, :bots, @fleet)

    case Keyword.get(opts, :got, :auto) do
      nil -> %{}
      :auto -> auto_trust(bots)
      got -> KinCompetence.trust_from_got(got, bots)
    end
  end

  # Build a live GoT snapshot; degrade to a uniform council if it can't be built.
  defp auto_trust(bots) do
    got = apply(Hypatia.Neural.GraphOfTrust, :build, [])
    KinCompetence.trust_from_got(got, bots)
  rescue
    e ->
      Logger.warning(
        "[merge-orchestration] GoT snapshot unavailable (#{inspect(e)}); uniform council"
      )

      %{}
  end
end
