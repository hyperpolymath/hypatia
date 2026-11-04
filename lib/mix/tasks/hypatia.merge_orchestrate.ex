# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Mix.Tasks.Hypatia.MergeOrchestrate do
  @shortdoc "Run one merge-orchestration cycle (sense → deliberate → gate → manifest)"
  @moduledoc """
  The scheduled trigger for the merge-orchestration runtime. Reads the shared
  store, decides + gates every open-PR observation, and writes the merge-decision
  manifest the `.git-private-farm` actuator consumes.

      mix hypatia.merge_orchestrate [--store PATH] [--holder NAME] [--actuation MODE]

  Invoke from cron / CI on a schedule. The competence weighting snapshots
  Graph-of-Trust automatically (degrading to a uniform council if the neural
  layer is unavailable). The brain only reads the store; it stays token-free.

  `--actuation` selects the actuation backend (default `manifest`):

    * `manifest` — write `merge-decisions.jsonl` for separate, explicitly
      authorized farm actuation. Writing the manifest does not merge a PR.
    * `baton` and `both` — currently unavailable: no qualified merge executor
      exists. The task raises before starting a cycle, even with zero armed
      decisions. It submits no Baton and writes no partial manifest.

  Use `manifest` for the available decision-artifact path. The pure policy model
  and descriptive refusal records do not establish an operational Baton backend.
  """
  use Mix.Task

  alias Hypatia.MergeOrchestration.{BatonEmitter, Scheduler}

  @actuations ~w(manifest baton both)

  @impl Mix.Task
  def run(argv) do
    {opts, _, _} =
      OptionParser.parse(argv, strict: [store: :string, holder: :string, actuation: :string])

    cycle_opts =
      opts
      |> Keyword.take([:store, :holder])
      |> put_actuation(opts[:actuation])

    require_backend!(Keyword.get(cycle_opts, :actuation, :manifest))
    Mix.Task.run("app.start")
    result = Scheduler.cycle(cycle_opts)

    Mix.shell().info("[merge-orchestration] #{inspect(result.stats)} -> #{result.manifest_path}")
  end

  defp require_backend!(:manifest), do: :ok

  defp require_backend!(mode) when mode in [:baton, :both] do
    # Availability currently has only this explicit refusal result. A future
    # qualified executor must update this preflight and its argv controls.
    {:error, reason} = BatonEmitter.availability()

    Mix.raise(
      "--actuation #{mode} unavailable: #{reason}; no cycle started, " <>
        "no Baton submitted and no manifest written. Use --actuation manifest."
    )
  end

  defp put_actuation(cycle_opts, nil), do: cycle_opts

  defp put_actuation(cycle_opts, mode) when mode in @actuations,
    do: Keyword.put(cycle_opts, :actuation, String.to_existing_atom(mode))

  defp put_actuation(_cycle_opts, mode),
    do: Mix.raise("--actuation must be one of #{Enum.join(@actuations, " | ")}, got: #{mode}")
end
