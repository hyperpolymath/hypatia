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

    * `manifest` — write `merge-decisions.jsonl`; the `.git-private-farm`
      actuator (a separate, token-bearing process) polls and performs the merges.
    * `baton` — submit one Bag-of-Actions Baton per armed decision. Each carries
      `required_cap: "secret_access"`, which only the token-bearing
      `mesh-github-runner` node holds — so the merge migrates off the (token-free)
      brain to the runner, off GitHub Actions minutes.
    * `both` — manifest as the durable record/fallback, Batons as the live path.
  """
  use Mix.Task

  alias Hypatia.MergeOrchestration.Scheduler

  @actuations ~w(manifest baton both)

  @impl Mix.Task
  def run(argv) do
    Mix.Task.run("app.start")

    {opts, _, _} =
      OptionParser.parse(argv, strict: [store: :string, holder: :string, actuation: :string])

    cycle_opts =
      opts
      |> Keyword.take([:store, :holder])
      |> put_actuation(opts[:actuation])

    result = Scheduler.cycle(cycle_opts)

    summary =
      [
        result[:manifest_path],
        result[:batons] && "#{length(result[:batons])} baton(s)"
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.join(" + ")

    Mix.shell().info("[merge-orchestration] #{inspect(result.stats)} -> #{summary}")
  end

  defp put_actuation(cycle_opts, nil), do: cycle_opts

  defp put_actuation(cycle_opts, mode) when mode in @actuations,
    do: Keyword.put(cycle_opts, :actuation, String.to_existing_atom(mode))

  defp put_actuation(_cycle_opts, mode),
    do: Mix.raise("--actuation must be one of #{Enum.join(@actuations, " | ")}, got: #{mode}")
end
