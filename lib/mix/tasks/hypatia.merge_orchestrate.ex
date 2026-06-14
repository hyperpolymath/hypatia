# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Mix.Tasks.Hypatia.MergeOrchestrate do
  @shortdoc "Run one merge-orchestration cycle (sense → deliberate → gate → manifest)"
  @moduledoc """
  The scheduled trigger for the merge-orchestration runtime. Reads the shared
  store, decides + gates every open-PR observation, and writes the merge-decision
  manifest the `.git-private-farm` actuator consumes.

      mix hypatia.merge_orchestrate [--store PATH] [--holder NAME]

  Invoke from cron / CI on a schedule. The competence weighting snapshots
  Graph-of-Trust automatically (degrading to a uniform council if the neural
  layer is unavailable). The brain only reads the store and writes the manifest;
  the actuator (a separate, token-bearing process) performs the merges.
  """
  use Mix.Task

  alias Hypatia.MergeOrchestration.Scheduler

  @impl Mix.Task
  def run(argv) do
    Mix.Task.run("app.start")
    {opts, _, _} = OptionParser.parse(argv, strict: [store: :string, holder: :string])

    result = Scheduler.cycle(Keyword.take(opts, [:store, :holder]))
    Mix.shell().info("[merge-orchestration] #{inspect(result.stats)} -> #{result.manifest_path}")
  end
end
