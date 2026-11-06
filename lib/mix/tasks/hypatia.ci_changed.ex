# SPDX-License-Identifier: MPL-2.0
defmodule Mix.Tasks.Hypatia.CiChanged do
  use Mix.Task
  @shortdoc "Run changed workflow jobs and the Hypatia scan before push"

  @impl true
  def run(argv) do
    {opts, args, invalid} = OptionParser.parse(argv, strict: [base: :string, plan: :boolean])

    if args != [] or invalid != [],
      do: Mix.raise("usage: mix hypatia.ci_changed [--base REV] [--plan]")

    Mix.Task.run("compile")
    repo = File.cwd!()
    plan = Hypatia.CiChanged.plan(repo, Keyword.get(opts, :base, "origin/main"))

    if opts[:plan] do
      Mix.shell().info(
        Jason.encode!(Map.put(plan, :status, "planned, not executed"), pretty: true)
      )
    else
      findings = Hypatia.CiChanged.scan(repo, plan.files)

      {_shown, blocking} =
        Hypatia.CLI.report_sets(findings, %{severity: "medium", advisory: false})

      if blocking != [] do
        Mix.shell().info(Jason.encode!(findings, pretty: true))
        Mix.raise("Hypatia found #{length(blocking)} blocking findings in changed files")
      end

      if plan.jobs != [] and !System.find_executable("act"),
        do: Mix.raise("act is required to execute changed jobs; no parity pass recorded")

      results = Hypatia.CiChanged.execute_jobs(repo, plan.jobs)
      Enum.each(results, fn result -> Mix.shell().info(result.output) end)

      if Enum.any?(results, &(&1.exit != 0)),
        do: Mix.raise("changed workflow jobs failed; push is not ready")

      Mix.shell().info(
        "#{length(plan.jobs)} changed jobs executed; Hypatia scan of #{length(plan.files)} changed files passed"
      )
    end
  end
end
