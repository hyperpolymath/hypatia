# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Mix.Tasks.Hypatia.Reconcile do
  @moduledoc """
  Close the GitHub code-scanning alert-lifecycle loop for a repo.

  ## Usage

      mix hypatia.reconcile owner/repo                # reconcile + write registry
      mix hypatia.reconcile owner/repo --dry-run      # classify only, no mutation
      mix hypatia.reconcile owner/repo --verify       # recurrence-defect check
      mix hypatia.reconcile owner/repo --conservative # escalate :fix_settings
                                                      # instead of auto-applying

  Policy (#265): default `:full_auto` auto-applies repo-configuration
  remediations (`:fix_settings` — branch protection / required reviews) via
  the GitHub settings API. `--conservative` escalates those for human review
  instead.

  Requires `GITHUB_TOKEN` (security_events scope). Non-actionable and
  design-correct-exception findings are dismissed-with-rationale on GitHub
  and recorded by stable fingerprint in
  `~/.git-private-farm/hypatia-exception-registry.json`, so a class
  adjudicated once is never re-reasoned. See hyperpolymath/hypatia#263.
  """
  use Mix.Task
  alias Hypatia.ScorecardReconciler

  @shortdoc "Reconcile code-scanning alerts (dismiss/fix/escalate + learn)"

  @impl true
  def run(argv) do
    {opts, rest, _} =
      OptionParser.parse(argv,
        switches: [dry_run: :boolean, verify: :boolean, conservative: :boolean]
      )

    case rest do
      [slug] ->
        [owner, repo] = String.split(slug, "/", parts: 2)
        Mix.Task.run("app.start")

        result =
          if opts[:verify] do
            ScorecardReconciler.verify(owner, repo)
          else
            ScorecardReconciler.reconcile(owner, repo,
              dry_run: !!opts[:dry_run],
              policy: if(opts[:conservative], do: :conservative, else: :full_auto)
            )
          end

        IO.puts(Jason.encode!(result, pretty: true))

      _ ->
        Mix.raise("Usage: mix hypatia.reconcile owner/repo [--dry-run|--verify]")
    end
  end
end
