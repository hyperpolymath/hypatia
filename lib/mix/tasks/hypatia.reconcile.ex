# SPDX-License-Identifier: MPL-2.0
defmodule Mix.Tasks.Hypatia.Reconcile do
  @moduledoc """
  Close the GitHub code-scanning alert-lifecycle loop for a repo.

  ## Usage

      mix hypatia.reconcile owner/repo                # reconcile + write registry
      mix hypatia.reconcile owner/repo --dry-run      # classify only, no mutation
      mix hypatia.reconcile owner/repo --verify       # recurrence-defect check
      mix hypatia.reconcile owner/repo --apply-settings # also change repo
                                                        # settings (:fix_settings)

  Policy (#265, owner answer on hypatia#1080 group E): by default the
  repo-configuration findings (`:fix_settings`: branch protection, required
  reviews) are escalated to the owner, never changed. `--apply-settings`
  applies them through the GitHub settings API. `--conservative` is still
  accepted and is the default.

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
        switches: [
          dry_run: :boolean,
          verify: :boolean,
          conservative: :boolean,
          apply_settings: :boolean
        ]
      )

    case rest do
      [slug] ->
        [owner, repo] = String.split(slug, "/", parts: 2)
        Mix.Task.run("app.start")

        result =
          if opts[:verify] do
            # verify/2 returns {:ok, summary} | {:error, reason}; reconcile/3
            # returns a bare map. Normalise verify to a bare map so the
            # Jason.encode! below never receives a tuple (it cannot encode
            # one — `mix hypatia.reconcile --verify` used to crash here).
            case ScorecardReconciler.verify(owner, repo) do
              {:ok, summary} -> summary
              {:error, reason} -> %{repo: "#{owner}/#{repo}", verified: false, error: reason}
            end
          else
            ScorecardReconciler.reconcile(owner, repo,
              dry_run: !!opts[:dry_run],
              policy: policy(opts)
            )
          end

        IO.puts(Jason.encode!(result, pretty: true))

      _ ->
        Mix.raise("Usage: mix hypatia.reconcile owner/repo [--dry-run|--verify|--apply-settings]")
    end
  end

  @doc false
  # Settings are changed only on an explicit --apply-settings; --conservative
  # wins if both are given.
  def policy(opts) do
    if opts[:apply_settings] && !opts[:conservative], do: :full_auto, else: :conservative
  end
end
