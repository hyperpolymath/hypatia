# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Mix.Tasks.Hypatia.DeployPreventionWorkflows do
  @moduledoc """
  Deploy prevention workflows to hyperpolymath repos with open alerts.

  Ported from `.audittraining/scripts/deploy-prevention-workflows.jl`.

  ## Usage

      mix hypatia.deploy_prevention_workflows
      mix hypatia.deploy_prevention_workflows --dry-run

  Reads workflow source files from `.audittraining/prevention/*.yml` and
  pushes each missing one to every repo listed in
  `.audittraining/security-errors/all_alerts.txt` (or every repo returned
  by `gh repo list hyperpolymath --limit 300` if that file is absent).

  Deploys via `gh api repos/hyperpolymath/<repo>/contents/.github/workflows/<wf> -X PUT`.

  `cargo-audit.yml` is only deployed to repos that have a `Cargo.toml`.
  """

  use Mix.Task

  @shortdoc "Deploy prevention workflows org-wide"

  @org "hyperpolymath"
  @workflows ~w(workflow-linter.yml cargo-audit.yml secret-scanner.yml scorecard-enforcer.yml)
  @prevention_dir ".audittraining/prevention"
  @alerts_file ".audittraining/security-errors/all_alerts.txt"

  @impl Mix.Task
  def run(args) do
    {opts, _rest, _invalid} =
      OptionParser.parse(args, switches: [dry_run: :boolean], aliases: [n: :dry_run])

    dry_run? = Keyword.get(opts, :dry_run, false)

    if dry_run?, do: Mix.shell().info("🔄 DRY RUN MODE — no changes will be made")

    Mix.shell().info("📦 Deploying prevention workflows to #{@org} repos...")

    repos = repos_with_alerts()
    Mix.shell().info("Processing #{length(repos)} repos...")

    summary =
      repos
      |> Enum.with_index(1)
      |> Enum.reduce(%{deployed: 0, skipped: 0, failed: 0}, fn {repo, i}, acc ->
        Mix.shell().info("\n[#{i}/#{length(repos)}] #{repo}")
        rust? = rust_repo?(repo)

        Enum.reduce(@workflows, acc, fn wf, acc2 ->
          cond do
            wf == "cargo-audit.yml" and not rust? ->
              acc2

            workflow_exists?(repo, wf) ->
              Mix.shell().info("  - #{wf} (already exists)")
              Map.update!(acc2, :skipped, &(&1 + 1))

            deploy_workflow(repo, wf, dry_run?) ->
              Map.update!(acc2, :deployed, &(&1 + 1))

            true ->
              Map.update!(acc2, :failed, &(&1 + 1))
          end
        end)
      end)

    Mix.shell().info("""

    📊 Summary:
      Deployed: #{summary.deployed}
      Skipped: #{summary.skipped}
      Failed: #{summary.failed}
    """)
  end

  defp repos_with_alerts do
    alerts_path = Path.expand(@alerts_file)

    if File.exists?(alerts_path) do
      alerts_path
      |> File.stream!()
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(fn line -> line |> String.split(":", parts: 2) |> List.first() end)
      |> Enum.uniq()
    else
      Mix.shell().info("Fetching repos with alerts from GitHub...")
      {out, 0} = System.cmd("gh", ["repo", "list", @org, "--limit", "300", "--json", "name", "--jq", ".[].name"])
      out |> String.split("\n", trim: true)
    end
  end

  defp rust_repo?(repo) do
    api_path = "repos/" <> @org <> "/" <> repo <> "/contents/Cargo.toml"

    case System.cmd("gh", ["api", api_path], stderr_to_stdout: true) do
      {_, 0} -> true
      _ -> false
    end
  end

  defp workflow_exists?(repo, workflow) do
    api_path =
      "repos/" <> @org <> "/" <> repo <> "/contents/.github/workflows/" <> workflow

    case System.cmd("gh", ["api", api_path], stderr_to_stdout: true) do
      {_, 0} -> true
      _ -> false
    end
  end

  defp deploy_workflow(repo, workflow, dry_run?) do
    src_path = Path.join(@prevention_dir, workflow)

    cond do
      not File.exists?(src_path) ->
        Mix.shell().info("  WARNING: workflow source not found: #{src_path}")
        false

      dry_run? ->
        Mix.shell().info("  [DRY-RUN] Would deploy #{workflow} to #{repo}")
        true

      true ->
        content = File.read!(src_path)
        content_b64 = Base.encode64(content)
        api_path = "repos/#{@org}/#{repo}/contents/.github/workflows/#{workflow}"
        message = "Add #{workflow} prevention workflow"

        existing_sha =
          case System.cmd("gh", ["api", api_path, "--jq", ".sha"], stderr_to_stdout: true) do
            {out, 0} -> String.trim(out)
            _ -> nil
          end

        base_args = [
          "api",
          api_path,
          "-X",
          "PUT",
          "-f",
          "message=#{message}",
          "-f",
          "content=#{content_b64}"
        ]

        args =
          if existing_sha, do: base_args ++ ["-f", "sha=#{existing_sha}"], else: base_args

        case System.cmd("gh", args, stderr_to_stdout: true) do
          {_, 0} ->
            Mix.shell().info("  ✓ Deployed #{workflow}")
            true

          {err, _} ->
            Mix.shell().info("  ✗ Failed to deploy #{workflow}: #{err}")
            false
        end
    end
  end
end
