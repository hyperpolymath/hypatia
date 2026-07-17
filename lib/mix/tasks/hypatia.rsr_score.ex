# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.RsrScore do
  @shortdoc "Score a repository against the RSR v2.0 criteria (the rsr-conformance oracle)"
  @moduledoc """
  Runs the RSR v2.0 conformance oracle (`Hypatia.Rules.RsrConformance`) against a
  repository tree and reports its capability-gated score, tier, and — crucially —
  its automatable coverage and provisional flag.

      mix hypatia.rsr_score REPO_PATH [--ssot PATH] [--write] [--fail-under TIER]

  Options:

    * `--ssot PATH`   the RSR v2.0 criteria SSOT (default:
      `rhodium-standard-repositories/spec/rsr-criteria-v2.a2ml` under `--standards`,
      else the bundled test fixture)
    * `--standards P` root of a checked-out `hyperpolymath/standards` (for the SSOT)
    * `--write`       write `REPO/.machine_readable/scorecards/rsr.scorecard.a2ml`
    * `--fail-under T` exit non-zero unless tier >= T (`bronze|silver|gold|rhodium`);
      the dogfood gate uses `--fail-under gold`. A *provisional* scorecard never
      satisfies `--fail-under` (no firm tier claim from partial coverage).

  This is the CLI the template dogfood gate and estate corpus run both call.
  """
  use Mix.Task

  alias Hypatia.Rules.RsrConformance
  alias Hypatia.Rules.RsrCriteria

  @tier_rank %{"none" => 0, "bronze" => 1, "silver" => 2, "gold" => 3, "rhodium" => 4}

  @impl Mix.Task
  def run(argv) do
    {opts, args, _} =
      OptionParser.parse(argv,
        strict: [ssot: :string, standards: :string, write: :boolean, fail_under: :string]
      )

    repo = List.first(args) || File.cwd!()
    ssot = resolve_ssot(opts)

    with {:ok, catalogue} <- RsrCriteria.load(ssot),
         {:ok, sc} <- RsrConformance.score(catalogue, repo) do
      report(sc)
      if opts[:write], do: write_scorecard(repo, sc)
      enforce(sc, opts[:fail_under])
    else
      {:error, reason} ->
        Mix.shell().error("rsr_score: #{inspect(reason)} (ssot=#{ssot})")
        exit({:shutdown, 1})
    end
  end

  defp resolve_ssot(opts) do
    cond do
      opts[:ssot] ->
        opts[:ssot]

      opts[:standards] ->
        Path.join(opts[:standards], "rhodium-standard-repositories/spec/rsr-criteria-v2.a2ml")

      true ->
        Path.join(:code.priv_dir(:hypatia) |> to_string(), "..")
        |> Path.join("test/fixtures/a2ml/rsr-criteria-v2.a2ml")
        |> Path.expand()
    end
  end

  defp report(sc) do
    shell = Mix.shell()
    shell.info("RSR v2.0 scorecard — #{sc.repo}  (spec #{sc.spec_version})")

    shell.info(
      "  tier=#{sc.tier}  score=#{sc.score}%  coverage=#{round(sc.automatable_coverage * 100)}%" <>
        "  provisional=#{sc.provisional}  profile=#{sc.profile_present}"
    )

    tally = Enum.frequencies_by(sc.results, & &1.verdict)

    shell.info(
      "  verdicts: " <>
        Enum.map_join([:pass, :partial, :fail, :unverified, :na], "  ", fn v ->
          "#{v}=#{Map.get(tally, v, 0)}"
        end)
    )

    fails = Enum.filter(sc.results, &(&1.verdict == :fail))

    unless fails == [] do
      shell.info("  failing (applicable): " <> Enum.map_join(fails, ", ", & &1.id))
    end
  end

  defp write_scorecard(repo, sc) do
    dir = Path.join([repo, ".machine_readable", "scorecards"])
    File.mkdir_p!(dir)
    path = Path.join(dir, "rsr.scorecard.a2ml")
    File.write!(path, RsrConformance.to_record_dialect(sc))
    Mix.shell().info("  wrote #{path}")
  end

  defp enforce(_sc, nil), do: :ok

  defp enforce(sc, tier) do
    want = Map.get(@tier_rank, tier, 99)
    got = Map.get(@tier_rank, sc.tier, 0)

    cond do
      sc.provisional ->
        Mix.shell().error(
          "FAIL: scorecard is provisional (coverage " <>
            "#{round(sc.automatable_coverage * 100)}% < 100%) — no firm tier claim; " <>
            "cannot satisfy --fail-under #{tier}"
        )

        exit({:shutdown, 1})

      got >= want ->
        Mix.shell().info("PASS: tier #{sc.tier} >= #{tier}")

      true ->
        Mix.shell().error("FAIL: tier #{sc.tier} < required #{tier}")
        exit({:shutdown, 1})
    end
  end
end
