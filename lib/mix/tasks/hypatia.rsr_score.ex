# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.RsrScore do
  @shortdoc "Score a repository against the RSR v2.0 criteria (the rsr-conformance oracle)"
  @moduledoc """
  Runs the RSR v2.0 conformance oracle (`Hypatia.Rules.RsrConformance`) against a
  repository tree and reports its capability-gated score, tier, and — crucially —
  its automatable coverage and provisional flag.

      mix hypatia.rsr_score REPO_PATH [--ssot PATH] [--write [--source-commit SHA]] [--fail-under TIER]

  Options:

    * `--ssot PATH`   the RSR v2.0 criteria SSOT (default:
      `0-canon/rsr/rsr-criteria-v2.a2ml` under `--standards` (post-reorg home; the pre-reorg path was moved in the September 2026 reorg),
      else the bundled test fixture)
    * `--standards P` root of a checked-out `hyperpolymath/standards` (for the SSOT)
    * `--write`       write `REPO/.machine_readable/scorecards/rsr-scorecard_praxis.deed`,
      the RSR scorecard deed (standards `clauses/rsr-scorecard.abnf`, DRAFT; owner
      ruling 2026-11-14). It replaced `rsr.scorecard.a2ml`: A2ML is retired, so the
      oracle no longer writes one, and leaves any already in the repo alone (its
      removal is the repo's own migration PR)
    * `--source-commit SHA` record the scored commit (40 lowercase hex) in the
      scorecard; only with `--write`
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
        strict: [
          ssot: :string,
          standards: :string,
          write: :boolean,
          source_commit: :string,
          fail_under: :string
        ]
      )

    repo = List.first(args) || File.cwd!()
    ssot = resolve_ssot(opts)

    with {:ok, catalogue} <- RsrCriteria.load(ssot),
         {:ok, sc} <- RsrConformance.score(catalogue, repo) do
      report(sc)
      if opts[:write], do: write_scorecard(repo, sc, opts[:source_commit])
      enforce(sc, opts[:fail_under])
    else
      {:error, reason} ->
        Mix.shell().error("rsr_score: #{inspect(reason)} (ssot=#{ssot})")
        exit({:shutdown, 1})
    end
  end

  # Resolve the SSOT from candidates that exist on disk (#695). The old
  # default computed `_build/<env>/lib/hypatia/priv/../test/fixtures/…`
  # from `:code.priv_dir/1`, which lands INSIDE `_build` — the bare command
  # (the one the template dogfood gate and estate corpus run) failed with
  # `:enoent` unless `--ssot` was passed. Candidates are tried in order:
  # source-tree `priv/` (release-shaped home), the current dogfood home
  # under `test/fixtures/a2ml/`, then the built app's `priv/` (escript/
  # release). First existing file wins; if none exist, the dogfood path is
  # returned so the error message names the location it expected.
  defp resolve_ssot(opts) do
    cond do
      opts[:ssot] ->
        opts[:ssot]

      opts[:standards] ->
        Path.join(opts[:standards], "0-canon/rsr/rsr-criteria-v2.a2ml")

      true ->
        source_tree_root = Path.expand("../../..", __DIR__)

        candidates = [
          Path.join(source_tree_root, "priv/a2ml/rsr-criteria-v2.a2ml"),
          Path.join(source_tree_root, "test/fixtures/a2ml/rsr-criteria-v2.a2ml"),
          built_priv_candidate()
        ]

        Enum.find(candidates, List.last(candidates), &File.regular?/1)
    end
  end

  defp built_priv_candidate do
    case :code.priv_dir(:hypatia) do
      dir when is_list(dir) -> Path.join(to_string(dir), "a2ml/rsr-criteria-v2.a2ml")
      _ -> "/nonexistent/rsr-criteria-v2.a2ml"
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

  @scorecard_file "rsr-scorecard_praxis.deed"
  @legacy_scorecard_file "rsr.scorecard.a2ml"

  defp write_scorecard(repo, sc, commit) do
    if commit != nil and not Regex.match?(~r/\A[0-9a-f]{40}\z/, commit) do
      Mix.shell().error("rsr_score: --source-commit must be a full 40-character lowercase SHA")
      exit({:shutdown, 1})
    end

    dir = Path.join([repo, ".machine_readable", "scorecards"])
    File.mkdir_p!(dir)
    path = Path.join(dir, @scorecard_file)
    opts = RsrConformance.deed_context(repo) ++ if(commit, do: [source_commit: commit], else: [])
    File.write!(path, RsrConformance.to_deed(sc, opts))
    Mix.shell().info("  wrote #{path}")

    legacy = Path.join(dir, @legacy_scorecard_file)

    if File.exists?(legacy) do
      Mix.shell().info(
        "  note: #{legacy} is a legacy A2ML scorecard; it is no longer written or " <>
          "updated, and is left for the repo's own migration to remove"
      )
    end
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
