# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.RsrConformance do
  @moduledoc """
  The RSR v2.0 conformance **oracle** (increment 3 of the HYP-S family):
  scores a repository tree against the criteria catalogue loaded by
  `Hypatia.Rules.RsrCriteria`, per `RSR-SPEC-v2.adoc` §5 (capability-gated
  scoring) and §7 (published automatable coverage).

  ## What this increment does — and does not — claim

  RSR v2.0 names hypatia's `rsr-conformance` family as the ONE normative
  oracle. This module implements the **scoring engine** faithfully:

    * applicable set = universal criteria ∪ criteria whose `gate` capability
      the repo declares in `.machine_readable/rsr-profile.a2ml`;
      non-applicable criteria are `:na` and excluded from the denominator;
    * verdicts `:pass` / `:partial` (half weight) / `:fail`;
    * score = passed weight / applicable **verified** weight;
    * tier from the catalogue's thresholds.

  Detection, however, is deliberately partial in this increment: a built-in
  table of ~30 file-presence/parse checks covers the mechanically-checkable
  criteria (community-health files, the descriptile substrate, workflows).
  Every other criterion — including all `detect = "manual"` ones and those
  whose `detect` names a scanner rule this oracle does not yet execute — is
  returned as `:unverified`, counted in `automatable_coverage`, and **never**
  silently passed. Because coverage < 100%, every scorecard this increment
  produces carries `provisional = true`: per the no-overclaim doctrine a firm
  tier claim requires full detection coverage (and the spec's ratification
  bar requires exactly that before RSR v2.0 leaves Draft).

  ## Weight interpretation

  The SSOT assigns weights per **category**; the spec's scoring model sums
  weights per **criterion**. This oracle distributes each category's weight
  equally across its criteria (documented interpretation; flagged as a spec
  clarification for RSR v2.0.x).
  """

  alias Hypatia.A2ml.RecordDialect
  alias Hypatia.Rules.RsrCriteria.Criterion

  defmodule Scorecard do
    @moduledoc "Result of scoring one repository against the RSR catalogue."
    defstruct repo: nil,
              spec_version: nil,
              capabilities: [],
              profile_present: false,
              results: [],
              applicable_weight: 0.0,
              verified_weight: 0.0,
              passed_weight: 0.0,
              score: 0.0,
              automatable_coverage: 0.0,
              tier: "none",
              provisional: true

    @type t :: %__MODULE__{}
  end

  @doc """
  Score the repository at `repo_path` against a catalogue from
  `RsrCriteria.load/1`. Returns `{:ok, %Scorecard{}}`; never raises on a
  missing or sparse tree (absent files are findings, not crashes).
  """
  @spec score(map(), String.t()) :: {:ok, Scorecard.t()}
  def score(%{criteria: criteria} = catalogue, repo_path) do
    caps = declared_capabilities(repo_path)
    weights = criterion_weights(catalogue)

    results =
      Enum.map(criteria, fn %Criterion{} = c ->
        verdict = evaluate(c, repo_path, caps)

        %{
          id: c.id,
          name: c.name,
          tier: c.tier,
          gate: c.gate,
          detect: c.detect,
          weight: Map.get(weights, c.id, 0.0),
          verdict: verdict
        }
      end)

    applicable = Enum.reject(results, &(&1.verdict == :na))
    verified = Enum.filter(applicable, &(&1.verdict in [:pass, :partial, :fail]))

    applicable_w = weight_sum(applicable)
    verified_w = weight_sum(verified)

    passed_w =
      Enum.reduce(verified, 0.0, fn r, acc ->
        case r.verdict do
          :pass -> acc + r.weight
          :partial -> acc + r.weight / 2
          _ -> acc
        end
      end)

    score = if verified_w > 0.0, do: passed_w / verified_w * 100.0, else: 0.0
    coverage = if applicable_w > 0.0, do: verified_w / applicable_w, else: 0.0
    provisional = coverage < 1.0

    {:ok,
     %Scorecard{
       repo: Path.basename(Path.expand(repo_path)),
       spec_version: Map.get(catalogue, :version),
       capabilities: caps,
       profile_present: caps != :none,
       results: results,
       applicable_weight: Float.round(applicable_w, 4),
       verified_weight: Float.round(verified_w, 4),
       passed_weight: Float.round(passed_w, 4),
       score: Float.round(score, 2),
       automatable_coverage: Float.round(coverage, 4),
       tier: tier_for(score, provisional, results, Map.get(catalogue, :tiers, %{})),
       provisional: provisional
     }}
  end

  @doc """
  Serialize a scorecard as A2ML record dialect (the estate scorecard shape),
  parseable back by `Hypatia.A2ml.RecordDialect` — the oracle's output is
  itself a conforming record-dialect document.
  """
  @spec to_record_dialect(Scorecard.t()) :: String.t()
  def to_record_dialect(%Scorecard{} = sc) do
    header = """
    # SPDX-License-Identifier: MPL-2.0
    # #{sc.repo}.rsr-scorecard.a2ml — GENERATED by Hypatia.Rules.RsrConformance.
    # Provisional scorecards MUST NOT be cited as a firm tier claim
    # (RSR-SPEC-v2.adoc section 10; automatable coverage below).

    [scorecard]
    repo = "#{sc.repo}"
    spec = "rsr-criteria-v2"
    spec-version = "#{sc.spec_version}"
    score = #{sc.score}
    tier = "#{sc.tier}"
    provisional = #{sc.provisional}
    automatable-coverage = #{sc.automatable_coverage}
    applicable-weight = #{sc.applicable_weight}
    verified-weight = #{sc.verified_weight}
    profile-present = #{sc.profile_present}
    """

    blocks =
      Enum.map(sc.results, fn r ->
        """

        [[result]]
        id = "#{r.id}"
        name = "#{r.name}"
        tier = "#{r.tier}"
        gate = "#{r.gate}"
        weight = #{Float.round(r.weight, 4)}
        verdict = "#{r.verdict}"
        """
      end)

    IO.iodata_to_binary([header | blocks])
  end

  # --- applicability ---------------------------------------------------------

  # Reads `.machine_readable/rsr-profile.a2ml` (record dialect). Accepts the
  # capability list under `[rsr-profile]` or `[profile]`, key `capabilities`.
  # Returns `:none` when the file is absent/unreadable — only universal
  # criteria are then applicable, and criterion 3.2.2 (profile presence,
  # itself universal) fails, which is exactly the intended signal.
  defp declared_capabilities(repo_path) do
    path = Path.join([repo_path, ".machine_readable", "rsr-profile.a2ml"])

    with {:ok, text} <- File.read(path),
         {:ok, tree} <- RecordDialect.parse(text),
         section when is_map(section) <-
           Map.get(tree, "rsr-profile") || Map.get(tree, "profile"),
         caps when is_list(caps) <- Map.get(section, "capabilities") do
      Enum.filter(caps, &is_binary/1)
    else
      _ -> :none
    end
  end

  defp applicable?(%Criterion{gate: "universal"}, _caps), do: true
  defp applicable?(%Criterion{}, :none), do: false
  defp applicable?(%Criterion{gate: gate}, caps), do: gate in caps

  defp criterion_weights(%{categories: categories}) do
    Enum.reduce(categories, %{}, fn cat, acc ->
      weight = Map.get(cat, "weight", 0)
      criteria = Map.get(cat, "criteria", [])
      n = max(length(criteria), 1)
      per = weight / n

      Enum.reduce(criteria, acc, fn c, inner ->
        Map.put(inner, Map.get(c, "id"), per)
      end)
    end)
  end

  defp weight_sum(results), do: Enum.reduce(results, 0.0, &(&1.weight + &2))

  # --- tiering ---------------------------------------------------------------

  # Thresholds come from the catalogue's [tiers] block. Rhodium additionally
  # requires every applicable rhodium-tier criterion to pass — and, like any
  # firm claim, full coverage; a provisional run therefore caps at gold.
  defp tier_for(score, provisional, results, tiers) do
    bronze = threshold(tiers, "bronze", 75)
    silver = threshold(tiers, "silver", 90)
    gold = threshold(tiers, "gold", 100)

    rhodium_ok =
      not provisional and
        results
        |> Enum.filter(&(&1.tier == "rhodium" and &1.verdict != :na))
        |> Enum.all?(&(&1.verdict == :pass))

    cond do
      score >= gold and rhodium_ok -> "rhodium"
      score >= gold -> "gold"
      score >= silver -> "silver"
      score >= bronze -> "bronze"
      true -> "none"
    end
  end

  defp threshold(tiers, key, default) do
    case Map.get(tiers, key) do
      n when is_integer(n) or is_float(n) -> n
      _ -> default
    end
  end

  # --- evaluation ------------------------------------------------------------

  defp evaluate(%Criterion{} = c, repo, caps) do
    if applicable?(c, caps) do
      case Map.fetch(detectors(), c.id) do
        {:ok, fun} -> fun.(repo)
        :error -> :unverified
      end
    else
      :na
    end
  end

  # Built-in detector tranche: file-presence and record-dialect-parse checks,
  # keyed by criterion id from the SSOT. Criteria not listed here are
  # :unverified — reported, never assumed. Extending this table (or delegating
  # to live scanner rules named in `detect`) raises automatable_coverage,
  # which is the published path from provisional to firm scorecards.
  defp detectors do
    %{
      "1.1.2" => any_of(["Justfile", "justfile"]),
      "1.1.3" => absent("Makefile"),
      "1.1.4" => present(".editorconfig"),
      "1.2.2" => present(".pre-commit-config.yaml"),
      "1.2.4" => present(".tool-versions"),
      "2.1.1" => present("README.adoc"),
      "2.1.2" => all_graded(["LICENSE", "LICENSES"]),
      "2.1.3" => present("SECURITY.md"),
      "2.1.4" => present("CODE_OF_CONDUCT.md"),
      "2.1.5" => present("CONTRIBUTING.md"),
      "2.1.6" => present("CHANGELOG.md"),
      "2.1.7" => present("MAINTAINERS.adoc"),
      "2.1.8" => present("GOVERNANCE.adoc"),
      "2.1.9" => any_of(["FUNDING.yml", ".github/FUNDING.yml"]),
      "2.1.10" => all_graded([".gitignore", ".gitattributes"]),
      "2.2.1" =>
        all_graded([
          ".well-known/security.txt",
          ".well-known/ai.txt",
          ".well-known/humans.txt"
        ]),
      "2.3.1" => present("0-AI-MANIFEST.a2ml"),
      "3.1.1" => present(".machine_readable/descriptiles"),
      "3.1.2" => descriptile("STATE"),
      "3.1.3" => descriptile("META"),
      "3.1.4" => descriptile("ECOSYSTEM"),
      "3.1.5" => descriptile("AGENTIC"),
      "3.1.6" => descriptile("NEUROSYM"),
      "3.1.7" => descriptile("PLAYBOOK"),
      "3.1.8" => descriptile("ANCHOR"),
      "3.1.9" => descriptile("CLADE"),
      "3.2.1" => &descriptiles_parse/1,
      "3.2.2" => present(".machine_readable/rsr-profile.a2ml"),
      "6.1.1" => &workflows_present/1,
      "6.1.2" => present(".github/workflows/hypatia-scan.yml"),
      "6.1.3" => present(".github/workflows/governance.yml")
    }
  end

  defp present(rel), do: fn repo -> if exists?(repo, rel), do: :pass, else: :fail end

  defp absent(rel), do: fn repo -> if exists?(repo, rel), do: :fail, else: :pass end

  defp any_of(rels) do
    fn repo -> if Enum.any?(rels, &exists?(repo, &1)), do: :pass, else: :fail end
  end

  # All present -> :pass, some -> :partial, none -> :fail.
  defp all_graded(rels) do
    fn repo ->
      case Enum.count(rels, &exists?(repo, &1)) do
        n when n == length(rels) -> :pass
        0 -> :fail
        _ -> :partial
      end
    end
  end

  defp descriptile(name),
    do: present(Path.join([".machine_readable", "descriptiles", name <> ".a2ml"]))

  # 3.2.1: every descriptile (plus rsr-profile if present) parses as record
  # dialect. An absent substrate is :fail — nothing to validate is not valid.
  defp descriptiles_parse(repo) do
    files =
      Path.wildcard(Path.join([repo, ".machine_readable", "descriptiles", "*.a2ml"])) ++
        Enum.filter(
          [Path.join([repo, ".machine_readable", "rsr-profile.a2ml"])],
          &File.exists?/1
        )

    cond do
      files == [] ->
        :fail

      Enum.all?(files, fn f ->
        match?({:ok, _}, with({:ok, t} <- File.read(f), do: RecordDialect.parse(t)))
      end) ->
        :pass

      true ->
        :fail
    end
  end

  defp workflows_present(repo) do
    yml = Path.wildcard(Path.join([repo, ".github", "workflows", "*.yml"]))
    yaml = Path.wildcard(Path.join([repo, ".github", "workflows", "*.yaml"]))
    if yml ++ yaml == [], do: :fail, else: :pass
  end

  defp exists?(repo, rel), do: File.exists?(Path.join(repo, rel))
end
