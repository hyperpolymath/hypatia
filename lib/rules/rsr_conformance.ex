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
      the repo declares in `machine-readable/rsr-profile.a2ml`
      (legacy `.machine_readable/` also accepted);
      non-applicable criteria are `:na` and excluded from the denominator;
    * verdicts `:pass` / `:partial` (half weight) / `:fail`;
    * score = passed weight / applicable **verified** weight;
    * tier from the catalogue's thresholds.

  Detection is layered:

    1. **Delegated** — criteria whose detection is *authoritative* only via a
       live scanner run once per score (`delegated_index/1` →
       `Hypatia.CLI.collect_findings/2`): the language bans (carve-out-aware,
       via `cicd_rules/banned_language_file`) and SHA-pinning (via
       `workflow_audit/{unpinned_action,wrong_sha_pin}`). Mapping is keyed on
       *observed* `(module,type)` finding keys, which deliberately do NOT match
       the SSOT's aspirational `detect` strings.
    2. **File-presence/parse tranche** — a built-in table for community-health
       files, the descriptile substrate, workflows, REUSE dir, guix stub.
    3. Everything else — `detect = "manual"`, intrinsically external criteria
       (OpenSSF Scorecard `4.2.x`), and flag-only licence (`7.1.1`) — is
       returned `:unverified`, counted in `automatable_coverage`, and **never**
       silently passed.

  Because some criteria are intrinsically external or flag-only, coverage < 100%
  is expected, so every scorecard carries `provisional = true`: per the
  no-overclaim doctrine a firm tier claim requires either full offline coverage
  or an accepted external attestation, a spec decision for RSR v2.0.x (§10).

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
    delegated = delegated_index(repo_path)

    results =
      Enum.map(criteria, fn %Criterion{} = c ->
        verdict = evaluate(c, repo_path, caps, delegated)

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

  # Reads `<machine tree>/rsr-profile.a2ml` (record dialect). Accepts the
  # capability list under `[rsr-profile]` or `[profile]`, key `capabilities`.
  # Returns `:none` when the file is absent/unreadable — only universal
  # criteria are then applicable, and criterion 3.2.2 (profile presence,
  # itself universal) fails, which is exactly the intended signal.
  defp declared_capabilities(repo_path) do
    path = Hypatia.Paths.machine_tree_join(repo_path, ["rsr-profile.a2ml"])

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

  defp evaluate(%Criterion{} = c, repo, caps, delegated) do
    if applicable?(c, caps) do
      # Criteria whose detection is DELEGATED to a live scanner take priority
      # over the file-presence tranche (they are authoritative — e.g. the
      # language bans respect the estate carve-outs, which a raw extension
      # check cannot). `:skip` falls through to the built-in detector table,
      # then to :unverified.
      case delegated_verdict(c.id, delegated) do
        :skip ->
          case Map.fetch(detectors(), c.id) do
            {:ok, fun} -> fun.(repo)
            :error -> :unverified
          end

        verdict ->
          verdict
      end
    else
      :na
    end
  end

  # --- delegation to live scanner rules --------------------------------------

  # Run the offline content-scanners ONCE per score and index the findings the
  # oracle can map to a criterion. Observed (module,type) keys — not the SSOT's
  # aspirational `detect` strings — drive the mapping (the two disagree):
  #   * language bans  -> cicd_rules/banned_language_file, keyed by file ext,
  #     already carve-out-filtered by the scanner;
  #   * SHA pinning    -> workflow_audit/{unpinned_action,wrong_sha_pin}.
  # A scan failure degrades to %{} (every delegated criterion then :skip's to
  # the file-presence tranche / :unverified) — never a false pass, never a crash.
  defp delegated_index(repo) do
    findings = safe_scan(repo, [:cicd_rules, :workflow_audit])

    banned_exts =
      for f <- findings,
          f.rule_module == "cicd_rules",
          f.type == "banned_language_file",
          into: MapSet.new(),
          do: f.file |> to_string() |> Path.extname()

    unpinned? =
      Enum.any?(findings, fn f ->
        f.rule_module == "workflow_audit" and f.type in ["unpinned_action", "wrong_sha_pin"]
      end)

    %{banned_exts: banned_exts, unpinned: unpinned?, scanned: findings != [] or true}
  end

  defp safe_scan(repo, rules) do
    Hypatia.CLI.collect_findings(repo, rules)
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  # Verdict for a delegated criterion, or `:skip` if this criterion is not
  # delegated. A language ban PASSES iff the scanner emitted no
  # banned_language_file finding for that extension (no such file, or every
  # such file is inside an approved carve-out — both are compliant).
  defp delegated_verdict("5.1.1", d), do: ban_verdict(d, [".py"])
  defp delegated_verdict("5.1.2", d), do: ban_verdict(d, [".ts", ".tsx"])
  defp delegated_verdict("5.1.3", d), do: ban_verdict(d, [".res", ".resi"])
  defp delegated_verdict("5.1.5", d), do: ban_verdict(d, [".go"])
  defp delegated_verdict("4.1.3", %{unpinned: true}), do: :fail
  defp delegated_verdict("4.1.3", %{unpinned: false}), do: :pass
  defp delegated_verdict(_, _), do: :skip

  defp ban_verdict(%{banned_exts: exts}, wanted) do
    if Enum.any?(wanted, &MapSet.member?(exts, &1)), do: :fail, else: :pass
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
      "1.2.2" => any_of([".pre-commit-config.yaml", "ci/.pre-commit-config.yaml"]),
      "1.2.4" => present(".tool-versions"),
      "2.1.1" => present("README.adoc"),
      "2.1.2" => all_graded(["LICENSE", "LICENSES"]),
      "2.1.3" => any_of(["SECURITY.md", ".github/SECURITY.md"]),
      "2.1.4" => any_of(["CODE_OF_CONDUCT.md", ".github/CODE_OF_CONDUCT.md"]),
      "2.1.5" => any_of(["CONTRIBUTING.md", ".github/CONTRIBUTING.md"]),
      "2.1.6" => any_of(["CHANGELOG.adoc", "CHANGELOG.md"]),
      "2.1.7" => any_of(["MAINTAINERS.adoc", "docs/MAINTAINERS.adoc"]),
      "2.1.8" => any_of(["GOVERNANCE.adoc", "docs/GOVERNANCE.adoc", ".github/GOVERNANCE.md"]),
      "2.1.9" => any_of(["FUNDING.yml", ".github/FUNDING.yml"]),
      "2.1.10" => all_graded([".gitignore", ".gitattributes"]),
      "2.2.1" =>
        all_graded([
          ".well-known/security.txt",
          ".well-known/ai.txt",
          ".well-known/humans.txt"
        ]),
      "2.3.1" => present("0-AI-MANIFEST.a2ml"),
      "3.1.1" => present_mr("descriptiles"),
      "3.1.2" => descriptile("STATE"),
      "3.1.3" => descriptile("META"),
      "3.1.4" => descriptile("ECOSYSTEM"),
      "3.1.5" => descriptile("AGENTIC"),
      "3.1.6" => descriptile("NEUROSYM"),
      "3.1.7" => descriptile("PLAYBOOK"),
      "3.1.8" => any_of_mr(["descriptiles/ANCHOR.a2ml", "descriptiles/anchors/ANCHOR.a2ml"]),
      "3.1.9" => descriptile("CLADE"),
      "3.2.1" => &descriptiles_parse/1,
      "3.2.2" => present_mr("rsr-profile.a2ml"),
      "6.1.1" => &workflows_present/1,
      "6.1.2" => present(".github/workflows/hypatia-scan.yml"),
      "6.1.3" => present(".github/workflows/governance.yml"),
      "6.1.6" => &dependabot_valid/1,
      "6.2.1" => present(".github/workflows/dogfood-gate.yml"),
      # Language bans 5.1.1/5.1.2/5.1.3/5.1.5 and SHA-pinning 4.1.3 are handled
      # by delegated_verdict/2 (live scanner, carve-out-aware) — not here.
      # v.mod / package-lock.json have no carve-out subtlety, so a file-presence
      # check is authoritative for them.
      "1.2.1" => any_of(["guix.scm", "build/guix.scm"]),
      "5.1.4" => no_file_named("v.mod"),
      "5.1.6" => no_file_named("package-lock.json"),
      "7.1.2" => present("LICENSES"),
      "8.1.4" => &guix_not_stub/1,
      "10.1.1" => any_of(["GOVERNANCE.adoc", "docs/GOVERNANCE.adoc", ".github/GOVERNANCE.md"]),
      "10.1.3" => any_of(["AFFIRMATION.adoc", "docs/AFFIRMATION.adoc"]),
      "11.1.1" => any_of(["AUDIT.adoc", "docs/AUDIT.adoc"])
    }
  end

  defp present(rel), do: fn repo -> if exists?(repo, rel), do: :pass, else: :fail end

  # Presence of a path INSIDE the repo's machine tree. The directory is named
  # `machine-readable/` canonically and `.machine_readable/` in the legacy
  # layout; this resolves per repo at check time so the oracle can score both
  # while the estate migrates. Hardcoding either name made whichever half had
  # not migrated unscoreable.
  defp present_mr(rel) do
    fn repo -> if exists?(repo, Path.join(Hypatia.Paths.machine_tree(repo), rel)), do: :pass, else: :fail end
  end

  defp absent(rel), do: fn repo -> if exists?(repo, rel), do: :fail, else: :pass end

  defp any_of(rels) do
    fn repo -> if Enum.any?(rels, &exists?(repo, &1)), do: :pass, else: :fail end
  end

  # any_of within the repo's machine tree, whichever name that tree uses.
  defp any_of_mr(rels) do
    fn repo ->
      mr = Hypatia.Paths.machine_tree(repo)
      if Enum.any?(rels, &exists?(repo, Path.join(mr, &1))), do: :pass, else: :fail
    end
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
    do: present_mr(Path.join(["descriptiles", name <> ".a2ml"]))

  # 3.2.1: every RECORD-DIALECT .a2ml under the substrate parses. Markup-dialect
  # files (the 0-AI-MANIFEST manifest and friends, which open with `@directive`
  # / prose rather than a `[section]` header) are a DIFFERENT A2ML surface and
  # are not validated by the record-dialect reader — requiring them to parse as
  # record dialect would false-fail every conformant repo. An absent substrate,
  # or no record-dialect file at all, is :fail — nothing to validate is not
  # valid.
  defp descriptiles_parse(repo) do
    candidates =
      Path.wildcard(Hypatia.Paths.machine_tree_join(repo, ["descriptiles", "*.a2ml"])) ++
        Enum.filter(
          [Hypatia.Paths.machine_tree_join(repo, ["rsr-profile.a2ml"])],
          &File.exists?/1
        )

    record_files = Enum.filter(candidates, &record_dialect?/1)

    cond do
      record_files == [] ->
        :fail

      Enum.all?(record_files, fn f ->
        match?({:ok, _}, with({:ok, t} <- File.read(f), do: RecordDialect.parse(t)))
      end) ->
        :pass

      true ->
        :fail
    end
  end

  # A record-dialect file's first non-comment, non-blank line is a `[section]`
  # header. Markup-dialect files open with `@` or prose. Cheap classifier that
  # avoids validating one surface with the other's reader.
  defp record_dialect?(path) do
    with {:ok, content} <- File.read(path) do
      content
      |> String.split("
")
      |> Stream.map(&String.trim/1)
      |> Enum.find(&(&1 != "" and not String.starts_with?(&1, "#")))
      |> case do
        line when is_binary(line) -> String.starts_with?(line, "[")
        _ -> false
      end
    else
      _ -> false
    end
  end

  defp dependabot_valid(repo) do
    path = Path.join(repo, ".github/dependabot.yml")

    if File.exists?(path) do
      content = File.read!(path)

      mix_present? = String.match?(content, ~r/package-ecosystem:\s*["']?mix["']?/)
      cargo_present? = String.match?(content, ~r/package-ecosystem:\s*["']?cargo["']?/)
      pip_present? = String.match?(content, ~r/package-ecosystem:\s*["']?pip["']?/)
      nix_present? = String.match?(content, ~r/package-ecosystem:\s*["']?nix["']?/)

      valid_mix = not mix_present? or File.exists?(Path.join(repo, "mix.exs"))
      valid_cargo = not cargo_present? or File.exists?(Path.join(repo, "Cargo.toml"))

      valid_pip =
        not pip_present? or
          (File.exists?(Path.join(repo, "requirements.txt")) or
             File.exists?(Path.join(repo, "pyproject.toml")))

      valid_nix =
        not nix_present? or
          (File.exists?(Path.join(repo, "flake.nix")) or
             File.exists?(Path.join(repo, "default.nix")))

      if valid_mix and valid_cargo and valid_pip and valid_nix do
        :pass
      else
        :fail
      end
    else
      :pass
    end
  end

  defp workflows_present(repo) do
    yml = Path.wildcard(Path.join([repo, ".github", "workflows", "*.yml"]))
    yaml = Path.wildcard(Path.join([repo, ".github", "workflows", "*.yaml"]))
    if yml ++ yaml == [], do: :fail, else: :pass
  end

  # :pass iff NO file with exactly `name` exists anywhere in the tree.
  defp no_file_named(name) do
    fn repo -> if tree_hits(repo, [name]) == [], do: :pass, else: :fail end
  end

  # 8.1.4: a present guix.scm must not be a scaffold stub (placeholders / empty
  # inputs / null source). Applicable-but-absent counts as :fail.
  defp guix_not_stub(repo) do
    files =
      [Path.join(repo, "guix.scm"), Path.join(repo, "build/guix.scm")]
      |> Enum.filter(&File.exists?/1)

    cond do
      files == [] ->
        :fail

      Enum.any?(files, fn f ->
        case File.read(f) do
          {:ok, c} ->
            String.contains?(c, "{{") or String.contains?(c, "(inputs (list))") or
                String.contains?(c, "(source #f)")

          _ ->
            true
        end
      end) ->
        :fail

      true ->
        :pass
    end
  end

  # Files matching any of `patterns` (glob basenames) at any depth, excluding
  # .git and vendored/build directories. Globs both root and nested so a
  # root-level `foo.py` is caught as well as `src/foo.py`.
  defp tree_hits(repo, patterns) do
    Enum.flat_map(patterns, fn p ->
      Path.wildcard(Path.join(repo, "**/" <> p)) ++ Path.wildcard(Path.join(repo, p))
    end)
    |> Enum.reject(&excluded_path?/1)
  end

  @excluded_segments ~w(/.git/ /deps/ /_build/ /node_modules/ /target/ /.deno/)
  defp excluded_path?(path), do: Enum.any?(@excluded_segments, &String.contains?(path, &1))

  defp exists?(repo, rel), do: File.exists?(Path.join(repo, rel))
end
