# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.RuleLoaderTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.RuleLoader
  alias Hypatia.Rules.RuleLoader.RuleDef

  # RuleLoader reads standards' hypatia-rules/*.a2ml, which is still the only
  # form those rule definitions take (A7 section 10: they move to A5 rule
  # registry entries in both repositories together). A2ML is retired, so this
  # repository ships no .a2ml fixture: the three definitions below are copies
  # of standards rule files, written to a temporary directory for each run.
  @rule_files %{
    "profile-drift-detector.a2ml" => ~S"""
    # SPDX-License-Identifier: MPL-2.0
    # HYP-S007 — A2ML Profile Drift Detector
    # Flags a 6a2 file whose content has drifted from the A2ML profile it
    # declares via @profile(id=...). "Drift" = the file no longer satisfies its
    # declared profile definition (missing required section, disallowed directive,
    # cardinality/reference-class violation, or attestation floor not met).
    #
    # This rule operates on the A2ML profile mechanism specified in
    # a2ml/SPEC.adoc Section 8 and the profile definitions in a2ml/profiles/.
    # It is the dogfooding check that keeps the 6a2 estate honest about the
    # profiles it claims to satisfy.
    #
    # FLAG-ONLY by construction: strategy :review, auto_fixable: false. This rule
    # is structural; it has nothing to do with licences. It must never emit a
    # licence/SPDX finding and must never auto-edit a file.

    @rule(version="1.0"):
    id: HYP-S007
    name: "A2ML profile drift detector"
    description: "Flag a 6a2 file whose content drifts from its declared @profile"
    severity: medium
    category: Dogfooding
    strategy: review
    auto_fixable: false
    source: standards/hypatia-rules
    @end

    @scanner(type="file-tree"):
    find:
      - glob: "**/.machine_readable/6a2/*.a2ml"
      - glob: "**/.machine_readable/anchors/ANCHOR.a2ml"
      - glob: "a2ml/profiles/*/PROFILE.a2ml"
      - glob: "a2ml/profiles/REGISTRY.a2ml"
    @end

    @logic(engine="built-in"):
    # 1. For each 6a2 file, read the @profile(id=ns/name) declaration in its
    #    preamble. A 6a2 file with no @profile declaration is itself a finding
    #    (undeclared: cannot be validated through the mechanism).
    # 2. Resolve ns/name against a2ml/profiles/REGISTRY.a2ml; confirm the
    #    definition's sha256 equals the registry-pinned hash. A hash mismatch is a
    #    finding (registry desync).
    # 3. Load the profile definition (the five axes) and extract the file's facts:
    #    section titles (TOML [sections] or, for the key-based anchor dialect,
    #    top-level keys), directive occurrences, and @ref classes.
    # 4. Compute conformance exactly as src/A2ML/Profiles.idr conformsB:
    #      - every required_section present,
    #      - every occurring directive permitted by allowed_directives,
    #      - required_directives cardinality satisfied,
    #      - required reference_classes present,
    #      - mode >= min_attestation.
    # 5. Emit one finding per violation (mirrors Profiles.idr ProfileViolation).
    steps:
      - read_profile_decl: "@profile(id=*)"
      - resolve_against_registry: "a2ml/profiles/REGISTRY.a2ml"
      - verify_definition_hash: "sha256"
      - extract_doc_facts: { sections: true, directives: true, ref_classes: true }
      - check_conformance: "conformsB"
      - emit_violations
    @end

    @action:
    emit_signal: compliance.finding.new
    message_template: "Profile drift: {file} declares @profile({profile_id}) but {violation} (see a2ml/profiles/{profile_dir})"
    recipe: none
    @end

    @end
    """,
    "registry-staleness.a2ml" => ~S"""
    # SPDX-License-Identifier: MPL-2.0
    # HYP-S006 — Registry Staleness / Doc Drift
    # Detects when the verifiable spec registry (.machine_readable/REGISTRY.a2ml)
    # no longer matches ground truth, or when DERIVED docs (TOPOLOGY.md) were
    # hand-edited away from their generated form. Drift becomes a routed finding
    # the moment it lands — not something noticed 60 days later.
    #
    # Covers BOTH registry entry shapes:
    #   * LOCAL specs    — recorded source_hash vs a fresh git-ls-files compute.
    #   * EXTERNAL specs (kind="external") — verified pointers to specs whose SSOT
    #     lives in another repo (e.g. AffineScript .affine/.affex/.affmap). The
    #     recorded source_hash is checked against a fresh fetch of canonical_url;
    #     re-pinning is capped to :review (owner decision, not an auto-regen).
    #
    # Companion to scripts/build-registry.sh (the generator) and
    # .github/workflows/registry-verify.yml (the in-repo --check gate). This rule
    # is the estate-side half: it lets Hypatia surface the same drift across the
    # fleet and route it through the hybrid automation router.

    @rule(version="1.0"):
    id: HYP-S006
    name: "Registry staleness / doc drift"
    description: "Alert when a spec's recorded source_hash in REGISTRY.a2ml no longer matches a fresh compute (local) or a fresh fetch of canonical_url (external pointer), or a DERIVED doc (TOPOLOGY.md) drifted from its generator"
    severity: medium
    category: StandardsCompliance
    auto_fixable: true
    source: standards/hypatia-rules

    @parameters:
    # How long a hash mismatch may persist before the finding escalates.
    stale_after_days: 14
    critical_after_days: 60
    @end

    @scanner(type="file-tree"):
    find:
      - glob: ".machine_readable/REGISTRY.a2ml"
      - glob: "TOPOLOGY.md"
      - glob: "scripts/build-registry.sh"
    @end

    @logic(engine="built-in"):
    # Two entry shapes share one registry:
    #   * LOCAL  (no `kind`)        — recompute sha256 over `git ls-files -s <home>`
    #                                 exactly as the generator does, compare to the
    #                                 recorded value.
    #   * EXTERNAL (kind="external")— a verified pointer to a spec whose SSOT is
    #                                 another repo. Re-fetch canonical_url and
    #                                 recompute with source_hash_algo (sha256+;
    #                                 md5/sha1 are rejected as UNVERIFIED), compare
    #                                 to the recorded source_hash. This is the
    #                                 network-side check the offline generator and
    #                                 the in-repo registry-verify.yml cannot do.
    steps:
      - parse_registry: ".machine_readable/REGISTRY.a2ml"
        extract:
          - key: "home"
            as: home
          - key: "kind"
            as: kind
          - key: "canonical_url"
            as: canonical_url
          - key: "source_hash_algo"
            as: algo
          - key: "sync_status"
            as: sync_status
          - key: "source_hash"
            as: recorded_hash
      # --- LOCAL specs -------------------------------------------------------
      - for_each_spec where "kind != external":
          recompute: "sha256(git ls-files -s ${home})"
          compare_to: recorded_hash
          emit_if: "recompute != recorded_hash"
            finding_kind: registry-hash-stale
      - check_home_exists where "kind != external":
          emit_if: "home directory absent"
          finding_kind: registry-dead-home
      # --- EXTERNAL specs (verified pointers; SSOT in another repo) ----------
      - reject_weak_algo where "kind == external":
          emit_if: "algo in [md5, sha1]"        # security policy: no MD5/SHA1
          finding_kind: registry-weak-algo
      - for_each_spec where "kind == external && recorded_hash != 'PENDING-FIRST-SYNC'":
          fetch: canonical_url
          recompute: "${algo}(fetched_content)"
          compare_to: recorded_hash
          emit_if: "recompute != recorded_hash"
            finding_kind: registry-pointer-stale
      - for_each_spec where "kind == external && recorded_hash == 'PENDING-FIRST-SYNC'":
          # Not drift — upstream has not landed yet. Surface as info, and as a
          # low finding only once it has sat unsynced beyond the grace window.
          fetch: canonical_url
          emit_if: "fetch succeeds"
            finding_kind: registry-pointer-ready      # ready for first sync (info)
          emit_if: "still PENDING beyond ${stale_after_days}d"
            finding_kind: registry-pointer-unsynced   # low
      # --- DERIVED docs ------------------------------------------------------
      - check_derived_docs:
          command: "bash scripts/build-registry.sh --check"
          emit_if: "non-zero exit"
          finding_kind: derived-doc-stale
    @end

    # ---------------------------------------------------------------------------
    # Hybrid automation router strategy.
    # The router decides whether a finding is auto-executed or held for review.
    # Doc/registry drift is mechanical and safe to regenerate, EXCEPT where the
    # drifted content touches licence/SPDX — those are FLAG-ONLY per the estate
    # Manual-Only licence guardrail (.claude/CLAUDE.md) and hypatia
    # license_finding_strategy/0. The router MUST honour the cap below.
    # ---------------------------------------------------------------------------
    @router:
    default_strategy: auto_execute        # regenerate REGISTRY.a2ml + TOPOLOGY.md
    recipe: rebuild-registry
    # Hard cap: any finding whose drift overlaps a licence/SPDX token is demoted
    # to :review and never auto-applied, mirroring license_finding_strategy/0.
    strategy_caps:
      - when: "finding touches /SPDX-License-Identifier|PMPL|MPL-2\\.0|AGPL|Palimpsest|licen[cs]e/i"
        cap: review
        reason: "Manual-Only licence guardrail — agents flag, owner edits (neurophone#99)"
      - when: "finding_kind == registry-dead-home"
        cap: review
        reason: "A missing home is a structural change, not a regen — needs human triage"
      - when: "finding_kind in [registry-pointer-stale, registry-pointer-unsynced, registry-weak-algo]"
        cap: review
        reason: "External pointers reference another repo's SSOT — re-pinning version_pin/source_hash is an owner decision, never an auto-regen (rebuild-registry cannot recompute a cross-repo hash offline)."
    @end

    @action:
    emit_signal: doc.drift
    message_template: >-
      Registry drift: {finding_kind} for spec '{id}' (home={home} url={canonical_url}).
      Recorded {recorded_hash}, fresh compute/fetch differs. For LOCAL drift run
      'just registry' to regenerate; for EXTERNAL pointers re-pin is an owner
      decision (capped :review). ({age_days}d since generated)
    recipe: rebuild-registry
    severity_escalates_at: 60
    halt_on_violation: false
    dedupe_window_days: 7
    @end

    @recipe(id="rebuild-registry"):
    description: "Regenerate the registry + derived topology from the file tree"
    command: "bash scripts/build-registry.sh"
    auto_applicable: true
    # NOTE: auto_applicable is gated by @router strategy_caps above — a licence-
    # overlapping drift is capped to :review and will NOT auto-run this recipe.
    @end

    @sibling_rules:
    - HYP-S003 proof-freshness        — same staleness shape, for proof artefacts
    - HYP-S004 rsr-self-compliance    — structural self-dogfooding gate
    - HYP-S005 crg-overclaim-detector — grade-honesty gate
    @end

    @end
    """,
    "rsr-self-compliance.a2ml" => ~S"""
    # SPDX-License-Identifier: MPL-2.0
    # HYP-S004 — RSR Self-Compliance
    # Validates the standards repo against its own RSR (Rhodium Standard
    # Repositories) definition. A constitution that exempts itself from its
    # own laws has no authority — this rule enforces self-dogfooding.

    @rule(version="1.0"):
    id: HYP-S004
    name: "RSR self-compliance"
    description: "Standards repo must satisfy every requirement it imposes on others"
    severity: critical
    category: Dogfooding
    auto_fixable: false
    source: standards/hypatia-rules

    @scanner(type="file-tree"):
    target_repo: "standards"
    reference: "rhodium-standard-repositories/RSR.adoc"
    @end

    @required_files:
    # RSR-mandated files that standards itself must have
    - path: "README.adoc"
      reason: "RSR R-001"
    - path: "SECURITY.md"
      reason: "RSR R-002"
    - path: "CONTRIBUTING.md"
      reason: "RSR R-003"
    - path: "LICENSE"
      reason: "RSR R-004"
    - path: "CODE_OF_CONDUCT.md"
      reason: "RSR R-005"
    - path: ".editorconfig"
      reason: "RSR R-010"
    - path: "Justfile"
      reason: "RSR R-020 (no Makefiles)"
    - path: "guix.scm"
      reason: "RSR R-030 (primary pkg manager)"
    - path: "flake.nix"
      reason: "RSR R-031 (fallback pkg manager)"
    - path: ".machine_readable/"
      reason: "RSR R-040 (A2ML metadata)"
    @end

    @required_workflows:
    - name: "hypatia-scan.yml"
      reason: "RSR R-050 (self-scan)"
    - name: "mirror.yml"
      reason: "RSR R-051 (forge mirroring)"
    - name: "scorecard.yml"
      reason: "RSR R-052 (OpenSSF Scorecard)"
    - name: "rsr-antipattern.yml"
      reason: "RSR R-053 (antipattern check)"
    @end

    @forbidden_files:
    - glob: "**/*.ts"
      reason: "TypeScript banned — use AffineScript (RS/TS/JS → AffineScript → typed-wasm per estate language defaults)"
    - glob: "**/*.res"
      reason: "ReScript banned — use AffineScript (RS/TS/JS → AffineScript → typed-wasm per estate language defaults)"
    - glob: "**/*.py"
      reason: "Python banned"
    - glob: "**/*.go"
      reason: "Go banned — use Rust"
    - glob: "Dockerfile"
      reason: "Use Containerfile (Podman)"
    - glob: "Makefile"
      reason: "Use Justfile"
    - glob: "STATE.a2ml"
      at: "/"
      reason: "A2ML files go in .machine_readable/ only"
    @end

    @action:
    emit_signal: compliance.finding.new
    message_template: "RSR self-compliance violation: {reason} ({path})"
    recipe: none
    halt_on_violation: true
    @end

    @end
    """
  }

  setup_all do
    dir =
      Path.join(System.tmp_dir!(), "hypatia-rule-loader-#{System.unique_integer([:positive])}")

    File.mkdir_p!(dir)
    Enum.each(@rule_files, fn {name, text} -> File.write!(Path.join(dir, name), text) end)
    on_exit(fn -> File.rm_rf!(dir) end)
    %{fixtures: dir}
  end

  describe "load_dir/1" do
    test "loads every .a2ml rule definition, sorted by id", %{fixtures: fixtures} do
      assert {:ok, rules} = RuleLoader.load_dir(fixtures)
      assert Enum.map(rules, & &1.id) == ["HYP-S004", "HYP-S006", "HYP-S007"]
      assert Enum.all?(rules, &match?(%RuleDef{}, &1))
    end

    test "returns an error (not a crash) for an unreadable directory" do
      assert {:error, {:cannot_read_dir, _, _}} =
               RuleLoader.load_dir("/no/such/rules/dir")
    end
  end

  describe "parse/1 — metadata extraction" do
    setup %{fixtures: fixtures} do
      {:ok, rules} = RuleLoader.load_dir(fixtures)
      %{by_id: Map.new(rules, &{&1.id, &1})}
    end

    test "extracts identity/severity/category from the @rule block", %{by_id: by_id} do
      s006 = by_id["HYP-S006"]
      assert s006.name =~ "Registry staleness"
      assert s006.severity == :medium
      assert s006.category == "StandardsCompliance"
      assert s006.auto_fixable == true

      s004 = by_id["HYP-S004"]
      assert s004.severity == :critical
      assert s004.category == "Dogfooding"
      assert s004.auto_fixable == false
    end

    test "extracts scanner globs", %{by_id: by_id} do
      globs = by_id["HYP-S006"].scanner_globs
      assert ".machine_readable/REGISTRY.a2ml" in globs
      assert "TOPOLOGY.md" in globs
    end

    test "extracts router default strategy, recipe, and action signal", %{by_id: by_id} do
      s006 = by_id["HYP-S006"]
      assert s006.router_default_strategy == :auto_execute
      assert s006.router_recipe == "rebuild-registry"
      assert s006.action_signal == "doc.drift"

      assert by_id["HYP-S004"].action_signal == "compliance.finding.new"
    end

    test "preserves the @logic block verbatim without interpreting it", %{by_id: by_id} do
      # This increment does NOT execute @logic — it keeps it for a later pass.
      assert is_binary(by_id["HYP-S006"].logic_raw)
      assert by_id["HYP-S006"].logic_raw =~ "for_each_spec"
      assert is_binary(by_id["HYP-S006"].parameters_raw)
    end
  end

  describe "strategy caps — the Manual-Only licence guardrail" do
    setup %{fixtures: fixtures} do
      {:ok, rules} = RuleLoader.load_dir(fixtures)
      %{rules: rules, by_id: Map.new(rules, &{&1.id, &1})}
    end

    test "parses all strategy_caps entries with when/cap/reason", %{by_id: by_id} do
      caps = by_id["HYP-S006"].strategy_caps
      # registry-staleness declares three caps: licence, dead-home, external-pointer.
      assert length(caps) == 3
      assert Enum.all?(caps, &(&1[:cap] in [:review, :auto_execute, :report_only]))
      assert Enum.all?(caps, &is_binary(&1[:reason]))
    end

    test "the licence/SPDX cap is present and demotes to :review", %{by_id: by_id} do
      caps = by_id["HYP-S006"].strategy_caps
      licence = Enum.find(caps, &String.match?(&1[:when], ~r/SPDX|PMPL|licen[cs]e/i))
      assert licence, "registry-staleness must carry a licence strategy cap"
      assert licence[:cap] == :review
    end

    test "licence_caps/1 surfaces exactly the licence-overlapping :review caps", %{rules: rules} do
      lic = RuleLoader.licence_caps(rules)
      assert length(lic) == 1
      assert hd(lic)[:cap] == :review
      assert String.match?(hd(lic)[:when], ~r/SPDX|licen[cs]e/i)
    end
  end

  describe "parse/1 — failure modes (fail loudly)" do
    test "text with no @rule block is a clear error, not a fabricated rule" do
      assert {:error, _} = RuleLoader.parse("# just a comment\n")
    end
  end
end
