# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.SensorTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.Sensor

  defp obs(o), do: Map.merge(%{"repo" => "hyperpolymath/panll", "number" => 1, "files" => []}, o)

  # ── classify: change_class (route axis) ──────────────────────────────────────

  test "a dependabot branch is a bump (by branch and by title)" do
    assert {:bump, :object, false} = Sensor.classify(obs(%{"branch" => "dependabot/cargo/serde"}))
    assert {:bump, :object, false} = Sensor.classify(obs(%{"title" => "Bump serde from 1 to 2"}))
  end

  test "a proof file is a proof" do
    assert {:proof, _, _} = Sensor.classify(obs(%{"files" => ["proofs/agda/All.agda"]}))
    assert {:proof, _, _} = Sensor.classify(obs(%{"files" => ["src/abi/Types.idr"]}))
  end

  test "a SECURITY touch (or label) is security" do
    assert {:security, _, _} = Sensor.classify(obs(%{"files" => ["SECURITY.md"]}))
    assert {:security, _, _} = Sensor.classify(obs(%{"files" => ["x.ex"], "labels" => ["security"]}))
  end

  test "all-docs is docs; mixed code+docs is not" do
    assert {:docs, _, _} = Sensor.classify(obs(%{"files" => ["docs/a.adoc", "README.adoc"]}))
    refute match?({:docs, _, _}, Sensor.classify(obs(%{"files" => ["docs/a.adoc", "lib/x.ex"]})))
  end

  test "chore and refactor by branch/title; anything else is a feature" do
    assert {:chore, _, _} = Sensor.classify(obs(%{"branch" => "chore/tidy"}))
    assert {:refactor, _, _} = Sensor.classify(obs(%{"title" => "refactor: split module"}))
    assert {:feature, _, _} = Sensor.classify(obs(%{"files" => ["lib/x.ex"], "title" => "add thing"}))
  end

  # ── classify: change_level (the meta reflexivity guard) ──────────────────────

  test "touching a workflow is meta, whatever the class" do
    assert {_, :meta, _} = Sensor.classify(obs(%{"files" => [".github/workflows/ci.yml"]}))
  end

  test "touching bot_directives or lib/rules is meta" do
    assert {_, :meta, _} = Sensor.classify(obs(%{"files" => [".machine_readable/bot_directives/x.scm"]}))
    assert {_, :meta, _} = Sensor.classify(obs(%{"files" => ["lib/rules/cicd_rules.ex"]}))
  end

  test "standards contractiles are meta; the same path elsewhere is object" do
    assert {_, :meta, _} =
             Sensor.classify(%{"repo" => "hyperpolymath/standards", "files" => ["contractiles/TRUST.ncl"]})

    assert {_, :object, _} =
             Sensor.classify(%{"repo" => "hyperpolymath/panll", "files" => ["contractiles/TRUST.ncl"]})
  end

  test "ordinary library code is object-level" do
    assert {_, :object, _} = Sensor.classify(obs(%{"files" => ["lib/x.ex", "lib/y.ex"]}))
  end

  # ── classify: license touch (owner-only symbolic veto upstream) ──────────────

  test "a LICENSE-family path flags license_touch and routes to :license_touch" do
    for f <- ["LICENSE", "LICENSE.txt", "licenses/MPL-2.0.txt", "headers.spdx"] do
      assert {:license_touch, _, true} = Sensor.classify(obs(%{"files" => [f]}))
    end
  end

  # ── to_context: shape the brain consumes ─────────────────────────────────────

  test "to_context produces the ctx the Strategist/Dispatcher read, with pool mapped and attestations normalised" do
    ctx =
      Sensor.to_context(
        obs(%{"number" => 412, "branch" => "dependabot/cargo/x", "files" => ["Cargo.lock"]}),
        %{"pool" => "P2", "product_tier" => "standard"},
        [%{"bot" => "ci", "verdict" => "approve", "confidence" => 0.99}]
      )

    assert ctx.pr == %{repo: "hyperpolymath/panll", number: 412}
    assert ctx.change_class == :bump
    assert ctx.change_level == :object
    assert ctx.pool == :p2
    assert ctx.branch == "dependabot/cargo/x"
    assert ctx.paths == ["Cargo.lock"]
    assert [%{bot: "ci", verdict: :approve, confidence: 0.99}] = ctx.attestations
    assert ctx.contributing_bots == ["ci"]
  end

  test "an unlabelled repo (nil pool) defaults to the conservative :p1" do
    ctx = Sensor.to_context(obs(%{}), nil, [])
    assert ctx.pool == :p1
  end

  # ── normalisers ──────────────────────────────────────────────────────────────

  test "normalize_attestation: disk (string-keyed) and in-memory (atom-keyed) both land on the council shape" do
    disk = %{"bot" => "panicbot", "verdict" => "veto", "confidence" => 0.2, "rationale" => "license/SPDX"}
    assert %{bot: "panicbot", verdict: :veto, confidence: 0.2, rationale: "license/SPDX"} =
             Sensor.normalize_attestation(disk)

    # missing rationale ⇒ key dropped so KinCouncil's default applies
    assert %{bot: "ci", verdict: :approve} = mem = Sensor.normalize_attestation(%{bot: "ci", verdict: :approve, confidence: 0.9})
    refute Map.has_key?(mem, :rationale)
  end

  test "pool_atom maps the RepoPoolPolicy enum, a bare string, a policy map, and nil" do
    assert Sensor.pool_atom("P0") == :p0
    assert Sensor.pool_atom("mass_squash") == :mass_squash
    assert Sensor.pool_atom(%{"pool" => "P3"}) == :p3
    assert Sensor.pool_atom(nil) == :p1
    assert Sensor.pool_atom(:p2) == :p2
  end

  # ── sense → deliberate (the integration proof) ───────────────────────────────

  test "sense_and_decide over a mixed batch hits all three safety outcomes" do
    pools = %{
      "hyperpolymath/a" => "P2",
      "hyperpolymath/b" => "P1",
      "hyperpolymath/c" => "P2",
      "hyperpolymath/d" => "P3"
    }

    observations = [
      # A: bump @ P2, council-approved → arm_auto
      %{"repo" => "hyperpolymath/a", "number" => 1, "branch" => "dependabot/cargo/x", "files" => ["Cargo.lock"]},
      # B: docs @ P1 → clamped to review
      %{"repo" => "hyperpolymath/b", "number" => 2, "files" => ["docs/x.adoc"]},
      # C: workflow edit → meta → flag
      %{"repo" => "hyperpolymath/c", "number" => 3, "branch" => "ci/pin", "files" => [".github/workflows/ci.yml"]},
      # D: LICENSE touch → owner-only veto → flag
      %{"repo" => "hyperpolymath/d", "number" => 4, "files" => ["LICENSE"]}
    ]

    resolve_pool = fn repo -> Map.get(pools, repo) end
    resolve_atts = fn _repo, _n -> [%{"bot" => "ci", "verdict" => "approve", "confidence" => 0.99}] end

    %{stats: stats, decisions: decisions} =
      Sensor.sense_and_decide(observations, resolve_pool, resolve_atts)

    assert stats == %{total: 4, auto_execute: 1, review: 1, report_only: 2}

    # the bump armed with a squash; the meta and license ones both flagged for the right reason
    assert %{change_class: :bump, method: :squash, safety: :arm_auto} = Enum.at(decisions, 0)
    assert %{change_level: :meta, safety: :flag} = Enum.at(decisions, 2)
    assert %{change_class: :license_touch, safety: :flag} = Enum.at(decisions, 3)
  end

  # ── store-backed resolvers: disk plumbing (decoder injected, dep-free) ────────

  test "dir_attestation_resolver reads every json and filters by subject (repo, number)" do
    dir = Path.join(System.tmp_dir!(), "moj-att-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    File.write!(Path.join(dir, "a.json"), "1")
    File.write!(Path.join(dir, "b.json"), "2")
    File.write!(Path.join(dir, "note.txt"), "ignored")

    decode = fn
      "1" -> %{"subject" => %{"repo" => "r", "number" => 1}, "bot" => "x", "verdict" => "approve"}
      "2" -> %{"subject" => %{"repo" => "r", "number" => 2}, "bot" => "y", "verdict" => "veto"}
    end

    resolve = Sensor.dir_attestation_resolver(dir, decode)
    assert [%{"bot" => "x"}] = resolve.("r", 1)
    assert resolve.("r", 99) == []
    File.rm_rf!(dir)
  end

  test "file_pool_resolver reads <owner__name>.json and returns nil when absent" do
    dir = Path.join(System.tmp_dir!(), "moj-pool-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    File.write!(Path.join(dir, "hyperpolymath__panll.json"), "x")

    resolve = Sensor.file_pool_resolver(dir, fn "x" -> %{"pool" => "P2"} end)
    assert %{"pool" => "P2"} = resolve.("hyperpolymath/panll")
    assert resolve.("hyperpolymath/missing") == nil
    File.rm_rf!(dir)
  end

  # ── real on-disk a3/a5 fixtures (only where Jason is resolvable, i.e. CI) ─────

  if Code.ensure_loaded?(Jason) do
    @fix Path.expand("../../docs/design/merge-orchestration/fixtures", __DIR__)

    test "store_resolvers parse the real signed attestation + pool fixtures via Jason" do
      dir = Path.join(System.tmp_dir!(), "moj-store-#{System.unique_integer([:positive])}")
      File.mkdir_p!(Path.join(dir, "pools"))
      File.mkdir_p!(Path.join(dir, "attestations"))
      File.cp!(Path.join(@fix, "attestation/signed-valid.json"), Path.join(dir, "attestations/ci.json"))
      File.cp!(Path.join(@fix, "pool/valid-p2-standard.json"), Path.join(dir, "pools/hyperpolymath__panll.json"))

      {resolve_pool, resolve_atts} = Sensor.store_resolvers(dir)

      assert Sensor.pool_atom(resolve_pool.("hyperpolymath/panll")) == :p2
      assert [att] = resolve_atts.("hyperpolymath/hypatia", 442)
      assert %{bot: "ci", verdict: :approve} = Sensor.normalize_attestation(att)
      File.rm_rf!(dir)
    end
  end
end
