# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.LoopTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.Loop
  alias Hypatia.MergeOrchestration.KinGate.FileStore

  @now ~U[2026-06-14 12:00:00Z]

  # a faithful round-trip codec that needs no JSON dep
  defp enc, do: fn t -> t |> :erlang.term_to_binary() |> Base.encode64() end
  defp dec, do: fn b -> b |> Base.decode64!() |> :erlang.binary_to_term() end

  defp tmp(prefix),
    do: Path.join(System.tmp_dir!(), "#{prefix}-#{System.unique_integer([:positive])}")

  defp file_acquire(dir),
    do: fn req -> FileStore.acquire(dir, req, now: @now, encode: enc(), decode: dec()) end

  defp bump(repo, n),
    do: %{
      "repo" => repo,
      "number" => n,
      "branch" => "dependabot/cargo/x#{n}",
      "files" => ["Cargo.lock"]
    }

  defp pool_p2, do: fn _repo -> "P2" end

  defp approve_ci,
    do: fn _repo, _n -> [%{"bot" => "ci", "verdict" => "approve", "confidence" => 0.99}] end

  # ── the gate bites: in-cycle one-arm-per-repo ────────────────────────────────

  test "two armed PRs for the same repo in one cycle: first arms, second is deferred to report_only" do
    leases = tmp("loop-incycle")

    r =
      Loop.plan(
        [bump("hyperpolymath/a", 1), bump("hyperpolymath/a", 2)],
        pool_p2(),
        approve_ci(),
        %{},
        "hypatia",
        file_acquire(leases)
      )

    assert r.stats.armed == 1
    assert r.stats.deferred == 1
    [e1, e2] = r.entries
    assert e1["strategy"] == "auto_execute"
    assert e2["strategy"] == "report_only"
    assert e2["merge"]["rationale"] =~ "same-repo-this-cycle"
    File.rm_rf!(leases)
  end

  # ── the gate bites: cross-agent persistent lease ─────────────────────────────

  test "an arm is deferred when another holder already holds the repo lease" do
    leases = tmp("loop-crossagent")
    # another agent already holds repo a
    {:ok, _} =
      FileStore.acquire(
        leases,
        %{
          repo: "hyperpolymath/a",
          holder: "robot-repo-automaton",
          territory: %{branch: "b", paths: [], is_meta: false}
        },
        now: @now,
        encode: enc(),
        decode: dec()
      )

    r =
      Loop.plan(
        [bump("hyperpolymath/a", 1)],
        pool_p2(),
        approve_ci(),
        %{},
        "hypatia",
        file_acquire(leases)
      )

    assert r.stats.armed == 0
    assert r.stats.deferred == 1
    [e1] = r.entries
    assert e1["strategy"] == "report_only"
    assert e1["merge"]["rationale"] =~ "lease-held:robot-repo-automaton"
    File.rm_rf!(leases)
  end

  # ── a full mixed cycle: arm / arm / review / meta-flag ───────────────────────

  test "a mixed cycle tallies armed / review / flagged, gating only the arms" do
    leases = tmp("loop-mixed")

    pool = fn
      "hyperpolymath/c" -> "P1"
      _ -> "P2"
    end

    observations = [
      bump("hyperpolymath/a", 1),
      bump("hyperpolymath/b", 2),
      %{"repo" => "hyperpolymath/c", "number" => 3, "files" => ["docs/x.adoc"]},
      %{
        "repo" => "hyperpolymath/d",
        "number" => 4,
        "branch" => "ci/pin",
        "files" => [".github/workflows/ci.yml"]
      }
    ]

    r = Loop.plan(observations, pool, approve_ci(), %{}, "hypatia", file_acquire(leases))

    assert r.stats == %{total: 4, armed: 2, deferred: 0, blocked: 0, review: 1, flagged: 1}
    assert length(r.leases) == 2
    File.rm_rf!(leases)
  end

  # ── the whole thing over a real store of files (codec injected) ──────────────

  test "run/1 reads a store of observation/pool/attestation files, gates, and writes the manifest + leases" do
    store = tmp("loop-store")

    for sub <- ~w(observations pools attestations leases),
        do: File.mkdir_p!(Path.join(store, sub))

    e = enc()

    File.write!(
      Path.join([store, "observations", "hyperpolymath__a__1.json"]),
      e.(bump("hyperpolymath/a", 1))
    )

    File.write!(Path.join([store, "pools", "hyperpolymath__a.json"]), e.(%{"pool" => "P2"}))

    File.write!(
      Path.join([store, "attestations", "ci.json"]),
      e.(%{
        "subject" => %{"repo" => "hyperpolymath/a", "number" => 1},
        "bot" => "ci",
        "verdict" => "approve",
        "confidence" => 0.99
      })
    )

    r = Loop.run(store: store, holder: "hypatia", now: @now, encode: e, decode: dec())

    assert r.stats.armed == 1
    assert String.ends_with?(r.manifest_path, "merge-decisions.jsonl")

    lines = r.manifest_path |> File.read!() |> String.trim() |> String.split("\n")
    assert length(lines) == 1
    entry = dec().(hd(lines))
    assert entry["strategy"] == "auto_execute"
    assert entry["merge"]["method"] == "squash"

    # the lease for repo a was persisted by the gate
    assert File.exists?(Path.join([store, "leases", "hyperpolymath__a.json"]))
    File.rm_rf!(store)
  end

  # ── real Jason over a real store (CI only) ───────────────────────────────────

  if Code.ensure_loaded?(Jason) do
    test "run/1 with the default Jason codec round-trips a store to a JSON manifest" do
      store = tmp("loop-json")

      for sub <- ~w(observations pools attestations leases),
          do: File.mkdir_p!(Path.join(store, sub))

      File.write!(
        Path.join([store, "observations", "hyperpolymath__a__1.json"]),
        Jason.encode!(bump("hyperpolymath/a", 1))
      )

      File.write!(
        Path.join([store, "pools", "hyperpolymath__a.json"]),
        Jason.encode!(%{"pool" => "P2"})
      )

      File.write!(
        Path.join([store, "attestations", "ci.json"]),
        Jason.encode!(%{
          "subject" => %{"repo" => "hyperpolymath/a", "number" => 1},
          "bot" => "ci",
          "verdict" => "approve",
          "confidence" => 0.99
        })
      )

      r = Loop.run(store: store, holder: "hypatia", now: @now)
      assert r.stats.armed == 1
      entry = r.manifest_path |> File.read!() |> String.trim() |> Jason.decode!()
      assert entry["strategy"] == "auto_execute"
      File.rm_rf!(store)
    end
  end
end
