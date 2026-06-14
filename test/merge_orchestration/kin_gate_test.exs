# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.KinGateTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.KinGate
  alias Hypatia.MergeOrchestration.KinGate.FileStore

  @now ~U[2026-06-14 04:00:00Z]

  defp req(o) do
    Map.merge(
      %{
        repo: "hyperpolymath/panll",
        holder: "hypatia",
        territory: %{branch: "main", paths: [], is_meta: false}
      },
      o
    )
  end

  defp held(holder, repo, expires) do
    %{
      "lease_id" => "lease-#{holder}",
      "holder" => holder,
      "repo" => repo,
      "territory" => %{"branch" => "b", "paths" => [], "is_meta" => false},
      "status" => "held",
      "acquired_at" => "2026-06-14T03:00:00Z",
      "expires_at" => expires
    }
  end

  # ── pure decision core ───────────────────────────────────────────────────────

  test "a fresh acquire mints a held lease with a TTL (LE1)" do
    assert {:ok, lease} = KinGate.decide_acquire([], req(%{}), @now)
    assert lease["status"] == "held"
    assert lease["repo"] == "hyperpolymath/panll"
    assert is_binary(lease["expires_at"]) and lease["expires_at"] != ""
    assert lease["territory"]["is_meta"] == false
  end

  test "a live lease held by a different holder conflicts" do
    existing = [held("robot-repo-automaton", "hyperpolymath/panll", "2026-06-14T05:00:00Z")]

    assert {:conflict, blocker} =
             KinGate.decide_acquire(existing, req(%{holder: "hypatia"}), @now)

    assert blocker["holder"] == "robot-repo-automaton"
  end

  test "the same holder re-acquiring renews (pushes the TTL forward, keeps identity)" do
    existing = [held("hypatia", "hyperpolymath/panll", "2026-06-14T04:30:00Z")]

    assert {:ok, lease} =
             KinGate.decide_acquire(existing, req(%{holder: "hypatia", ttl: 3600}), @now)

    assert lease["lease_id"] == "lease-hypatia"
    assert lease["expires_at"] == "2026-06-14T05:00:00Z"
  end

  test "an expired lease is taken over, not treated as a conflict" do
    existing = [held("robot-repo-automaton", "hyperpolymath/panll", "2026-06-14T03:30:00Z")]
    assert {:ok, lease} = KinGate.decide_acquire(existing, req(%{holder: "hypatia"}), @now)
    assert lease["holder"] == "hypatia"
  end

  test "a meta claim is refused without owner authorization (LE2)" do
    r = req(%{territory: %{branch: "b", paths: [".github/workflows/ci.yml"], is_meta: true}})
    assert {:refused, "meta:owner-unauthorized"} = KinGate.decide_acquire([], r, @now)
  end

  test "a meta claim with owner authorization is granted and records it" do
    r = req(%{territory: %{branch: "b", paths: ["x"], is_meta: true}, owner_authorized: true})
    assert {:ok, lease} = KinGate.decide_acquire([], r, @now)
    assert lease["territory"]["is_meta"] == true
    assert lease["owner_authorized"] == true
  end

  test "a live lease on a different repo does not block" do
    existing = [held("robot-repo-automaton", "hyperpolymath/other", "2026-06-14T05:00:00Z")]
    assert {:ok, _} = KinGate.decide_acquire(existing, req(%{holder: "hypatia"}), @now)
  end

  test "release marks the lease released; expired?/live? read the TTL" do
    lease = held("hypatia", "hyperpolymath/panll", "2026-06-14T05:00:00Z")
    assert KinGate.release(lease)["status"] == "released"
    assert KinGate.live?(lease, "hyperpolymath/panll", @now)

    refute KinGate.live?(
             held("hypatia", "hyperpolymath/panll", "2026-06-14T03:00:00Z"),
             "hyperpolymath/panll",
             @now
           )

    assert KinGate.expired?(held("h", "r", nil), @now)
  end

  # ── FileStore plumbing (faithful codec, dependency-free) ─────────────────────

  test "FileStore: acquire writes a per-repo lock; a rival conflicts; release frees it" do
    # a real round-trip codec that needs no JSON dep
    enc = fn t -> t |> :erlang.term_to_binary() |> Base.encode64() end
    dec = fn b -> b |> Base.decode64!() |> :erlang.binary_to_term() end
    dir = Path.join(System.tmp_dir!(), "kg-#{System.unique_integer([:positive])}")
    opts = [now: @now, encode: enc, decode: dec]

    assert {:ok, _} = FileStore.acquire(dir, req(%{holder: "hypatia"}), opts)
    assert {:conflict, blk} = FileStore.acquire(dir, req(%{holder: "rhodibot"}), opts)
    assert blk["holder"] == "hypatia"

    assert [%{"holder" => "hypatia"}] = FileStore.list(dir, decode: dec)
    assert {:ok, rel} = FileStore.release(dir, "hyperpolymath/panll", decode: dec)
    assert rel["status"] == "released"

    # freed → the rival can now take it
    assert {:ok, _} = FileStore.acquire(dir, req(%{holder: "rhodibot"}), opts)
    File.rm_rf!(dir)
  end

  # ── real a5 fixture (only where Jason is resolvable, i.e. CI) ─────────────────

  if Code.ensure_loaded?(Jason) do
    @fix Path.expand("../../docs/design/merge-orchestration/fixtures", __DIR__)

    test "FileStore round-trips a lease through real Jason, and the golden a5 fixture reads back live" do
      dir = Path.join(System.tmp_dir!(), "kg-json-#{System.unique_integer([:positive])}")
      assert {:ok, lease} = FileStore.acquire(dir, req(%{holder: "hypatia"}), now: @now)
      assert [^lease] = FileStore.list(dir)

      golden = @fix |> Path.join("lease/valid-held.json") |> File.read!() |> Jason.decode!()
      # the golden fixture's TTL (05:00Z) is live at the fixture's acquired time
      assert KinGate.live?(golden, "hyperpolymath/reposystem", ~U[2026-06-14 04:00:00Z])
      File.rm_rf!(dir)
    end
  end
end
