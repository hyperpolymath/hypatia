# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.LoopTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.{Loop, TestObservationStore}
  alias TestObservationStore, as: Evidence

  defp cleanup(store), do: on_exit(fn -> File.rm_rf!(store) end)
  defp no_acquire(_), do: flunk("unverified evidence must never acquire an execution lease")
  defp pool_p2(_), do: "P2"

  defp approval(repo, number, head),
    do: [Evidence.attestation(%{"repo" => repo, "number" => number, "head_sha" => head})]

  test "two high-confidence Sensor decisions remain reviews and never acquire a lease" do
    observations = [Evidence.observation(), Evidence.observation(%{"number" => 2})]
    result = Loop.plan(observations, &pool_p2/1, &approval/3, %{}, "hypatia", &no_acquire/1)
    assert result.stats.armed == 0
    assert result.stats.review == 2
    assert result.leases == []
    assert Enum.all?(result.entries, &(&1["strategy"] == "review"))
  end

  test "meta and veto guards stay stronger than the review cap" do
    observations = [
      Evidence.observation(),
      Evidence.observation(%{"number" => 2, "files" => [".github/workflows/ci.yml"]}),
      Evidence.observation(%{"number" => 3, "files" => ["LICENSE"]})
    ]

    result = Loop.plan(observations, &pool_p2/1, &approval/3, %{}, "hypatia", &no_acquire/1)
    assert result.stats == %{total: 3, armed: 0, deferred: 0, blocked: 0, review: 1, flagged: 2}
  end

  test "legacy incomplete observations remain non-executable without querying a guessed revision" do
    resolver = fn _, _, _ -> flunk("legacy record cannot resolve revision-bound evidence") end

    result =
      Loop.plan(
        [%{"repo" => "hyperpolymath/a", "number" => 1, "files" => ["Cargo.lock"]}],
        &pool_p2/1,
        resolver,
        %{},
        "hypatia",
        &no_acquire/1
      )

    assert result.stats.armed == 0
    assert result.stats.flagged == 1
    assert hd(result.entries)["strategy"] == "report_only"
  end

  test "complete receipt round-trips exact subject/event into a review manifest" do
    store = Evidence.seed()
    cleanup(store)
    result = Loop.run(store: store, acquire: &no_acquire/1)
    assert result.stats.review == 1
    assert result.stats.armed == 0
    entry = result.manifest_path |> File.read!() |> Jason.decode!()
    assert entry["event_id"] == Evidence.event()
    assert entry["merge"]["head_sha"] == Evidence.head()
    assert entry["merge"]["pool"] == "P2"
    assert entry["merge"]["evidence_authenticity"] == "unverified"
    assert entry["strategy"] == "review"
    assert File.ls!(Path.join(store, "leases")) == []
  end

  test "a complete batch includes canonical standards without lifting review or meta guards" do
    observations = [
      Evidence.observation(),
      Evidence.observation(%{"repo" => "metadatastician/standards", "number" => 2}),
      Evidence.observation(%{
        "repo" => "metadatastician/standards",
        "number" => 3,
        "files" => ["rulesets/main.json"]
      })
    ]

    store = Evidence.seed(observations: observations)
    cleanup(store)
    result = Loop.run(store: store, acquire: &no_acquire/1)
    assert result.stats == %{total: 3, armed: 0, deferred: 0, blocked: 0, review: 2, flagged: 1}
    assert result.leases == []
    assert File.ls!(Path.join(store, "leases")) == []
    assert File.read!(result.manifest_path) =~ "metadatastician/standards"
  end

  test "unlisted historical records are retained without becoming current observations" do
    store = Evidence.seed()
    cleanup(store)
    historical = Path.join([store, "observations", "hyperpolymath__a__999.json"])
    File.write!(historical, "{broken historical input")
    assert Loop.run(store: store).stats.total == 1
    assert File.read!(historical) == "{broken historical input"
  end

  test "a complete empty scoped batch differs from an unreadable or failed batch" do
    store = Evidence.seed()
    cleanup(store)
    Evidence.write_batch(store, [])
    result = Loop.run(store: store)
    assert result.stats.total == 0
    assert File.read!(result.manifest_path) == ""
    assert File.exists?(Path.join([store, "observations", "hyperpolymath__a__1.json"]))
  end

  test "missing, failed, collecting, duplicate and contradictory receipts preserve the prior manifest" do
    transforms = [
      fn receipt -> %{receipt | "status" => "collecting"} end,
      fn receipt -> %{receipt | "status" => "failed"} end,
      fn receipt -> %{receipt | "schema_version" => 999} end,
      fn receipt -> Map.delete(receipt, "event_id") end,
      fn receipt -> %{receipt | "repos" => []} end,
      fn receipt ->
        %{receipt | "observations" => receipt["observations"] ++ receipt["observations"]}
      end,
      fn receipt ->
        put_in(receipt, ["observations", Access.at(0), "sha256"], String.duplicate("0", 64))
      end,
      fn receipt ->
        put_in(receipt, ["observations", Access.at(0), "head_sha"], String.duplicate("b", 40))
      end,
      fn receipt -> put_in(receipt, ["observations", Access.at(0), "file"], "../outside.json") end
    ]

    for transform <- transforms do
      store = Evidence.seed()
      cleanup(store)
      previous = Path.join(store, "merge-decisions.jsonl")
      File.write!(previous, "previous review-only manifest")
      receipt = Evidence.receipt_path(store)

      File.write!(
        receipt,
        receipt |> File.read!() |> Jason.decode!() |> transform.() |> Jason.encode!()
      )

      assert_raise ArgumentError, ~r/unqualified observation batch/, fn ->
        Loop.run(store: store)
      end

      assert File.read!(previous) == "previous review-only manifest"
    end

    store = Evidence.seed()
    cleanup(store)
    File.rm!(Evidence.receipt_path(store))

    assert_raise ArgumentError, ~r/unqualified observation batch/, fn ->
      Loop.run(store: store)
    end

    refute File.exists?(Path.join(store, "merge-decisions.jsonl"))
  end

  test "file tampering, missing members and symlinks cannot satisfy a complete receipt" do
    for action <- [:tamper, :missing, :symlink] do
      store = Evidence.seed()
      cleanup(store)
      member = Path.join([store, "observations", "hyperpolymath__a__1.json"])

      case action do
        :tamper ->
          File.write!(member, Jason.encode!(Evidence.observation(%{"event_id" => "test:other"})))

        :missing ->
          File.rm!(member)

        :symlink ->
          File.rename!(member, member <> ".saved")
          File.ln_s!(member <> ".saved", member)
      end

      assert_raise ArgumentError, ~r/unqualified observation batch/, fn ->
        Loop.run(store: store)
      end

      refute File.exists?(Path.join(store, "merge-decisions.jsonl"))
    end
  end

  test "matching bytes with inconsistent record metadata still fail qualification" do
    for field <- ["head_sha", "event_id", "complete", "files", "changed_file_count"] do
      store = Evidence.seed()
      cleanup(store)

      values = %{
        "head_sha" => String.duplicate("c", 40),
        "event_id" => "test:other",
        "complete" => false,
        "files" => [],
        "changed_file_count" => 9
      }

      member = Path.join([store, "observations", "hyperpolymath__a__1.json"])

      body =
        member
        |> File.read!()
        |> Jason.decode!()
        |> Map.put(field, values[field])
        |> Jason.encode!()

      File.write!(member, body)
      receipt_path = Evidence.receipt_path(store)
      receipt = receipt_path |> File.read!() |> Jason.decode!()

      receipt =
        put_in(
          receipt,
          ["observations", Access.at(0), "sha256"],
          :crypto.hash(:sha256, body) |> Base.encode16(case: :lower)
        )

      File.write!(receipt_path, Jason.encode!(receipt))

      assert_raise ArgumentError, ~r/unqualified observation batch/, fn ->
        Loop.run(store: store)
      end
    end
  end

  test "a receipt changed during file decoding cannot publish a mixed batch" do
    store = Evidence.seed()
    cleanup(store)

    decoder = fn body ->
      decoded = Jason.decode!(body)

      if is_map(decoded) and Map.has_key?(decoded, "head_sha") do
        path = Evidence.receipt_path(store)
        receipt = path |> File.read!() |> Jason.decode!() |> Map.put("status", "collecting")
        File.write!(path, Jason.encode!(receipt))
      end

      decoded
    end

    assert_raise ArgumentError, ~r/receipt changed while reading/, fn ->
      Loop.run(store: store, decode: decoder)
    end

    refute File.exists?(Path.join(store, "merge-decisions.jsonl"))
  end

  test "Baton selections do not invoke a submit function or resolve an unqualified Bag backend" do
    for mode <- [:baton, :both] do
      store = Evidence.seed()
      cleanup(store)

      result =
        Loop.run(
          store: store,
          actuation: mode,
          submit: fn _ -> flunk("unqualified executor called") end
        )

      assert result.stats.armed == 0
      assert result.batons == []
      assert Map.has_key?(result, :manifest_path) == (mode == :both)
    end
  end

  test "unknown backend is an error, not a successful no-op" do
    store = Evidence.seed()
    cleanup(store)

    assert_raise ArgumentError, ~r/unknown actuation backend/, fn ->
      Loop.run(store: store, actuation: :unknown)
    end
  end
end
