# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.SchedulerTest do
  # async: false — one test mutates MERGE_ORCH_STORE in the process env
  use ExUnit.Case, async: false
  alias Hypatia.MergeOrchestration.Scheduler

  # a minimal struct exposing trust_score/2 (mirrors Neural.GraphOfTrust's neutral 0.5 default)
  defmodule FakeGoT do
    defstruct scores: %{}
    def trust_score(%__MODULE__{scores: s}, bot), do: Map.get(s, bot, 0.5)
  end

  @now ~U[2026-06-14 12:00:00Z]

  defp enc, do: fn t -> t |> :erlang.term_to_binary() |> Base.encode64() end
  defp dec, do: fn b -> b |> Base.decode64!() |> :erlang.binary_to_term() end

  # a store with one council-approved bump (sole approver "ci")
  defp seed_store do
    store = Path.join(System.tmp_dir!(), "sched-#{System.unique_integer([:positive])}")

    for sub <- ~w(observations pools attestations leases),
        do: File.mkdir_p!(Path.join(store, sub))

    e = enc()

    File.write!(
      Path.join([store, "observations", "hyperpolymath__a__1.json"]),
      e.(%{
        "repo" => "hyperpolymath/a",
        "number" => 1,
        "branch" => "dependabot/cargo/x",
        "files" => ["Cargo.lock"]
      })
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

    store
  end

  defp codec, do: [now: @now, encode: enc(), decode: dec()]

  test "a cycle with no trust source runs a uniform council and arms the bump" do
    store = seed_store()
    r = Scheduler.cycle([store: store, got: nil] ++ codec())
    assert r.stats.armed == 1
    File.rm_rf!(store)
  end

  test "the GoT snapshot flows into the council: a zero-trust sole approver recuses → flagged, not armed" do
    store = seed_store()

    r =
      Scheduler.cycle(
        [store: store, got: %FakeGoT{scores: %{"ci" => 0.0}}, bots: ["ci"]] ++ codec()
      )

    assert r.stats.armed == 0
    assert r.stats.flagged == 1
    File.rm_rf!(store)
  end

  test "the store path falls back to MERGE_ORCH_STORE when :store is not given" do
    store = seed_store()
    System.put_env("MERGE_ORCH_STORE", store)

    r = Scheduler.cycle([got: nil] ++ codec())
    assert r.manifest_path == Path.join(store, "merge-decisions.jsonl")
    assert r.stats.armed == 1

    System.delete_env("MERGE_ORCH_STORE")
    File.rm_rf!(store)
  end
end
