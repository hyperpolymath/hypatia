# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.TickerTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.Ticker

  @now ~U[2026-06-14 12:00:00Z]

  defp enc, do: fn t -> t |> :erlang.term_to_binary() |> Base.encode64() end
  defp dec, do: fn b -> b |> Base.decode64!() |> :erlang.binary_to_term() end

  defp seed_store do
    store = Path.join(System.tmp_dir!(), "tick-#{System.unique_integer([:positive])}")

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

  defp state(cycle_opts),
    do: %{
      interval_ms: 1000,
      cycle_opts: cycle_opts,
      last_run: nil,
      runs: 0,
      last_stats: nil,
      errors: []
    }

  test "do_cycle runs one cycle and records the stats" do
    store = seed_store()
    s = Ticker.do_cycle(state(store: store, got: nil, now: @now, encode: enc(), decode: dec()))

    assert s.runs == 1
    assert s.last_stats.armed == 1
    assert s.errors == []
    assert %DateTime{} = s.last_run
    File.rm_rf!(store)
  end

  test "do_cycle catches a cycle failure and records it without raising (the tick never crashes)" do
    store = seed_store()
    # the manifest/lease encoder raises → the cycle blows up mid-run
    s =
      Ticker.do_cycle(
        state(store: store, got: nil, now: @now, decode: dec(), encode: fn _ -> raise "boom" end)
      )

    assert s.runs == 0
    assert [msg | _] = s.errors
    assert msg =~ "boom"
    File.rm_rf!(store)
  end

  test "the supervised GenServer runs on demand: run_now then status shows one completed run" do
    store = seed_store()

    {:ok, pid} =
      Ticker.start_link(
        name: :"ticker_#{System.unique_integer([:positive])}",
        # long first delay so the auto-tick can't race the manual one
        first_delay_ms: 60_000,
        cycle_opts: [store: store, got: nil, now: @now, encode: enc(), decode: dec()]
      )

    Ticker.run_now(pid)
    status = Ticker.status(pid)

    assert status.runs == 1
    assert status.last_stats.armed == 1

    GenServer.stop(pid)
    File.rm_rf!(store)
  end
end
