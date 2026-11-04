# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.TickerTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.Ticker

  @now ~U[2026-06-14 12:00:00Z]

  defp enc, do: fn t -> t |> :erlang.term_to_binary() |> Base.encode64() end
  defp dec, do: fn b -> b |> Base.decode64!() |> :erlang.binary_to_term() end

  defp seed_store, do: Hypatia.MergeOrchestration.TestObservationStore.seed(encode: enc())

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
    assert s.last_stats.armed == 0
    assert s.errors == []
    assert s.last_outcome == :completed
    assert s.last_stats.review == 1
    assert %DateTime{} = s.last_run
    File.rm_rf!(store)
  end

  test "a failed later observation cannot expose prior successful stats as current" do
    store = seed_store()

    first =
      Ticker.do_cycle(state(store: store, got: nil, now: @now, encode: enc(), decode: dec()))

    assert first.last_outcome == :completed
    File.rm!(Hypatia.MergeOrchestration.TestObservationStore.receipt_path(store))
    failed = Ticker.do_cycle(first)
    assert failed.last_outcome == :failed
    assert failed.last_stats == nil
    assert failed.runs == first.runs
    assert [message | _] = failed.errors
    assert message =~ "unqualified observation batch"
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
    assert status.last_stats.armed == 0

    GenServer.stop(pid)
    File.rm_rf!(store)
  end
end
