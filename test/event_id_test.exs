# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# E0-09 (owner ruling R5-A): Hypatia mints the event_id of a row itself, with an
# Elixir implementation of the ADR-008 profile T layout, and makes no
# `System.cmd` call. These tests check the layout that ADR-008 fixes.
defmodule Hypatia.EventIdTest do
  use ExUnit.Case, async: true

  alias Hypatia.EventId

  @text ~r/\A[0-9a-f]{8}-[0-9a-f]{4}-8[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}\z/

  test "the text is the lowercase 8-4-4-4-12 form, version 8, variant 10" do
    for _ <- 1..200 do
      assert EventId.generate() =~ @text
    end
  end

  test "the version nibble is 8 and the variant bits are 10, in the bytes" do
    <<_ts::48, version::4, _a::12, variant::2, _b::62>> = EventId.to_binary(EventId.generate())
    assert version == 8
    assert variant == 0b10
  end

  test "the first 48 bits are the Unix time in milliseconds, big-endian" do
    before_ms = System.system_time(:millisecond)
    id = EventId.generate()
    after_ms = System.system_time(:millisecond)

    <<ts::48, _rest::80>> = EventId.to_binary(id)
    # The monotonic counter may carry into the millisecond, so allow 1 ms of slack.
    assert ts >= before_ms and ts <= after_ms + 1
  end

  test "ids increase within one process, as text and as bytes" do
    ids = for _ <- 1..5_000, do: EventId.generate()
    assert ids == Enum.sort(ids)
    assert length(Enum.uniq(ids)) == 5_000

    bins = Enum.map(ids, &EventId.to_binary/1)
    assert bins == Enum.sort(bins)
  end

  test "ids from many processes are distinct" do
    ids =
      1..8
      |> Task.async_stream(fn _ -> for _ <- 1..500, do: EventId.generate() end)
      |> Enum.flat_map(fn {:ok, ids} -> ids end)

    assert length(Enum.uniq(ids)) == 4_000
  end

  test "valid?/1 accepts an id and refuses what is not one" do
    assert EventId.valid?(EventId.generate())

    for bad <- [
          "",
          nil,
          42,
          "0198a7b2-6f0c-7000-8000-000000000000",
          "0198a7b2-6f0c-8000-c000-000000000000",
          "0198A7B2-6F0C-8000-8000-000000000000",
          "0198a7b2-6f0c-8000-8000-00000000000"
        ] do
      refute EventId.valid?(bad)
    end
  end

  test "the module makes no call to an external command" do
    source = File.read!("lib/hypatia/event_id.ex")
    refute source =~ "System.cmd"
    refute source =~ ":os.cmd"
  end
end
