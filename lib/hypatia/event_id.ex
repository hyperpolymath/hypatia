# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.EventId do
  @moduledoc """
  The identity of an event: an ADR-008 version 8 identifier under profile T
  (the time-ordered profile), written by Hypatia itself (owner ruling R5-A of
  2026-11-15, `docs/design/e0-receipt.adoc`). It makes no call to an external
  command, so there is no tool that can be missing.

  The layout is the one ADR-008 fixes (`metadatastician/standards`,
  `docs/decisions/ADR-008-uuid-v8-estate-standard.adoc`):

      48 bits  unix_ts_ms   big-endian Unix epoch milliseconds
       4 bits  ver          0b1000 (8)
      12 bits  rand_a       a counter that makes ids increase within a millisecond
       2 bits  var          0b10
      62 bits  rand_b       from a cryptographic random source

  The text is lowercase `8-4-4-4-12`. The field that holds an `event_id`
  declares profile T. The timestamp is never an audited event time and never a
  basis for authorisation (ADR-008).

  The counter is shared by the whole node (an `:atomics` word that holds the
  millisecond and the counter together), so ids increase within one process and
  are distinct across processes. If the clock goes backwards, ids keep
  increasing: the counter carries into the millisecond instead.
  """

  import Bitwise

  @key {__MODULE__, :state}
  @text ~r/\A[0-9a-f]{8}-[0-9a-f]{4}-8[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}\z/

  @doc "Mint an id: lowercase `8-4-4-4-12` text, version 8, variant `10`, profile T."
  @spec generate() :: String.t()
  def generate do
    packed = next_packed()
    ms = packed >>> 12
    counter = packed &&& 0xFFF
    <<rand_b::62, _::2>> = :crypto.strong_rand_bytes(8)

    format(<<ms::48, 8::4, counter::12, 2::2, rand_b::62>>)
  end

  @doc "The 16 bytes of an id in the text form."
  @spec to_binary(String.t()) :: <<_::128>>
  def to_binary(text) when is_binary(text) do
    text |> String.replace("-", "") |> Base.decode16!(case: :lower)
  end

  @doc "Is this a valid `event_id`: a string in the text form, version 8, variant `10`?"
  @spec valid?(term()) :: boolean()
  def valid?(text) when is_binary(text), do: Regex.match?(@text, text)
  def valid?(_other), do: false

  # The shared word holds `(milliseconds << 12) | counter`. A new millisecond
  # resets the counter; the same or an earlier one adds 1, which carries into the
  # millisecond when the 12 bits overflow, so the value only ever increases.
  defp next_packed do
    ref = state()
    now = System.system_time(:millisecond) <<< 12
    advance(ref, now)
  end

  defp advance(ref, now) do
    current = :atomics.get(ref, 1)
    next = if now > current, do: now, else: current + 1

    case :atomics.compare_exchange(ref, 1, current, next) do
      :ok -> next
      _changed -> advance(ref, now)
    end
  end

  defp state do
    case :persistent_term.get(@key, nil) do
      nil ->
        ref = :atomics.new(1, signed: false)
        :persistent_term.put(@key, ref)
        :persistent_term.get(@key)

      ref ->
        ref
    end
  end

  defp format(
         <<a::binary-size(4), b::binary-size(2), c::binary-size(2), d::binary-size(2),
           e::binary-size(6)>>
       ) do
    [a, b, c, d, e]
    |> Enum.map_join("-", &Base.encode16(&1, case: :lower))
  end
end
