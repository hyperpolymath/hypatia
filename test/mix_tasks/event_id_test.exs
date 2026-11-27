# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.EventIdTest do
  @moduledoc """
  E0-31: `mix hypatia.event_id` prints one event id and nothing else, so that the fleet runner
  can capture it with `$(...)`. The id is an ADR-008 version 8 id of profile T
  (`Hypatia.EventId`).
  """
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Hypatia.EventId
  alias Mix.Tasks.Hypatia.EventId, as: Task

  @root Path.expand("../..", __DIR__)

  test "prints one line, and it is a valid event id" do
    out = capture_io(fn -> Task.run([]) end)

    assert String.ends_with?(out, "\n")
    assert [id] = String.split(out, "\n", trim: true)
    assert EventId.valid?(id)
  end

  test "2 calls give 2 ids, the second after the first" do
    [a, b] = for _ <- 1..2, do: capture_io(fn -> Task.run([]) end) |> String.trim()

    assert a != b
    assert a < b
  end

  test "an argument is refused with exit code 1 and nothing on standard output" do
    out =
      capture_io(fn ->
        capture_io(:stderr, fn ->
          assert catch_exit(Task.run(["--count", "3"])) == {:shutdown, 1}
        end)
      end)

    assert out == ""
  end

  test "a real `mix hypatia.event_id` prints exactly 1 line on standard output" do
    {out, status} =
      System.cmd("mix", ["hypatia.event_id"],
        cd: @root,
        env: [{"MIX_ENV", "test"}],
        stderr_to_stdout: false
      )

    assert status == 0
    assert [id] = String.split(out, "\n", trim: true)
    assert EventId.valid?(id)
  end
end
