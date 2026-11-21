# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.EventId do
  @moduledoc """
  Print one event id, and nothing else.

  The fleet runner (`gitbot-fleet`, `scripts/dispatch-runner.sh`) mints the id of a fix
  attempt before the fix runs, and writes it in the start record and in the outcome row of the
  attempt (work order E0-12). It already runs `mix` in the Hypatia checkout for
  `mix hypatia.record_outcome`, so this is the tool that it uses to mint the id:

      id=$(cd "$HYPATIA_HOME" && mix hypatia.event_id)

  The id is an ADR-008 version 8 id of profile T (time-ordered: 48 bits of Unix milliseconds,
  then random bits), from `Hypatia.EventId`, which the owner ruled on in the receipt design
  (`docs/design/e0-receipt.adoc`, decision D3 and ruling R5). It is never derived from the
  content of a row: 2 attempts with the same input get 2 ids.

  The task does not start the application, so it is quick and writes only the id to standard
  output: one line, ended by a line feed. A runner should still check what it captured, because
  a build that has to compile first writes its own lines before the id; take the last line and
  check it with `Hypatia.EventId.valid?/1` or the pattern of ADR-008.

  Takes no argument. An argument is refused, with exit code 1 and nothing on standard output.
  """

  use Mix.Task

  @shortdoc "Print one event id (ADR-008 version 8, profile T)"

  @impl Mix.Task
  def run([]), do: Mix.shell().info(Hypatia.EventId.generate())

  def run(argv) do
    Mix.shell().error("mix hypatia.event_id takes no argument, got: #{Enum.join(argv, " ")}")
    exit({:shutdown, 1})
  end
end
