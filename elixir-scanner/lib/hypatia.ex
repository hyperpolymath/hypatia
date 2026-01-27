# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia do
  @moduledoc """
  Hypatia - Security Scanner with Autonomous Learning

  A concurrent code security scanner that:
  - Detects unsafe patterns in Rust, ReScript, OCaml
  - Generates findings in JSON format
  - Learns from observations to auto-generate new rules
  - Integrates with gitbot-fleet coordination layer

  ## Usage

      # Scan a directory
      report = Hypatia.Scanner.generate_report(".")

      # Observe a pattern for learning
      Hypatia.Learning.observe_pattern("new_pattern", %{...})

      # Get learning statistics
      Hypatia.Learning.get_stats()

  ## CLI Usage

      $ mix escript.build
      $ ./hypatia scan .
      $ ./hypatia --format=text --severity=critical src/
  """

  @version "2.0.0"

  def version, do: @version
end
