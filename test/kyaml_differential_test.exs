# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.KyamlDifferentialTest do
  @moduledoc """
  Hypatia.Kyaml against kyq's reference parser, file by file. KYQ_DUMP names
  the JSON lines `test/fixtures/kyaml/kyq_dump.mjs` wrote for a set of files
  from a kyq checkout; for each, this reader must give the same documents
  (every node's kind, location and value, aliases seen through) or the same
  error (code, line, column, path). The kyaml-conformance CI job runs it over
  kyq's own fixtures and every workflow in this repository, at a pinned kyq
  commit. Excluded by default (it needs the dump).
  """
  use ExUnit.Case, async: true

  alias Hypatia.Kyaml

  @moduletag :kyq_differential

  test "Hypatia.Kyaml and kyq agree on every dumped file" do
    dump = System.get_env("KYQ_DUMP") || flunk("set KYQ_DUMP to the output of kyq_dump.mjs")
    rows = dump |> File.read!() |> String.split("\n", trim: true) |> Enum.map(&Jason.decode!/1)

    assert length(rows) >= 50,
           "the dump covers #{length(rows)} files; a vacuous run is not a pass"

    disagreements =
      for row <- rows,
          mine = mine(File.read!(row["file"])),
          mine != Map.take(row, ["docs", "error"]),
          do:
            "#{row["file"]}\n  kyq:  #{inspect(row["error"] || :documents)}\n  mine: #{inspect(mine["error"] || :documents)}"

    IO.puts(
      "kyq differential: #{length(rows) - length(disagreements)} of #{length(rows)} files agree"
    )

    assert disagreements == [], Enum.join(disagreements, "\n")
  end

  defp mine(bytes) do
    result =
      case Kyaml.parse(bytes) do
        {:ok, docs} -> %{"docs" => Enum.map(docs, &Kyaml.canonical/1)}
        {:error, e} -> %{"error" => [e.code, e.line, e.column, e.path]}
      end

    # Round-trip through JSON, as kyq's side did.
    result |> Jason.encode!() |> Jason.decode!()
  end
end
