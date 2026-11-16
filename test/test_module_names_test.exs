# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.TestModuleNamesTest do
  @moduledoc """
  Two test files that define a module with the same name do not both run: the second
  definition replaces the first, and the tests of the first are silently dropped
  (Elixir only warns "redefining module"). The E0-27 rollback test was never run in
  the full suite for that reason, and neither were 3 tests of the branch-protection
  rules. This test fails when 2 test files give the same module name.
  """
  use ExUnit.Case, async: true

  test "no 2 test files define a module with the same name" do
    root = Path.expand(".", __DIR__)

    defs =
      for file <- Path.wildcard(Path.join(root, "**/*.exs")),
          line <- File.read!(file) |> String.split("\n"),
          # Top level only: a module nested in another has a qualified name of its own.
          [_, name] <- [Regex.run(~r/^defmodule\s+([A-Za-z0-9_.]+)\s+do/, line)],
          do: {name, Path.relative_to(file, root)}

    assert length(defs) > 100, "the scan found #{length(defs)} modules: the pattern is broken"

    duplicates =
      defs
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.map(fn {name, files} -> {name, Enum.uniq(files)} end)
      |> Enum.filter(fn {_name, files} -> length(files) > 1 end)

    assert duplicates == [],
           "a test module name is defined in more than one file, so one set of tests " <>
             "does not run: #{inspect(duplicates)}"
  end
end
