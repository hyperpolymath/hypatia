# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.ActionsLockClosureCorpusTest do
  @moduledoc """
  Contract test for actions.lock closure, shared with hyperpolymath/standards
  (standards#963, criterion 5).

  A lock is closed when every ref under `workflows:` and in a record's nested
  `uses:` has a top-level record of its own, with owner/repo compared
  case-insensitively and the ref exactly. `test/fixtures/actions_lock/closure/`
  is a byte-identical copy of standards'
  `scripts/tests/fixtures/actions-lock-closure/`. Its `expected.tsv` gives each
  lock's verdict and, for a dangling lock, the refs that dangle.

  standards' `close-lock.sh --check` runs against the same table, so the two
  implementations cannot drift apart without one of these suites going red.
  Change both copies together.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ActionsLock

  @dir "test/fixtures/actions_lock/closure"
  @table Path.join(@dir, "expected.tsv")
  @external_resource @table

  @rows (for line <- String.split(File.read!(@table), "\n"),
             line != "",
             not String.starts_with?(line, "#") do
           [file, verdict, dangling] = String.split(line, "\t")
           refs = if dangling == "-", do: [], else: String.split(dangling, " ")
           {file, verdict, refs}
         end)

  @dangling_kinds [:workflow_dependencies_missing, :transitive_dependencies_missing]

  test "expected.tsv has exactly one row for each lock in the corpus" do
    locks =
      @dir
      |> Path.join("*.lock")
      |> Path.wildcard()
      |> Enum.map(&Path.basename/1)
      |> Enum.sort()

    assert locks == @rows |> Enum.map(&elem(&1, 0)) |> Enum.sort()
    assert length(locks) >= 5
  end

  for {file, verdict, refs} <- @rows do
    test "#{file} is #{verdict}" do
      result = ActionsLock.parse(File.read!(Path.join(@dir, unquote(file))))
      assert_verdict(result, unquote(verdict), unquote(refs))
    end
  end

  defp assert_verdict(result, "closed", []) do
    assert {:ok, %ActionsLock{}} = result
  end

  defp assert_verdict(result, "dangling", refs) do
    assert {:error, {kind, entries}} = result
    assert kind in @dangling_kinds

    got = entries |> Enum.map(&elem(&1, 1)) |> Enum.map(&ActionsLock.normalise_ref/1)
    assert Enum.sort(got) == refs |> Enum.map(&ActionsLock.normalise_ref/1) |> Enum.sort()
  end
end
