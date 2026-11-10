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

  # verification/proofs/idris2/ActionsLockClosure.idr transcribes this corpus
  # into its model, numbering each ref, and proves each row's verdict as an
  # equation. The type checker cannot read the fixtures, so these tests bind
  # the transcription to them: every fixture's edges, and every expected.tsv
  # row, must match the model row its `-- corpus:` line names.
  @model "verification/proofs/idris2/ActionsLockClosure.idr"
  @external_resource @model

  test "the Idris2 model names exactly the corpus locks" do
    corpus = model_corpus(File.read!(@model))
    assert corpus |> Map.keys() |> Enum.sort() == @rows |> Enum.map(&elem(&1, 0)) |> Enum.sort()
  end

  for {file, _verdict, _refs} <- @rows do
    test "the Idris2 model transcribes the edges of #{file}" do
      src = File.read!(@model)
      numbering = model_numbering(src)
      row = Map.fetch!(model_corpus(src), unquote(file))
      {workflow_refs, records} = model_lock(src, row)

      {fixture_refs, fixture_records} =
        lock_edges(File.read!(Path.join(@dir, unquote(file))), numbering)

      assert Enum.sort(fixture_refs) == Enum.sort(workflow_refs)
      assert fixture_records == records
    end
  end

  for {file, _verdict, refs} <- @rows do
    test "the Idris2 equation for #{file} states its expected.tsv row" do
      src = File.read!(@model)
      numbering = model_numbering(src)
      row = Map.fetch!(model_corpus(src), unquote(file))
      expected = Enum.map(unquote(refs), &Map.fetch!(numbering, ActionsLock.normalise_ref(&1)))
      assert model_equation(src, row) == expected
    end
  end

  # `--   N  owner/repo@ref`: the model's ref numbering, keyed after the fold.
  defp model_numbering(src) do
    Regex.scan(~r/^--\s+(\d+)\s+(\S+@\S+)\s*$/m, src)
    |> Map.new(fn [_, n, ref] -> {ActionsLock.normalise_ref(ref), String.to_integer(n)} end)
  end

  # `-- corpus: <fixture> <model row>`
  defp model_corpus(src) do
    Regex.scan(~r/^-- corpus: (\S+) (\w+)\s*$/m, src)
    |> Map.new(fn [_, file, row] -> {file, row} end)
  end

  # `<row> = MkLock [w, ...] [(k, [u, ...]), ...]`
  defp model_lock(src, row) do
    [_, wf, recs] = Regex.run(~r/^#{row} = MkLock \[([\d, ]*)\] \[(.*)\]\s*$/m, src)

    records =
      Regex.scan(~r/\((\d+), \[([\d, ]*)\]\)/, recs)
      |> Map.new(fn [_, k, uses] -> {String.to_integer(k), uses |> nats() |> Enum.sort()} end)

    {nats(wf), records}
  end

  # `row<Name> : danglingOf ActionsLockClosure.<row> = [n, ...]`
  defp model_equation(src, row) do
    [_, refs] = Regex.run(~r/: danglingOf ActionsLockClosure\.#{row} = \[([\d, ]*)\]/, src)
    nats(refs)
  end

  defp nats(list),
    do: list |> String.split(",", trim: true) |> Enum.map(&String.to_integer(String.trim(&1)))

  # The edges a fixture names: every quoted ref under workflows:, and each
  # dependency record with its nested uses:, mapped through the numbering.
  defp lock_edges(content, numbering) do
    id = &Map.fetch!(numbering, ActionsLock.normalise_ref(&1))

    {_section, _key, refs, records} =
      content
      |> String.split("\n")
      |> Enum.reduce({nil, nil, [], %{}}, fn line, {section, key, refs, records} ->
        cond do
          line == "workflows:" ->
            {:workflows, nil, refs, records}

          line == "dependencies:" ->
            {:dependencies, nil, refs, records}

          section == :workflows and Regex.match?(~r/^\s+- '/, line) ->
            [_, ref] = Regex.run(~r/^\s+- '([^']+)'$/, line)
            {section, key, [id.(ref) | refs], records}

          section == :dependencies and Regex.match?(~r/^    '[^']+':$/, line) ->
            [_, ref] = Regex.run(~r/^    '([^']+)':$/, line)
            {section, id.(ref), refs, Map.put(records, id.(ref), [])}

          section == :dependencies and Regex.match?(~r/^\s{12}- '/, line) ->
            [_, ref] = Regex.run(~r/^\s+- '([^']+)'$/, line)
            {section, key, refs, Map.update!(records, key, &Enum.sort([id.(ref) | &1]))}

          true ->
            {section, key, refs, records}
        end
      end)

    {refs, records}
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
