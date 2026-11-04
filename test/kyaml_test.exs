# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.KyamlTest do
  @moduledoc """
  What Hypatia.Kyaml adds to kyq's tree for its consumers (visible aliases,
  block-scalar line segments, lookups, the node budget), the fixes it carries
  ahead of kyq, and that every workflow in this repository parses.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Kyaml

  defp doc!(text) do
    assert {:ok, [doc]} = Kyaml.parse(text)
    doc
  end

  test "YAML 1.2 Core Schema: yes, on and off are strings; true, 0x1F, 1e3, ~ are not" do
    doc = doc!("a: yes\nb: on\nc: true\nd: 0x1F\ne: 1e3\nf: ~\ng: 0o17\nh: .inf\n")
    kinds = for k <- ~w(a b c d e f g h), do: {Kyaml.get(doc, k).kind, Kyaml.get(doc, k).value}

    assert kinds == [
             string: "yes",
             string: "on",
             bool: true,
             int: 31,
             float: 1000.0,
             null: nil,
             int: 15,
             float: :infinity
           ]
  end

  test "an alias stays visible, at its own location, and deref/1 sees through it" do
    doc = doc!("base: &b\n  x: 1\ncopy: *b\n")
    alias_node = Kyaml.get(doc, "copy")
    assert %{kind: :alias, name: "b", loc: {3, 7}} = alias_node
    assert Kyaml.deref(alias_node) == Kyaml.get(doc, "base")
    assert Kyaml.get(alias_node, "x").value == 1
  end

  test "an alias may only name an anchor defined earlier, so a cycle cannot be written" do
    assert {:error, %{code: "UNKNOWN_ANCHOR", line: 2}} = Kyaml.parse("node: &n\n  self: *n\n")
  end

  test "the YAML 1.1 merge key is refused, as GitHub Actions refuses it" do
    assert {:error, %{code: "UNSUPPORTED_MERGE_KEY", line: 4, column: 3, path: "copy"}} =
             Kyaml.parse("base: &b\n  x: 1\ncopy:\n  <<: *b\n")
  end

  test "a literal block scalar maps each content line to its source line" do
    doc = doc!("run: |\n  npm ci\n\n  deploy ${{ secrets.X }}\n")
    run = Kyaml.get(doc, "run")
    assert run.value == "npm ci\n\ndeploy ${{ secrets.X }}\n"
    assert run.segments == [{0, 2}, {8, 4}]
  end

  test "a folded block scalar keeps the source line of every folded piece" do
    doc = doc!("run: >-\n  deploy\n  --token=${{ secrets.X }}\n")
    run = Kyaml.get(doc, "run")
    assert run.value == "deploy --token=${{ secrets.X }}"
    assert run.segments == [{0, 2}, {7, 3}]
  end

  test "within_budget?/2 stops counting at the limit (the billion-laughs document)" do
    anchors =
      for i <- 1..9 do
        prev = if i == 1, do: "x", else: "*a#{i - 1}"
        "a#{i}: &a#{i} [#{Enum.map_join(1..10, ", ", fn _ -> prev end)}]"
      end

    doc = doc!(Enum.join(anchors, "\n") <> "\nall: *a9\n")
    {micros, within?} = :timer.tc(fn -> Kyaml.within_budget?(doc) end)
    refute within?
    assert micros < 5_000_000
    assert Kyaml.canonical(doc) == ["budget"]
  end

  describe "fixed ahead of kyq (test/fixtures/kyaml/KNOWN_DIVERGENCES.adoc)" do
    test "a single leading BOM is legal; a second one is not" do
      assert {:ok, [_]} = Kyaml.parse("﻿a: 1\n")
      assert {:error, %{code: "BOM_MIDSTREAM", line: 2, column: 1}} = Kyaml.parse("a: 1\n﻿b: 2\n")
    end

    test "a comment may follow a block scalar header" do
      doc = doc!("run: | # the script\n  echo one\n  echo two\n")
      assert Kyaml.get(doc, "run").value == "echo one\necho two\n"
    end

    test "whole-input diagnostics count columns in code points" do
      assert {:error, %{code: "INVALID_NEWLINE", line: 1, column: 5}} =
               Kyaml.parse("x: 🟠\ry: 1\n")
    end
  end

  test "every workflow in this repository parses" do
    files = Path.wildcard(Path.expand("../.github/workflows/*.{yml,yaml}", __DIR__))
    assert length(files) > 10

    failures =
      for file <- files,
          match?({:error, _}, Kyaml.parse(File.read!(file))),
          do: {Path.basename(file), Kyaml.parse(File.read!(file))}

    assert failures == []
  end
end
