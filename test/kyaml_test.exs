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

  test "within_budget?/2 includes the limit itself" do
    doc = doc!("a: 1\n")
    # A mapping and its one value: two nodes (kyq does not count keys).
    assert Kyaml.within_budget?(doc, 2)
    refute Kyaml.within_budget?(doc, 1)
  end

  # verification/proofs/idris2/KyamlBudget.idr proves, of a model of this
  # count, that a total within the limit is the expanded size and that a
  # document within the limit is passed. This holds the Elixir count to that
  # model's `size` on every tree of a small domain: scalars, sequences,
  # mappings, and aliases (to a non-alias, as the reader allows) sharing a
  # subtree.
  test "within_budget?/2 passes exactly the trees whose expanded size is within the limit" do
    leaf = %{kind: :string, value: "x"}
    seq = &%{kind: :seq, items: &1}
    map = fn values -> %{kind: :map, entries: Enum.map(values, &%{key: leaf, value: &1})} end
    ali = &%{kind: :alias, target: &1}

    size = fn
      size, %{kind: :alias, target: t} -> size.(size, t)
      size, %{kind: :seq, items: items} -> 1 + Enum.sum(Enum.map(items, &size.(size, &1)))
      size, %{kind: :map, entries: es} -> 1 + Enum.sum(Enum.map(es, &size.(size, &1.value)))
      _, _ -> 1
    end

    grow = fn trees ->
      non_alias = Enum.reject(trees, &(&1.kind == :alias))
      lists = [[]] ++ for(a <- trees, do: [a]) ++ for(a <- trees, b <- trees, do: [a, b])
      trees ++ Enum.map(non_alias, ali) ++ Enum.map(lists, seq) ++ Enum.map(lists, map)
    end

    trees = [leaf] |> grow.() |> grow.() |> Enum.uniq()
    assert length(trees) > 150

    mismatches =
      for t <- trees,
          n = size.(size, t),
          limit <- 0..(n + 1),
          Kyaml.within_budget?(t, limit) != n <= limit,
          do: {t, limit}

    assert mismatches == []
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
      assert {:ok, [_]} = Kyaml.parse("\uFEFFa: 1\n")

      assert {:error, %{code: "BOM_MIDSTREAM", line: 2, column: 1}} =
               Kyaml.parse("a: 1\n\uFEFFb: 2\n")
    end

    test "a comment may follow a block scalar header" do
      doc = doc!("run: | # the script\n  echo one\n  echo two\n")
      assert Kyaml.get(doc, "run").value == "echo one\necho two\n"
    end

    test "a trailing comma in a flow collection, as KYAML (KEP-5295) writes it" do
      doc = doc!("{\n  ports: [80, 443,],\n  meta: {a: \"1\",},\n}\n")
      assert %{kind: :seq, items: items} = Kyaml.get(doc, "ports")
      assert Enum.map(items, & &1.value) == [80, 443]
      assert Kyaml.get(Kyaml.get(doc, "meta"), "a").value == "1"
    end

    # The same inputs as kyq's bridge vectors of the same names
    # (conformance/fixtures/bridge/vectors.json on the kyq fix), found by
    # CodeRabbit's review of hyperpolymath/hypatia#1008.
    @yaml_1_2_cases [
      {"bom-after-leading-bom", "\uFEFFa\uFEFF\n", {:error, "BOM_MIDSTREAM", 1, 2, ""}},
      {"anchor-scoped-to-document", "---\na: &x 1\n---\nb: *x\n",
       {:error, "UNKNOWN_ANCHOR", 4, 4, "b"}},
      {"directive-trailing-comment", "%YAML 1.2 # version\n---\na: 1\n", {:ok, %{"a" => 1}}},
      {"second-tag-refused", "x: !!int !!str true\n", {:error, "DUPLICATE_PROPERTY", 1, 10, ""}},
      {"second-anchor-refused", "x: &a &b 1\n", {:error, "DUPLICATE_PROPERTY", 1, 7, ""}},
      {"float-tag-on-integer", "x: !!float 1\n", {:ok, %{"x" => 1.0}}},
      {"plain-indicator-at", "x: @foo\n", {:error, "PLAIN_INDICATOR", 1, 4, ""}},
      {"plain-indicator-backtick", "x: `foo\n", {:error, "PLAIN_INDICATOR", 1, 4, ""}},
      {"plain-indicator-percent", "x: %foo\n", {:error, "PLAIN_INDICATOR", 1, 4, ""}},
      {"plain-indicator-dash-space", "a: - item\n", {:error, "PLAIN_INDICATOR", 1, 4, ""}},
      {"plain-indicator-question-space", "a: ? x\n", {:error, "PLAIN_INDICATOR", 1, 4, ""}},
      {"plain-indicator-lone-dash", "a: -\n", {:error, "PLAIN_INDICATOR", 1, 4, ""}},
      {"plain-indicator-in-flow", "a: [- x]\n", {:error, "PLAIN_INDICATOR", 1, 5, ""}},
      {"dash-before-safe-character", "a: -x\nb: -1\nc: ?x\nd: :x\n",
       {:ok, %{"a" => "-x", "b" => -1, "c" => "?x", "d" => ":x"}}},
      {"flow-complex-key-refused", "x: {? a: 1}\n",
       {:error, "UNSUPPORTED_COMPLEX_KEY", 1, 5, ""}},
      # Found by running the YAML test suite (yaml/yaml-test-suite) against
      # both readers; the same names are kyq bridge vectors.
      {"comment-needs-space-after-quoted", "a: \"x\"#c\n", {:error, "BAD_COMMENT", 1, 7, ""}},
      {"comment-needs-space-after-flow", "a: [1, 2]#c\n", {:error, "BAD_COMMENT", 1, 10, ""}},
      {"comment-needs-space-after-block-header", "a: >#c\n  x\n",
       {:error, "BAD_COMMENT", 1, 5, ""}},
      {"control-character-in-comment", "a: 1 # bad\u0001\n",
       {:error, "CONTROL_CHARACTER", 1, 11, ""}},
      {"leading-blank-line-past-content", "x: |\n    \n  a\n",
       {:error, "BAD_BLOCK_SCALAR", 3, 3, ""}},
      {"folded-tab-line-is-spaced", "x: >\n  foo\n  \tbar\n", {:ok, %{"x" => "foo\n\tbar\n"}}},
      {"directive-per-document", "%YAML 1.2\n---\na: 1\n...\n%YAML 1.2\n---\nb: 2\n",
       {:ok_docs, [%{"a" => 1}, %{"b" => 2}]}},
      {"directive-without-document", "%YAML 1.2\n", {:error, "MISSING_DOC_START", 2, 1, ""}},
      {"duplicate-yaml-directive", "%YAML 1.2\n%YAML 1.2\n---\na: 1\n",
       {:error, "DUPLICATE_DIRECTIVE", 2, 1, ""}},
      {"trailing-document-start-is-null", "a: b\n---\n", {:ok_docs, [%{"a" => "b"}, nil]}},
      {"flow-line-not-indented", "x: [a,\nb]\n", {:error, "BAD_INDENT", 2, 1, ""}},
      {"flow-closer-at-parent-column", "x: [\n  a,\n]\n", {:ok, %{"x" => ["a"]}}},
      {"root-flow-closes-at-column-one", "{\n  a: 1,\n}\n", {:ok, %{"a" => 1}}},
      {"flow-tab-indentation", "x: [\n\t1\n]\n", {:error, "TAB_INDENT", 2, 1, ""}},
      {"flow-tab-after-indentation", "x: [\n \t1\n]\n", {:ok, %{"x" => [1]}}},
      {"document-marker-in-flow", "[\n---\n]\n", {:error, "UNTERMINATED_FLOW", 2, 1, ""}},
      {"flow-key-on-two-lines", "x: {\n  a\n  : 1}\n", {:error, "MULTILINE_KEY", 2, 3, "x"}},
      {"flow-key-holds-url-colon", "x: {http://example.com: ok}\n",
       {:ok, %{"x" => %{"http://example.com" => "ok"}}}},
      {"flow-plain-cannot-hold-bracket", "x: [a[b]\n", {:error, "FLOW_SEPARATOR", 1, 6, "x"}},
      {"plain-indicator-comma-after-tag", "- !!str, x\n", {:error, "PLAIN_INDICATOR", 1, 8, ""}},
      {"tab-indents-nested-sequence", "-\t- a\n", {:error, "TAB_INDENT", 1, 3, ""}},
      {"tab-before-comment", "x: value\t# comment\n", {:ok, %{"x" => "value"}}},
      {"block-header-indent-then-chomp", "x: |2-\n  a\n", {:ok, %{"x" => "a"}}},
      {"block-header-chomp-then-indent", "x: >-2\n  a\n", {:ok, %{"x" => "a"}}},
      {"block-whitespace-line-is-content", "x: |2\n  a\n    \n  b\n",
       {:ok, %{"x" => "a\n  \nb\n"}}},
      {"block-clip-at-end-of-input", "x: |\n  a", {:ok, %{"x" => "a\n"}}},
      {"block-keep-at-end-of-input", "x: |+\n  a", {:ok, %{"x" => "a\n"}}},
      {"block-keep-only-blank-lines", "x: |+\n\n\n", {:ok, %{"x" => "\n\n"}}},
      {"flow-empty-entry", "x: [1,,2]\n", {:error, "EMPTY_VALUE", 1, 7, "x"}},
      {"flow-lone-comma", "x: [,]\n", {:error, "EMPTY_VALUE", 1, 5, "x"}},
      {"tagged-key-refused", "a: 1\n!!str a: 2\n", {:error, "KEY_PROPERTY", 2, 1, ""}},
      {"anchored-first-key-refused", "&k a: 1\n", {:error, "KEY_PROPERTY", 1, 1, ""}},
      {"flow-tagged-key-refused", "x: {!!str a: 1}\n", {:error, "KEY_PROPERTY", 1, 5, ""}},
      {"flow-alias-decorated", "a: &a 1\nb: [!!int *a]\n", {:error, "ALIAS_DECORATED", 2, 5, ""}},
      # The printable set (YAML 1.2 c-printable) is checked over the whole
      # stream, so the error stands at the character, wherever it is.
      {"control-character-in-plain", "a: x\u0001y\n", {:error, "CONTROL_CHARACTER", 1, 5, ""}},
      {"control-character-in-key", "a\u0001: 1\n", {:error, "CONTROL_CHARACTER", 1, 2, ""}},
      {"control-character-in-flow", "a: [x\u0001y]\n", {:error, "CONTROL_CHARACTER", 1, 6, ""}},
      {"control-character-in-block", "a: |\n  x\u0001y\n",
       {:error, "CONTROL_CHARACTER", 2, 4, ""}},
      {"control-character-in-quoted", "a: \"x\u0001\"\n",
       {:error, "CONTROL_CHARACTER", 1, 6, ""}},
      {"control-character-in-directive-comment", "%YAML 1.2 # c\u0001\n---\na: 1\n",
       {:error, "CONTROL_CHARACTER", 1, 14, ""}},
      {"control-character-in-anchor", "a: &x\u0001 1\n", {:error, "CONTROL_CHARACTER", 1, 6, ""}},
      {"delete-character", "a: 'x\u007F'\n", {:error, "CONTROL_CHARACTER", 1, 6, ""}},
      {"c1-control-in-comment", "a: 1 # \u0085ok \u0090\n",
       {:error, "CONTROL_CHARACTER", 1, 12, ""}},
      {"noncharacter", "a: x\uFFFE\n", {:error, "NONCHARACTER", 1, 5, ""}},
      {"next-line-is-printable", "a: \"x\u0085y\"\n", {:ok, %{"a" => "x\u0085y"}}},
      {"tab-in-block-is-printable", "a: |\n  tab\tok\n", {:ok, %{"a" => "tab\tok\n"}}},
      {"anchor-needs-separation", "a: &x[1]\n", {:error, "PROPERTY_SEPARATION", 1, 6, ""}},
      {"tag-needs-separation", "a: !!seq{}\n", {:error, "PROPERTY_SEPARATION", 1, 9, ""}},
      {"anchor-separated", "a: &x [1]\n", {:ok, %{"a" => [1]}}},
      {"flow-key-keeps-nbsp", "x: {\u00A0a\u00A0: 1}\n",
       {:ok, %{"x" => %{"\u00A0a\u00A0" => 1}}}},
      {"flow-key-stops-at-comment", "x: {a #c\n  : 1}\n", {:error, "MULTILINE_KEY", 1, 5, "x"}},
      {"root-explicit-indent", "|2\n  text\n", {:ok, "text\n"}},
      {"root-explicit-indent-one", "|1\n  text\n", {:ok, " text\n"}},
      # YAML white space is space and tab: a no-break space is content.
      {"directive-nbsp-separator", "%YAML\u00A01.2\n---\na: 1\n",
       {:error, "UNSUPPORTED_DIRECTIVE", 1, 1, ""}},
      {"nbsp-before-hash-is-key-text", "a\u00A0#b: 1\n", {:ok, %{"a\u00A0#b" => 1}}},
      {"nbsp-is-part-of-anchor-name", "a: &x\u00A0 1\nb: *x\u00A0\n",
       {:ok, %{"a" => 1, "b" => 1}}},
      {"nbsp-anchor-is-not-the-short-name", "a: &x\u00A0 1\nb: *x\n",
       {:error, "UNKNOWN_ANCHOR", 2, 4, "b"}},
      # An implicit key is at most 1024 characters (YAML 1.2 §7.4.2).
      {"implicit-key-at-limit", String.duplicate("k", 1024) <> ": 1\n",
       {:ok, %{String.duplicate("k", 1024) => 1}}},
      {"implicit-key-over-limit", String.duplicate("k", 1025) <> ": 1\n",
       {:error, "KEY_TOO_LONG", 1, 1, ""}},
      {"flow-key-over-limit", "x: {" <> String.duplicate("k", 1025) <> ": 1}\n",
       {:error, "KEY_TOO_LONG", 1, 5, ""}},
      {"astral-key-at-limit", String.duplicate("\u{1F600}", 1024) <> ": 1\n",
       {:ok, %{String.duplicate("\u{1F600}", 1024) => 1}}},
      {"quoted-key-over-limit", "\"" <> String.duplicate("q", 1023) <> "\": 1\n",
       {:error, "KEY_TOO_LONG", 1, 1, ""}},
      # `...` with no open document is legal (YAML test suite HWV9).
      {"document-end-alone", "...\n", {:ok_docs, []}},
      # Only `---` and `...` are document markers.
      {"flow-dashes-and-dot-are-a-scalar", "[\n--. ,\n]\n", {:ok, ["--."]}},
      {"flow-dash-dot-dash-is-a-scalar", "[\n-.- ,\n]\n", {:ok, ["-.-"]}},
      {"flow-document-marker-refused", "[\n--- ,\n]\n", {:error, "UNTERMINATED_FLOW", 2, 1, ""}},
      # Properties continued on the next line belong to the same node.
      {"anchor-continued-on-next-line-refused", "a: &one\n  &two value\n",
       {:error, "DUPLICATE_PROPERTY", 2, 3, ""}},
      {"tag-continued-on-next-line-refused", "a: !!str\n  !!int 1\n",
       {:error, "DUPLICATE_PROPERTY", 2, 3, ""}},
      {"anchor-then-tag-on-next-line", "a: &one\n  !!str 1\nb: *one\n",
       {:ok, %{"a" => "1", "b" => "1"}}},
      {"root-anchor-continued-refused", "&x\n&y v\n", {:error, "DUPLICATE_PROPERTY", 2, 1, ""}},
      # The document root sits at indentation -1, so its block scalar's
      # content may start at column 1 (yaml-test-suite W4TN, FP8R, DK3J).
      {"root-block-scalar-at-column-one", "|\ntext\n", {:ok, "text\n"}},
      # A bare property's value on the next line stays inside the node: a
      # line that belongs to the enclosing collection makes the node null.
      {"seq-anchor-then-sibling", "- &a\n- b\n", {:ok, [nil, "b"]}},
      {"seq-tag-then-sibling", "- !!str\n- b\n", {:ok, ["", "b"]}},
      {"nested-anchor-then-sibling-key", "a:\n  &x\nb: 1\n", {:ok, %{"a" => nil, "b" => 1}}},
      {"seq-anchor-then-map-refused", "- &a\nkey: v\n", {:error, "UNEXPECTED_CONTENT", 2, 1, ""}},
      {"seq-anchor-then-nested-seq", "- &a\n - b\n", {:ok, [["b"]]}},
      {"nested-anchor-then-own-map", "a:\n  &x\n  b: 1\n", {:ok, %{"a" => %{"b" => 1}}}},
      # A map value may be a compact sequence at the map's own indentation.
      {"anchor-then-compact-seq", "seq:\n &anchor\n- a\n- b\n", {:ok, %{"seq" => ["a", "b"]}}}
    ]

    defp plain(node) do
      case Kyaml.deref(node) do
        %{kind: :map, entries: es} -> Map.new(es, &{&1.key.value, plain(&1.value)})
        %{kind: :seq, items: items} -> Enum.map(items, &plain/1)
        %{value: value} -> value
      end
    end

    for {name, yaml, expected} <- @yaml_1_2_cases do
      test "YAML 1.2: #{name}" do
        yaml = unquote(yaml)

        case unquote(Macro.escape(expected)) do
          {:ok, value} ->
            assert {:ok, [doc | _]} = Kyaml.parse(yaml)
            assert plain(doc) == value

          {:ok_docs, values} ->
            assert {:ok, docs} = Kyaml.parse(yaml)
            assert Enum.map(docs, &plain/1) == values

          {:error, code, line, column, path} ->
            assert {:error, %{code: ^code, line: ^line, column: ^column, path: ^path}} =
                     Kyaml.parse(yaml)
        end
      end
    end

    # Four times the entries must cost well under sixteen times the time: the
    # collections used to be rebuilt per entry (20,000 block mapping entries
    # took 4.8 s, now 0.5 s).
    test "flat collections parse in linear time" do
      shapes = [
        fn n -> Enum.map_join(1..n, &"k#{&1}: #{&1}\n") end,
        fn n -> Enum.map_join(1..n, &"- #{&1}\n") end,
        fn n -> "[" <> Enum.map_join(1..n, ", ", &Integer.to_string/1) <> "]\n" end
      ]

      for shape <- shapes do
        [small, large] =
          for n <- [5_000, 20_000] do
            text = shape.(n)
            # The best of three, to keep a scheduler hiccup out of the ratio.
            Enum.min(
              for _ <- 1..3, do: elem(:timer.tc(fn -> {:ok, _} = Kyaml.parse(text) end), 0)
            )
          end

        assert large / max(small, 1) < 10
      end
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
