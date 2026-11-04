# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Kyaml do
  @moduledoc """
  Reader for KYAML, the block-and-flow YAML subset the estate writes its
  configuration in (`metadatastician/kyq` `docs/KYAML_SPEC.adoc` §4; standards
  YAML-POLICY, owner rulings D280/D307).

  This is an Elixir port of kyq's reference parser,
  `conformance/src/kyaml.js`, function for function. kyq is the canonical
  reader, and its conformance corpus is the contract:
  `test/kyaml_conformance_test.exs` runs the corpus, and the `kyaml-conformance`
  CI job runs this reader and kyq's side by side over the corpus and every
  workflow in the repository, at a pinned kyq commit. It diverges from kyq
  only where kyq has a bug, and each divergence is listed in
  `test/fixtures/kyaml/KNOWN_DIVERGENCES.adoc` with its upstream fix.

  Everything outside the subset is a located error, never a guess: duplicate
  keys (on the resolved key), complex keys, the YAML 1.1 `<<` merge key,
  non-core tags, directives other than `%YAML 1.2`, multi-line plain and
  quoted scalars, tabs as indentation. A trailing comma in a flow collection
  is accepted, as YAML 1.2 and KYAML (KEP-5295) write it. Plain scalars resolve with the YAML 1.2 Core Schema (`yes` and `on` are
  strings).

  ## Nodes

      %{kind: :map, entries: [%{key: scalar, value: node, loc: loc}], loc: loc}
      %{kind: :seq, items: [node], loc: loc}
      %{kind: :string | :int | :float | :bool | :null, value: term,
        raw: String.t(), style: :plain | :quoted | :block, loc: loc,
        segments: [{byte_offset, line}]}
      %{kind: :alias, name: String.t(), loc: loc, target: node}

  `loc` is `{line, column}`, both 1-based. Two things differ from kyq's
  in-memory tree, for the benefit of consumers that report findings by line:

    * an alias stays visible as an `:alias` node (kyq returns the anchored node
      itself), so a consumer can attribute what it finds through the alias to
      the alias's line. `deref/1` sees through it;
    * a block scalar (`|`, `>`) carries `segments`: where each of its content
      lines starts in the value, and the source line it came from.

  Floats that JSON cannot carry are `:infinity`, `:neg_infinity` and `:nan`.
  """

  defmodule Error do
    @moduledoc "A located KYAML diagnostic, with kyq's stable code."
    defexception [:code, :message, :line, :column, path: ""]

    @impl true
    def message(%{code: code, message: message, line: line, column: column}),
      do: "#{code}: #{message} (line #{line}, column #{column})"
  end

  @null_words ["null", "Null", "NULL", "~", ""]
  @true_words ["true", "True", "TRUE"]
  @false_words ["false", "False", "FALSE"]
  @core_tags ["!!str", "!!int", "!!float", "!!bool", "!!null", "!!map", "!!seq", "!"]

  # kyq's anchor budget for alias expansion (bridge.js NODE_BUDGET, SEC-1).
  @node_budget 100_000

  @doc "The number of nodes a document may expand to through its aliases."
  def node_budget, do: @node_budget

  @doc """
  Parse a KYAML stream. Returns one node per document; an empty stream is
  `{:ok, []}`.
  """
  def parse(bytes) when is_binary(bytes) do
    if String.valid?(bytes) do
      try do
        {:ok, parse_stream(bytes)}
      catch
        {:kyaml, %Error{} = error} -> {:error, error}
      end
    else
      {:error,
       %Error{code: "INVALID_UTF8", message: "input is not valid UTF-8", line: nil, column: nil}}
    end
  end

  @doc "See through an alias to the node it names."
  def deref(%{kind: :alias, target: target}), do: deref(target)
  def deref(node), do: node

  @doc """
  The value at `key` in a mapping node (aliases seen through), or `nil`. Keys
  match on their resolved string.
  """
  def get(node, key) do
    case deref(node) do
      %{kind: :map, entries: entries} ->
        Enum.find_value(entries, fn %{key: k, value: v} ->
          if k.kind == :string and k.value == key, do: v
        end)

      _ ->
        nil
    end
  end

  @doc """
  Whether `node` expands to no more than `limit` nodes once aliases are
  followed (kyq's `assertNodeBudget`). Stops counting at the limit, so a
  billion-laughs document costs at most `limit` steps.
  """
  def within_budget?(node, limit \\ @node_budget) do
    count([node], 0, limit) <= limit
  end

  defp count(_, n, limit) when n > limit, do: n
  defp count([], n, _limit), do: n

  defp count([node | rest], n, limit) do
    case node do
      %{kind: :alias, target: target} ->
        count([target | rest], n, limit)

      %{kind: :map, entries: entries} ->
        count(Enum.map(entries, & &1.value) ++ rest, n + 1, limit)

      %{kind: :seq, items: items} ->
        count(items ++ rest, n + 1, limit)

      _ ->
        count(rest, n + 1, limit)
    end
  end

  @doc """
  A document as plain terms, aliases seen through, in the form kyq's side of
  the conformance differential prints (`test/fixtures/kyaml/kyq_dump.mjs`):
  `["map", line, col, [[key, value], ...]]`, `["seq", line, col, [node, ...]]`,
  or `[kind, line, col, value]`, where an int is a decimal string and a float
  its source text. A document over the node budget is `["budget"]`.
  """
  def canonical(node) do
    if within_budget?(node), do: canon(node), else: ["budget"]
  end

  defp canon(%{kind: :alias, target: t}), do: canon(t)

  defp canon(%{kind: :map, loc: {l, c}, entries: es}),
    do: ["map", l, c, Enum.map(es, &[canon(&1.key), canon(&1.value)])]

  defp canon(%{kind: :seq, loc: {l, c}, items: is}), do: ["seq", l, c, Enum.map(is, &canon/1)]
  defp canon(%{kind: :int, loc: {l, c}, value: v}), do: ["int", l, c, Integer.to_string(v)]
  defp canon(%{kind: :float, loc: {l, c}, raw: raw}), do: ["float", l, c, raw]
  defp canon(%{kind: kind, loc: {l, c}, value: v}), do: [Atom.to_string(kind), l, c, v]

  @doc """
  Resolve a plain scalar with the YAML 1.2 Core Schema: `{kind, value}`.
  `yes`/`no`/`on`/`off` are strings, as in YAML 1.2 (unlike 1.1).
  """
  def resolve_plain(text) do
    cond do
      text in @null_words ->
        {:null, nil}

      text in @true_words ->
        {:bool, true}

      text in @false_words ->
        {:bool, false}

      Regex.match?(~r/\A[-+]?[0-9]+\z/, text) ->
        {:int, String.to_integer(String.trim_leading(text, "+"))}

      Regex.match?(~r/\A0o[0-7]+\z/, text) ->
        {:int, String.to_integer(binary_part(text, 2, byte_size(text) - 2), 8)}

      Regex.match?(~r/\A0x[0-9a-fA-F]+\z/, text) ->
        {:int, String.to_integer(binary_part(text, 2, byte_size(text) - 2), 16)}

      Regex.match?(~r/\A[-+]?(?:\.[0-9]+|[0-9]+(?:\.[0-9]*)?)(?:[eE][-+]?[0-9]+)?\z/, text) ->
        {:float, to_float(text)}

      Regex.match?(~r/\A[-+]?\.(?:inf|Inf|INF)\z/, text) ->
        {:float, if(String.starts_with?(text, "-"), do: :neg_infinity, else: :infinity)}

      Regex.match?(~r/\A\.(?:nan|NaN|NAN)\z/, text) ->
        {:float, :nan}

      true ->
        {:string, text}
    end
  end

  # JavaScript's Number(text) for a FLOAT-shaped string: Erlang needs digits on
  # both sides of the point, and saturates to an atom where JS gives ±Infinity.
  defp to_float(text) do
    [_, sign, mantissa, exponent] =
      (Regex.run(~r/\A([-+]?)([0-9.]+)(?:[eE]([-+]?[0-9]+))?\z/, text) ++ [""]) |> Enum.take(4)

    mantissa =
      case String.split(mantissa, ".") do
        [whole] -> whole <> ".0"
        ["", frac] -> "0." <> frac
        [whole, ""] -> whole <> ".0"
        [whole, frac] -> whole <> "." <> frac
      end

    exponent = if exponent == "", do: "0", else: exponent

    try do
      :erlang.binary_to_float(if(sign == "-", do: "-", else: "") <> mantissa <> "e" <> exponent)
    rescue
      ArgumentError ->
        cond do
          String.to_integer(exponent) < 0 -> if sign == "-", do: -0.0, else: 0.0
          sign == "-" -> :neg_infinity
          true -> :infinity
        end
    end
  end

  # ── stream ────────────────────────────────────────────────────────────────

  defp parse_stream(bytes) do
    text = normalise_newlines(bytes)
    t = text |> String.to_charlist() |> List.to_tuple()
    s = %{t: t, n: tuple_size(t), i: 0, line: 1, col: 1, at_content: false, anchors: %{}}

    # A single leading BOM is legal and does not move the first column. Any
    # BOM after it is smuggled in mid-stream. (kyq refused a leading BOM too;
    # fixed upstream, see KNOWN_DIVERGENCES.)
    s = if peek(s) == 0xFEFF, do: %{s | i: 1}, else: s

    case Enum.find(s.i..(s.n - 1)//1, &(elem(t, &1) == 0xFEFF)) do
      nil ->
        :ok

      at ->
        throw_error(
          "BOM_MIDSTREAM",
          "a byte order mark is only allowed at the start of a stream",
          location_of(t, at)
        )
    end

    stream(skip_gap(s), [], false)
  end

  defp stream(s, docs, saw_directive) do
    cond do
      eof?(s) ->
        Enum.reverse(docs)

      at_directive?(s) ->
        if docs != [],
          do:
            throw_error(
              "UNSUPPORTED_DIRECTIVE",
              "a directive is only allowed before the first document",
              loc(s)
            )

        stream(s |> parse_directive() |> skip_gap(), docs, true)

      at_marker?(s, "...") ->
        stream(s |> consume_marker() |> skip_gap(), docs, saw_directive)

      at_marker?(s, "---") ->
        s = s |> consume_marker() |> skip_gap()

        cond do
          # A final `---` with no content: a trailing empty document is dropped.
          eof?(s) ->
            Enum.reverse(docs)

          at_marker?(s, "---") or at_marker?(s, "...") ->
            stream(s, [null_node(loc(s)) | docs], false)

          true ->
            {doc, s} = parse_content_document(s)
            stream(s, [doc | docs], false)
        end

      saw_directive ->
        throw_error(
          "MISSING_DOC_START",
          "a `---` document start marker is required after a directive",
          loc(s)
        )

      docs != [] ->
        throw_error("MISSING_DOC_MARKER", "a second document must start with `---`", loc(s))

      true ->
        {doc, s} = parse_content_document(s)
        stream(s, [doc | docs], false)
    end
  end

  # YAML line breaks are LF, CRLF and CR. CRLF is normalised to LF; a lone CR
  # is refused rather than guessed at.
  defp normalise_newlines(text) do
    if String.contains?(text, "\r") do
      case Regex.run(~r/(^|[^\r])\r(?!\n)/, text, return: :index) do
        [{at, _}, {_, prefix}] ->
          offset = at + prefix
          t = text |> binary_part(0, offset) |> String.to_charlist() |> List.to_tuple()

          throw_error(
            "INVALID_NEWLINE",
            "a bare CR line break is outside the KYAML subset; use LF or CRLF",
            location_of(t, tuple_size(t))
          )

        nil ->
          String.replace(text, "\r\n", "\n")
      end
    else
      text
    end
  end

  # ── cursor primitives ─────────────────────────────────────────────────────

  defp eof?(s), do: s.i >= s.n
  defp peek(s, k \\ 0), do: char_at(s, s.i + k)
  defp char_at(s, j) when j >= 0 and j < s.n, do: elem(s.t, j)
  defp char_at(_, _), do: nil
  defp loc(s), do: {s.line, s.col}

  defp location_of(t, offset) do
    Enum.reduce(0..(offset - 1)//1, {1, 1}, fn j, {line, col} ->
      if elem(t, j) == ?\n, do: {line + 1, 1}, else: {line, col + 1}
    end)
  end

  defp bump(s) do
    if eof?(s) do
      {nil, s}
    else
      c = elem(s.t, s.i)
      s = %{s | i: s.i + 1, at_content: false}
      s = if c == ?\n, do: %{s | line: s.line + 1, col: 1}, else: %{s | col: s.col + 1}
      {c, s}
    end
  end

  defp bump!(s), do: s |> bump() |> elem(1)
  defp bump_n(s, 0), do: s
  defp bump_n(s, n), do: s |> bump!() |> bump_n(n - 1)

  defp rewind(s, {i, line}), do: %{s | i: i, line: line, col: 1, at_content: false}

  defp skip_blanks(s) do
    if peek(s) in [?\s, ?\t], do: s |> bump!() |> skip_blanks(), else: s
  end

  defp skip_spaces(s) do
    if peek(s) == ?\s, do: s |> bump!() |> skip_spaces(), else: s
  end

  defp throw_error(code, message, {line, column}, path \\ "") do
    throw(
      {:kyaml, %Error{code: code, message: message, line: line, column: column, path: path || ""}}
    )
  end

  # JavaScript's \s, which kyq's anchor, alias and key scans use.
  defp js_space?(c),
    do:
      c in [
        ?\s,
        ?\t,
        ?\n,
        ?\v,
        ?\f,
        ?\r,
        0xA0,
        0x1680,
        0x2028,
        0x2029,
        0x202F,
        0x205F,
        0x3000,
        0xFEFF
      ] or (c != nil and c >= 0x2000 and c <= 0x200A)

  defp show(nil), do: "end of input"
  defp show(c), do: inspect(<<c::utf8>>)

  # Skip indentation, comments, blank lines and trailing whitespace. Idempotent:
  # on a content character it returns at once.
  defp skip_gap(s) do
    cond do
      eof?(s) ->
        %{s | at_content: false}

      s.at_content ->
        s

      s.col == 1 ->
        s = skip_spaces(s)

        cond do
          peek(s) == ?\t ->
            throw_error("TAB_INDENT", "a tab may not be used as indentation; use spaces", loc(s))

          eof?(s) ->
            %{s | at_content: false}

          peek(s) == ?\n ->
            s |> bump!() |> skip_gap()

          peek(s) == ?# ->
            s |> skip_to_eol() |> skip_gap()

          true ->
            %{s | at_content: true}
        end

      true ->
        case peek(s) do
          c when c in [?\s, ?\t, ?\n] ->
            s |> bump!() |> skip_gap()

          ?# ->
            s |> skip_to_eol() |> skip_gap()

          c ->
            throw_error(
              "TRAILING_CONTENT",
              "unexpected content #{show(c)} after a node; only a comment may follow",
              loc(s)
            )
        end
    end
  end

  defp skip_to_eol(s) do
    cond do
      eof?(s) -> %{s | at_content: false}
      peek(s) == ?\n -> %{bump!(s) | at_content: false}
      true -> s |> bump!() |> skip_to_eol()
    end
  end

  defp expect_end_of_line(s) do
    s = skip_blanks(s)

    cond do
      peek(s) == ?# ->
        skip_to_eol(s)

      eof?(s) or peek(s) == ?\n ->
        s

      true ->
        throw_error(
          "TRAILING_CONTENT",
          "unexpected content #{show(peek(s))} after a node",
          loc(s)
        )
    end
  end

  defp at_marker?(s, marker), do: s.col == 1 and marker_at?(s, s.i, marker)

  # Three of `.`/`-` at an offset, then a separator. Without a marker, any
  # three (kyq's `markerAt(index)`).
  defp marker_at?(s, j, marker) do
    three = for k <- 0..2, do: char_at(s, j + k)

    Enum.all?(three, &(&1 in [?., ?-])) and
      (marker == nil or List.to_string(three) == marker) and
      char_at(s, j + 3) in [nil, ?\s, ?\t, ?\n, ?#]
  end

  defp consume_marker(s) do
    marker = List.to_string(for k <- 0..2, do: peek(s, k))
    s = s |> bump_n(3) |> skip_blanks()

    cond do
      peek(s) == ?# ->
        skip_to_eol(s)

      not eof?(s) and peek(s) != ?\n ->
        throw_error(
          "DOC_MARKER_CONTENT",
          "content on the same line as the `#{marker}` marker is outside the KYAML subset; put it on the next line",
          loc(s)
        )

      true ->
        s
    end
  end

  defp at_directive?(s), do: s.col == 1 and peek(s) == ?%

  defp parse_directive(s) do
    at = loc(s)
    {chars, s} = take_line(s, [])
    line = List.to_string(chars)
    [name | args] = String.split(String.trim(line), ~r/\s+/u)

    if name == "%YAML" do
      if args != ["1.2"],
        do:
          throw_error(
            "UNSUPPORTED_VERSION",
            "only `%YAML 1.2` is supported, got `#{String.trim(line)}`",
            at
          )

      s
    else
      throw_error("UNSUPPORTED_DIRECTIVE", "directive `#{name}` is outside the KYAML subset", at)
    end
  end

  defp take_line(s, acc) do
    if eof?(s) or peek(s) == ?\n do
      {Enum.reverse(acc), s}
    else
      {c, s} = bump(s)
      take_line(s, [c | acc])
    end
  end

  # ── block structure ───────────────────────────────────────────────────────

  defp parse_content_document(s) do
    {node, s} = parse_block(s, s.col - 1, "")
    s = skip_gap(s)

    if not eof?(s) and not at_marker?(s, "---") and not at_marker?(s, "...") do
      throw_error(
        "UNEXPECTED_CONTENT",
        "unexpected content after the document root; multi-line plain scalars are outside the KYAML subset (quote the value or use a block scalar)",
        loc(s)
      )
    end

    {node, s}
  end

  defp parse_block(s, indent, path, line_indent \\ nil) do
    line_indent = line_indent || indent

    cond do
      eof?(s) -> {null_node(loc(s)), s}
      seq_entry?(s) -> parse_seq(s, indent, path)
      map_entry?(s) -> parse_map(s, indent, path)
      true -> parse_inline_node(s, indent, path, :root, line_indent)
    end
  end

  defp seq_entry?(s), do: peek(s) == ?- and peek(s, 1) in [nil, ?\s, ?\t, ?\n]

  # Whether the cursor starts a block mapping entry. A line opening with a flow
  # indicator, anchor, alias, tag or block scalar indicator is a node: scanning
  # it for a colon would find one inside the collection or the scalar.
  defp map_entry?(s) do
    c = peek(s)

    cond do
      c == nil or c in ~c"{[&*!|>" ->
        false

      c == ?? and (peek(s, 1) || ?\n) in [?\s, ?\t, ?\n] ->
        throw_error(
          "UNSUPPORTED_COMPLEX_KEY",
          "complex mapping keys (`? `) are outside the KYAML subset",
          loc(s)
        )

      c in [?', ?"] ->
        case find_closing_quote(s, c) do
          -1 ->
            false

          close ->
            j = skip_blank_at(s, close + 1)
            char_at(s, j) == ?: and separator_after?(s, j + 1)
        end

      true ->
        scan_key_colon(s, s.i)
    end
  end

  defp skip_blank_at(s, j),
    do: if(char_at(s, j) in [?\s, ?\t], do: skip_blank_at(s, j + 1), else: j)

  defp scan_key_colon(s, j) do
    c = char_at(s, j)

    cond do
      c == nil or c == ?\n -> false
      c == ?# and (j == s.i or js_space?(char_at(s, j - 1))) -> false
      c == ?: and separator_after?(s, j + 1) -> j > s.i
      true -> scan_key_colon(s, j + 1)
    end
  end

  defp separator_after?(s, j), do: char_at(s, j) in [nil, ?\s, ?\t, ?\n]

  defp find_closing_quote(s, q), do: find_closing_quote(s, q, s.i + 1)

  defp find_closing_quote(s, q, j) do
    c = char_at(s, j)

    cond do
      c == nil or c == ?\n -> -1
      q == ?" and c == ?\\ -> find_closing_quote(s, q, j + 2)
      c == q and q == ?' and char_at(s, j + 1) == ?' -> find_closing_quote(s, q, j + 2)
      c == q -> j
      true -> find_closing_quote(s, q, j + 1)
    end
  end

  defp parse_map(s, indent, path), do: map_entries(s, indent, path, loc(s), [], %{})

  defp map_entries(s, indent, path, map_loc, entries, seen) do
    key_loc = loc(s)
    {key, s} = parse_key(s)

    if peek(s) != ?:,
      do: throw_error("MISSING_COLON", "expected `:` after a mapping key", key_loc)

    s = s |> bump!() |> skip_blanks()
    value_path = join_path(path, path_token(key))

    {value, s} =
      if eof?(s) or peek(s) in [?\n, ?#],
        do: parse_nested_value(s, indent, true, value_path),
        else: parse_inline_node(s, indent, value_path, :map_value, indent)

    reject_merge_key(key, key_loc, path)
    seen = check_duplicate(seen, key, key_loc, path)
    entries = [%{key: key, value: value, loc: key_loc} | entries]
    s = skip_gap(s)
    done = {%{kind: :map, entries: Enum.reverse(entries), loc: map_loc}, s}

    cond do
      eof?(s) or at_marker?(s, "---") or at_marker?(s, "...") ->
        done

      s.col - 1 < indent ->
        done

      s.col - 1 > indent ->
        throw_error(
          "UNEXPECTED_INDENT",
          "unexpected indentation (#{s.col - 1} spaces, expected #{indent})",
          loc(s)
        )

      seq_entry?(s) ->
        throw_error(
          "MIXED_COLLECTION",
          "a sequence entry cannot appear at mapping indentation",
          loc(s)
        )

      true ->
        map_entries(s, indent, path, map_loc, entries, seen)
    end
  end

  defp parse_seq(s, indent, path), do: seq_items(s, indent, path, loc(s), [])

  defp seq_items(s, indent, path, seq_loc, items) do
    s = s |> bump!() |> skip_blanks()
    item_path = join_path(path, Integer.to_string(length(items)))

    {item, s} =
      if eof?(s) or peek(s) in [?\n, ?#] do
        parse_nested_value(s, indent, false, item_path)
      else
        item_indent = s.col - 1

        if item_indent <= indent,
          do:
            throw_error(
              "BAD_INDENT",
              "a sequence entry value must be indented past the `-`",
              loc(s)
            )

        parse_block(s, item_indent, item_path, indent)
      end

    items = [item | items]
    s = skip_gap(s)
    done = {%{kind: :seq, items: Enum.reverse(items), loc: seq_loc}, s}

    cond do
      eof?(s) or at_marker?(s, "---") or at_marker?(s, "...") ->
        done

      s.col - 1 < indent ->
        done

      s.col - 1 > indent ->
        throw_error(
          "UNEXPECTED_INDENT",
          "unexpected indentation (#{s.col - 1} spaces, expected #{indent})",
          loc(s)
        )

      # The sequence ended; the parent resumes at the same indentation.
      not seq_entry?(s) ->
        done

      true ->
        seq_items(s, indent, path, seq_loc, items)
    end
  end

  # A value that starts on a following line (after `key:` or `-`). A mapping
  # value's sequence may sit at the key's own indentation.
  defp parse_nested_value(s, parent_indent, allow_same_indent_seq, path) do
    s = skip_gap(s)

    cond do
      eof?(s) or at_marker?(s, "---") or at_marker?(s, "...") ->
        {null_node(loc(s)), s}

      s.col - 1 > parent_indent ->
        parse_block(s, s.col - 1, path)

      allow_same_indent_seq and s.col - 1 == parent_indent and seq_entry?(s) ->
        parse_seq(s, parent_indent, path)

      true ->
        {null_node(loc(s)), s}
    end
  end

  # ── scalars, flow collections, anchors and tags ───────────────────────────

  defp parse_key(s) do
    at = loc(s)

    if peek(s) in [?', ?"] do
      {node, s} = parse_quoted(s, at)
      {node, skip_blanks(s)}
    else
      {raw, s} = plain_key_chars(s, [])
      raw = trim_blanks_right(raw)
      if raw == "", do: throw_error("EMPTY_KEY", "empty mapping key", at)
      {scalar_from_raw(raw, at, :plain), s}
    end
  end

  defp trim_blanks_right(text), do: String.replace(text, ~r/[ \t]+\z/, "")

  defp plain_key_chars(s, acc) do
    c = peek(s)

    cond do
      eof?(s) -> {List.to_string(Enum.reverse(acc)), s}
      c == ?: and separator_after?(s, s.i + 1) -> {List.to_string(Enum.reverse(acc)), s}
      c == ?\n -> {List.to_string(Enum.reverse(acc)), s}
      c == ?# and match?([?\s | _], acc) -> {List.to_string(Enum.reverse(acc)), s}
      true -> plain_key_chars(bump!(s), [c | acc])
    end
  end

  # A node that may carry an anchor and a tag: a scalar, a flow collection, a
  # block scalar, an alias, or (on the next lines) a block collection.
  defp parse_inline_node(s, indent, path, context, line_indent) do
    at = loc(s)
    {anchor, tag, s} = node_properties(s, at, nil, nil, &skip_blanks/1)

    if peek(s) == ?* do
      if anchor != nil or tag != nil,
        do: throw_error("ALIAS_DECORATED", "an alias may not carry an anchor or a tag", at)

      {node, s} = parse_alias(s, at, path)
      {node, expect_end_of_line(s)}
    else
      {node, s} =
        cond do
          eof?(s) or peek(s) in [?\n, ?#] ->
            # A bare anchor/tag with its value on the following line(s). At the
            # document root the value may sit at the same indentation.
            if context == :root do
              s = skip_gap(s)

              if eof?(s) or at_marker?(s, "---") or at_marker?(s, "..."),
                do: {null_node(loc(s)), s},
                else: parse_block(s, s.col - 1, path)
            else
              parse_nested_value(s, indent, context == :map_value, path)
            end

          # Content must be indented past the line that carries the indicator.
          peek(s) in [?|, ?>] ->
            parse_block_scalar(s, line_indent, at)

          peek(s) in [?{, ?[] ->
            {node, s} = parse_flow_node(s, path)
            {node, expect_end_of_line(s)}

          peek(s) in [?', ?"] ->
            {node, s} = parse_quoted(s, at)
            {node, expect_end_of_line(s)}

          true ->
            {node, s} = parse_plain_block_scalar(s, path, context)
            {node, expect_end_of_line(s)}
        end

      node = if tag, do: apply_tag(node, tag, at, path), else: node
      s = if anchor, do: %{s | anchors: Map.put(s.anchors, anchor, node)}, else: s
      {node, s}
    end
  end

  defp node_properties(s, at, anchor, tag, skip) do
    case peek(s) do
      ?& ->
        s = bump!(s)
        {name, s} = take_name(s)
        if name == "", do: throw_error("BAD_ANCHOR", "anchor name is missing", at)
        node_properties(skip.(s), at, name, tag, skip)

      ?! ->
        {tag, s} = parse_tag(s)
        node_properties(skip.(s), at, anchor, tag, skip)

      _ ->
        {anchor, tag, s}
    end
  end

  # An anchor or alias name: kyq's /[^\s,[\]{}]+/.
  defp take_name(s, acc \\ []) do
    c = peek(s)

    if c == nil or js_space?(c) or c in ~c",[]{}",
      do: {List.to_string(Enum.reverse(acc)), s},
      else: take_name(bump!(s), [c | acc])
  end

  defp parse_alias(s, at, path) do
    s = bump!(s)
    {name, s} = take_name(s)
    if name == "", do: throw_error("BAD_ALIAS", "alias name is missing", at)

    case Map.fetch(s.anchors, name) do
      {:ok, target} ->
        {%{kind: :alias, name: name, loc: at, target: target}, s}

      :error ->
        throw_error(
          "UNKNOWN_ANCHOR",
          "alias `*#{name}` names no anchor defined earlier in the stream",
          at,
          path
        )
    end
  end

  defp parse_tag(s) do
    at = loc(s)
    {_, s} = bump(s)
    {tag, s} = if peek(s) == ?!, do: {"!!", bump!(s)}, else: {"!", s}
    {rest, s} = take_tag(s, [])
    tag = tag <> rest

    cond do
      tag == "!" ->
        {tag, s}

      tag in @core_tags ->
        {tag, s}

      true ->
        throw_error(
          "UNSUPPORTED_TAG",
          "tag `#{tag}` is outside the KYAML subset (only the YAML 1.2 core tags and `!` are accepted)",
          at
        )
    end
  end

  defp take_tag(s, acc) do
    c = peek(s)

    if c == nil or js_space?(c) or c in ~c",[]{}",
      do: {List.to_string(Enum.reverse(acc)), s},
      else: take_tag(bump!(s), [c | acc])
  end

  defp scalar?(%{kind: kind}), do: kind not in [:map, :seq, :alias]

  defp apply_tag(node, "!", _at, _path) do
    if scalar?(node) and node.style == :plain,
      do: %{node | kind: :string, value: node.raw},
      else: node
  end

  defp apply_tag(node, tag, at, path) when tag in ["!!map", "!!seq"] do
    want = if tag == "!!map", do: :map, else: :seq

    if node.kind != want,
      do:
        throw_error("TAG_KIND_MISMATCH", "tag `#{tag}` applied to a #{node.kind} node", at, path)

    node
  end

  defp apply_tag(node, tag, at, path) do
    if not scalar?(node),
      do:
        throw_error("TAG_KIND_MISMATCH", "tag `#{tag}` applied to a #{node.kind} node", at, path)

    if tag == "!!str" do
      %{node | kind: :string, value: node.raw}
    else
      {kind, value} = resolve_plain(node.raw)
      expected = String.to_atom(binary_part(tag, 2, byte_size(tag) - 2))

      if kind != expected,
        do:
          throw_error(
            "TAG_VALUE_MISMATCH",
            "tag `#{tag}` cannot be applied to #{inspect(node.raw)} (resolves to #{kind})",
            at,
            path
          )

      %{node | kind: kind, value: value}
    end
  end

  defp parse_plain_block_scalar(s, path, context) do
    at = loc(s)
    {raw, s} = plain_value_chars(s, [], path, context)
    raw = trim_blanks_right(raw)
    if raw == "", do: throw_error("EMPTY_VALUE", "empty scalar in value position", at, path)
    {scalar_from_raw(raw, at, :plain), s}
  end

  defp plain_value_chars(s, acc, path, context) do
    c = peek(s)
    ends_space = match?([?\s | _], acc)

    cond do
      eof?(s) or c == ?\n ->
        {List.to_string(Enum.reverse(acc)), s}

      c == ?# and (acc == [] or ends_space) ->
        {List.to_string(Enum.reverse(acc)), s}

      c == ?: and separator_after?(s, s.i + 1) and acc != [] and context != :root ->
        throw_error(
          "MAPPING_IN_VALUE",
          "a nested mapping cannot start on the same line as its parent value; put it on the next line",
          loc(s),
          path
        )

      c == ?: and separator_after?(s, s.i + 1) and acc == [] ->
        throw_error(
          "MAPPING_IN_VALUE",
          "a nested mapping cannot start on the same line as its parent value",
          loc(s),
          path
        )

      true ->
        plain_value_chars(bump!(s), [c | acc], path, context)
    end
  end

  defp scalar_from_raw(raw, at, style) do
    {kind, value} = resolve_plain(raw)
    if kind == :string, do: assert_ijson_string(value, at)
    %{kind: kind, value: value, raw: raw, loc: at, style: style, segments: []}
  end

  defp parse_quoted(s, at) do
    {quote, s} = bump(s)
    {out, s} = quoted_chars(s, quote, at, [])
    out = List.to_string(out)
    assert_ijson_string(out, at)
    {%{kind: :string, value: out, raw: out, loc: at, style: :quoted, segments: []}, s}
  end

  defp quoted_chars(s, quote, at, acc) do
    if eof?(s) or peek(s) == ?\n do
      which = if quote == ?', do: "single", else: "double"

      throw_error(
        "UNTERMINATED_QUOTE",
        "unterminated #{which}-quoted scalar (multi-line quoted scalars are outside the KYAML subset)",
        at
      )
    end

    {c, s} = bump(s)

    cond do
      c == quote and quote == ?' and peek(s) == ?' ->
        quoted_chars(bump!(s), quote, at, [?' | acc])

      c == quote ->
        {Enum.reverse(acc), s}

      quote == ?" and c == ?\\ ->
        {cp, s} = parse_escape(s)
        quoted_chars(s, quote, at, [cp | acc])

      c < 0x20 and c != 0x09 ->
        hex = c |> Integer.to_string(16) |> String.pad_leading(4, "0")
        throw_error("CONTROL_CHARACTER", "raw control character U+#{hex} in a quoted scalar", at)

      true ->
        quoted_chars(s, quote, at, [c | acc])
    end
  end

  @simple_escapes %{
    ?0 => 0,
    ?a => 7,
    ?b => 8,
    ?t => 9,
    ?\t => 9,
    ?n => 10,
    ?v => 11,
    ?f => 12,
    ?r => 13,
    ?e => 27,
    ?\s => 32,
    ?" => 34,
    ?/ => 47,
    ?\\ => 92,
    ?N => 0x85,
    ?_ => 0xA0,
    ?L => 0x2028,
    ?P => 0x2029
  }

  defp parse_escape(s) do
    at = loc(s)
    {escape, s} = bump(s)

    case Map.fetch(@simple_escapes, escape) do
      {:ok, cp} ->
        {cp, s}

      :error ->
        width =
          case escape do
            ?x -> 2
            ?u -> 4
            ?U -> 8
            _ -> 0
          end

        if width == 0,
          do:
            throw_error(
              "BAD_ESCAPE",
              "unknown escape sequence \\#{if escape, do: <<escape::utf8>>, else: ""}",
              at
            )

        {hex, s} = Enum.map_reduce(1..width, s, fn _, s -> bump(s) end)
        hex = hex |> Enum.reject(&is_nil/1) |> List.to_string()

        if not Regex.match?(~r/\A[0-9a-fA-F]{#{width}}\z/, hex),
          do: throw_error("BAD_ESCAPE", "invalid escape sequence \\#{<<escape::utf8>>}#{hex}", at)

        cp = String.to_integer(hex, 16)

        cond do
          cp >= 0xD800 and cp <= 0xDFFF ->
            throw_error(
              "LONE_SURROGATE",
              "escape \\#{<<escape::utf8>>}#{hex} is a lone surrogate; JSON strings must not contain one",
              at
            )

          cp > 0x10FFFF ->
            throw_error("BAD_ESCAPE", "escape \\#{<<escape::utf8>>}#{hex} is above U+10FFFF", at)

          true ->
            {cp, s}
        end
    end
  end

  # I-JSON (RFC 7493 §2.1) as ijson-jcs applies it: no noncharacters. (A
  # lone surrogate cannot reach here: Elixir strings are valid UTF-8, and the
  # escape parser refuses one.)
  defp assert_ijson_string(value, at) do
    value
    |> String.to_charlist()
    |> Enum.with_index()
    |> Enum.each(fn {cp, index} ->
      if (cp >= 0xFDD0 and cp <= 0xFDEF) or Bitwise.band(cp, 0xFFFF) >= 0xFFFE do
        hex = cp |> Integer.to_string(16) |> String.pad_leading(4, "0")

        throw_error(
          "NONCHARACTER",
          "String contains noncharacter: U+#{hex} at index #{index}",
          at
        )
      end
    end)
  end

  # A `|` or `>` block scalar, with chomping and indentation indicators.
  defp parse_block_scalar(s, parent_indent, at) do
    {indicator, s} = bump(s)
    style = if indicator == ?|, do: :literal, else: :folded

    {chomp, s} =
      case peek(s) do
        ?+ -> {:keep, bump!(s)}
        ?- -> {:strip, bump!(s)}
        _ -> {:clip, s}
      end

    {explicit, s} = if peek(s) in ?1..?9, do: {peek(s) - ?0, bump!(s)}, else: {nil, s}

    s = skip_blanks(s)

    s =
      cond do
        # The comment runs to, and consumes, the header line break. (kyq
        # consumed one more character; fixed upstream, see KNOWN_DIVERGENCES.)
        peek(s) == ?# ->
          skip_to_eol(s)

        not eof?(s) and peek(s) != ?\n ->
          throw_error("BAD_BLOCK_SCALAR", "unexpected character in a block scalar header", at)

        not eof?(s) ->
          bump!(s)

        true ->
          s
      end

    content_indent = if explicit, do: parent_indent + explicit, else: nil
    {lines, s} = block_lines(s, parent_indent, content_indent, [])

    # Trailing blank lines are chomping's to keep or drop.
    {trailing, lines} = lines |> Enum.reverse() |> Enum.split_while(& &1.blank)
    lines = Enum.reverse(lines)
    {body, segments} = if style == :literal, do: literal_body(lines), else: folded_body(lines)

    body =
      case chomp do
        :strip -> String.replace(body, ~r/\n+\z/, "")
        :clip -> String.replace(body, ~r/\n+\z/, "") <> if(lines != [], do: "\n", else: "")
        :keep -> body <> String.duplicate("\n", length(trailing))
      end

    assert_ijson_string(body, at)
    {%{kind: :string, value: body, raw: body, loc: at, style: :block, segments: segments}, s}
  end

  defp block_lines(s, parent_indent, content_indent, acc) do
    cond do
      eof?(s) ->
        {Enum.reverse(acc), s}

      s.col != 1 ->
        throw_error("BAD_BLOCK_SCALAR", "block scalar content must start on a new line", loc(s))

      marker_at?(s, s.i, nil) ->
        {Enum.reverse(acc), s}

      true ->
        start = {s.i, s.line}
        s = skip_spaces(s)
        spaces = s.i - elem(start, 0)

        if peek(s) == ?\t and content_indent != nil and spaces < content_indent,
          do:
            throw_error("TAB_INDENT", "a tab may not be used as block scalar indentation", loc(s))

        cond do
          peek(s) == ?\n or eof?(s) ->
            s = if eof?(s), do: s, else: bump!(s)

            block_lines(s, parent_indent, content_indent, [
              %{text: "", more: false, blank: true, line: elem(start, 1)} | acc
            ])

          content_indent == nil and spaces <= parent_indent ->
            {Enum.reverse(acc), rewind(s, start)}

          spaces < (content_indent || spaces) ->
            {Enum.reverse(acc), rewind(s, start)}

          true ->
            content_indent = content_indent || spaces
            {chars, s} = take_line(s, [])
            s = if eof?(s), do: s, else: bump!(s)
            text = String.duplicate(" ", spaces - content_indent) <> List.to_string(chars)

            line = %{
              text: text,
              more: spaces > content_indent,
              blank: false,
              line: elem(start, 1)
            }

            block_lines(s, parent_indent, content_indent, [line | acc])
        end
    end
  end

  defp literal_body(lines) do
    {parts, segments, _} =
      Enum.reduce(lines, {[], [], 0}, fn line, {parts, segments, offset} ->
        offset = if parts == [], do: offset, else: offset + 1
        segments = if line.blank, do: segments, else: [{offset, line.line} | segments]
        {[line.text | parts], segments, offset + byte_size(line.text)}
      end)

    body = parts |> Enum.reverse() |> Enum.join("\n")
    {if(lines != [], do: body <> "\n", else: body), Enum.reverse(segments)}
  end

  # Folded style (YAML 1.2 §8.1.3): a break between two ordinary text lines is
  # a space; breaks next to a blank or more-indented line are kept, and N
  # consecutive breaks become N−1 newlines.
  defp folded_body(lines) do
    {body, segments, _pending, first, _prev_more} =
      Enum.reduce(lines, {"", [], 0, true, false}, fn line,
                                                      {body, segments, pending, first, prev_more} ->
        cond do
          line.blank ->
            {body, segments, pending + 1, first, prev_more}

          true ->
            sep =
              cond do
                first -> String.duplicate("\n", pending)
                line.more or prev_more -> String.duplicate("\n", pending + 1)
                pending > 0 -> String.duplicate("\n", pending)
                true -> " "
              end

            body = body <> sep
            {body <> line.text, [{byte_size(body), line.line} | segments], 0, false, line.more}
        end
      end)

    body = if lines != [] and not first, do: body <> "\n", else: body
    {body, Enum.reverse(segments)}
  end

  # A flow collection, which may span lines.
  defp parse_flow_node(s, path) do
    at = loc(s)

    if peek(s) == ?[ do
      flow_seq(bump!(s), path, at, [])
    else
      flow_map(bump!(s), path, at, [], %{})
    end
  end

  defp flow_seq(s, path, at, items) do
    s = skip_flow_gap(s)

    cond do
      peek(s) == ?] ->
        {%{kind: :seq, items: Enum.reverse(items), loc: at}, bump!(s)}

      eof?(s) ->
        throw_error("UNTERMINATED_FLOW", "unterminated flow sequence", at, path)

      true ->
        {item, s} = parse_flow_item(s, join_path(path, Integer.to_string(length(items))))
        items = [item | items]
        s = skip_flow_gap(s)

        cond do
          # YAML 1.2 allows a final `,`, and KYAML (KEP-5295) writes one after
          # every entry. (kyq refuses it; see KNOWN_DIVERGENCES.)
          peek(s) == ?, ->
            flow_seq(s |> bump!() |> skip_flow_gap(), path, at, items)

          peek(s) == ?] ->
            {%{kind: :seq, items: Enum.reverse(items), loc: at}, bump!(s)}

          eof?(s) ->
            throw_error("UNTERMINATED_FLOW", "unterminated flow sequence", at, path)

          true ->
            throw_error(
              "FLOW_SEPARATOR",
              "expected `,` or `]` in a flow sequence, found #{show(peek(s))}",
              loc(s),
              path
            )
        end
    end
  end

  defp flow_map(s, path, at, entries, seen) do
    s = skip_flow_gap(s)

    cond do
      peek(s) == ?} ->
        {%{kind: :map, entries: Enum.reverse(entries), loc: at}, bump!(s)}

      eof?(s) ->
        throw_error("UNTERMINATED_FLOW", "unterminated flow mapping", at, path)

      true ->
        key_loc = loc(s)
        {key, s} = parse_flow_key(s)
        s = skip_flow_gap(s)

        if peek(s) != ?:,
          do: throw_error("FLOW_SEPARATOR", "expected `:` after a flow mapping key", loc(s), path)

        {value, s} = parse_flow_item(bump!(s), join_path(path, path_token(key)))
        reject_merge_key(key, key_loc, path)
        seen = check_duplicate(seen, key, key_loc, path)
        entries = [%{key: key, value: value, loc: key_loc} | entries]
        s = skip_flow_gap(s)

        cond do
          peek(s) == ?, ->
            flow_map(s |> bump!() |> skip_flow_gap(), path, at, entries, seen)

          peek(s) == ?} ->
            {%{kind: :map, entries: Enum.reverse(entries), loc: at}, bump!(s)}

          eof?(s) ->
            throw_error("UNTERMINATED_FLOW", "unterminated flow mapping", at, path)

          true ->
            throw_error(
              "FLOW_SEPARATOR",
              "expected `,` or `}` in a flow mapping, found #{show(peek(s))}",
              loc(s),
              path
            )
        end
    end
  end

  defp skip_flow_gap(s) do
    case peek(s) do
      c when c in [?\s, ?\t, ?\n] -> s |> bump!() |> skip_flow_gap()
      ?# -> s |> skip_to_eol() |> skip_flow_gap()
      _ -> s
    end
  end

  defp parse_flow_item(s, path) do
    s = skip_flow_gap(s)
    at = loc(s)
    {anchor, tag, s} = node_properties(s, at, nil, nil, &skip_flow_gap/1)

    # kyq does not refuse a decorated alias inside a flow collection.
    if peek(s) == ?* do
      parse_alias(s, at, path)
    else
      {node, s} =
        cond do
          peek(s) in [?[, ?{] ->
            parse_flow_node(s, path)

          peek(s) in [?', ?"] ->
            parse_quoted(s, at)

          eof?(s) or peek(s) in ~c",]}\n" ->
            {null_node(at), s}

          true ->
            {raw, s} = flow_plain_chars(s, [])
            raw = trim_blanks_right(raw)
            if raw == "", do: throw_error("EMPTY_VALUE", "empty flow scalar", at, path)
            {scalar_from_raw(raw, at, :plain), s}
        end

      node = if tag, do: apply_tag(node, tag, at, path), else: node
      s = if anchor, do: %{s | anchors: Map.put(s.anchors, anchor, node)}, else: s
      {node, s}
    end
  end

  defp flow_plain_chars(s, acc) do
    c = peek(s)

    cond do
      eof?(s) or c in ~c",]}\n" -> {List.to_string(Enum.reverse(acc)), s}
      c == ?: and separator_after?(s, s.i + 1) -> {List.to_string(Enum.reverse(acc)), s}
      c == ?# and (acc == [] or match?([?\s | _], acc)) -> {List.to_string(Enum.reverse(acc)), s}
      true -> flow_plain_chars(bump!(s), [c | acc])
    end
  end

  defp parse_flow_key(s) do
    at = loc(s)

    if peek(s) in [?', ?"] do
      parse_quoted(s, at)
    else
      {raw, s} = flow_key_chars(s, [])
      raw = String.trim(raw)
      if raw == "", do: throw_error("EMPTY_KEY", "empty flow mapping key", at)
      {scalar_from_raw(raw, at, :plain), s}
    end
  end

  defp flow_key_chars(s, acc) do
    c = peek(s)

    if eof?(s) or c == ?: or c in ~c",]}\n",
      do: {List.to_string(Enum.reverse(acc)), s},
      else: flow_key_chars(bump!(s), [c | acc])
  end

  # ── small helpers ─────────────────────────────────────────────────────────

  # `<<` is not in the YAML 1.2 Core Schema, and GitHub Actions refuses it
  # too. Read as an ordinary key it would silently change the document's
  # meaning for every consumer that expects merge semantics.
  defp reject_merge_key(%{kind: :string, value: "<<"}, at, path),
    do:
      throw_error(
        "UNSUPPORTED_MERGE_KEY",
        "the YAML merge key `<<` is outside the KYAML subset; spell the merged keys out",
        at,
        path
      )

  defp reject_merge_key(_, _, _), do: :ok

  defp check_duplicate(seen, key, key_loc, path) do
    identity = key_identity(key)

    case Map.fetch(seen, identity) do
      {:ok, {line, column}} ->
        throw_error(
          "DUPLICATE_KEY",
          "duplicate mapping key #{inspect(key_text(key))}; first defined at line #{line}, column #{column}",
          key_loc,
          join_path(path, path_token(key))
        )

      :error ->
        Map.put(seen, identity, key_loc)
    end
  end

  # Keys compare after resolution: `a`, `"a"` and `!!str a` are one key; the
  # integer 1 and the string "1" are not.
  defp key_identity(%{kind: :float, value: v}) when is_float(v) and v == 0.0, do: {:float, 0.0}
  defp key_identity(%{kind: kind, value: value}), do: {kind, value}

  defp path_token(%{kind: :string, value: v}), do: v
  defp path_token(%{kind: :null}), do: "null"
  defp path_token(%{raw: raw}), do: raw

  defp key_text(%{kind: :string, value: v}), do: v
  defp key_text(%{raw: raw}), do: raw

  defp join_path(parent, token) do
    encoded = token |> String.replace("~", "~0") |> String.replace("/", "~1")
    if parent == "", do: encoded, else: parent <> "/" <> encoded
  end

  defp null_node(at),
    do: %{kind: :null, value: nil, raw: "", loc: at, style: :plain, segments: []}
end
