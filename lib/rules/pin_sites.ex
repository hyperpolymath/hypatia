# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.PinSites do
  @moduledoc """
  Structural `uses:` pin sites on one line of workflow YAML.

  One reader for every rule that judges a pin by the line it sits on —
  PI001/PI002 (`PinIntegrity`) over a file, PA001-PA004 (`PrAutomerge`) over
  a diff — so a pin shape one of them learns, the others see too.

  A site is a `uses:` key the YAML structure makes a pin:

    * the line's own leading `uses:` key (`uses:`, `- uses:`, `- { uses:`,
      quoted or not);
    * an entry of a flow mapping that is a step or a job — an anonymous
      `{ ... }` at the line's level or in a `[` that `steps:` (or nothing on
      the line) opens, or a mapping a job key opens inside `jobs: { ... }`.

  Text is not a site: a pin inside a quoted `name:` or `run:` scalar, after
  `#`, inside an input's mapping (`with: { uses: ... }`), or after free text
  (`echo { uses: ... }` in a script).

  Every position this module returns is a byte offset into the line as
  given: the mask keeps the line's bytes where they were.
  """

  # One `uses:` site anywhere on a line, bounded on the left so `reuses:` is
  # not one. Matched against the MASKED line.
  @site_re ~r/(?:^|[^A-Za-z0-9_.\/-])["']?uses["']?\s*:\s*["']?(?<action>[A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@(?<ref>[^\s#"',}\]]+)/

  @uses_tail_re ~r/(?:^|[-{,\s])["']?uses["']?\s*:\s*$/

  @doc """
  The structural sites of `line`, in order, as
  `%{action: a, ref: r, action_at: byte, ref_at: byte}`.
  """
  @spec sites(String.t()) :: [map()]
  def sites(line) when is_binary(line) do
    masked = mask(line)

    masked
    |> scan()
    |> Enum.map(fn [_full, {a_at, a_len}, {r_at, r_len}] ->
      %{
        action: binary_part(masked, a_at, a_len),
        ref: binary_part(masked, r_at, r_len),
        action_at: a_at,
        ref_at: r_at
      }
    end)
  end

  @doc """
  The structural site matches of an already masked line, each as
  `[full, action, ref]` `{byte, length}` pairs (`Regex.scan/3` with
  `return: :index`).
  """
  @spec scan(String.t()) :: [[{non_neg_integer(), non_neg_integer()}]]
  def scan(masked) when is_binary(masked) do
    @site_re
    |> Regex.scan(masked, return: :index, capture: :all)
    |> Enum.filter(fn [_full, {action_at, _} | _] ->
      structural?(binary_part(masked, 0, action_at))
    end)
  end

  @doc """
  The line cut at its comment, with every quoted scalar that is neither a
  key nor the value of a `uses` key blanked to `x` byte for byte. A `uses`
  value and a quoted key are kept as written, so text read from the masked
  line at a site is the pin's own text, and an offset into it is an offset
  into the line.
  """
  @spec mask(String.t()) :: String.t()
  def mask(line) when is_binary(line), do: do_mask(line, "")

  @doc """
  Is the line nothing but pin sites? Every structural site removed (with its
  value's closing quote), what is left of the masked line may only be
  indentation, list dashes, flow brackets and commas — so `- uses: a/b@x`,
  `{ uses: "a/b@x" },` and `[ { uses: a/b@x }, { uses: c/d@y } ]` are pin
  lines, and `steps: [ { uses: a/b@x } ]`, `- { name: x, uses: a/b@x }`
  and `uses: a/b@x extra` are not. The comment is never part of it.
  """
  @spec pin_line?(String.t()) :: boolean()
  def pin_line?(line) when is_binary(line) do
    masked = mask(line)
    sites = scan(masked)

    remaining =
      sites
      |> Enum.reverse()
      |> Enum.reduce(masked, fn [{at, len}, {action_at, _}, _], rest ->
        # The site match includes an opening value quote but ends at the ref.
        # Remove its matching closing quote along with the pin.
        quote = binary_part(masked, action_at - 1, 1)
        closing = binary_part(rest, at + len, min(1, byte_size(rest) - at - len))
        len = if quote in ["\"", "'"] and closing == quote, do: len + 1, else: len
        binary_part(rest, 0, at) <> binary_part(rest, at + len, byte_size(rest) - at - len)
      end)

    sites != [] and Regex.match?(~r/^[\s\-\[\]{},]*$/, remaining)
  end

  @doc """
  The text after the line's comment `#`, as written (untrimmed), or `""`.
  The `#` is found outside quoted scalars, so `name: "C# build"` has none.
  """
  @spec comment(String.t()) :: String.t()
  def comment(line) when is_binary(line) do
    cut = byte_size(mask(line))

    if cut < byte_size(line),
      do: binary_part(line, cut + 1, byte_size(line) - cut - 1),
      else: ""
  end

  @doc "The line up to (not including) its comment `#`."
  @spec head(String.t()) :: String.t()
  def head(line) when is_binary(line), do: binary_part(line, 0, byte_size(mask(line)))

  @doc """
  The lines of a whole file, each with the bytes that belong to a scalar
  opened on an earlier line blanked to `x`, byte for byte: every content
  line of a block scalar (`run: |`), and a multi-line quoted scalar's
  continuation up to and including its closing quote. A line read through
  `sites/1` one at a time cannot see either, so a `uses:` written in a
  `run: |` script reads as a key; read from these lines it does not.

  A quote opens a scalar only where YAML lets one start: after `key: `, a
  list dash, or, inside a flow collection, `[`, `{` or `,`; at the start of
  a line only when the line above left a value pending (`key:`). `echo
  it's` stays a plain scalar. A quoted scalar still open at the end of the
  file was misread, and the lines after its opening quote are returned as
  written, each judged on its own as before: a misread quote never hides a
  pin.
  """
  @spec mask_lines(String.t()) :: [String.t()]
  def mask_lines(content) when is_binary(content) do
    lines = String.split(content, "\n")

    {masked, st} =
      lines
      |> Enum.with_index()
      |> Enum.map_reduce(%{q: nil, opened: nil, block: nil, depth: 0, pending: false}, fn
        {line, n}, st -> mask_file_line(line, n, st)
      end)

    if st.q,
      do: Enum.take(masked, st.opened + 1) ++ Enum.drop(lines, st.opened + 1),
      else: masked
  end

  # Inside a block scalar: a blank line or one deeper than the key is its
  # content; anything else ends it.
  defp mask_file_line(line, n, %{block: col} = st) when is_integer(col) do
    if String.trim(line) == "" or indent_of(line) > col,
      do: {String.duplicate("x", byte_size(line)), st},
      else: mask_file_line(line, n, %{st | block: nil})
  end

  defp mask_file_line(line, n, st) do
    {cont, st} =
      case st.q && close_at(line, st.q, 0) do
        nil -> {0, st}
        :open -> {byte_size(line), st}
        at -> {at + 1, %{st | q: nil, opened: nil}}
      end

    rest = binary_part(line, cont, byte_size(line) - cont)
    view = String.duplicate("x", cont) <> rest
    st = if st.q, do: st, else: walk(rest, String.duplicate("x", cont), n, st)
    {view, st}
  end

  # The byte offset of the quote that closes a `q` scalar in `line` from
  # `at`, or :open. `\` escapes inside double quotes; `''` is a literal `'`.
  defp close_at(line, _q, at) when at >= byte_size(line), do: :open

  defp close_at(line, ?", at) do
    case :binary.at(line, at) do
      ?\\ -> close_at(line, ?", at + 2)
      ?" -> at
      _ -> close_at(line, ?", at + 1)
    end
  end

  defp close_at(line, ?', at) do
    cond do
      :binary.at(line, at) != ?' -> close_at(line, ?', at + 1)
      at + 1 < byte_size(line) and :binary.at(line, at + 1) == ?' -> close_at(line, ?', at + 2)
      true -> at
    end
  end

  # One line outside any scalar, `seen` being what precedes `rest` on it
  # (quote bodies as `x`). Tracks flow depth, opens a quote that does not
  # close on the line, and at its end a block scalar or a pending value.
  defp walk(<<>>, seen, _n, st) do
    trimmed = String.trim_trailing(seen)

    cond do
      String.trim(seen) == "" ->
        st

      st.depth == 0 and
          Regex.match?(~r/(?:^\s*(?:-\s+)*|:\s+)(?:[&!]\S*\s+)*[|>][-+0-9]*$/, trimmed) ->
        %{st | block: block_parent(seen), pending: false}

      true ->
        pending =
          Regex.match?(~r/(?::|^\s*-)$/, trimmed) or
            (st.depth > 0 and Regex.match?(~r/[\[{,]$/, trimmed))

        %{st | pending: pending}
    end
  end

  defp walk(<<?#, _::binary>>, seen, n, st)
       when seen == "" or binary_part(seen, byte_size(seen) - 1, 1) in [" ", "\t"],
       do: walk(<<>>, seen, n, st)

  defp walk(<<q, tail::binary>>, seen, n, st) when q in [?", ?'] do
    if scalar_start?(seen, st) do
      case close_at(tail, q, 0) do
        :open ->
          %{st | q: q, opened: n}

        at ->
          rest = binary_part(tail, at + 1, byte_size(tail) - at - 1)
          walk(rest, seen <> String.duplicate("x", at + 2), n, st)
      end
    else
      walk(tail, seen <> <<q>>, n, st)
    end
  end

  defp walk(<<c, tail::binary>>, seen, n, st) when c in [?[, ?{] do
    st = if st.depth > 0 or value_start?(seen), do: %{st | depth: st.depth + 1}, else: st
    walk(tail, seen <> <<c>>, n, st)
  end

  defp walk(<<c, tail::binary>>, seen, n, %{depth: d} = st) when c in [?], ?}] and d > 0,
    do: walk(tail, seen <> <<c>>, n, %{st | depth: d - 1})

  defp walk(<<c, tail::binary>>, seen, n, st), do: walk(tail, seen <> <<c>>, n, st)

  # Where a value starts: after `key: ` or a list dash, past any anchor or
  # tag (`key: &a [`, `- !!str "`).
  defp value_start?(seen), do: Regex.match?(~r/(?:^\s*(?:-\s+)*|:\s+)(?:[&!]\S*\s+)*$/, seen)

  defp scalar_start?(seen, st) do
    cond do
      Regex.match?(~r/^\s*$/, seen) -> st.pending or st.depth > 0
      Regex.match?(~r/(?:^\s*(?:-\s+)+|:\s+)(?:[&!]\S*\s+)*$/, seen) -> true
      st.depth > 0 -> Regex.match?(~r/[\[{,]\s*$/, seen)
      true -> false
    end
  end

  # A block scalar's content is deeper than its key (`run: |`, `- run: |`)
  # or, for a bare sequence entry (`- |`), deeper than the dash.
  defp block_parent(seen) do
    if Regex.match?(~r/^\s*-\s+(?:[&!]\S*\s+)*[|>]/, seen),
      do: indent_of(seen),
      else: byte_size(hd(Regex.run(~r/^\s*(?:-\s+)*/, seen)))
  end

  defp indent_of(line), do: byte_size(line) - byte_size(String.trim_leading(line, " "))

  # `prefix` is the masked line up to the site's action.
  defp structural?(prefix) do
    cond do
      Regex.match?(~r/^\s*(?:-\s+)?(?:\{\s*)?["']?uses["']?\s*:\s*["']?$/, prefix) ->
        true

      Regex.match?(~r/[{,]\s*["']?uses["']?\s*:\s*["']?$/, prefix) ->
        prefix |> flow_stack() |> structural_flow_stack?()

      true ->
        false
    end
  end

  # The brackets still open in `text`, innermost first, each with what opens
  # it: the key before it (`steps: [`), :none (nothing but indentation or a
  # list dash), or :text (anything else — `echo {` is a script, not YAML).
  defp flow_stack(text) do
    text
    |> String.graphemes()
    |> Enum.reduce({[], ""}, fn
      c, {stack, seg} when c in ["{", "["] -> {[{c, opener_key(seg)} | stack], ""}
      c, {stack, _seg} when c in ["}", "]"] -> {Enum.drop(stack, 1), ""}
      ",", {stack, _seg} -> {stack, ""}
      c, {stack, seg} -> {stack, seg <> c}
    end)
    |> elem(0)
  end

  defp opener_key(seg) do
    cond do
      Regex.match?(~r/^\s*(?:-\s*)?$/, seg) -> :none
      m = Regex.run(~r/^\s*(?:-\s+)?["']?([\w.-]+)["']?\s*:\s*$/, seg) -> Enum.at(m, 1)
      true -> :text
    end
  end

  defp structural_flow_stack?(stack) do
    Enum.all?(stack, fn {_, key} -> key != :text end) and
      case stack do
        [{"{", :none}] -> true
        [{"{", :none}, {"[", key} | _] when key in ["steps", :none] -> true
        [{"{", job}, {"{", "jobs"} | _] when is_binary(job) -> true
        _ -> false
      end
  end

  defp do_mask("", out), do: out

  defp do_mask(<<q::utf8, rest::binary>>, out) when q in [?", ?'] do
    {body, close, tail} = take_quoted(rest, q, "")

    kept =
      if Regex.match?(~r/^\s*:/, tail) or Regex.match?(@uses_tail_re, out),
        do: body,
        else: String.duplicate("x", byte_size(body))

    do_mask(tail, out <> <<q::utf8>> <> kept <> close)
  end

  defp do_mask(<<?#, rest::binary>>, out) do
    if out == "" or String.ends_with?(out, [" ", "\t"]),
      do: out,
      else: do_mask(rest, out <> "#")
  end

  defp do_mask(<<c::utf8, rest::binary>>, out), do: do_mask(rest, out <> <<c::utf8>>)
  defp do_mask(<<c, rest::binary>>, out), do: do_mask(rest, out <> <<c>>)

  # The body of a quoted scalar up to its closing quote; a backslash escapes
  # the next character inside double quotes. An unterminated scalar runs to
  # the end of the line.
  defp take_quoted("", _q, acc), do: {acc, "", ""}

  defp take_quoted(<<?\\, c::utf8, rest::binary>>, ?", acc),
    do: take_quoted(rest, ?", acc <> <<?\\, c::utf8>>)

  defp take_quoted(<<c::utf8, rest::binary>>, q, acc) when c == q, do: {acc, <<c::utf8>>, rest}

  defp take_quoted(<<c::utf8, rest::binary>>, q, acc),
    do: take_quoted(rest, q, acc <> <<c::utf8>>)

  defp take_quoted(<<c, rest::binary>>, q, acc), do: take_quoted(rest, q, acc <> <<c>>)
end
