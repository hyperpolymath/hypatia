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

  A double-quoted value is read as YAML decodes it — KYAML (YAML-POLICY
  Y-3) writes every value so, and an escape (`"a/b@\\x31…"`) is the
  reference GitHub runs, not its spelling (`decode_dq/1`). A composite
  action's steps are its root `runs:` steps in flow form too (`{ runs: {
  steps: [ { uses: ... } ] } }`), as they are in block form.

  Every position this module returns is a byte offset into the line as
  given: the mask keeps the line's bytes where they were.
  """

  # One `uses:` site anywhere on a line, bounded on the left so `reuses:` is
  # not one. Matched against the MASKED line. The value may carry YAML node
  # properties before it (`uses: &rust a/b@x`, `uses: !!str a/b@x`): the
  # anchor or tag names the node, the scalar is still the pin.
  @site_re ~r/(?:^|[^A-Za-z0-9_.\/-])["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?(?<action>[A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@(?<ref>[^\s#"',}\]]+)/

  @uses_tail_re ~r/(?:^|[-{,\s])["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*$/

  # A block scalar's content line that is a whole reference: no quotes (they
  # would be part of the value) and nothing after it.
  @block_ref_re ~r/^\s*([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)\s*$/

  # Root keys a workflow or an action defines whose values are data: no step
  # or job sits under any of them (`block_rows/1`).
  @data_root_keys ~w(name run-name on permissions env defaults concurrency description author inputs outputs branding)

  @doc """
  The structural sites of `line`, in order, as
  `%{action: a, ref: r, action_at: byte, ref_at: byte}`.

  `flow_prefix` is, for a line inside a flow collection opened on an earlier
  line, the structure of the lines since it opened (`flow_prefixes/1`): a
  `uses:` there is a site when the brackets still open before it make it a
  step's or a job's own key, however the line itself begins (`name:
  "hello` then `world", uses: a/b@x }]`). Without it the line is judged on
  its own.
  """
  @spec sites(String.t(), String.t()) :: [map()]
  def sites(line, flow_prefix \\ "") when is_binary(line) and is_binary(flow_prefix) do
    masked = mask(line)
    escaped = escaped_values(masked, flow_prefix)

    masked
    |> scan(flow_prefix)
    |> Enum.reject(fn [_full, {a_at, _} | _] ->
      Enum.any?(escaped, &(a_at >= &1.from and a_at <= &1.to))
    end)
    |> Enum.map(fn [_full, {a_at, a_len}, {r_at, r_len}] ->
      %{
        action: binary_part(masked, a_at, a_len),
        ref: binary_part(masked, r_at, r_len),
        action_at: a_at,
        ref_at: r_at
      }
    end)
    |> Kernel.++(for %{site: %{} = site} <- escaped, do: site)
    |> Enum.sort_by(& &1.action_at)
  end

  # The double-quoted `uses` values of a masked line that hold an escape
  # (`"github/codeql-action/init@\x31c5b…"`), each as `%{from, to, site}`:
  # the body's byte span and the site its value decodes to (`decoded_site/2`),
  # or nil — a value that is no structural site, that the line does not
  # close (a double-quoted scalar continued over lines is not read here), or
  # that does not decode to a reference. Such a value is read only through
  # its decoding: GitHub runs the decoded reference, never the text.
  defp escaped_values(masked, flow_prefix) do
    ~r/(?:^|[^A-Za-z0-9_.\/-])["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*"/
    |> Regex.scan(masked, return: :index)
    |> Enum.flat_map(fn [{at, len}] ->
      from = at + len

      {body, close, _tail} =
        take_quoted(binary_part(masked, from, byte_size(masked) - from), ?", "")

      prefix = binary_part(masked, 0, from)

      structural? =
        if flow_prefix == "",
          do: structural?(prefix),
          else: structural_in_flow?(flow_prefix <> prefix)

      cond do
        not String.contains?(body, "\\") ->
          []

        structural? and close != "" ->
          [%{from: from, to: from + byte_size(body), site: decoded_site(body, from)}]

        true ->
          [%{from: from, to: from + byte_size(body), site: nil}]
      end
    end)
  end

  # The site a double-quoted body (starting at byte `from` of its line)
  # decodes to: the action and ref as YAML reads them, `action_at`/`ref_at`
  # the source bytes they start at, `ref_len` the source length of the ref
  # as written (a repair replaces exactly those bytes), `escaped: true`.
  defp decoded_site(body, from) do
    with {:ok, decoded, spans} <- decode_dq(body),
         [_, {a_at, a_len}, {r_at, r_len}] <-
           Regex.run(~r/\A([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)/, decoded,
             return: :index
           ) do
      spans = List.to_tuple(spans)
      {action_from, _} = elem(spans, a_at)
      {ref_from, _} = elem(spans, r_at)
      {_, ref_to} = elem(spans, r_at + r_len - 1)

      %{
        action: binary_part(decoded, a_at, a_len),
        ref: binary_part(decoded, r_at, r_len),
        action_at: from + action_from,
        ref_at: from + ref_from,
        ref_len: ref_to - ref_from,
        escaped: true
      }
    else
      _ -> nil
    end
  end

  @doc """
  The body of a YAML double-quoted scalar (the text between its quotes, on
  one line) as YAML decodes it: `{:ok, decoded, spans}`, `spans` the source
  byte span `{from, to}` of every decoded byte, or `:error` for an escape
  YAML does not define. The escapes are YAML 1.2's: `\\0 \\a \\b \\t \\n \\v
  \\f \\r \\e \\  \\" \\/ \\\\ \\N \\_ \\L \\P`, `\\xXX`, `\\uXXXX`, `\\UXXXXXXXX` — the
  JSON-style subset is what `yq -o kyaml` writes.
  """
  @spec decode_dq(String.t()) ::
          {:ok, String.t(), [{non_neg_integer(), non_neg_integer()}]} | :error
  def decode_dq(body) when is_binary(body), do: decode_dq(body, 0, [], [])

  defp decode_dq(<<>>, _at, out, spans),
    do: {:ok, out |> Enum.reverse() |> IO.iodata_to_binary(), Enum.reverse(spans)}

  defp decode_dq(<<?\\, rest::binary>>, at, out, spans) do
    case escape(rest) do
      {:ok, bytes, used} ->
        span = {at, at + 1 + used}
        spans = Enum.reduce(1..byte_size(bytes)//1, spans, fn _, acc -> [span | acc] end)

        decode_dq(
          binary_part(rest, used, byte_size(rest) - used),
          at + 1 + used,
          [bytes | out],
          spans
        )

      :error ->
        :error
    end
  end

  defp decode_dq(<<c, rest::binary>>, at, out, spans),
    do: decode_dq(rest, at + 1, [<<c>> | out], [{at, at + 1} | spans])

  @simple_escapes %{
    ?0 => <<0>>,
    ?a => <<7>>,
    ?b => <<8>>,
    ?t => <<9>>,
    ?\t => <<9>>,
    ?n => <<10>>,
    ?v => <<11>>,
    ?f => <<12>>,
    ?r => <<13>>,
    ?e => <<27>>,
    ?\s => " ",
    ?" => "\"",
    ?/ => "/",
    ?\\ => "\\",
    ?N => <<0x85::utf8>>,
    ?_ => <<0xA0::utf8>>,
    ?L => <<0x2028::utf8>>,
    ?P => <<0x2029::utf8>>
  }

  # The escape after a `\` in `rest`: `{:ok, bytes, bytes of rest used}`.
  defp escape(<<c, _::binary>>) when is_map_key(@simple_escapes, c),
    do: {:ok, Map.fetch!(@simple_escapes, c), 1}

  defp escape(<<c, rest::binary>>) when c in [?x, ?u, ?U] do
    n = %{?x => 2, ?u => 4, ?U => 8}[c]

    with <<hex::binary-size(n), _::binary>> <- rest,
         true <- Regex.match?(~r/\A[0-9A-Fa-f]+\z/, hex),
         cp = String.to_integer(hex, 16),
         true <- cp <= 0x10FFFF and cp not in 0xD800..0xDFFF do
      {:ok, <<cp::utf8>>, 1 + n}
    else
      _ -> :error
    end
  end

  defp escape(_), do: :error

  @doc """
  The structural site matches of an already masked line, each as
  `[full, action, ref]` `{byte, length}` pairs (`Regex.scan/3` with
  `return: :index`).
  """
  @spec scan(String.t(), String.t()) :: [[{non_neg_integer(), non_neg_integer()}]]
  def scan(masked, flow_prefix \\ "") when is_binary(masked) do
    @site_re
    |> Regex.scan(masked, return: :index, capture: :all)
    |> Enum.filter(fn [_full, {action_at, _} | _] ->
      prefix = binary_part(masked, 0, action_at)

      if flow_prefix == "",
        do: structural?(prefix),
        else: structural_in_flow?(flow_prefix <> prefix)
    end)
  end

  @doc """
  The lines of a whole file as their structure reads: `mask_lines/1`, each
  then masked (`mask/1`, so cut at its comment), with a quoted scalar the
  line leaves open blanked from its opening quote. Joined, they read as one
  text of keys, brackets and commas whatever scalars span them: a `#` that
  continues a quoted scalar (`name: "hello` then `# world",`) cuts nothing,
  and no quote is left open.
  """
  @spec structure_lines(String.t()) :: [String.t()]
  def structure_lines(content) when is_binary(content),
    do: content |> mask_lines() |> Enum.map(&do_mask(&1, "", true))

  @doc """
  For each line of `content`, the structure of the lines since the flow
  collection it sits in was opened (`structure_lines/1`, joined, ending in a
  newline), or `""` for a line at flow depth 0 (`line_depths/1`): what
  `sites/2` reads a line inside a multi-line flow collection against.
  """
  @spec flow_prefixes(String.t()) :: [String.t()]
  def flow_prefixes(content) when is_binary(content) do
    content
    |> structure_lines()
    |> Enum.zip(line_depths(content))
    |> Enum.map_reduce("", fn {text, depth}, carry ->
      prefix = if depth > 0, do: carry <> "\n", else: ""
      {prefix, if(depth > 0, do: carry <> "\n" <> text, else: text)}
    end)
    |> elem(0)
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
  and `uses: a/b@x extra` are not. The comment is never part of it. Nor
  is a line whose `uses` value is written with escapes (`"a/b@\\x31…"`): its
  reference is read decoded (`sites/2`), but it is no plain re-pin.
  """
  @spec pin_line?(String.t()) :: boolean()
  def pin_line?(line) when is_binary(line) do
    masked = mask(line)
    sites = if escaped_values(masked, "") == [], do: scan(masked), else: []

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
  The site a `uses:` key with nothing after it on its line holds on the next
  line with content, where YAML continues its scalar (`- uses:` then
  `    github/codeql-action/init@<sha>`; `continuations/1` steps over the
  blank and comment-only lines between), as `sites/1` returns one, its
  offsets into `next_line`; nil otherwise. Both lines are as `mask_lines/1`
  returns them.

  The key is a block key (at the line's start or after its list dash), or an
  entry of a flow mapping in a step or job position as `sites/1` reads one
  (`steps: [ { uses:`, `- { name: x, uses:`). A block key's value sits deeper
  than the key and holds nothing but the reference, quoted or plain, and a
  comment; a flow entry's value is the reference up to the delimiter that
  ends it (`,`, `}` or `]`), wherever the line is indented — inside a flow
  collection the scalar continues on any line. A block key may also hold a
  block scalar (`- uses: >-`): its one content line, a whole reference
  (`line_states/1`, `:value`), is the value; in masked lines any other
  content is blanked, so a scalar that folds more lines names no site.
  """
  @spec continued_site(String.t(), String.t()) :: map() | nil
  def continued_site(key_line, next_line) when is_binary(key_line) and is_binary(next_line) do
    key = head(key_line)

    # Each form's value: the reference it may be, and what may follow it.
    form =
      cond do
        m = Regex.run(~r/^(\s*(?:-\s+)?)["']?uses["']?\s*:(?:\s+[&!]\S*)*\s*$/, key) ->
          if indent_of(next_line) > byte_size(Enum.at(m, 1)),
            do:
              {~r/^\s*(?:[&!]\S*\s+)*["']?([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)["']?\s*(?:#.*)?$/,
               ~r/\A\s*(?:#.*)?\z/}

        m =
            Regex.run(
              ~r/^(\s*(?:-\s+)?)["']?uses["']?\s*:(?:\s+[&!]\S*)*\s+[|>][-+0-9]*\s*$/,
              key
            ) ->
          if indent_of(next_line) > byte_size(Enum.at(m, 1)), do: {@block_ref_re, nil}

        Regex.match?(~r/[{,]\s*["']?uses["']?\s*:(?:\s+[&!]\S*)*\s*$/, key) and
            structural?(String.trim_trailing(key) <> " ") ->
          {~r/^\s*(?:[&!]\S*\s+)*["']?([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)["']?\s*(?:[,}\]]|#|$)/,
           ~r/\A\s*(?:[,}\]]|#|\z)/}

        true ->
          nil
      end

    with {value_re, tail_re} <- form,
         :plain <- escaped_value(next_line, tail_re),
         [_, {a_at, a_len}, {r_at, r_len}] <- Regex.run(value_re, next_line, return: :index) do
      %{
        action: binary_part(next_line, a_at, a_len),
        ref: binary_part(next_line, r_at, r_len),
        action_at: a_at,
        ref_at: r_at
      }
    else
      {:escaped, site} -> site
      _ -> nil
    end
  end

  # A continued value written double-quoted with an escape (`"a/b@\x31…"`)
  # is read decoded: `{:escaped, site}` — nil when it decodes to no
  # reference or more than `tail_re` allows follows its closing quote — or
  # :plain for any other value (a block scalar's, `tail_re` nil, included).
  defp escaped_value(_line, nil), do: :plain

  defp escaped_value(line, tail_re) do
    with [{0, from}] <- Regex.run(~r/\A\s*(?:[&!]\S*\s+)*"/, line, return: :index),
         {body, close, tail} =
           take_quoted(binary_part(line, from, byte_size(line) - from), ?", ""),
         true <- String.contains?(body, "\\") do
      {:escaped, if(close != "" and Regex.match?(tail_re, tail), do: decoded_site(body, from))}
    else
      _ -> :plain
    end
  end

  @doc """
  Every `uses:` whose value is an alias (`- uses: *checkout`, a block key or a
  step/job flow entry as `sites/1` reads one), resolved to the site the
  anchor it names holds: `%{line, at, len, anchor_line, site}` — the alias'
  line, its byte offset and length, the anchor's line, and the site as
  `sites/1` returns one, its offsets into the anchor's line. YAML resolves an
  alias to the latest `&name` before it, so a later definition does not
  count and an earlier one is shadowed — by any value, including one that is
  no `action@ref` scalar, which names no site. Lines as `mask_lines/1`
  returns them; `flow_prefixes` (`flow_prefixes/1` of the same file) lets a
  line inside a multi-line flow collection be judged against the brackets
  the lines above opened, as `sites/2` judges a literal site: a `uses:
  *name` in an `env: {` spread over lines is data, not a step's action.
  """
  @spec alias_sites([String.t()], [String.t()]) :: [map()]
  def alias_sites(lines, flow_prefixes \\ []) when is_list(lines) do
    prefixes = flow_prefixes ++ List.duplicate("", max(length(lines) - length(flow_prefixes), 0))
    numbered = lines |> Enum.zip(prefixes) |> Enum.with_index(1)

    defs = resolved_defs(numbered)

    for {{line, prefix}, n} <- numbered,
        {name, at, len} <- uses_aliases(line, prefix),
        %{site: %{} = site} = d <- [latest_anchor(defs, name, n, at)] do
      %{line: n, at: at, len: len, anchor_line: Map.get(d, :site_line, d.line), site: site}
    end
  end

  # Every anchor the numbered lines define, each with the site its value is
  # (`anchor_defs/2`, `continued_anchor/3`) and its `:line`.
  defp resolved_defs(numbered) do
    for {{line, prefix}, n} <- numbered, d <- anchor_defs(line, prefix) do
      below = for {{l, _}, m} <- Enum.drop(numbered, n), do: {l, m}
      d |> Map.put(:line, n) |> continued_anchor(line, below)
    end
  end

  # The site the alias `*name` at byte `at` of line `n` runs, as
  # `[{line, site}]` (the line its reference stands on), or `[]`.
  defp alias_value(defs, name, n, at) do
    case latest_anchor(defs, name, n, at) do
      %{site: %{} = site} = d -> [{Map.get(d, :site_line, d.line), site}]
      _ -> []
    end
  end

  # An anchor with nothing after it on its line but a tag or a comment
  # (`BAD: &bad`) names the scalar YAML continues on the next line with
  # content (`    github/codeql-action/init@<sha>`): its site is there,
  # `:site_line` that line. In block context the scalar must sit deeper than
  # the key, and is all the line holds; inside a flow collection it may
  # stand at any indentation, even column 0, up to the `,`, `}` or `]` that
  # ends it. An anchored block scalar (`BAD: &bad >-`) names the reference
  # its one content line holds (`line_states/1`, `:value`).
  defp continued_anchor(%{site: nil, rest: rest} = d, line, below) do
    block? = not d.flow and Regex.match?(~r/^\s*(?:![^\s]*\s+)*[|>][-+0-9]*\s*(?:#.*)?$/, rest)

    value_re =
      cond do
        block? ->
          @block_ref_re

        d.flow ->
          ~r/^\s*(?:![^\s]*\s+)*["']?([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)["']?\s*(?:[,}\]]|#|$)/

        true ->
          ~r/^\s*(?:![^\s]*\s+)*["']?([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)["']?\s*(?:#.*)?$/
      end

    with true <- block? or Regex.match?(~r/^\s*(?:![^\s]*\s*)*(?:#.*)?$/, rest),
         {next, n} <- Enum.find(below, fn {l, _} -> String.trim(head(l)) != "" end),
         true <- d.flow or indent_of(next) > indent_of(line),
         [_, {a_at, a_len}, {r_at, r_len}] <- Regex.run(value_re, next, return: :index) do
      site = %{
        action: binary_part(next, a_at, a_len),
        ref: binary_part(next, r_at, r_len),
        action_at: a_at,
        ref_at: r_at
      }

      %{d | site: site} |> Map.put(:site_line, n)
    else
      _ -> d
    end
  end

  defp continued_anchor(d, _line, _below), do: d

  # The anchors `line` defines where a node starts (outside quotes and the
  # comment), each `%{name, at, site, rest, flow}`: the site its scalar value
  # is, or nil; the text after it; whether it sits inside a flow collection.
  # A name runs to whitespace or a flow indicator (`&rust/tool`). A node
  # starts at the line's start or after its list dashes, after `key: `, and
  # — inside a flow collection only (one the line opens, or `flow_prefix`
  # says it sits in) — after `[`, `{` or `,`, or directly after the colon of
  # a quoted key (`{"BAD":&bad ...}`, JSON-style), each past any tag.
  # `NOTE: a, &x b` in block context is one plain scalar: no anchor.
  defp anchor_defs(line, flow_prefix) do
    masked = mask(line)

    ~r/&([^\s,\[\]{}]+)/
    |> Regex.scan(masked, return: :index)
    |> Enum.filter(fn [{at, _}, _] ->
      before = binary_part(masked, 0, at)

      Regex.match?(~r/(?:^\s*(?:-\s+)*|:\s+)(?:![^\s]*\s+)*$/, before) or
        ((flow_prefix != "" or String.contains?(before, ["[", "{"])) and
           Regex.match?(~r/(?:[\[{,]|["']:)\s*(?:![^\s]*\s+)*$/, before))
    end)
    |> Enum.map(fn [{at, len}, {n_at, n_len}] ->
      rest_at = at + len
      rest = binary_part(line, rest_at, byte_size(line) - rest_at)

      site =
        case Regex.run(
               ~r/^\s+(?:![^\s]*\s+)*["']?([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)["']?(?=[\s,}\]#]|$)/,
               rest,
               return: :index
             ) do
          [_, {a_at, a_len}, {r_at, r_len}] ->
            %{
              action: binary_part(rest, a_at, a_len),
              ref: binary_part(rest, r_at, r_len),
              action_at: rest_at + a_at,
              ref_at: rest_at + r_at
            }

          _ ->
            nil
        end

      before = binary_part(masked, 0, at)
      open = length(Regex.scan(~r/[\[{]/, before)) - length(Regex.scan(~r/[\]}]/, before))

      %{
        name: binary_part(masked, n_at, n_len),
        at: at,
        site: site,
        rest: rest,
        flow: flow_prefix != "" or open > 0
      }
    end)
  end

  # The `uses:` keys of `line` whose value is an alias, at a site position,
  # as `{name, offset of the "*", length of "*name"}`.
  defp uses_aliases(line, flow_prefix) do
    masked = mask(line)

    ~r/["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*\*([^\s,\[\]{}]+)/
    |> Regex.scan(masked, return: :index)
    |> Enum.flat_map(fn [{at, len}, {n_at, n_len}] ->
      star = n_at - 1
      prefix = binary_part(masked, 0, at)
      head = binary_part(masked, 0, star)

      structural? =
        if flow_prefix == "",
          do: structural?(head),
          else: structural_in_flow?(flow_prefix <> head)

      if structural? and
           not Regex.match?(~r/[A-Za-z0-9_.\/-]$/, prefix),
         do: [{binary_part(masked, n_at, n_len), star, at + len - star}],
         else: []
    end)
  end

  defp latest_anchor(defs, name, line, at) do
    defs
    |> Enum.filter(&(&1.name == name and (&1.line < line or (&1.line == line and &1.at < at))))
    |> Enum.max_by(&{&1.line, &1.at}, fn -> nil end)
  end

  @doc """
  Every step or job given whole as an alias — `- *step` in a job's
  `steps:`, `*step` as an entry of a `steps: [ ... ]`, `rust: *job` under
  `jobs:` — resolved to the `uses:` of the mapping its anchor holds:
  `%{line, key_line, anchor_line, site}`, the alias' line, the line of that
  `uses:` key, the line its reference stands on, and its site as `sites/1`
  returns one (offsets into that line). The reference is the key's value as
  YAML resolves it: written on the key's line, continued below it
  (`continued_site/2`, a block scalar included), or an alias (`uses: *bad`,
  on the line of `&bad`'s value).
  YAML resolves the alias to that very node, so its `uses:` is the step's
  or the job's action even where the anchor sits in data (`include: -
  &step { uses: ... }` in a matrix). The anchored mapping is read as a flow
  mapping on the anchor's own line (`&step { uses: ... }`) or in block form
  (`- &step`, `key: &step`, its keys on the deeper lines below); any other
  shape names no site here. A `- *step` whose list owner the lines above
  do not show is judged a step, as a lone `- uses:` line is.
  """
  @spec node_alias_sites(String.t()) :: [map()]
  def node_alias_sites(content) when is_binary(content) do
    lines = mask_lines(content)
    numbered = lines |> Enum.zip(flow_prefixes(content)) |> Enum.with_index(1)
    texts = List.to_tuple(lines)

    rows = block_rows(content)

    defs = resolved_defs(numbered)

    for {{line, prefix}, n} <- numbered,
        {name, at} <- node_aliases(line, prefix, rows, n - 1),
        %{} = d <- [latest_anchor(defs, name, n, at)],
        {key_line, site_line, site} <- anchored_uses(d, texts, rows, defs) do
      %{line: n, key_line: key_line, anchor_line: site_line, site: site}
    end
  end

  # The aliases on `line` that stand for a whole step or job, as `{name,
  # offset of the "*"}`: a block list item that is nothing but the alias, in
  # a sequence `steps:` holds (or one whose owner is not shown); a block key
  # of `jobs:` whose value is the alias; or a flow entry whose brackets make
  # it an item of a job's step sequence, or a job's value in `jobs: { ... }`.
  defp node_aliases(line, prefix, rows, i) do
    masked = mask(line)
    row = elem(rows, i)

    item = Regex.run(~r/^(\s*-\s+)\*([^\s,\[\]{}]+)\s*$/, masked)
    value = Regex.run(~r/^(\s*["']?[^"':\s]+["']?\s*:\s+)\*([^\s,\[\]{}]+)\s*$/, masked)

    block =
      case {row, item, value} do
        {%{dash: d}, [_, lead, name], _} when d != nil ->
          if sequence_owner(rows, seq_owner(rows, i, d)) != :data,
            do: [{name, byte_size(lead)}],
            else: []

        {%{dash: nil}, _, [_, lead, name]} ->
          if match?({:value, _, "jobs"}, mapping_of(rows, i)),
            do: [{name, byte_size(lead)}],
            else: []

        _ ->
          []
      end

    flow =
      ~r/([\[{,:])\s*\*([^\s,\[\]{}]+)/
      |> Regex.scan(masked, return: :index)
      |> Enum.flat_map(fn [_, {c_at, _}, {n_at, n_len}] ->
        star = n_at - 1
        stack = flow_stack(prefix <> binary_part(masked, 0, star))

        node? =
          if binary_part(masked, c_at, 1) == ":",
            do: match?([{"{", "jobs"} | _], stack),
            else: alias_step_stack?(stack)

        if node?, do: [{binary_part(masked, n_at, n_len), star}], else: []
      end)

    block ++ flow
  end

  # An entry directly in a `[` that `steps:` opens: the whole stack the text
  # shows (a block job's `steps: [*step]`), or one that goes on through a
  # job key into `jobs`.
  defp alias_step_stack?([{"[", "steps"}]), do: true

  defp alias_step_stack?([{"[", "steps"}, {"{", job}, {"{", "jobs"} | _]) when is_binary(job),
    do: true

  defp alias_step_stack?(_), do: false

  # The `uses:` of the mapping the anchor `d` holds, as `[{key_line,
  # site_line, site}]`: a flow mapping opening on the anchor's line, read
  # through the lines below to the brace that closes it, or a block mapping
  # on the lines below, read to its `uses:` key.
  defp anchored_uses(d, texts, rows, defs) do
    line = elem(texts, d.line - 1)
    rest_at = byte_size(line) - byte_size(d.rest)

    cond do
      m = Regex.run(~r/^\s*(?:![^\s]*\s+)*\{/, d.rest, return: :index) ->
        [{0, len}] = m
        flow_mapping_uses(texts, d.line, rest_at + len - 1, defs)

      Regex.match?(~r/^\s*(?:![^\s]*\s*)*(?:#.*)?$/, d.rest) ->
        block_uses(d.line, texts, rows, defs)

      true ->
        []
    end
  end

  # The top-level `uses` entry of the flow mapping whose `{` is at byte
  # `open` of line `n`, as `[{key_line, site_line, site}]` (an alias value
  # resolved to its anchor's), or `[]` when the mapping closes without one.
  # The mapping is read through the lines below to the brace that closes
  # it: an entry starts after its `{` or one of its commas — on the same
  # line, or as the last thing a line above held — with no other bracket
  # open between. Lines are read masked (`mask/1`), so brackets and
  # commas inside quoted text and comments count for nothing.
  defp flow_mapping_uses(texts, n, open, defs) do
    n..tuple_size(texts)//1
    |> Enum.reduce_while({:open, 0, nil}, fn k, {:open, depth, last} ->
      from = if k == n, do: open, else: 0

      case mapping_line_uses(mask(elem(texts, k - 1)), from, depth, last) do
        {:found, {:site, site}} ->
          {:halt, [{k, k, site}]}

        {:found, {:alias, name, at}} ->
          {:halt, for({line, site} <- alias_value(defs, name, k, at), do: {k, line, site})}

        :closed ->
          {:halt, []}

        open_state ->
          {:cont, open_state}
      end
    end)
    |> case do
      found when is_list(found) -> found
      _ -> []
    end
  end

  defp mapping_line_uses(m, from, depth, last) do
    m
    |> :binary.bin_to_list()
    |> Enum.with_index()
    |> Enum.drop(from)
    |> Enum.reduce_while({:open, depth, last}, fn {c, i}, {:open, d, l} ->
      cond do
        c in [?{, ?[] ->
          {:cont, {:open, d + 1, c}}

        c in [?}, ?]] and d <= 1 ->
          {:halt, :closed}

        c in [?}, ?]] ->
          {:cont, {:open, d - 1, c}}

        c in [?\s, ?\t, ?,] ->
          {:cont, {:open, d, if(c == ?,, do: c, else: l)}}

        d == 1 and l in [?{, ?,] ->
          case entry_uses(m, i) do
            nil -> {:cont, {:open, d, c}}
            site -> {:halt, site}
          end

        true ->
          {:cont, {:open, d, c}}
      end
    end)
    |> case do
      {:open, _, _} = state -> state
      :closed -> :closed
      found -> {:found, found}
    end
  end

  # The `uses` entry starting at byte `i` of masked line `m`: `{:site,
  # site}` for a reference written there, `{:alias, name, offset of the
  # "*"}` for an alias, or nil.
  defp entry_uses(m, i) do
    rest = binary_part(m, i, byte_size(m) - i)

    case Regex.run(
           ~r/\A["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*(?:["']?([A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]*?)@([^\s#"',}\]]+)|\*([^\s,\[\]{}]+))/,
           rest,
           return: :index
         ) do
      [_, {a_at, a_len}, {r_at, r_len}] when a_at >= 0 ->
        {:site,
         %{
           action: binary_part(rest, a_at, a_len),
           ref: binary_part(rest, r_at, r_len),
           action_at: i + a_at,
           ref_at: i + r_at
         }}

      [_, _, _, {n_at, n_len}] ->
        {:alias, binary_part(rest, n_at, n_len), i + n_at - 1}

      _ ->
        nil
    end
  end

  # A block mapping anchored with nothing after the anchor (`- &step`,
  # `step: &step`) on row `n`: its keys are the rows below deeper than the
  # dash or the key, the first of them setting their column; the one named
  # `uses` holds the site(s) (`key_value/3`).
  defp block_uses(n, texts, rows, defs) do
    case elem(rows, n - 1) do
      %{} = row ->
        parent = if row.key == nil, do: row.dash || row.indent, else: row.key_col

        children =
          n..(tuple_size(rows) - 1)//1
          |> Enum.map(&{&1, elem(rows, &1)})
          |> Enum.reject(fn {_, r} -> r == :skip end)
          |> Enum.take_while(fn {_, r} -> r.indent > parent end)

        case children do
          [] ->
            []

          [{_, first} | _] ->
            for {j, %{indent: ind, dash: nil, key: "uses"}} <- children,
                ind == first.indent,
                {line, site} <- key_value(texts, j + 1, defs),
                do: {j + 1, line, site}
        end

      _ ->
        []
    end
  end

  # The site(s) the block `uses:` key on line `k` holds, as `[{line, site}]`:
  # written on its line, an alias (`uses: *bad`) resolved to the anchor's
  # value, or a reference continued below it (`continued_site/2`: the next
  # line with content, a block scalar's included).
  defp key_value(texts, k, defs) do
    text = elem(texts, k - 1)
    masked = mask(text)

    alias_re = ~r/^\s*["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*\*([^\s,\[\]{}]+)\s*$/

    case {sites(text), Regex.run(alias_re, masked, return: :index)} do
      {[_ | _] = found, _} ->
        for site <- found, do: {k, site}

      {[], [_, {n_at, n_len}]} ->
        alias_value(defs, binary_part(masked, n_at, n_len), k, n_at - 1)

      {[], nil} ->
        with v when is_integer(v) <-
               Enum.find(
                 (k + 1)..tuple_size(texts)//1,
                 &(String.trim(head(elem(texts, &1 - 1))) != "")
               ),
             %{} = site <- continued_site(text, elem(texts, v - 1)) do
          [{v, site}]
        else
          _ -> []
        end
    end
  end

  @doc """
  The one-based numbers of the lines of `content` whose leading block `uses:`
  key the indentation above it proves is neither a step's nor a job's own:
  an input of that name (`with:` then `uses: a/b@x`), an entry of a sequence
  some key other than `steps:` holds, a key of a `steps:` that is no
  job's, or one under a root key that holds no job (`env:` then `uses:
  a/b@x` in a workflow). Such a key holds data, not a pin. A line whose
  ancestry the lines above do not show far enough — a diff hunk cut short,
  a fragment with no `jobs:` — is not listed: it is judged on its own line,
  as before.
  """
  @spec data_uses_lines(String.t()) :: MapSet.t(pos_integer())
  def data_uses_lines(content) when is_binary(content) do
    rows = block_rows(content)

    for i <- 0..(tuple_size(rows) - 1)//1,
        match?(%{key: "uses"}, elem(rows, i)),
        uses_owner(rows, i) == :data,
        into: MapSet.new(),
        do: i + 1
  end

  # The lines of `content` as block structure (`block_row/2`), a tuple. A key
  # at column 0 is a root key; a root key that is neither `jobs:` nor
  # `runs:` (`env:`, `on:`, `inputs:`) holds no step or job, so it is marked
  # `data_root?` — when the text shows the root (a `jobs:` or `runs:` at
  # column 0), or the key is one a workflow or an action defines for data,
  # so a hunk that shows `env:` at column 0 reads it the same. A fragment
  # with neither (`build:` then `uses: ...`) is judged as before.
  defp block_rows(content) do
    rows = content |> line_states() |> Enum.zip_with(line_depths(content), &block_row/2)

    shown? =
      Enum.any?(rows, &match?(%{indent: 0, dash: nil, key: k} when k in ["jobs", "runs"], &1))

    rows
    |> Enum.map(fn
      %{indent: 0, dash: nil, key: k} = row when is_binary(k) and k not in ["jobs", "runs"] ->
        Map.put(row, :data_root?, shown? or k in @data_root_keys)

      row ->
        row
    end)
    |> List.to_tuple()
  end

  # One line as block structure: its indentation, the column of its last
  # list dash (nil without one), the column and name of the key after the
  # dashes, whether that key's value is on the lines below (`key:` alone,
  # past any node properties), and whether the line is a bare dash. Blank,
  # comment-only and scalar lines, and lines inside a flow collection, are
  # :skip: nothing there is a block parent.
  defp block_row({text, :yaml}, 0) do
    h = head(text)

    if String.trim(h) == "" do
      :skip
    else
      [_, ind, dashes, rest] = Regex.run(~r/^( *)((?:-(?:\s+|$))*)(.*)$/, h)

      {key, open?} =
        case Regex.run(~r/^["']?([^"':\s][^"':]*?)["']?\s*:(?:\s+(.*))?$/, rest) do
          [_, k | value] ->
            {k, Regex.match?(~r/^(?:[&!]\S*\s*)*$/, String.trim(Enum.join(value)))}

          nil ->
            {nil, false}
        end

      dash =
        case :binary.matches(dashes, "-") do
          [] -> nil
          found -> byte_size(ind) + elem(List.last(found), 0)
        end

      %{
        indent: byte_size(ind),
        dash: dash,
        key_col: byte_size(ind) + byte_size(dashes),
        key: key,
        open?: open?,
        bare?: String.trim(rest) == ""
      }
    end
  end

  defp block_row(_, _), do: :skip

  # :site, :data or :unknown for the `uses` key on row i: a step's when its
  # mapping is an item of a job's `steps:`, a job's when its mapping is the
  # value of a key of `jobs:`.
  defp uses_owner(rows, i) do
    case mapping_of(rows, i) do
      {:item, j, d} -> sequence_owner(rows, seq_owner(rows, j, d))
      {:value, j, _key} -> job_key(rows, j)
      :root -> root_owner(rows, i)
      _ -> :unknown
    end
  end

  # A key of the root mapping (`block_rows/1`): :data when it is a data root,
  # else :unknown — the text shows no more of where it sits.
  defp root_owner(rows, j),
    do: if(match?(%{data_root?: true}, elem(rows, j)), do: :data, else: :unknown)

  defp sequence_owner(rows, {:value, s, "steps"}), do: steps_owner(rows, s)
  defp sequence_owner(_rows, {:value, _, _}), do: :data
  defp sequence_owner(_rows, _), do: :unknown

  # `steps:` is a job's, or a composite action's under `runs:`; inside a
  # list item or another key's mapping it is data (`with:` then `steps:`).
  defp steps_owner(rows, s) do
    case mapping_of(rows, s) do
      {:value, _p, "runs"} -> :site
      {:value, p, _job} -> job_key(rows, p)
      {:item, _, _} -> :data
      :root -> root_owner(rows, s)
      _ -> :unknown
    end
  end

  # Is the key on row j a job: its mapping the value of `jobs:`? A key
  # inside a list item (a step's `with:`) or under any other key is not.
  defp job_key(rows, j) do
    case mapping_of(rows, j) do
      {:value, _, "jobs"} -> :site
      {:value, _, _} -> :data
      {:item, _, _} -> :data
      :root -> root_owner(rows, j)
      _ -> :unknown
    end
  end

  # The mapping row j's key sits in: the list item its own dash opens, or
  # the one the rows above give it.
  defp mapping_of(rows, j) do
    case elem(rows, j) do
      %{dash: d} when d != nil -> {:item, j, d}
      %{key_col: col} -> container(rows, j, col)
    end
  end

  # The mapping a key at column `col` of row i belongs to, from the nearest
  # row above it that sits shallower: a list item whose first key is at
  # `col` (`- name: x` above `  uses:`) or a bare dash, `{:item, row, dash}`;
  # a key whose value is below it, `{:value, row, key}`. Rows as deep or
  # deeper are its siblings and their contents. :root when no row is
  # shallower; :unknown when the shallower row is neither.
  defp container(rows, i, col) do
    Enum.reduce_while((i - 1)..0//-1, :root, fn j, acc ->
      case elem(rows, j) do
        :skip -> {:cont, acc}
        %{indent: ind} when ind >= col -> {:cont, acc}
        %{dash: d, key_col: ^col} when d != nil -> {:halt, {:item, j, d}}
        %{dash: d, bare?: true} when d != nil -> {:halt, {:item, j, d}}
        %{key: k, open?: true, key_col: kc} when k != nil and kc < col -> {:halt, {:value, j, k}}
        _ -> {:halt, :unknown}
      end
    end)
  end

  # The key that holds the sequence whose item's dash is at column `d` of
  # row j: the nearest row above that is not a sibling item or deeper —
  # `steps:` above it, or at its own column (`steps:` then `- uses:`).
  # :unknown when no row above shows it.
  defp seq_owner(rows, j, d) do
    Enum.reduce_while((j - 1)..0//-1, :unknown, fn k, acc ->
      case elem(rows, k) do
        :skip ->
          {:cont, acc}

        %{indent: ind} when ind > d ->
          {:cont, acc}

        %{indent: ^d, dash: dd} when dd != nil ->
          {:cont, acc}

        %{key: key, open?: true, key_col: kc} when key != nil and kc <= d ->
          {:halt, {:value, k, key}}

        _ ->
          {:halt, :unknown}
      end
    end)
  end

  @doc """
  The flow depth at the start of each line of `content` — how many `[` and
  `{` the lines above left open, as the walk of `line_states/1` counts them
  (outside quoted scalars, comments and block-scalar content). A line at
  depth 0 is block structure; one deeper sits inside a flow collection, even
  when nothing on the line itself shows it (`steps: [`, then `{` alone, then
  `uses: ...`).
  """
  @spec line_depths(String.t()) :: [non_neg_integer()]
  def line_depths(content) when is_binary(content) do
    lines = String.split(content, "\n")

    lines
    |> Enum.with_index()
    |> Enum.map_reduce(initial_state(lines), fn
      {line, n}, st ->
        {_, next} = mask_file_line(line, n, st)
        {st.depth, next}
    end)
    |> elem(0)
  end

  @doc """
  Every `uses:` key in `lines` (as `mask_lines/1` returns them) whose
  reference YAML continues below it, as `{key_line, value_line, site}`:
  one-based line numbers and the `continued_site/2` site on the value line.
  The value is the next line that holds anything but a comment — YAML lets
  blank and comment-only lines stand between a key and its scalar
  (`- uses:`, `  # rationale`, `  github/codeql-action/init@<sha>`).
  """
  @spec continuations([String.t()]) :: [{pos_integer(), pos_integer(), map()}]
  def continuations(lines) when is_list(lines) do
    content =
      lines
      |> Enum.with_index(1)
      |> Enum.reject(fn {line, _} -> String.trim(head(line)) == "" end)

    content
    |> Enum.zip(Enum.drop(content, 1))
    |> Enum.flat_map(fn {{key, k}, {next, v}} ->
      case continued_site(key, next) do
        nil -> []
        site -> [{k, v, site}]
      end
    end)
  end

  @doc """
  The lines of a whole file, each with the bytes that belong to a scalar
  opened on an earlier line blanked to `x`, byte for byte: every content
  line of a block scalar (`run: |`) — but the reference one a `uses:` key or
  an anchor holds is whole (`:value`, `line_states/1`) — and a multi-line
  quoted scalar's continuation up to and including its closing quote. A line read through
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
  def mask_lines(content) when is_binary(content),
    do: content |> line_states() |> Enum.map(&elem(&1, 0))

  @doc """
  The lines of `mask_lines/1`, each with what the reader made of it:
  `:yaml` (structure, as written), `:text` (inside a scalar an earlier line
  opened: a block scalar's content, or a quoted scalar's continuation — at
  least up to its closing quote), `:value` (the one content line of a block
  scalar a `uses:` key or an anchor holds, when that line is a whole
  reference — `- uses: >-` then `    a/b@<sha>`: the scalar's value, kept as
  written so the key's site can be read there) or `:open` (after the
  opening quote of a quoted scalar the text never closes, returned as
  written: what it is cannot be said from these lines alone). A diff hunk,
  which shows a few lines of a file, is read this way too: a hunk that cuts
  a quoted scalar off leaves its lines `:open`.
  """
  @spec line_states(String.t()) :: [{String.t(), :yaml | :text | :value | :open}]
  def line_states(content) when is_binary(content) do
    lines = String.split(content, "\n")

    {masked, st} =
      lines
      |> Enum.with_index()
      |> Enum.map_reduce(initial_state(lines), fn
        {line, n}, st -> mask_file_line(line, n, st)
      end)

    if st.q do
      Enum.take(masked, st.opened + 1) ++
        Enum.map(Enum.drop(lines, st.opened + 1), &{&1, :open})
    else
      masked
    end
  end

  defp initial_state(lines),
    do: %{
      q: nil,
      opened: nil,
      block: nil,
      depth: 0,
      pending: false,
      keep: nil,
      lines: List.to_tuple(lines)
    }

  # Inside a block scalar: a blank line or one deeper than the key is its
  # content; anything else ends it. The one line that is a `uses:` key's or
  # an anchor's whole reference (`keep`, `block_value/4`) is kept as written.
  defp mask_file_line(line, n, %{block: col} = st) when is_integer(col) do
    cond do
      n == st.keep ->
        {{line, :value}, st}

      String.trim(line) == "" or indent_of(line) > col ->
        {{String.duplicate("x", byte_size(line)), :text}, st}

      true ->
        mask_file_line(line, n, %{st | block: nil, keep: nil})
    end
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
    {{view, if(cont > 0, do: :text, else: :yaml)}, st}
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
  defp walk(<<>>, seen, n, st) do
    trimmed = String.trim_trailing(seen)

    cond do
      String.trim(seen) == "" ->
        st

      st.depth == 0 and
          Regex.match?(~r/(?:^\s*(?:-\s+)*|:\s+)(?:[&!]\S*\s+)*[|>][-+0-9]*$/, trimmed) ->
        col = block_parent(seen)
        %{st | block: col, pending: false, keep: block_value(st.lines, n, col, trimmed)}

      true ->
        # A key or dash followed only by node properties (`- run: &cmd`,
        # `key: !!str`) leaves its value pending as surely as a bare one.
        pending =
          Regex.match?(~r/(?::|^\s*-)(?:\s+[&!]\S*)*$/, trimmed) or
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

  # Inside a flow collection a scalar also starts after `[`, `{` or `,`, and
  # directly after a `:`, JSON-style (`{"run":"echo ...`). Every start may
  # carry an anchor or tag before the quote (`"run": &cmd "echo`).
  defp scalar_start?(seen, st) do
    cond do
      Regex.match?(~r/^\s*(?:[&!]\S*\s+)*$/, seen) -> st.pending or st.depth > 0
      Regex.match?(~r/(?:^\s*(?:-\s+)+|:\s+)(?:[&!]\S*\s+)*$/, seen) -> true
      st.depth > 0 -> Regex.match?(~r/[\[{,:]\s*(?:[&!]\S*\s+)*$/, seen)
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

  # A block scalar a `uses:` key holds (`- uses: >-`), or one an anchor names
  # (`BAD: &bad |`), whose content is a single line holding nothing but a
  # reference (`    github/codeql-action/init@<sha>`): YAML resolves the
  # scalar to that reference, so that line is the value. Its index in
  # `lines`, or nil — for any other block scalar, and for one whose content
  # runs over more lines (`>-` folds them into one value, no reference).
  # Inside a block scalar `#` is text, so a reference with a comment after
  # it is no reference either.
  defp block_value(lines, n, col, header) do
    anchored? =
      case Regex.run(~r/(?:^\s*(?:-\s+)*|:\s+)((?:[&!]\S*\s+)*)[|>][-+0-9]*$/, header) do
        [_, props] -> String.contains?(props, "&")
        _ -> false
      end

    if anchored? or
         Regex.match?(~r/^\s*(?:-\s+)*["']?uses["']?\s*:\s+(?:[&!]\S*\s+)*[|>][-+0-9]*$/, header) do
      (n + 1)..(tuple_size(lines) - 1)//1
      |> Stream.map(&{&1, elem(lines, &1)})
      |> Stream.reject(fn {_, l} -> String.trim(l) == "" end)
      |> Stream.take_while(fn {_, l} -> indent_of(l) > col end)
      |> Enum.take(2)
      |> case do
        [{v, l}] -> if Regex.match?(@block_ref_re, l), do: v
        _ -> nil
      end
    end
  end

  defp indent_of(line), do: byte_size(line) - byte_size(String.trim_leading(line, " "))

  # `prefix` is the masked line up to the site's action.
  defp structural?(prefix) do
    cond do
      Regex.match?(~r/^\s*(?:-\s+)?(?:\{\s*)?["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?$/, prefix) ->
        true

      Regex.match?(~r/[{,]\s*["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?$/, prefix) ->
        prefix |> flow_stack() |> structural_flow_stack?(prefix)

      true ->
        false
    end
  end

  # A `uses:` read against the flow collections the lines above opened
  # (`text` is their structure and the line up to the action): the key must
  # stand at an entry position — after `{`, `,` or at the line's start —
  # and the brackets still open must make it a step's or a job's own.
  defp structural_in_flow?(text) do
    Regex.match?(~r/(?:[{,]|\n)\s*(?:-\s+)?["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?$/, text) and
      text |> flow_stack() |> structural_flow_stack?(text)
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

  # Node properties before a bracket name the collection, not its key:
  # `steps: [ &step {` opens an anonymous step, `rust: &job {` a job.
  defp opener_key(seg) do
    seg = Regex.replace(~r/(?:[&!]\S*\s+)+$/, seg, "")

    cond do
      Regex.match?(~r/^\s*(?:-\s*)?$/, seg) -> :none
      m = Regex.run(~r/^\s*(?:-\s+)?["']?([\w.-]+)["']?\s*:\s*$/, seg) -> Enum.at(m, 1)
      true -> :text
    end
  end

  # A `{` item of a sequence is a step when that sequence is `steps:` of a
  # job: the whole stack the text shows (`steps: [ {` on a block job's
  # line, which this reader cannot see past), or one that goes on through a
  # job key into `jobs` (`jobs: { b: { steps: [ {`). The same `steps:`
  # inside any other mapping (`fixture: { steps: [ {` in a matrix) is data.
  # A composite action's steps are those of its root `runs:` (`runs: {
  # steps: [ {`, KYAML or a flow line at column 0): `runs` must be the
  # document's own key, its `{` the outermost bracket or one directly inside
  # a root `{` the text starts with (`text`, from where the collection
  # opened).
  defp structural_flow_stack?(stack, text) do
    Enum.all?(stack, fn {_, key} -> key != :text end) and
      case stack do
        [{"{", :none}] ->
          true

        [{"{", :none}, {"[", key}] when key in ["steps", :none] ->
          true

        [{"{", :none}, {"[", "steps"}, {"{", job}, {"{", "jobs"} | _] when is_binary(job) ->
          true

        [{"{", job}, {"{", "jobs"} | _] when is_binary(job) ->
          true

        [{"{", :none}, {"[", "steps"}, {"{", "runs"}] ->
          Regex.match?(~r/\A["']?runs["']?\s*:/, text)

        [{"{", :none}, {"[", "steps"}, {"{", "runs"}, {"{", :none}] ->
          String.starts_with?(text, "{")

        _ ->
          false
      end
  end

  # `open?` blanks a quoted scalar the line leaves open, its quote too
  # (`structure_lines/1`); `mask/1` keeps that quote.
  defp do_mask(line, out, open? \\ false)

  defp do_mask("", out, _open?), do: out

  defp do_mask(<<q::utf8, rest::binary>>, out, open?) when q in [?", ?'] do
    {body, close, tail} = take_quoted(rest, q, "")

    kept =
      if Regex.match?(~r/^\s*:/, tail) or Regex.match?(@uses_tail_re, out),
        do: body,
        else: String.duplicate("x", byte_size(body))

    if open? and close == "",
      do: out <> String.duplicate("x", 1 + byte_size(body)),
      else: do_mask(tail, out <> <<q::utf8>> <> kept <> close, open?)
  end

  defp do_mask(<<?#, rest::binary>>, out, open?) do
    if out == "" or String.ends_with?(out, [" ", "\t"]),
      do: out,
      else: do_mask(rest, out <> "#", open?)
  end

  defp do_mask(<<c::utf8, rest::binary>>, out, open?),
    do: do_mask(rest, out <> <<c::utf8>>, open?)

  defp do_mask(<<c, rest::binary>>, out, open?), do: do_mask(rest, out <> <<c>>, open?)

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
