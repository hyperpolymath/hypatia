# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.WorkflowYaml do
  @moduledoc """
  The workflow reader shared by the rules that look inside GitHub Actions
  workflows: it reads a workflow the way YAML (and GitHub) does, as far as the
  line- and indent-based rules need, while every line keeps its physical line
  number so a finding points at the source.

  It was RE001's reader (`Hypatia.Rules.ResearchExtensions`) and moved here
  unchanged so other rules (WH013 first) read workflows the same way instead
  of each carrying its own approximation.

    * `read_workflow/1` and `flow_to_block/1` read a KYAML (flow-style)
      workflow as the equivalent block layout, one line for one line.
    * `active_lines/1` numbers the lines, drops comment lines and expands
      YAML anchors and aliases (`expand_aliases/1`).
    * `workflow_job_lines/1` splits the lines under `jobs:` into jobs,
      `job_steps/1` a job into its steps, and `step_key_column/1` gives the
      column of a step's own keys.
    * `key_line?/2`, `direct_job_field?/2`, `job_field_indent/1`,
      `strip_yaml_comment/2`, `nesting_at/2` and `indent_of/1` are the
      primitives those are built from.

  It is a static text reader, not a YAML parser: it covers the shapes GitHub
  workflows use, and it stays bounded on hostile input (alias expansion stops
  after a fixed number of copied lines).
  """

  @doc """
  The workflow's lines as `{line, line_number}`, without comment lines, and
  with YAML aliases expanded in place (each expanded line carries the alias's
  line number). Physical line numbers are kept, so a finding never points at
  a line that a header comment shifted.
  """
  def active_lines(content) when is_binary(content) do
    content
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.reject(fn {line, _} -> String.starts_with?(String.trim_leading(line), "#") end)
    |> expand_aliases()
  end

  # YAML anchors and aliases, which GitHub expands in workflows: `*name`
  # stands for the node anchored `&name`, so `env: *common`, `run: *script`,
  # `<<: *defaults` or `deploy: *build` brings the anchored node into the job
  # that holds the alias. Each alias is expanded in place before any job is
  # read, and every line it brings carries the alias's line number, so a
  # finding points at the alias:
  #
  #   * an alias that is a key's whole value, or a list item, is replaced by
  #     what follows its anchor on the anchor's line (a scalar, a block
  #     scalar's `|`, a flow collection), and the anchor's indented lines
  #     follow at the column of the `*`;
  #   * a `<<:` merge puts the anchored mapping's entries into the mapping
  #     that holds the key, at the key's column, minus every entry whose key
  #     that mapping sets itself: YAML gives the explicit key precedence;
  #   * an alias inside a flow collection is followed by a copy of the
  #     anchored lines, deeper than the line.
  #
  # Anchors and aliases are recognised only where a node starts, outside
  # quoted scalars and block-scalar text, so `echo "x *common"` and a `run: |`
  # line `ls *common` stay text. The anchor is the nearest one of that name
  # above the alias.
  @anchor ~r/(?:^\s*(?:-\s+)*|:\s+|[\[{,]\s*)&([A-Za-z0-9_-]+)(?=\s|$)/
  @alias ~r/(?:^\s*(?:-\s+)*|:\s+|[\[{,]\s*)\*([A-Za-z0-9_-]+)\s*(?=$|[,\]}])/
  @merge_key ~r/^(\s*(?:-\s+)?)(?:<<|"<<"|'<<')\s*:/
  @mapping_key ~r/^\s*(?:-\s+)?(?:"([^"]*)"|'([^']*)'|([^\s"'#:][^:]*?))\s*:(?:\s|$)/
  @block_scalar_start ~r/(?:^\s*|[:-]\s+)(?:&[A-Za-z0-9_-]+\s+)?[>|][0-9+-]*\s*$/
  @block_scalar_header ~r/^[>|][0-9+-]*$/
  # Nested anchors can multiply copies exponentially (the "billion laughs"
  # file), so expansion stops after this many copied lines per workflow.
  @alias_budget 5_000
  def expand_aliases(lines) do
    codes = yaml_codes(lines)

    anchors =
      for {{{line, n}, code}, index} <- lines |> Enum.zip(codes) |> Enum.with_index(),
          code != nil,
          [_, {at, len}] <- Regex.scan(@anchor, code, return: :index),
          node_start?(code, at) do
        {binary_part(code, at, len), n,
         anchored(lines, index, strip_yaml_comment(line), at + len)}
      end

    # Only what jobs read is expanded in place: the jobs and the top-level
    # env:. An anchor kept elsewhere is expanded where a job uses it.
    {expanded, _} =
      lines
      |> Enum.zip(codes)
      |> Enum.with_index()
      |> Enum.flat_map_reduce({nil, @alias_budget}, fn {{{line, _} = entry, code}, index},
                                                       {section, budget} ->
        section =
          case Regex.run(~r/^["']?([^\s"':]+)["']?\s*:/, line) do
            [_, key] -> key
            nil -> section
          end

        if section in ["jobs", "env"] do
          {out, budget} = expand_line(entry, code, lines, index, anchors, budget)
          {out, {section, budget}}
        else
          {[entry], {section, budget}}
        end
      end)

    expanded
  end

  defp expand_aliases(lines, anchors, budget) do
    lines
    |> Enum.zip(yaml_codes(lines))
    |> Enum.with_index()
    |> Enum.flat_map_reduce(budget, fn {{entry, code}, index}, budget ->
      expand_line(entry, code, lines, index, anchors, budget)
    end)
  end

  # One line with its aliases expanded. A line of block-scalar text (code nil)
  # holds no alias.
  defp expand_line(entry, nil, _lines, _index, _anchors, budget), do: {[entry], budget}

  defp expand_line(entry, _code, _lines, _index, _anchors, budget) when budget <= 0,
    do: {[entry], budget}

  defp expand_line({line, n} = entry, code, lines, index, anchors, budget) do
    matches =
      for [_, {at, len}] <- Regex.scan(@alias, code, return: :index),
          node_start?(code, at),
          name = binary_part(code, at, len),
          {_, _, node} <-
            anchors |> Enum.filter(fn {a, m, _} -> a == name and m < n end) |> Enum.take(-1),
          do: {at, len, node}

    # The alias that ends the line is the whole value of its key or item.
    {value, in_flow} =
      case List.last(matches) do
        {at, len, _} = last ->
          if String.trim(binary_part(code, at + len, byte_size(code) - at - len)) == "",
            do: {last, Enum.drop(matches, -1)},
            else: {nil, matches}

        nil ->
          {nil, []}
      end

    {flow_copies, budget} =
      Enum.flat_map_reduce(in_flow, budget, fn {at, _, {inline, block}}, budget ->
        indent = indent_of(code)
        inline_lines = if inline == "", do: [], else: [{indent_spaces(indent + 2) <> inline, n}]
        {copy, budget} = anchored_block(inline, inline_lines ++ block, anchors, budget)
        {renumber(reindent(copy, column(code, at - 1)), n), budget}
      end)

    case value do
      nil ->
        {[entry | flow_copies], budget}

      {at, _, {inline, block}} ->
        text = strip_yaml_comment(line)
        head = String.trim_trailing(binary_part(text, 0, at - 1) <> inline)
        {block, budget} = anchored_block(inline, block, anchors, budget - 1)

        block =
          case Regex.run(@merge_key, code) do
            [_, prefix] ->
              merge_column = String.length(prefix)

              block
              |> reindent(merge_column)
              |> drop_entries(merge_column, sibling_keys(lines, index, merge_column))

            nil ->
              reindent(block, column(code, at - 1))
          end

        # The inline part may hold aliases of its own (`&a [*b, *c]`).
        {head_lines, budget} =
          if inline == "" or Regex.match?(@block_scalar_header, inline),
            do: {[{head, n}], budget},
            else: expand_line({head, n}, yaml_code(head), [{head, n}], 0, anchors, budget)

        {head_lines ++ renumber(block, n) ++ flow_copies, budget}
    end
  end

  # An anchor's indented lines, their own aliases expanded. Under a block
  # scalar's header they are text and are copied as they are.
  defp anchored_block(inline, block, anchors, budget) do
    if Regex.match?(@block_scalar_header, inline),
      do: {block, budget - length(block)},
      else: expand_aliases(block, anchors, budget - length(block))
  end

  # The node an anchor names: what follows the anchor on its own line, and
  # every line indented deeper than that line.
  defp anchored(lines, index, text, after_name) do
    inline = text |> binary_part(after_name, byte_size(text) - after_name) |> String.trim()
    indent = indent_of(text)

    block =
      lines
      |> Enum.drop(index + 1)
      |> Enum.take_while(fn {line, _} -> String.trim(line) == "" or indent_of(line) > indent end)

    {inline, block}
  end

  # The keys the mapping holding line `index` sets at `column` itself.
  defp sibling_keys(lines, index, column) do
    {before, [_ | after_line]} = Enum.split(lines, index)

    in_mapping = fn ls ->
      Enum.take_while(ls, fn {line, _} -> String.trim(line) == "" or indent_of(line) >= column end)
    end

    (in_mapping.(Enum.reverse(before)) ++ in_mapping.(after_line))
    |> Enum.filter(fn {line, _} -> indent_of(line) == column end)
    |> Enum.flat_map(fn {line, _} -> mapping_key(line) end)
    |> Enum.reject(&(&1 == "<<"))
  end

  # Merged entries, less those whose key the destination mapping sets.
  defp drop_entries(lines, column, keys) do
    lines
    |> Enum.chunk_while(
      [],
      fn {line, _} = entry, acc ->
        if indent_of(line) == column and String.trim(line) != "" and acc != [],
          do: {:cont, Enum.reverse(acc), [entry]},
          else: {:cont, [entry | acc]}
      end,
      fn
        [] -> {:cont, []}
        acc -> {:cont, Enum.reverse(acc), []}
      end
    )
    |> Enum.reject(fn [{first, _} | _] -> Enum.any?(mapping_key(first), &(&1 in keys)) end)
    |> Enum.concat()
  end

  defp mapping_key(line) do
    case Regex.run(@mapping_key, strip_yaml_comment(line)) do
      nil -> []
      captures -> captures |> tl() |> Enum.reject(&(&1 == "")) |> Enum.take(1)
    end
  end

  # Each line's YAML structure, for finding anchors and aliases: the line
  # without its comment and with quoted scalars blanked, byte offsets kept;
  # nil for a line of block-scalar text.
  defp yaml_codes(lines) do
    {codes, _} =
      Enum.map_reduce(lines, nil, fn {line, _}, scalar_indent ->
        if scalar_indent && (String.trim(line) == "" or indent_of(line) > scalar_indent) do
          {nil, scalar_indent}
        else
          code = yaml_code(line)
          {code, if(Regex.match?(@block_scalar_start, code), do: indent_of(line))}
        end
      end)

    codes
  end

  defp yaml_code(line), do: strip_yaml_comment(line, true)

  # A `,` separates nodes only inside a flow collection; in a plain scalar
  # (`run: echo a, *b`) it is text. The name starts at `at`, after `*` or `&`.
  defp node_start?(code, at) do
    before = code |> binary_part(0, at - 1) |> String.trim_trailing()
    not String.ends_with?(before, ",") or nesting_at(code, at) > 0
  end

  defp column(text, byte_offset), do: String.length(binary_part(text, 0, byte_offset))
  defp indent_spaces(n), do: String.duplicate(" ", n)
  defp renumber(lines, n), do: Enum.map(lines, fn {line, _} -> {line, n} end)

  defp reindent(lines, target) do
    base =
      lines
      |> Enum.map(&elem(&1, 0))
      |> Enum.reject(&(String.trim(&1) == ""))
      |> Enum.map(&indent_of/1)
      |> Enum.min(fn -> 0 end)

    Enum.map(lines, fn {line, n} ->
      if String.trim(line) == "",
        do: {"", n},
        else: {indent_spaces(indent_of(line) - base + target) <> String.trim_leading(line), n}
    end)
  end

  # The column of a step's own properties: just past `- ` on its opening line,
  # or the indent of the first property under a bare `-`. A `-` followed only
  # by a comment (`-   # publish`) is a bare `-`: YAML ignores the comment, so
  # the step's keys start on the next line.
  def step_key_column([{first, _} | rest]) do
    case Regex.run(~r/^(\s*-\s+)[^\s#]/, first) do
      [_, prefix] ->
        String.length(prefix)

      _ ->
        Enum.find_value(rest, indent_of(first) + 2, fn {line, _} ->
          String.trim(line) != "" && indent_of(line)
        end)
    end
  end

  def step_key_column([]), do: 0

  # The job's steps, each as its own physical lines.
  def job_steps(job_lines) do
    with field_indent when is_integer(field_indent) <- job_field_indent(job_lines),
         step_lines when step_lines != [] <- lines_in_steps(job_lines, field_indent),
         step_indent when is_integer(step_indent) <- step_indent(step_lines, field_indent) do
      step_lines
      |> Enum.chunk_while(
        [],
        fn {line, _} = entry, acc ->
          if indent_of(line) == step_indent and Regex.match?(~r/^\s*-(?:\s|$)/, line) and
               acc != [] do
            {:cont, Enum.reverse(acc), [entry]}
          else
            {:cont, [entry | acc]}
          end
        end,
        fn
          [] -> {:cont, []}
          acc -> {:cont, Enum.reverse(acc), []}
        end
      )
    else
      _ -> []
    end
  end

  def nesting_at(text, position) do
    text
    |> binary_part(0, position)
    |> String.graphemes()
    |> Enum.reduce(0, fn
      c, depth when c in ["{", "["] -> depth + 1
      c, depth when c in ["}", "]"] -> depth - 1
      _, depth -> depth
    end)
  end

  def direct_job_field?(job_lines, field) do
    case job_field_indent(job_lines) do
      nil ->
        false

      field_indent ->
        Enum.any?(job_lines, fn {line, _} ->
          indent_of(line) == field_indent and key_line?(line, field)
        end)
    end
  end

  # A line that opens the mapping key `key`: YAML reads `env:`, `"env":` and
  # `'env':` as the same key, and so does GitHub.
  def key_line?(line, key) do
    key = Regex.escape(key)
    Regex.match?(~r/^\s*(?:#{key}|"#{key}"|'#{key}')\s*:(?:\s|$)/, line)
  end

  def job_field_indent([_header | job_lines]) do
    job_lines
    |> Enum.map(&elem(&1, 0))
    |> Enum.reject(&(String.trim(&1) == ""))
    |> Enum.map(&indent_of/1)
    |> Enum.min(fn -> nil end)
  end

  def job_field_indent([]), do: nil

  # The lines of the job's `steps:` value. Its items may sit deeper than the
  # key or, as an indentless sequence YAML also allows, at the key's own
  # column: `steps:` then `- run: …` directly beneath it.
  def lines_in_steps(job_lines, field_indent) do
    case Enum.find_index(job_lines, fn {line, _} ->
           indent_of(line) == field_indent and key_line?(line, "steps")
         end) do
      nil ->
        []

      index ->
        job_lines
        |> Enum.drop(index + 1)
        |> Enum.take_while(fn {line, _} ->
          String.trim(line) == "" or indent_of(line) > field_indent or
            (indent_of(line) == field_indent and sequence_item?(line))
        end)
    end
  end

  # The column of the step items: the shallowest `-` in the steps value, which
  # is the key's own column for an indentless sequence.
  def step_indent(step_lines, field_indent) do
    step_lines
    |> Enum.filter(fn {line, _} ->
      indent_of(line) >= field_indent and sequence_item?(line)
    end)
    |> Enum.map(fn {line, _} -> indent_of(line) end)
    |> Enum.min(fn -> nil end)
  end

  defp sequence_item?(line), do: Regex.match?(~r/^\s*-(?:\s|$)/, line)

  # A line without its YAML comment: a `#` at the start or after whitespace,
  # outside a quoted scalar. A quote opens a scalar only where one can start
  # (after whitespace or a flow indicator), so the apostrophe in `it's` does not.
  # With `blank_quoted`, each quoted scalar's text becomes spaces of the same
  # byte length, so what remains is structure at its original offsets.
  def strip_yaml_comment(line, blank_quoted \\ false) do
    text = fn c -> if blank_quoted, do: String.duplicate(" ", byte_size(c)), else: c end

    line
    |> String.graphemes()
    |> Enum.reduce_while({[], nil, " ", false}, fn c, {acc, quote, prev, escaped} ->
      cond do
        quote == "\"" and escaped ->
          {:cont, {[text.(c) | acc], quote, c, false}}

        quote == "\"" and c == "\\" ->
          {:cont, {[text.(c) | acc], quote, c, true}}

        quote != nil and c == quote ->
          {:cont, {[c | acc], nil, c, false}}

        quote != nil ->
          {:cont, {[text.(c) | acc], quote, c, false}}

        c == "#" and String.trim(prev) == "" ->
          {:halt, {acc, nil, c, false}}

        c in ["'", "\""] and (String.trim(prev) == "" or prev in ["{", "[", ","]) ->
          {:cont, {[c | acc], c, c, false}}

        true ->
          {:cont, {[c | acc], nil, c, false}}
      end
    end)
    |> elem(0)
    |> Enum.reverse()
    |> Enum.join()
    |> String.trim_trailing()
  end

  # Follow block-style jobs by indentation, retaining physical source lines.
  # As with the other research rules, this is a local static text analysis.
  def workflow_job_lines(lines) do
    {_in_jobs, _indent, groups} =
      Enum.reduce(lines, {false, nil, []}, fn {line, _} = entry, {in_jobs, indent, groups} ->
        cond do
          Regex.match?(~r/^(?:jobs|"jobs"|'jobs')\s*:\s*(?:#.*)?$/, line) ->
            {true, nil, groups}

          not in_jobs ->
            {false, indent, groups}

          Regex.match?(~r/^\S/, line) ->
            {false, nil, groups}

          true ->
            header = Regex.run(~r/^(\s+)(?:[A-Za-z0-9_-]+|"[^"]+"|'[^']+'):/, line)
            width = if header, do: String.length(Enum.at(header, 1)), else: nil

            cond do
              width && (is_nil(indent) || width == indent) ->
                {true, width, [[entry] | groups]}

              groups != [] ->
                [current | rest] = groups
                {true, indent, [[entry | current] | rest]}

              true ->
                {true, indent, groups}
            end
        end
      end)

    groups |> Enum.reverse() |> Enum.map(&Enum.reverse/1)
  end

  # Read a workflow for the line- and indent-based rules. A KYAML workflow
  # (YAML-POLICY Y-3: the estate's base form, `yq -o kyaml`) is rewritten to
  # the equivalent block layout one line for one line, so physical line
  # numbers in findings still point at the source. Block YAML is returned
  # byte-identical.
  @doc false
  def read_workflow(path), do: path |> File.read!() |> flow_to_block()

  # Rewrite a KYAML document to block layout, preserving the line count:
  # the root `{` and every closer become blank lines, `key: {`/`key: [`
  # lose their opener, the first key of a `{` sequence item and each scalar
  # sequence item gain `- `, trailing commas go, and plain double-quoted
  # scalars are unquoted. Anything whose first significant line is not a
  # bare `{` is not KYAML and passes through untouched — block YAML's own
  # `on: [push]` and `permissions: {}` are never rewritten.
  @doc false
  def flow_to_block(content) when is_binary(content) do
    lines = String.split(content, "\n")

    if kyaml_root?(lines) do
      {out, _stack, _dash} = Enum.reduce(lines, {[], [], false}, &flow_line/2)
      out |> Enum.reverse() |> Enum.join("\n")
    else
      content
    end
  end

  # True when the first line that is neither blank nor a comment is a bare `{`.
  defp kyaml_root?(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.find(&(&1 != "" and not String.starts_with?(&1, "#")))
    |> Kernel.==("{")
  end

  # Rewrite one KYAML line. The stack holds the open collections (`:map` or
  # `:seq`); `dash` marks that the next property opens a sequence item.
  defp flow_line(line, {out, stack, dash}) do
    trimmed = String.trim(line)

    if trimmed == "" or String.starts_with?(trimmed, "#") do
      {[dedent(line) | out], stack, dash}
    else
      {head, comment} =
        case Regex.run(~r/^(.*?),?(\s+#[^"]*)?$/, String.trim_trailing(line)) do
          [_, head, comment] -> {head, comment}
          [_, head] -> {head, ""}
        end

      indent = String.duplicate(" ", indent_of(head))
      body = String.trim(head)
      in_seq? = match?([:seq | _], stack)

      cond do
        body in ["}", "]"] ->
          {["" | out], Enum.drop(stack, 1), false}

        body == "{" ->
          {["" | out], [:map | stack], in_seq?}

        String.ends_with?(body, ": {") or String.ends_with?(body, ": [") ->
          kind = if String.ends_with?(body, "{"), do: :map, else: :seq
          key = String.slice(body, 0..-3//1)
          {[emit(indent, key, comment, dash) | out], [kind | stack], false}

        in_seq? ->
          item = Regex.replace(~r/^\{\s*(.*?)\s*\}$/, body, "\\1")
          {[emit(indent <> "  ", unquote_scalar(item), comment, true) | out], stack, false}

        true ->
          {[emit(indent, unquote_scalar(body), comment, dash) | out], stack, false}
      end
    end
  end

  # Assemble one block-layout line, two columns left of the KYAML one, with a
  # `- ` sequence marker in the two columns just above the key when `dash`.
  defp emit(indent, body, comment, dash) do
    prefix = if dash, do: String.slice(indent, 2..-1//1) <> "- ", else: indent
    dedent(prefix <> body <> (comment || ""))
  end

  # Drop the two columns of indentation the KYAML root mapping adds.
  defp dedent("  " <> rest), do: rest
  defp dedent(line), do: line

  # Unquote a `key: "value"` or bare `"value"` whose value holds no quote or
  # escape; anything else keeps its quotes.
  defp unquote_scalar(body) do
    Regex.replace(~r/^((?:[^\s"':]+:\s+)?)"([^"\\]*)"$/, body, "\\1\\2")
  end

  def indent_of(line) do
    case Regex.run(~r/^(\s*)/, line) do
      [_, ws] -> String.length(ws)
      _ -> 0
    end
  end
end
