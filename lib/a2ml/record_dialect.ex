# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.A2ml.RecordDialect do
  @moduledoc """
  A reference reader for the **A2ML record dialect** — the TOML-like
  `[section]` / `key = value` surface specified in
  `hyperpolymath/standards` `a2ml/RECORD-DIALECT-SPEC.adoc`.

  This is the "reference reader" that spec's Appendix D calls for (a second
  front-end over the same document model, not a second model), and it is what
  lets hypatia *consume* record-dialect artefacts directly — first among them
  the RSR v2.0 criteria SSOT (`Hypatia.Rules.RsrCriteria`).

  ## What it parses

    * `[section]` and dotted `[a.b.c]` tables → nested maps
    * `[[section]]` arrays-of-tables → a list appended under the key
    * `key = value` with kebab/snake keys
    * scalar values: double-quoted strings (incl. `\"\"\"triple\"\"\"`),
      integers, floats, booleans
    * `[ ... ]` arrays (multi-line, trailing comma, embedded comments) and
      `{ k = v }` inline tables, nested arbitrarily
    * `#` comments — full-line and trailing (outside strings)

  Deliberately faithful to the spec's divergences from TOML: strings are
  **double-quoted only**, and there is **no native date type** (dates are
  strings). Returns `{:ok, tree}` where `tree` is a map, or `{:error, reason}`.
  It never raises on malformed input — it reports.
  """

  @doc "Parse record-dialect `text` into a nested map (the record tree)."
  @spec parse(String.t()) :: {:ok, map()} | {:error, term()}
  def parse(text) when is_binary(text) do
    text
    |> logical_lines()
    |> Enum.reduce_while({:ok, %{}, []}, fn line, {:ok, tree, ctx} ->
      case classify(line) do
        {:section, path} ->
          {:cont, {:ok, ensure_path(tree, path, :table), path}}

        {:array_section, path} ->
          {:cont, {:ok, append_array_table(tree, path), path ++ [:__last__]}}

        {:kv, key, raw} ->
          case parse_value(raw) do
            {:ok, val} -> {:cont, {:ok, put_in_ctx(tree, ctx, key, val), ctx}}
            {:error, why} -> {:halt, {:error, {:bad_value, key, why}}}
          end

        :blank ->
          {:cont, {:ok, tree, ctx}}

        {:error, why} ->
          {:halt, {:error, why}}
      end
    end)
    |> case do
      {:ok, tree, _ctx} -> {:ok, tree}
      {:error, _} = e -> e
    end
  end

  # --- structure: fold physical lines into logical ones ---------------------

  # A logical line is a section header or a full `key = value` whose value may
  # span multiple physical lines when it opens an unclosed `[` or `{`.
  defp logical_lines(text) do
    text
    |> String.split("\n")
    |> Enum.reduce({[], nil}, fn raw, {acc, pending} ->
      case pending do
        nil ->
          stripped = strip_line_comment(raw)

          cond do
            String.trim(stripped) == "" -> {acc, nil}
            balanced?(stripped) -> {[stripped | acc], nil}
            # value spans further lines — start accumulating
            true -> {acc, stripped}
          end

        buf ->
          # inside a multi-line value: keep raw (comments inside arrays are ok
          # to drop, but keep structure). Strip trailing comments per line.
          joined = buf <> "\n" <> strip_line_comment(raw)
          if balanced?(joined), do: {[joined | acc], nil}, else: {acc, joined}
      end
    end)
    |> then(fn {acc, pending} -> Enum.reverse(if pending, do: [pending | acc], else: acc) end)
  end

  defp classify(line) do
    t = String.trim(line)

    cond do
      t == "" ->
        :blank

      match = Regex.run(~r/^\[\[\s*([A-Za-z0-9_.\-]+)\s*\]\]$/, t) ->
        {:array_section, split_key_path(Enum.at(match, 1))}

      match = Regex.run(~r/^\[\s*([A-Za-z0-9_.\-]+)\s*\]$/, t) ->
        {:section, split_key_path(Enum.at(match, 1))}

      match = Regex.run(~r/^([A-Za-z0-9_\-]+)\s*=\s*(.*)$/s, t) ->
        [_, key, raw] = match
        {:kv, key, String.trim(raw)}

      true ->
        {:error, {:unparseable_line, String.slice(t, 0, 60)}}
    end
  end

  defp split_key_path(s), do: String.split(s, ".")

  # --- value parsing (recursive over a value string) ------------------------

  @doc false
  @spec parse_value(String.t()) :: {:ok, term()} | {:error, term()}
  def parse_value(raw) do
    s = String.trim(raw)

    cond do
      s == "" -> {:ok, ""}
      String.starts_with?(s, "\"\"\"") -> {:ok, strip_triple(s)}
      String.starts_with?(s, "\"") -> {:ok, unescape_string(s)}
      String.starts_with?(s, "[") -> parse_array(s)
      String.starts_with?(s, "{") -> parse_inline_table(s)
      s in ["true", "false"] -> {:ok, s == "true"}
      Regex.match?(~r/^[+-]?\d[\d_]*$/, s) -> {:ok, String.to_integer(String.replace(s, "_", ""))}
      Regex.match?(~r/^[+-]?\d[\d_]*\.\d+([eE][+-]?\d+)?$/, s) -> {:ok, parse_float(s)}
      true -> {:error, {:unquoted_scalar, String.slice(s, 0, 40)}}
    end
  end

  defp parse_array(s) do
    with {:ok, inner} <- strip_delims(s, "[", "]") do
      inner
      |> split_top_level(",")
      |> Enum.map(&strip_line_comment/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> reduce_ok(&parse_value/1)
    end
  end

  defp parse_inline_table(s) do
    with {:ok, inner} <- strip_delims(s, "{", "}") do
      inner
      |> split_top_level(",")
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> reduce_ok(fn pair ->
        case Regex.run(~r/^([A-Za-z0-9_\-]+)\s*=\s*(.*)$/s, pair) do
          [_, k, v] -> with {:ok, val} <- parse_value(String.trim(v)), do: {:ok, {k, val}}
          _ -> {:error, {:bad_inline_pair, String.slice(pair, 0, 40)}}
        end
      end)
      |> case do
        {:ok, pairs} -> {:ok, Map.new(pairs)}
        e -> e
      end
    end
  end

  # Split `inner` on `delim` at bracket-depth 0, ignoring delims inside strings
  # and nested [] / {}. Newlines are treated as whitespace.
  defp split_top_level(inner, delim) do
    graphemes = String.graphemes(inner)
    do_split(graphemes, delim, 0, false, "", [])
  end

  defp do_split([], _delim, _depth, _instr, cur, acc), do: Enum.reverse([cur | acc])

  defp do_split([c | rest], delim, depth, instr, cur, acc) do
    cond do
      instr ->
        # inside a string; only a non-escaped quote closes it
        instr2 = not (c == "\"" and not String.ends_with?(cur, "\\"))
        do_split(rest, delim, depth, instr2, cur <> c, acc)

      c == "\"" ->
        do_split(rest, delim, depth, true, cur <> c, acc)

      c in ["[", "{"] ->
        do_split(rest, delim, depth + 1, false, cur <> c, acc)

      c in ["]", "}"] ->
        do_split(rest, delim, depth - 1, false, cur <> c, acc)

      c == delim and depth == 0 ->
        do_split(rest, delim, 0, false, "", [cur | acc])

      true ->
        do_split(rest, delim, depth, false, cur <> c, acc)
    end
  end

  # --- tree building --------------------------------------------------------

  defp ensure_path(tree, [], _kind), do: tree

  defp ensure_path(tree, [k | rest], kind) do
    child = Map.get(tree, k, %{})
    child = if is_map(child), do: child, else: %{}
    Map.put(tree, k, ensure_path(child, rest, kind))
  end

  # For [[path]], append a fresh table to the list living at `path`.
  defp append_array_table(tree, path) do
    update_at(tree, path, fn
      list when is_list(list) -> list ++ [%{}]
      _ -> [%{}]
    end)
  end

  defp update_at(tree, [k], fun), do: Map.put(tree, k, fun.(Map.get(tree, k)))

  defp update_at(tree, [k | rest], fun) do
    child = Map.get(tree, k, %{})
    Map.put(tree, k, update_at(child, rest, fun))
  end

  # Put `key => val` at the current context. When ctx ends in :__last__ the
  # context is the last element of an array-of-tables at that path.
  defp put_in_ctx(tree, [], key, val), do: Map.put(tree, key, val)

  defp put_in_ctx(tree, ctx, key, val) do
    case List.last(ctx) do
      :__last__ ->
        path = Enum.drop(ctx, -1)

        update_at(tree, path, fn list ->
          {front, [last]} = Enum.split(list, -1)
          front ++ [Map.put(last, key, val)]
        end)

      _ ->
        update_at(tree, ctx, fn m -> Map.put(m || %{}, key, val) end)
    end
  end

  # --- small helpers --------------------------------------------------------

  # Strip a trailing ` # comment` that is not inside a double-quoted string.
  defp strip_line_comment(line) do
    do_strip_comment(String.graphemes(line), false, "")
  end

  defp do_strip_comment([], _instr, acc), do: String.trim_trailing(acc)

  defp do_strip_comment(["\"" | rest], instr, acc),
    do: do_strip_comment(rest, not instr, acc <> "\"")

  defp do_strip_comment(["#" | _rest], false, acc), do: String.trim_trailing(acc)
  defp do_strip_comment([c | rest], instr, acc), do: do_strip_comment(rest, instr, acc <> c)

  # A line/buffer is "balanced" when its [] and {} nest to zero outside strings.
  defp balanced?(s) do
    do_balance(String.graphemes(s), 0, false)
  end

  defp do_balance([], depth, _instr), do: depth <= 0
  defp do_balance(["\"" | rest], depth, instr), do: do_balance(rest, depth, not instr)
  defp do_balance([_ | rest], depth, true), do: do_balance(rest, depth, true)

  defp do_balance([c | rest], depth, false) when c in ["[", "{"],
    do: do_balance(rest, depth + 1, false)

  defp do_balance([c | rest], depth, false) when c in ["]", "}"],
    do: do_balance(rest, depth - 1, false)

  defp do_balance([_ | rest], depth, false), do: do_balance(rest, depth, false)

  defp strip_delims(s, open, close) do
    t = String.trim(s)

    if String.starts_with?(t, open) and String.ends_with?(t, close) do
      {:ok, t |> String.slice(1, String.length(t) - 2)}
    else
      {:error, {:unbalanced, open, close}}
    end
  end

  defp strip_triple(s) do
    s |> String.trim() |> String.trim_leading("\"\"\"") |> String.trim_trailing("\"\"\"")
  end

  defp unescape_string(s) do
    s
    |> String.trim()
    |> String.trim_leading("\"")
    |> String.trim_trailing("\"")
    |> String.replace("\\\"", "\"")
    |> String.replace("\\\\", "\\")
  end

  defp parse_float(s), do: s |> String.replace("_", "") |> String.to_float()

  # map with short-circuit on the first {:error, _}
  defp reduce_ok(list, fun) do
    Enum.reduce_while(list, {:ok, []}, fn item, {:ok, acc} ->
      case fun.(item) do
        {:ok, v} -> {:cont, {:ok, [v | acc]}}
        {:error, _} = e -> {:halt, e}
      end
    end)
    |> case do
      {:ok, acc} -> {:ok, Enum.reverse(acc)}
      e -> e
    end
  end
end
