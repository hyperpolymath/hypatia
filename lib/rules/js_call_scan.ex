# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.JsCallScan do
  @moduledoc """
  A small JavaScript-aware reader for one question: does a call to a
  process API pass an options object whose own `shell` key is `true` or a
  string? (`js_exec_sync`, hyperpolymath/mathesis#90.)

  A regex cannot answer it: `;` and parentheses inside string or template
  arguments end a regex span early, and a `shell` key inside a nested
  object (`{env: {shell: '/bin/bash'}}`) is not the call's option. This
  reader walks the call's argument list skipping strings, template
  literals (with `${...}` nesting), and `//` and `/* */` comments, tracks
  `()[]{}` depth, and looks only at the top level of each object-literal
  argument.

  Limits, stated rather than guessed at: a regex literal containing a quote
  or bracket can desynchronise the walk; an options object passed through a
  variable is not followed; a call written inside a template literal's
  `${...}` is treated as text, not code, for every form of the rule.
  """

  @spawn_family ~w(spawn spawnSync execFile execFileSync)

  @doc """
  Byte offsets of calls whose options set `shell`. `locals` are extra
  callee names the file binds the spawn family to (`{spawn as run}`).
  Calls are found as `spawn(`, `ns.spawn(`, a local name, or the compiled
  form `(0, ns.spawn)(`, outside strings and comments.
  """
  def shell_option_calls(src, locals \\ []) when is_binary(src) do
    family = Enum.join(@spawn_family, "|")

    local_alt =
      case locals do
        [] -> []
        names -> ["(?<![\\w$.])(?:" <> Enum.map_join(names, "|", &Regex.escape/1) <> ")\\s*\\("]
      end

    source =
      Enum.join(
        [
          "\\b(?:" <> family <> ")\\s*\\(",
          "\\(\\s*0\\s*,\\s*[A-Za-z_$][\\w$]*\\s*\\.\\s*(?:" <> family <> ")\\s*\\)\\s*\\("
          | local_alt
        ],
        "|"
      )

    noncode = noncode_spans(src)

    source
    |> Regex.compile!()
    |> Regex.scan(src, return: :index)
    |> Enum.flat_map(fn [{off, len} | _] ->
      open = off + len - 1

      if not inside?(noncode, off) and shell_option?(src, open), do: [off], else: []
    end)
  end

  # ── the call's arguments ────────────────────────────────────────────────

  # `open` is the byte of the call's `(`.
  defp shell_option?(src, open) do
    case split_top_level(src, open + 1, ?)) do
      {:ok, args, _close} ->
        Enum.any?(args, fn {from, to} ->
          case first_code_byte(src, from, to) do
            {:ok, brace} when brace < to ->
              if :binary.at(src, brace) == ?{, do: object_has_shell?(src, brace), else: false

            _ ->
              false
          end
        end)

      :error ->
        false
    end
  end

  defp object_has_shell?(src, brace) do
    case split_top_level(src, brace + 1, ?}) do
      {:ok, props, _close} ->
        Enum.any?(props, fn {from, to} ->
          # A comment may sit before the key: start at the first code byte.
          case first_code_byte(src, from, to) do
            {:ok, key} ->
              Regex.match?(
                ~r/\A(?:shell|'shell'|"shell")\s*:\s*(?:true\b|['"`])/,
                binary_part(src, key, to - key)
              )

            :none ->
              false
          end
        end)

      :error ->
        false
    end
  end

  # Split the text from `i` up to the matching `closer` (at depth 0) into
  # top-level comma-separated spans. Strings, templates and comments are
  # skipped whole, so their commas and brackets do not count.
  defp split_top_level(src, i, closer), do: split(src, i, closer, 0, i, [])

  defp split(src, i, _closer, _depth, _start, _acc) when i >= byte_size(src), do: :error

  defp split(src, i, closer, depth, start, acc) do
    case :binary.at(src, i) do
      c when c in [?', ?", ?`] ->
        split(src, skip_string(src, i), closer, depth, start, acc)

      ?/ ->
        case comment_end(src, i) do
          nil -> split(src, i + 1, closer, depth, start, acc)
          j -> split(src, j, closer, depth, start, acc)
        end

      c when c in [?(, ?[, ?{] ->
        split(src, i + 1, closer, depth + 1, start, acc)

      c when c in [?), ?], ?}] and depth > 0 ->
        split(src, i + 1, closer, depth - 1, start, acc)

      ^closer when depth == 0 ->
        {:ok, Enum.reverse([{start, i} | acc]), i}

      c when c in [?), ?], ?}] ->
        :error

      ?, when depth == 0 ->
        split(src, i + 1, closer, depth, i + 1, [{start, i} | acc])

      _ ->
        split(src, i + 1, closer, depth, start, acc)
    end
  end

  defp first_code_byte(src, i, to) when i < to do
    case :binary.at(src, i) do
      c when c in [?\s, ?\t, ?\r, ?\n] ->
        first_code_byte(src, i + 1, to)

      ?/ ->
        case comment_end(src, i) do
          nil -> {:ok, i}
          j -> first_code_byte(src, j, to)
        end

      _ ->
        {:ok, i}
    end
  end

  defp first_code_byte(_src, _i, _to), do: :none

  # ── strings, templates, comments ────────────────────────────────────────

  # Index just past the string, template or comment that starts at `i`.
  defp skip_string(src, i) do
    case :binary.at(src, i) do
      ?` -> skip_template(src, i + 1)
      q -> skip_quoted(src, i + 1, q)
    end
  end

  defp skip_quoted(src, i, _q) when i >= byte_size(src), do: i

  defp skip_quoted(src, i, q) do
    case :binary.at(src, i) do
      ?\\ -> skip_quoted(src, i + 2, q)
      ^q -> i + 1
      ?\n -> i + 1
      _ -> skip_quoted(src, i + 1, q)
    end
  end

  defp skip_template(src, i) when i >= byte_size(src), do: i

  defp skip_template(src, i) do
    case :binary.at(src, i) do
      ?\\ ->
        skip_template(src, i + 2)

      ?` ->
        i + 1

      ?$ when i + 1 < byte_size(src) ->
        if :binary.at(src, i + 1) == ?{ do
          case split_top_level(src, i + 2, ?}) do
            {:ok, _, close} -> skip_template(src, close + 1)
            :error -> byte_size(src)
          end
        else
          skip_template(src, i + 1)
        end

      _ ->
        skip_template(src, i + 1)
    end
  end

  # End of a `//` or `/* */` comment starting at `i`, or nil if `/` at `i`
  # does not start one.
  defp comment_end(src, i) do
    if i + 1 < byte_size(src) do
      case :binary.at(src, i + 1) do
        ?/ ->
          case :binary.match(src, "\n", scope: {i, byte_size(src) - i}) do
            {nl, _} -> nl + 1
            :nomatch -> byte_size(src)
          end

        ?* ->
          case :binary.match(src, "*/", scope: {i + 2, byte_size(src) - i - 2}) do
            {e, _} -> e + 2
            :nomatch -> byte_size(src)
          end

        _ ->
          nil
      end
    end
  end

  @doc """
  The offsets in `offsets` that are code, not inside a string, template
  literal or comment of `src`.
  """
  def code_offsets(src, offsets) when is_binary(src) do
    spans = noncode_spans(src)
    Enum.reject(offsets, &inside?(spans, &1))
  end

  # Spans of the file that are strings, templates or comments.
  defp noncode_spans(src), do: noncode(src, 0, [])

  defp noncode(src, i, acc) when i >= byte_size(src), do: Enum.reverse(acc)

  defp noncode(src, i, acc) do
    case :binary.at(src, i) do
      c when c in [?', ?", ?`] ->
        j = skip_string(src, i)
        noncode(src, j, [{i, j} | acc])

      ?/ ->
        case comment_end(src, i) do
          nil -> noncode(src, i + 1, acc)
          j -> noncode(src, j, [{i, j} | acc])
        end

      _ ->
        noncode(src, i + 1, acc)
    end
  end

  defp inside?(spans, off), do: Enum.any?(spans, fn {a, b} -> off >= a and off < b end)
end
