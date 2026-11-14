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
  `${...}` is treated as text, not code, for every form of the rule; each
  spawn-family call site re-reads its own argument list, so the cost is
  quadratic in how deeply such calls nest inside one another's arguments
  (real code does not nest them deeply, and only those four callees are
  candidates).
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

    hits =
      source
      |> Regex.compile!()
      |> Regex.scan(src, return: :index)
      |> Map.new(fn [{off, len} | _] -> {off, off + len - 1} end)

    src
    |> code_offsets(Map.keys(hits))
    |> Enum.filter(&shell_option?(src, Map.fetch!(hits, &1)))
  end

  # ── the call's arguments ────────────────────────────────────────────────

  # `open` is the byte of the call's `(`.
  defp shell_option?(src, open) do
    case split_top_level(src, open + 1, ?)) do
      {:ok, args, _close} ->
        Enum.any?(args, fn {from, to} ->
          case object_start(src, from, to) do
            {:ok, brace} -> object_has_shell?(src, brace)
            :none -> false
          end
        end)

      :error ->
        false
    end
  end

  # The `{` of an argument that is an object literal, stepping over wrapping
  # parentheses (`({shell: true})`) and the whitespace and comments around
  # them. The object (or the parenthesised one) must be the whole argument:
  # in a comma expression `({shell: true}, {shell: false})` the value is the
  # last operand, so it is not read as this object.
  defp object_start(src, from, to) do
    case first_code_byte(src, from, to) do
      {:ok, i} when i < to ->
        case :binary.at(src, i) do
          ?{ ->
            case split_top_level(src, i + 1, ?}) do
              {:ok, _props, close} -> if trivia?(src, close + 1, to), do: {:ok, i}, else: :none
              :error -> :none
            end

          ?( ->
            case split_top_level(src, i + 1, ?)) do
              {:ok, [{a, b}], close} ->
                if trivia?(src, close + 1, to), do: object_start(src, a, b), else: :none

              _ ->
                :none
            end

          _ ->
            :none
        end

      _ ->
        :none
    end
  end

  # Only whitespace and comments from `from` up to `to`.
  defp trivia?(src, from, to), do: first_code_byte(src, from, to) == :none

  defp object_has_shell?(src, brace) do
    case split_top_level(src, brace + 1, ?}) do
      {:ok, props, _close} ->
        Enum.any?(props, fn {from, to} ->
          # A comment may sit before the key: start at the first code byte.
          case first_code_byte(src, from, to) do
            {:ok, key} ->
              text = binary_part(src, key, to - key)

              if String.starts_with?(text, "...") do
                # An inline spread `...{shell: true}` contributes its own
                # top-level keys.
                case object_start(src, key + 3, to) do
                  {:ok, inner} -> object_has_shell?(src, inner)
                  :none -> false
                end
              else
                Regex.match?(
                  ~r/\A(?:shell|'shell'|"shell"|\[\s*(['"`])shell\1\s*\])\s*:\s*(?:true\b|['"`])/,
                  text
                )
              end

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
    # Spans come out sorted and disjoint: walk them with the sorted offsets
    # in one pass.
    keep_code(Enum.sort(offsets), noncode_spans(src), [])
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

  defp keep_code([], _spans, acc), do: Enum.reverse(acc)
  defp keep_code(offs, [], acc), do: Enum.reverse(acc, offs)

  defp keep_code([off | rest] = offs, [{a, b} | spans_rest] = spans, acc) do
    cond do
      off >= b -> keep_code(offs, spans_rest, acc)
      off >= a -> keep_code(rest, spans, acc)
      true -> keep_code(rest, spans, [off | acc])
    end
  end
end
