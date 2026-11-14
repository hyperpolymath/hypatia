# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.CoverExport do
  @moduledoc """
  Render Erlang `:cover` line data as LCOV (design §4.1, Elixir row).

  Pure: no `:cover` process is needed. The input is the payload of
  `:cover.analyse(mod, :calls, :line)`, that is `[{{mod, line}, calls}]`, for
  each module, plus a map from module to source path.

      to_lcov(%{MyMod => [{{MyMod, 3}, 2}, {{MyMod, 4}, 0}]}, %{MyMod => "lib/my_mod.ex"})

  Rules:

  * Line `0` is generated code (Elixir's `__info__` and friends) and is dropped.
  * Only lines that `:cover` reports appear. Those are the executable lines.
    A line with `0` calls is executable and not run.
  * Several modules in one source file are merged: calls on the same line are summed.
  * A module with no source in the map is skipped, never guessed.
  * No `BRF`, `BRDA` or `BRH` is written. `:cover` has no branch data, so
    `Hypatia.Coverage.Lcov` reports `branch_measurable: false` (TOOL-LIMIT, design §4.1).
  * Output is sorted by path and line, so it is deterministic.

  `analyses` may be a map or a list of `{module, entries}`, where `entries` is
  the list, or `{:ok, list}` as `:cover.analyse/3` returns it. A module whose
  analysis was `{:error, _}` is skipped.
  """

  @type entries :: [{{module(), non_neg_integer()}, non_neg_integer()}]
  @type analyses :: %{module() => entries() | {:ok, entries()}} | [{module(), entries()}]

  @spec to_lcov(analyses(), %{module() => String.t()}) :: String.t()
  def to_lcov(analyses, sources) when is_map(sources) do
    analyses
    |> Enum.reduce(%{}, fn {mod, result}, acc ->
      with path when is_binary(path) <- Map.get(sources, mod),
           entries when is_list(entries) <- entries(result) do
        Enum.reduce(entries, acc, fn
          {{_m, line}, calls}, acc
          when is_integer(line) and line > 0 and is_integer(calls) and calls >= 0 ->
            Map.update(acc, path, %{line => calls}, fn lines ->
              Map.update(lines, line, calls, &(&1 + calls))
            end)

          _other, acc ->
            acc
        end)
      else
        _ -> acc
      end
    end)
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(fn {path, lines} ->
      sorted = Enum.sort(lines)
      hit = Enum.count(sorted, fn {_, h} -> h > 0 end)

      [
        "SF:",
        path,
        "\n",
        Enum.map(sorted, fn {l, h} -> "DA:#{l},#{h}\n" end),
        "LF:#{length(sorted)}\n",
        "LH:#{hit}\n",
        "end_of_record\n"
      ]
    end)
    |> IO.iodata_to_binary()
  end

  defp entries({:ok, list}) when is_list(list), do: list
  defp entries(list) when is_list(list), do: list
  defp entries(_), do: nil

  @doc """
  Make a compile-source path repo-relative: strip `root` (default the current
  directory). A path outside the root is returned unchanged, so the report
  flags it (COV-E003) rather than the exporter hiding it.
  """
  @spec relative_source(String.t() | charlist(), String.t()) :: String.t()
  def relative_source(source, root \\ File.cwd!()) do
    source = to_string(source)
    root = String.trim_trailing(root, "/") <> "/"
    String.replace_prefix(source, root, "")
  end
end
