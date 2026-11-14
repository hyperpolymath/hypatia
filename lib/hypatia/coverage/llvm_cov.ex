# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.LlvmCov do
  @moduledoc """
  Parse a `cargo llvm-cov --json` export (`llvm.coverage.json.export`, the
  format `rust-ci.yml` produces) into `Hypatia.Coverage.Facts`.

  Reads `data[*].files[]` with `filename`, `segments` and `branches`.

  ## Lines

  A segment is `[line, col, count, has_count, is_region_entry, is_gap_region]`.
  Per-line hit counts follow llvm-cov's own `LineCoverageStats` rule:

  * *wrapped segment* of a line = the last segment that starts on an earlier line;
  * a *region start* on the line = a segment with `has_count`, `is_region_entry`
    and not `is_gap_region`;
  * the line is **not executable** (left out of the map) if its first segment is
    a skipped-region entry (entry without `has_count`), or if it has neither a
    counted wrapped segment nor a region start;
  * otherwise its count is the maximum of the wrapped segment's count (when
    there is one) and the counts of the region starts on the line.

  The rule is checked against llvm's own `summary.lines` in the test suite.

  ## Branches

  A `branches` entry is
  `[line_start, col_start, line_end, col_end, true_count, false_count, file_id, expanded_file_id, kind]`
  and yields two arms, keyed `{line_start, col_start, 0}` (true) and
  `{line_start, col_start, 1}` (false). Duplicate entries (generic
  instantiations) are summed. A report with no branch entries anywhere and
  `summary.branches.count == 0` is `branch_measurable: false`; this is what a
  stable-toolchain run (no `--branch`) looks like.

  ## Paths

  Pass `root: "/abs/checkout/dir"` to make filenames repo-relative.
  """

  alias Hypatia.Coverage.Facts
  alias Hypatia.Coverage.Merge

  @spec parse(String.t(), keyword()) :: {:ok, Facts.t()} | {:error, {:parse, String.t()}}
  def parse(json, opts \\ []) when is_binary(json) do
    case Jason.decode(json) do
      {:ok, doc} ->
        from_document(doc, opts)

      {:error, %Jason.DecodeError{position: pos}} ->
        {:error, {:parse, "invalid JSON at byte #{pos}"}}
    end
  end

  @spec from_document(term(), keyword()) :: {:ok, Facts.t()} | {:error, {:parse, String.t()}}
  def from_document(%{"data" => [_ | _] = data}, opts) do
    root = Keyword.get(opts, :root)

    data
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {entry, i}, {:ok, acc} ->
      case entry do
        %{"files" => files} when is_list(files) ->
          with {:ok, facts} <- file_list(files, root, "data[#{i}]"),
               :ok <- check_totals(entry, files, "data[#{i}]") do
            {:cont, {:ok, [facts | acc]}}
          else
            {:error, _} = e -> {:halt, e}
          end

        _ ->
          {:halt, {:error, {:parse, "data[#{i}].files missing or not a list"}}}
      end
    end)
    |> case do
      {:ok, list} -> {:ok, list |> Enum.reverse() |> Merge.merge_all() |> measurable(data)}
      {:error, _} = e -> e
    end
  end

  def from_document(_other, _opts),
    do: {:error, {:parse, "not an llvm-cov export: expected {\"data\": [ {\"files\": [...]} ]}"}}

  @doc """
  Per-line hit counts from a list of llvm-cov segments, as `%{line => hits}`
  containing only executable lines.
  """
  @spec line_counts([list()]) :: %{pos_integer() => non_neg_integer()}
  def line_counts([]), do: %{}

  def line_counts(segments) do
    segs =
      Enum.map(segments, fn [line, _col, count, has_count, entry, gap] ->
        %{line: line, count: count, has_count: has_count, entry: entry, gap: gap}
      end)

    by_line = Enum.group_by(segs, & &1.line)
    first = segs |> hd() |> Map.fetch!(:line)
    last = segs |> List.last() |> Map.fetch!(:line)

    {_wrapped, acc} =
      Enum.reduce(first..last//1, {nil, %{}}, fn line, {wrapped, acc} ->
        line_segs = Map.get(by_line, line, [])
        acc = put_stats(acc, line, line_segs, wrapped)
        {List.last(line_segs) || wrapped, acc}
      end)

    acc
  end

  defp put_stats(acc, line, line_segs, wrapped) do
    starts = Enum.filter(line_segs, &(not &1.gap and &1.has_count and &1.entry))

    skipped_start =
      case line_segs do
        [%{has_count: false, entry: true} | _] -> true
        _ -> false
      end

    mapped = not skipped_start and ((wrapped != nil and wrapped.has_count) or starts != [])

    if mapped do
      base = if wrapped, do: wrapped.count, else: 0
      Map.put(acc, line, Enum.reduce(starts, base, &max(&1.count, &2)))
    else
      acc
    end
  end

  defp file_list(files, root, where) do
    files
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {file, i}, {:ok, acc} ->
      case file_facts(file, root) do
        {:ok, facts} -> {:cont, {:ok, [facts | acc]}}
        {:error, why} -> {:halt, {:error, {:parse, "#{where}.files[#{i}]: #{why}"}}}
      end
    end)
    |> case do
      {:ok, list} -> {:ok, list |> Enum.reverse() |> Merge.merge_all()}
      {:error, _} = e -> e
    end
  end

  defp file_facts(%{"filename" => name, "segments" => segs} = file, root)
       when is_binary(name) and is_list(segs) do
    with :ok <- check_segments(segs),
         {:ok, branches} <- branches(Map.get(file, "branches", [])),
         :ok <- check_declared(file, Map.get(file, "branches")),
         lines = line_counts(segs),
         :ok <- check_lines(file, map_size(lines)) do
      path = Facts.relativise(name, root)

      {:ok,
       Facts.new(%{path => %{lines: lines, branches: branches}},
         branch_measurable: false
       )}
    end
  end

  defp file_facts(_other, _root), do: {:error, "needs string \"filename\" and list \"segments\""}

  defp check_segments(segs) do
    if Enum.all?(segs, &segment?/1),
      do: :ok,
      else: {:error, "malformed segment (want [line, col, count, has_count, is_entry, is_gap])"}
  end

  defp segment?([l, c, n, hc, e, g]),
    do:
      is_integer(l) and l >= 1 and is_integer(c) and c >= 1 and is_integer(n) and n >= 0 and
        is_boolean(hc) and is_boolean(e) and is_boolean(g)

  defp segment?(_), do: false

  defp branches(list) when is_list(list) do
    Enum.reduce_while(list, {:ok, %{}}, fn
      [l, c, _le, _ce, t, f | _rest], {:ok, acc}
      when is_integer(l) and is_integer(c) and is_integer(t) and is_integer(f) ->
        acc =
          acc
          |> Map.update({l, c, 0}, t, &(&1 + t))
          |> Map.update({l, c, 1}, f, &(&1 + f))

        {:cont, {:ok, acc}}

      _bad, _acc ->
        {:halt, {:error, "malformed branch entry"}}
    end)
  end

  defp branches(_), do: {:error, "\"branches\" is not a list"}

  # llvm's `summary.branches.count` counts arms: each `branches` entry is two (true, false).
  defp check_totals(entry, files, where) do
    declared = get_in(entry, ["totals", "branches", "count"])

    arms =
      Enum.reduce(files, 0, fn f, n ->
        case is_map(f) && Map.get(f, "branches") do
          l when is_list(l) -> n + 2 * length(l)
          _ -> n
        end
      end)

    if positive?(declared) and declared != arms,
      do:
        {:error,
         {:parse, "#{where}.totals declares #{declared} branch arms but files carry #{arms}"}},
      else: :ok
  end

  defp check_declared(file, entries) do
    declared = get_in(file, ["summary", "branches", "count"])

    cond do
      not positive?(declared) ->
        :ok

      not is_list(entries) ->
        {:error, "summary declares #{declared} branch arms but \"branches\" is missing"}

      2 * length(entries) == declared ->
        :ok

      true ->
        {:error,
         "summary declares #{declared} branch arms but \"branches\" has #{length(entries)} entries (#{2 * length(entries)} arms)"}
    end
  end

  # Truncation guard for lines. llvm-cov's JSON `summary.lines.count` can be a little
  # higher than its own per-line result (one line on the real fixture, which matches
  # `cargo llvm-cov --text`), so equality would reject genuine reports. Only clear
  # truncation is an error: a positive summary with no executable lines parsed, or
  # parsed lines short of the summary by more than max(2, 1%).
  @doc false
  def line_shortfall_allowed(declared), do: max(2, div(declared + 99, 100))

  defp check_lines(file, parsed) do
    declared = get_in(file, ["summary", "lines", "count"])

    cond do
      not positive?(declared) ->
        :ok

      parsed == 0 ->
        {:error, "summary declares #{declared} lines but no executable lines were parsed"}

      declared - parsed > line_shortfall_allowed(declared) ->
        {:error,
         "summary declares #{declared} lines but only #{parsed} were parsed (segments look truncated)"}

      true ->
        :ok
    end
  end

  defp measurable(facts, data) do
    declared =
      Enum.any?(data, fn d ->
        get_in(d, ["totals", "branches", "count"]) |> positive?() or
          Enum.any?(
            Map.get(d, "files", []),
            &(get_in(&1, ["summary", "branches", "count"]) |> positive?())
          )
      end)

    %{
      facts
      | branch_measurable:
          declared or Enum.any?(facts.files, fn {_p, f} -> map_size(f.branches) > 0 end)
    }
  end

  defp positive?(n), do: is_integer(n) and n > 0
end
