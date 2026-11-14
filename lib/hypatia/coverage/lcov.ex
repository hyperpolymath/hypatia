# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.Lcov do
  @moduledoc """
  Parse (and render) LCOV tracefiles into `Hypatia.Coverage.Facts`.

  Understood records: `SF:`, `DA:line,hits[,checksum]`,
  `BRDA:line,block,branch,taken` (`taken` of `-` means the block was never
  reached and counts as 0) and `end_of_record`. Other records (`TN:`, `FN:`,
  `FNDA:`, `LH:`, `BRH:` ...) are summary data derived from the above and
  are ignored. `BRF:N` makes branches measurable even with zero arms (`BRF:0`);
  a declared `BRF:N` must equal the number of parsed `BRDA:` arms of that file
  and a declared `LF:N` the number of `DA:` lines, else it is a parse error. This is the plug-in point for Elixir, JS, Julia and
  Zig later (design §4.1).

  A report with no `BRDA:` and no `BRF:` line has `branch_measurable: false`.

  Errors are `{:error, {:parse, "line N: ..."}}`.
  """

  alias Hypatia.Coverage.Facts
  alias Hypatia.Coverage.Merge

  @doc "Options: `:root` strips a directory prefix from `SF:` paths."
  @spec parse(String.t(), keyword()) :: {:ok, Facts.t()} | {:error, {:parse, String.t()}}
  def parse(text, opts \\ []) when is_binary(text) do
    root = Keyword.get(opts, :root)

    text
    |> String.split(~r/\r?\n/)
    |> Enum.with_index(1)
    |> Enum.reduce_while({nil, [], false}, fn {raw, n}, {cur, done, saw_brda} ->
      line = String.trim(raw)

      case record(line) do
        :skip ->
          {:cont, {cur, done, saw_brda}}

        {:sf, path} ->
          if cur do
            {:halt, {:error, "line #{n}: SF before end_of_record"}}
          else
            {:cont, {{Facts.relativise(path, root), Facts.empty_file(), %{}}, done, saw_brda}}
          end

        :eor ->
          case cur do
            nil ->
              {:halt, {:error, "line #{n}: end_of_record with no SF"}}

            {p, f, decl} ->
              case check_declared(p, f, decl, n) do
                :ok -> {:cont, {nil, [{p, f} | done], saw_brda}}
                {:error, why} -> {:halt, {:error, why}}
              end
          end

        {:da, args} ->
          with_file(cur, n, "DA", fn {p, f} ->
            case parse_da(args) do
              {:ok, l, h} ->
                {:ok, {p, %{f | lines: Map.update(f.lines, l, h, &(&1 + h))}}}

              :error ->
                {:error, "line #{n}: bad DA record #{inspect(line)}"}
            end
          end)
          |> step(done, saw_brda)

        {:brda, args} ->
          with_file(cur, n, "BRDA", fn {p, f} ->
            case parse_brda(args) do
              {:ok, key, h} ->
                {:ok, {p, %{f | branches: Map.update(f.branches, key, h, &(&1 + h))}}}

              :error ->
                {:error, "line #{n}: bad BRDA record #{inspect(line)}"}
            end
          end)
          |> step(done, true)

        {:summary, tag, arg} ->
          with_decl(cur, n, tag, arg, line) |> step(done, saw_brda or tag == :brf)
      end
    end)
    |> case do
      {:error, why} ->
        {:error, {:parse, why}}

      {nil, done, saw_brda} ->
        facts =
          done
          |> Enum.reverse()
          |> Enum.map(fn {p, f} -> Facts.new(%{p => f}, branch_measurable: false) end)
          |> Merge.merge_all()

        {:ok, %{facts | branch_measurable: saw_brda}}

      {_open, _done, _} ->
        {:error, {:parse, "unterminated record: missing end_of_record at end of input"}}
    end
  end

  @doc "Render facts as LCOV. `parse/1` of the result gives the same facts."
  @spec render(Facts.t()) :: String.t()
  def render(%{files: files} = facts) do
    measurable = Map.get(facts, :branch_measurable, false)

    files
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(fn {path, f} ->
      da = f.lines |> Enum.sort() |> Enum.map(fn {l, h} -> "DA:#{l},#{h}\n" end)

      brda =
        f.branches
        |> Enum.sort()
        |> Enum.map(fn {{l, b, a}, h} -> "BRDA:#{l},#{b},#{a},#{h}\n" end)

      brf =
        if measurable,
          do: [
            "BRF:#{map_size(f.branches)}\n",
            "BRH:#{Enum.count(f.branches, fn {_, h} -> h > 0 end)}\n"
          ],
          else: []

      ["SF:", path, "\n", da, brda, brf, "end_of_record\n"]
    end)
    |> IO.iodata_to_binary()
  end

  defp step({:error, why}, _done, _saw), do: {:halt, {:error, why}}
  defp step({:ok, cur}, done, saw), do: {:cont, {cur, done, saw}}

  defp with_file(nil, n, tag, _fun), do: {:error, "line #{n}: #{tag} outside an SF record"}

  defp with_file({p, f, d}, _n, _tag, fun) do
    with {:ok, {p, f}} <- fun.({p, f}), do: {:ok, {p, f, d}}
  end

  # A declared BRF/LF is kept and checked against the parsed arms/lines at end_of_record.
  defp with_decl(nil, n, tag, _arg, _line),
    do: {:error, "line #{n}: #{String.upcase(to_string(tag))} outside an SF record"}

  defp with_decl({p, f, d}, n, tag, arg, line) do
    case Integer.parse(arg) do
      {v, ""} when v >= 0 -> {:ok, {p, f, Map.put(d, tag, v)}}
      _ -> {:error, "line #{n}: bad #{String.upcase(to_string(tag))} record #{inspect(line)}"}
    end
  end

  defp check_declared(path, f, decl, n) do
    cond do
      decl[:brf] != nil and decl[:brf] != map_size(f.branches) ->
        {:error,
         "line #{n}: #{path}: BRF:#{decl[:brf]} declares #{decl[:brf]} branch arm(s) but #{map_size(f.branches)} BRDA record(s) were parsed"}

      decl[:lf] != nil and decl[:lf] != map_size(f.lines) ->
        {:error,
         "line #{n}: #{path}: LF:#{decl[:lf]} declares #{decl[:lf]} line(s) but #{map_size(f.lines)} DA record(s) were parsed"}

      true ->
        :ok
    end
  end

  defp record(""), do: :skip
  defp record("SF:" <> path), do: {:sf, path}
  defp record("end_of_record"), do: :eor
  defp record("DA:" <> rest), do: {:da, rest}
  defp record("BRDA:" <> rest), do: {:brda, rest}
  defp record("BRF:" <> rest), do: {:summary, :brf, String.trim(rest)}
  defp record("LF:" <> rest), do: {:summary, :lf, String.trim(rest)}
  defp record(_other), do: :skip

  defp parse_da(args) do
    case String.split(args, ",") do
      [l, h | _checksum] ->
        with {l, ""} <- Integer.parse(l), {h, ""} <- Integer.parse(h), true <- l > 0 and h >= 0 do
          {:ok, l, h}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp parse_brda(args) do
    case String.split(args, ",") do
      [l, b, a, taken] ->
        with {l, ""} <- Integer.parse(l),
             {b, ""} <- Integer.parse(b),
             {a, ""} <- Integer.parse(a),
             {:ok, h} <- taken(taken) do
          {:ok, {l, b, a}, h}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp taken("-"), do: {:ok, 0}

  defp taken(s) do
    case Integer.parse(s) do
      {h, ""} when h >= 0 -> {:ok, h}
      _ -> :error
    end
  end
end
