# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.ProofBinding do
  @moduledoc """
  Binds a proof (kind P) record to the checkout being judged (design §2.2, §4.2; COV-E006).

  A P record lists what it covers in `run.proof.covers`:

      {"decl": "Demo.f", "file": "src/lib.rs", "lines": [3, 9], "source_sha256": "<64 hex>"}

  `source_sha256` is the SHA-256 of the exact bytes of lines `start..end` (1-based, inclusive)
  of `file`, joined with `"\\n"`, with no trailing newline, as they were when the proof checker ran.

  `bind/2` recomputes that hash from the file under `root`. Every entry must match.

    * all match: `{:ok, [{file, first, last}]}`
    * a file that is missing, a range outside the file, a path that leaves the root, or a
      hash that differs: `{:error, :proof_stale}`
    * no root: `{:error, :proof_unbound}` (the source cannot be read)

  `coverage/2` turns bound ranges into facts: every line in a range that the raw report lists
  as executable, and every branch arm the raw report has on those lines. Lines the raw report
  does not list are ignored, so the result is always a subset of raw's units.
  """

  alias Hypatia.Coverage.Facts

  @type range :: {String.t(), pos_integer(), pos_integer()}

  @doc """
  The well-formed `covers` entries of a record: `{:ok, [entry]}`, or `:error` when `covers`
  is absent, empty, or has an entry without `decl`, `file`, `lines: [start, end]` (integers,
  `1 <= start <= end`) and a 64-hex `source_sha256`.
  """
  @spec covers(map()) :: {:ok, [map()]} | :error
  def covers(record) do
    case get_in_safe(record, ["run", "proof", "covers"]) do
      [_ | _] = l -> if Enum.all?(l, &entry?/1), do: {:ok, l}, else: :error
      _ -> :error
    end
  end

  @doc "Bind a record's covers to the checkout at `root` (see the moduledoc)."
  @spec bind(map(), String.t() | nil) ::
          {:ok, [range()]} | {:error, :proof_stale | :proof_unbound | :no_covers}
  def bind(record, root) do
    case covers(record) do
      :error ->
        {:error, :no_covers}

      {:ok, _} when not is_binary(root) or root == "" ->
        {:error, :proof_unbound}

      {:ok, entries} ->
        results = Enum.map(entries, &bind_entry(&1, root))

        if Enum.all?(results, &match?({:ok, _}, &1)),
          do: {:ok, Enum.map(results, fn {:ok, r} -> r end)},
          else: {:error, :proof_stale}
    end
  end

  @doc """
  True when `path` stays under `root` (relative, no escape) and names a regular file.
  The same rule the report uses for COV-E003.
  """
  @spec under_root_file?(String.t(), term()) :: boolean()
  def under_root_file?(root, path) when is_binary(path) do
    root = Path.expand(root)
    full = Path.expand(path, root)
    prefix = if String.ends_with?(root, "/"), do: root, else: root <> "/"

    Path.type(path) != :absolute and String.starts_with?(full, prefix) and File.regular?(full)
  end

  def under_root_file?(_root, _path), do: false

  @doc "SHA-256 (lower hex) of lines `first..last` of `text`, or `:error` when out of range."
  @spec range_sha256(String.t(), pos_integer(), pos_integer()) :: String.t() | :error
  def range_sha256(text, first, last) do
    lines = String.split(text, "\n")
    lines = if List.last(lines) == "", do: Enum.drop(lines, -1), else: lines

    if first >= 1 and last >= first and last <= length(lines) do
      lines
      |> Enum.slice(first - 1, last - first + 1)
      |> Enum.join("\n")
      |> then(&:crypto.hash(:sha256, &1))
      |> Base.encode16(case: :lower)
    else
      :error
    end
  end

  @doc """
  Proof coverage as facts over `raw`: lines and branch arms (hit count 1) that fall in
  `ranges` and that `raw` lists. Always a subset of raw's units.
  """
  @spec coverage(Facts.t(), [range()]) :: Facts.t()
  def coverage(%{files: raw_files}, ranges) do
    files =
      ranges
      |> Enum.group_by(fn {f, _, _} -> Facts.relativise(f, nil) end, fn {_, a, b} -> {a, b} end)
      |> Enum.flat_map(fn {file, rs} ->
        case Map.fetch(raw_files, file) do
          {:ok, rf} ->
            in_range? = fn l -> Enum.any?(rs, fn {a, b} -> l >= a and l <= b end) end

            lines = for {l, _} <- rf.lines, in_range?.(l), into: %{}, do: {l, 1}

            branches =
              for {{l, _, _} = k, _} <- rf.branches, in_range?.(l), into: %{}, do: {k, 1}

            if lines == %{} and branches == %{},
              do: [],
              else: [{file, %{lines: lines, branches: branches}}]

          :error ->
            []
        end
      end)
      |> Map.new()

    Facts.new(files, branch_measurable: false)
  end

  # -- internals --------------------------------------------------------------

  defp bind_entry(%{"file" => file, "lines" => [a, b], "source_sha256" => want}, root) do
    with true <- under_root_file?(root, file),
         {:ok, text} <- File.read(Path.expand(file, Path.expand(root))),
         got when got != :error <- range_sha256(text, a, b),
         true <- String.downcase(want) == got do
      {:ok, {Facts.relativise(file, nil), a, b}}
    else
      _ -> :stale
    end
  end

  defp entry?(%{"decl" => d, "file" => f, "lines" => [a, b], "source_sha256" => h}),
    do:
      nonblank?(d) and nonblank?(f) and is_integer(a) and is_integer(b) and a >= 1 and b >= a and
        is_binary(h) and Regex.match?(~r/\A[0-9a-fA-F]{64}\z/, h)

  defp entry?(_), do: false

  defp nonblank?(s), do: is_binary(s) and String.trim(s) != ""

  defp get_in_safe(m, [k | rest]) when is_map(m), do: get_in_safe(Map.get(m, k), rest)
  defp get_in_safe(v, []), do: v
  defp get_in_safe(_, _), do: nil
end
