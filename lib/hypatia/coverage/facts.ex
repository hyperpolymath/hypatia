# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.Facts do
  @moduledoc """
  Normalised coverage facts: the one shape every parser produces and every
  consumer (merge, report) reads. See `docs/design/proven-coverage.adoc` §3.2, §4.3.

      %{
        files: %{
          "src/lib.rs" => %{
            lines:    %{line => hits},
            branches: %{{line, block, branch} => hits}
          }
        },
        branch_measurable: boolean
      }

  * `lines` holds only *executable* lines. A line absent from the map is not
    executable (no mappable region); a line present with `0` hits is executable
    and not run.
  * `branches` has one key per branch arm. `block` and `branch` are whatever
    the producing tool uses to tell arms apart (LCOV: block and branch ids;
    llvm-cov: start column and arm 0 = true, 1 = false).
  * `branch_measurable` is `false` when the report carries no branch data at
    all. Hypatia never turns "no data" into a number.
  * Paths are repo-relative where `relativise/2` could make them so.
  """

  @type file_facts :: %{
          lines: %{pos_integer() => non_neg_integer()},
          branches: %{{integer(), integer(), integer()} => non_neg_integer()}
        }
  @type t :: %{files: %{String.t() => file_facts()}, branch_measurable: boolean()}

  @doc "The identity of `Hypatia.Coverage.Merge.merge/2`."
  @spec empty() :: t()
  def empty, do: %{files: %{}, branch_measurable: false}

  @doc "An empty per-file record."
  @spec empty_file() :: file_facts()
  def empty_file, do: %{lines: %{}, branches: %{}}

  @doc """
  Build facts from a files map. `branch_measurable` defaults to "some file has
  at least one branch arm"; pass `branch_measurable: bool` to override (a
  tool that ran with branch support but found no branches).
  """
  @spec new(%{String.t() => file_facts()}, keyword()) :: t()
  def new(files, opts \\ []) when is_map(files) do
    measurable =
      Keyword.get_lazy(opts, :branch_measurable, fn ->
        Enum.any?(files, fn {_path, f} -> map_size(f.branches) > 0 end)
      end)

    %{files: files, branch_measurable: measurable}
  end

  @doc """
  Make `path` repo-relative by stripping `root` (a directory prefix). With no
  root the path is returned unchanged apart from a leading `./`. A path that
  does not start with the root is left as is; the report then flags it as
  COV-E003 rather than silently dropping it.
  """
  @spec relativise(String.t(), String.t() | nil) :: String.t()
  def relativise(path, root) when is_binary(path) do
    path = String.replace_prefix(path, "./", "")

    case root do
      r when is_binary(r) and r != "" ->
        prefix = String.trim_trailing(r, "/") <> "/"
        String.replace_prefix(path, prefix, "")

      _ ->
        path
    end
  end

  @doc "Flat map `{path, line} => hits`."
  @spec line_hits(t()) :: %{{String.t(), pos_integer()} => non_neg_integer()}
  def line_hits(%{files: files}) do
    for {path, f} <- files, {line, hits} <- f.lines, into: %{}, do: {{path, line}, hits}
  end

  @doc "Flat map `{path, line, block, branch} => hits`."
  @spec branch_hits(t()) :: %{{String.t(), integer(), integer(), integer()} => non_neg_integer()}
  def branch_hits(%{files: files}) do
    for {path, f} <- files,
        {{l, b, a}, hits} <- f.branches,
        into: %{},
        do: {{path, l, b, a}, hits}
  end

  @doc "Set of `{path, line}` with at least one hit."
  @spec covered_lines(t()) :: MapSet.t()
  def covered_lines(facts), do: covered(line_hits(facts))

  @doc "Set of `{path, line, block, branch}` with at least one hit."
  @spec covered_branches(t()) :: MapSet.t()
  def covered_branches(facts), do: covered(branch_hits(facts))

  defp covered(hits) do
    for {k, h} <- hits, h > 0, into: MapSet.new(), do: k
  end
end
