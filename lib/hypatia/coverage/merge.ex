# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.Merge do
  @moduledoc """
  Merge coverage facts (design §4.3): hits are summed, so the covered set of a
  merge is the union of the covered sets. Merging is commutative, associative
  and has `Hypatia.Coverage.Facts.empty/0` as identity.

  `branch_measurable` is the OR of the inputs: if any input measured branches,
  the merge did. (A caller that must not mix branch and non-branch runs should
  check before merging; this module does not guess.)
  """

  alias Hypatia.Coverage.Facts

  @spec merge(Facts.t(), Facts.t()) :: Facts.t()
  def merge(%{files: a, branch_measurable: ma}, %{files: b, branch_measurable: mb}) do
    files =
      Map.merge(a, b, fn _path, fa, fb ->
        %{
          lines: sum(fa.lines, fb.lines),
          branches: sum(fa.branches, fb.branches)
        }
      end)

    %{files: files, branch_measurable: ma or mb}
  end

  @spec merge_all([Facts.t()]) :: Facts.t()
  def merge_all(list), do: Enum.reduce(list, Facts.empty(), &merge(&2, &1))

  defp sum(a, b), do: Map.merge(a, b, fn _k, x, y -> x + y end)
end
