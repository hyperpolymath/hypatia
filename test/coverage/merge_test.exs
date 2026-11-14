# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.MergeTest do
  use ExUnit.Case, async: true

  alias Hypatia.Coverage.{Facts, Merge}

  defp facts(files, bm \\ nil),
    do: if(bm == nil, do: Facts.new(files), else: Facts.new(files, branch_measurable: bm))

  test "hits are summed per line and per branch arm" do
    a = facts(%{"f" => %{lines: %{1 => 2, 2 => 0}, branches: %{{1, 0, 0} => 1}}})
    b = facts(%{"f" => %{lines: %{2 => 3, 3 => 1}, branches: %{{1, 0, 0} => 4, {1, 0, 1} => 0}}})

    assert Merge.merge(a, b).files["f"] == %{
             lines: %{1 => 2, 2 => 3, 3 => 1},
             branches: %{{1, 0, 0} => 5, {1, 0, 1} => 0}
           }
  end

  test "files unique to one side are kept" do
    a = facts(%{"a" => %{lines: %{1 => 1}, branches: %{}}})
    b = facts(%{"b" => %{lines: %{1 => 0}, branches: %{}}})
    assert Map.keys(Merge.merge(a, b).files) == ["a", "b"]
  end

  test "branch_measurable is the OR of the inputs" do
    a = facts(%{}, true)
    b = facts(%{}, false)
    assert Merge.merge(a, b).branch_measurable
    refute Merge.merge(b, b).branch_measurable
  end

  test "empty is the identity and merge_all folds" do
    a = facts(%{"f" => %{lines: %{1 => 1}, branches: %{{1, 0, 0} => 1}}})
    assert Merge.merge(a, Facts.empty()) == a
    assert Merge.merge(Facts.empty(), a) == a
    assert Merge.merge_all([a, a, a]).files["f"].lines == %{1 => 3}
    assert Merge.merge_all([]) == Facts.empty()
  end

  test "covered sets treat 0 hits as not covered" do
    a = facts(%{"f" => %{lines: %{1 => 0, 2 => 1}, branches: %{{1, 0, 0} => 0, {1, 0, 1} => 2}}})
    assert Facts.covered_lines(a) == MapSet.new([{"f", 2}])
    assert Facts.covered_branches(a) == MapSet.new([{"f", 1, 0, 1}])
  end
end
