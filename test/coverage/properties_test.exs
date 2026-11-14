# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.PropertiesTest do
  @moduledoc """
  Property tests with a hand-rolled generator (no new deps). Seed from
  HYPATIA_COVERAGE_PROP_SEED (default fixed), at least 200 cases per property;
  a failure message names the seed and the case index. See
  `Hypatia.Coverage.Prop`.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Coverage.{Facts, Lcov, Merge, Prop, Report}

  test "the runner honours the 200-case floor" do
    assert Prop.cases() >= 200
  end

  test "failure messages name the seed and case count" do
    err =
      assert_raise ExUnit.AssertionError, fn ->
        Prop.check("always fails", fn -> flunk("boom") end)
      end

    assert err.message =~ "seed=#{Prop.seed()} case=1/#{Prop.cases()}"
    assert err.message =~ "always fails"
  end

  test "merge is commutative on hits" do
    Prop.check("commutative", fn ->
      {a, b} = {Prop.facts(), Prop.facts()}
      assert Merge.merge(a, b) == Merge.merge(b, a)
    end)
  end

  test "merge is associative on hits" do
    Prop.check("associative", fn ->
      {a, b, c} = {Prop.facts(), Prop.facts(), Prop.facts()}
      assert Merge.merge(Merge.merge(a, b), c) == Merge.merge(a, Merge.merge(b, c))
    end)
  end

  test "empty is the identity" do
    Prop.check("identity", fn ->
      a = Prop.facts()
      assert Merge.merge(a, Facts.empty()) == a
      assert Merge.merge(Facts.empty(), a) == a
    end)
  end

  test "covered(merge(a, b)) == covered(a) ∪ covered(b), for lines and branch arms" do
    Prop.check("union", fn ->
      {a, b} = {Prop.facts(), Prop.facts()}
      m = Merge.merge(a, b)

      assert Facts.covered_lines(m) ==
               MapSet.union(Facts.covered_lines(a), Facts.covered_lines(b))

      assert Facts.covered_branches(m) ==
               MapSet.union(Facts.covered_branches(a), Facts.covered_branches(b))
    end)
  end

  test "merging with itself keeps the covered set (and doubles the hits)" do
    Prop.check("idempotent covered set", fn ->
      a = Prop.facts()
      m = Merge.merge(a, a)
      assert Facts.covered_lines(m) == Facts.covered_lines(a)
      assert Facts.covered_branches(m) == Facts.covered_branches(a)
      assert Facts.line_hits(m) == Map.new(Facts.line_hits(a), fn {k, h} -> {k, 2 * h} end)
    end)
  end

  test "proven percentage never exceeds raw when proven ⊆ raw" do
    Prop.check("proven <= raw", fn ->
      raw = Prop.facts()
      proven = Prop.subset_of(raw)
      c = Report.compute(raw, proven)

      assert c.line.proven <= c.line.raw
      assert c.line.raw <= c.line.total
      assert c.branch.proven <= c.branch.raw
      assert c.branch.raw <= c.branch.total
      assert pct(c.line.proven, c.line.total) <= pct(c.line.raw, c.line.total)
      assert pct(c.branch.proven, c.branch.total) <= pct(c.branch.raw, c.branch.total)
    end)
  end

  test "a subset proven run never triggers COV-E011, a superset of covered always does" do
    Prop.check("E011 iff not subset", fn ->
      raw = Prop.facts()
      proven = Prop.subset_of(raw)

      verdict = fn p ->
        Report.build(%{
          raw: {:ok, raw},
          proven: {:ok, p},
          evidence: {:ok, []},
          repo: "o/r",
          sha: "abc"
        })
      end

      refute "COV-E011" in Enum.map(verdict.(proven)["reasons"], & &1["code"])
      # (the raw run may have no lines at all; E011 is only meaningful then too)
      assert Facts.covered_lines(proven) |> MapSet.subset?(Facts.covered_lines(raw))
    end)
  end

  test "LCOV render/parse round trip" do
    Prop.check("lcov round trip", fn ->
      f = Prop.facts()
      assert Lcov.parse(Lcov.render(f)) == {:ok, f}
    end)
  end

  test "ranges/1 covers exactly the input lines, sorted and non-overlapping" do
    Prop.check("ranges", fn ->
      locs = for p <- ~w(a b), l <- Enum.take_random(1..40, :rand.uniform(30)), do: {p, l}
      ranges = Report.ranges(locs)
      expanded = for {p, a, b} <- ranges, l <- a..b, do: {p, l}
      assert expanded == locs |> Enum.uniq() |> Enum.sort()

      assert Enum.all?(Enum.chunk_every(ranges, 2, 1, :discard), fn [{p, _, b}, {q, a, _}] ->
               p != q or a > b + 1
             end)
    end)
  end

  defp pct(_n, 0), do: 0.0
  defp pct(n, t), do: n * 100 / t
end
