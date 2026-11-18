# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.KyamlComplexityTest do
  @moduledoc """
  Hypatia.Kyaml parses flat collections in linear time.

  The collections used to be rebuilt per entry: 20,000 block mapping entries
  took 4.8 s, now about 0.25 s. This test fits the growth exponent `k` in
  `time ∝ n^k` over four input sizes, by least squares on log-log points,
  and asserts `k < 1.4`.

  Measured on 2026-11-18 (best of five per size, sizes 2,500 to 20,000):

      shape   current parser   pre-fix parser (6a3407a)
      map     1.11             1.58
      seq     0.87             1.89
      flow    0.98             1.76

  A fit over four sizes is not swung by one slow sample the way the old
  two-point ratio was (11.4 against a bound of 10, on a loaded runner, for
  the current parser). The module is synchronous so it runs after the async
  modules rather than beside them, and each sample runs in a fresh process
  so every size starts from the same empty heap.

  Reductions were measured too and rejected as the measure: the quadratic
  copying ran inside BIFs, which charge few reductions, so the pre-fix
  parser's map shape fits 1.06 on reductions and would pass.
  """
  use ExUnit.Case, async: false

  alias Hypatia.Kyaml

  @sizes [2_500, 5_000, 10_000, 20_000]
  @runs 5
  @bound 1.4

  @shapes [
    map: &__MODULE__.block_map/1,
    seq: &__MODULE__.block_seq/1,
    flow: &__MODULE__.flow_seq/1
  ]

  for {name, _} <- @shapes do
    test "flat #{name} collections parse in linear time" do
      shape = Keyword.fetch!(@shapes, unquote(name))
      k = exponent(&Kyaml.parse/1, shape)

      assert k < @bound,
             "#{unquote(name)}: time grows as n^#{Float.round(k, 2)}; linear is n^1, quadratic n^2"
    end
  end

  test "the fit itself: an exact power law recovers its exponent" do
    assert_in_delta slope(for n <- @sizes, do: {n, n * n}), 2.0, 1.0e-9
    assert_in_delta slope(for n <- @sizes, do: {n, 7 * n}), 1.0, 1.0e-9
  end

  @doc false
  def block_map(n), do: Enum.map_join(1..n, &"k#{&1}: #{&1}\n")
  @doc false
  def block_seq(n), do: Enum.map_join(1..n, &"- #{&1}\n")
  @doc false
  def flow_seq(n), do: "[" <> Enum.map_join(1..n, ", ", &Integer.to_string/1) <> "]\n"

  @doc false
  # The growth exponent of `parse` over `shape`, fitted over @sizes.
  def exponent(parse, shape) do
    @sizes
    |> Enum.map(fn n -> {n, best_time(parse, shape.(n))} end)
    |> slope()
  end

  defp best_time(parse, text) do
    1..@runs
    |> Enum.map(fn _ -> time_in_fresh_process(parse, text) end)
    |> Enum.min()
  end

  defp time_in_fresh_process(parse, text) do
    task =
      Task.async(fn ->
        {us, {:ok, _}} = :timer.tc(fn -> parse.(text) end)
        us
      end)

    task |> Task.await(:infinity) |> max(1)
  end

  # Least-squares slope of log(t) against log(n).
  defp slope(points) do
    xs = Enum.map(points, fn {n, _} -> :math.log(n) end)
    ys = Enum.map(points, fn {_, t} -> :math.log(t) end)
    mx = Enum.sum(xs) / length(xs)
    my = Enum.sum(ys) / length(ys)

    Enum.sum(Enum.zip_with(xs, ys, fn x, y -> (x - mx) * (y - my) end)) /
      Enum.sum(Enum.map(xs, fn x -> (x - mx) * (x - mx) end))
  end
end
