# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.Prop do
  @moduledoc """
  Tiny hand-rolled property runner for the coverage tests (no new deps).

  Every case is seeded with `:rand.seed(:exsss, {seed, case_index, 0})`, so a
  failure message with `seed=S case=I` is replayed exactly by setting
  `HYPATIA_COVERAGE_PROP_SEED=S` and re-running. Case count comes from
  `HYPATIA_COVERAGE_PROP_CASES` (default 200, never fewer than 200).
  """

  import ExUnit.Assertions

  alias Hypatia.Coverage.Facts

  @default_seed 20_261_114

  def seed do
    case Integer.parse(System.get_env("HYPATIA_COVERAGE_PROP_SEED", "")) do
      {n, ""} -> n
      _ -> @default_seed
    end
  end

  def cases do
    case Integer.parse(System.get_env("HYPATIA_COVERAGE_PROP_CASES", "")) do
      {n, ""} when n >= 200 -> n
      _ -> 200
    end
  end

  @doc "Run `fun` for every case; on failure, flunk with seed, case index and case count."
  def check(name, fun) when is_function(fun, 0) do
    seed = seed()
    n = cases()

    for i <- 1..n do
      :rand.seed(:exsss, {seed, i, 0})

      try do
        fun.()
      rescue
        e ->
          flunk(
            "property #{inspect(name)} failed: seed=#{seed} case=#{i}/#{n} (#{n} cases): " <>
              Exception.message(e)
          )
      end
    end

    :ok
  end

  # -- generators (all read the process-local :rand state) ------------------

  @doc "Random facts: 0-3 files, lines 1..30 with hits 0..5, branch arms with hits 0..5."
  def facts do
    files =
      for p <- Enum.take_random(~w(src/a.rs src/b.rs lib/c.ex d.zig), :rand.uniform(4) - 1),
          into: %{} do
        lines = for l <- sample(1..30), into: %{}, do: {l, hits()}

        branches =
          for l <- sample(1..30),
              b <- 0..:rand.uniform(2),
              a <- 0..1,
              into: %{},
              do: {{l, b, a}, hits()}

        {p, %{lines: lines, branches: if(:rand.uniform(3) == 1, do: %{}, else: branches)}}
      end

    Facts.new(files)
  end

  @doc "A `proven` run that is a subset of `raw`: every hit count is at most raw's."
  def subset_of(%{files: files} = raw) do
    files =
      Map.new(files, fn {p, f} ->
        {p, %{lines: shrink(f.lines), branches: shrink(f.branches)}}
      end)

    %{raw | files: files}
  end

  defp shrink(map), do: Map.new(map, fn {k, h} -> {k, :rand.uniform(h + 1) - 1} end)

  defp sample(range), do: Enum.take_random(range, :rand.uniform(12) - 1)
  defp hits, do: :rand.uniform(6) - 1
end
