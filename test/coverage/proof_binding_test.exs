# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.ProofBindingTest do
  use ExUnit.Case, async: true

  alias Hypatia.Coverage.{Facts, Merge, Prop, ProofBinding}

  @root Path.expand("../fixtures/coverage/demo_repo", __DIR__)
  @text File.read!(Path.join(@root, "src/lib.rs"))

  defp entry(a, b, over \\ %{}) do
    Map.merge(
      %{
        "decl" => "classify",
        "file" => "src/lib.rs",
        "lines" => [a, b],
        "source_sha256" => ProofBinding.range_sha256(@text, a, b)
      },
      over
    )
  end

  defp rec(entries), do: %{"kind" => "P", "run" => %{"proof" => %{"covers" => entries}}}

  describe "range_sha256/3" do
    test "hashes lines joined with newline, no trailing newline" do
      want = :crypto.hash(:sha256, "pub fn classify(n: i32) -> &'static str {\n    if n < 0 {")
      assert ProofBinding.range_sha256(@text, 1, 2) == Base.encode16(want, case: :lower)
    end

    test "out of range is :error; a trailing newline adds no line" do
      assert ProofBinding.range_sha256("a\nb\n", 1, 2) != :error
      assert ProofBinding.range_sha256("a\nb\n", 1, 3) == :error
      assert ProofBinding.range_sha256("a", 0, 1) == :error
      assert ProofBinding.range_sha256("", 1, 1) == :error
    end
  end

  describe "bind/2" do
    test "matching hashes bind, and return the ranges" do
      assert ProofBinding.bind(rec([entry(1, 11), entry(13, 19)]), @root) ==
               {:ok, [{"src/lib.rs", 1, 11}, {"src/lib.rs", 13, 19}]}
    end

    test "an uppercase hash still matches" do
      e = entry(1, 11)
      e = %{e | "source_sha256" => String.upcase(e["source_sha256"])}
      assert {:ok, _} = ProofBinding.bind(rec([e]), @root)
    end

    test "a hash mismatch is stale, even when only one entry differs" do
      bad = entry(1, 11, %{"source_sha256" => String.duplicate("0", 64)})
      assert ProofBinding.bind(rec([bad]), @root) == {:error, :proof_stale}
      assert ProofBinding.bind(rec([entry(13, 19), bad]), @root) == {:error, :proof_stale}
    end

    test "a shifted range is stale" do
      shifted = entry(1, 11, %{"lines" => [2, 12]})
      assert ProofBinding.bind(rec([shifted]), @root) == {:error, :proof_stale}
    end

    test "missing file, range out of bounds, path escape are stale" do
      assert ProofBinding.bind(rec([entry(1, 11, %{"file" => "src/nope.rs"})]), @root) ==
               {:error, :proof_stale}

      assert ProofBinding.bind(rec([entry(1, 11, %{"lines" => [1, 9999]})]), @root) ==
               {:error, :proof_stale}

      for f <- ["../evidence.json", "/etc/hostname"] do
        assert ProofBinding.bind(rec([entry(1, 1, %{"file" => f})]), @root) ==
                 {:error, :proof_stale},
               f
      end
    end

    test "no root is unbound" do
      assert ProofBinding.bind(rec([entry(1, 11)]), nil) == {:error, :proof_unbound}
      assert ProofBinding.bind(rec([entry(1, 11)]), "") == {:error, :proof_unbound}
    end

    test "no usable covers" do
      assert ProofBinding.bind(rec([]), @root) == {:error, :no_covers}
      assert ProofBinding.bind(%{"kind" => "P"}, @root) == {:error, :no_covers}
    end
  end

  describe "coverage/2" do
    test "only raw executable lines and arms in range, hit count 1" do
      raw =
        Facts.new(%{
          "src/lib.rs" => %{
            lines: %{1 => 3, 2 => 0, 5 => 1},
            branches: %{{2, 0, 0} => 0, {2, 0, 1} => 2, {5, 0, 0} => 1}
          }
        })

      f = ProofBinding.coverage(raw, [{"src/lib.rs", 1, 3}, {"src/other.rs", 1, 9}])

      assert f.files == %{
               "src/lib.rs" => %{
                 lines: %{1 => 1, 2 => 1},
                 branches: %{{2, 0, 0} => 1, {2, 0, 1} => 1}
               }
             }
    end
  end

  test "property: proof-added coverage is always a subset of raw executable units" do
    Prop.check("proof subset of raw", fn ->
      raw = Prop.facts()

      ranges =
        for _ <- 1..:rand.uniform(4) do
          a = :rand.uniform(35)

          {Enum.random(~w(src/a.rs src/b.rs lib/c.ex d.zig src/none.rs ./src/a.rs)), a,
           a + :rand.uniform(10) - 1}
        end

      added = ProofBinding.coverage(raw, ranges)
      raw_lines = Facts.line_hits(raw)
      raw_arms = Facts.branch_hits(raw)

      assert Enum.all?(Facts.line_hits(added), fn {k, h} ->
               h == 1 and Map.has_key?(raw_lines, k)
             end)

      assert Enum.all?(Facts.branch_hits(added), fn {k, h} ->
               h == 1 and Map.has_key?(raw_arms, k)
             end)

      # merged into a proven run that is itself a subset of raw, the universe never grows
      merged = Merge.merge(Prop.subset_of(raw), added)

      assert MapSet.subset?(
               MapSet.new(Map.keys(Facts.line_hits(merged))),
               MapSet.new(Map.keys(raw_lines))
             )

      assert MapSet.subset?(
               MapSet.new(Map.keys(Facts.branch_hits(merged))),
               MapSet.new(Map.keys(raw_arms))
             )
    end)
  end
end
