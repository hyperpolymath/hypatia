# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.KyamlConformanceTest do
  @moduledoc """
  kyq's KYAML conformance corpus, vendored in test/fixtures/kyaml/corpus (see
  the README there for its source commit). Every valid fixture parses into the
  pinned number of documents; every invalid one fails with the pinned code,
  line, column and pointer path.

  A few of kyq's invalid fixtures fail in kyq's JSON *bridge*, not its parser
  (a non-string key, an integer past I-JSON's range, a non-finite number).
  Hypatia.Kyaml is a parser only, so it must accept those: the bridge's
  refusal is not a parse error.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Kyaml

  @corpus Path.expand("fixtures/kyaml/corpus", __DIR__)
  @bridge_codes ["NON_STRING_KEY", "UNSAFE_INTEGER", "NON_FINITE_NUMBER"]
  @cases (@corpus <> "/cases.json") |> File.read!() |> Jason.decode!() |> Map.fetch!("cases")

  test "the corpus is not vacuous and every file has exactly one case" do
    files =
      for dir <- ["valid", "invalid"],
          name <- File.ls!(Path.join(@corpus, dir)),
          do: "#{dir}/#{name}"

    assert Enum.sort(files) == Enum.sort(Enum.map(@cases, & &1["file"]))
    assert Enum.count(@cases, &(&1["expect"] == "valid")) >= 10
    assert Enum.count(@cases, &(&1["expect"] == "invalid")) >= 30
  end

  for %{"file" => file} = c <- @cases do
    @case c
    test "#{file}" do
      bytes = File.read!(Path.join(@corpus, @case["file"]))

      case {@case["expect"], @case["code"]} do
        {"valid", _} ->
          assert {:ok, docs} = Kyaml.parse(bytes)
          assert length(docs) == @case["documents"]

        {"invalid", code} when code in @bridge_codes ->
          assert {:ok, _} = Kyaml.parse(bytes)

        {"invalid", code} ->
          assert {:error, error} = Kyaml.parse(bytes)

          assert {error.code, error.line, error.column, error.path} ==
                   {code, @case["line"], @case["column"], @case["path"] || ""}
      end
    end
  end
end
