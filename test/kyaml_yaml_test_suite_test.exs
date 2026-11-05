# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.KyamlYamlTestSuiteTest do
  @moduledoc """
  Hypatia.Kyaml against the YAML test suite (yaml/yaml-test-suite, `data`
  branch), the spec's own corpus of valid and invalid streams.

  KYAML is a subset of YAML, so refusing a valid case is allowed. Two things
  are not: accepting a case the suite marks invalid, and reading a valid case
  as anything but its `in.json`. Either would mean a workflow is read
  otherwise than YAML reads it.

  Needs the suite checked out at `YAML_TEST_SUITE` (the kyaml-conformance CI
  job pins the commit); excluded from the default `mix test`.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Kyaml

  @moduletag :yaml_test_suite

  test "no invalid case is accepted and every valid case read is read as YAML reads it" do
    root =
      System.get_env("YAML_TEST_SUITE") ||
        flunk("set YAML_TEST_SUITE to a yaml-test-suite data checkout")

    # The four-character case directories; `name/` and `tags/` index them again.
    cases =
      root
      |> File.ls!()
      |> Enum.filter(&Regex.match?(~r/\A[0-9A-Z]{4}\z/, &1))
      |> Enum.flat_map(fn id ->
        dir = Path.join(root, id)
        subs = for sub <- File.ls!(dir), File.dir?(Path.join(dir, sub)), do: Path.join(dir, sub)
        if subs == [], do: [dir], else: subs
      end)
      |> Enum.filter(&File.exists?(Path.join(&1, "in.yaml")))

    assert length(cases) > 300

    results =
      for dir <- cases do
        id = Path.relative_to(dir, root)
        parsed = Kyaml.parse(File.read!(Path.join(dir, "in.yaml")))

        cond do
          File.exists?(Path.join(dir, "error")) ->
            if match?({:ok, _}, parsed), do: {:accepted_error, id}, else: :ok

          not match?({:ok, _}, parsed) or not File.exists?(Path.join(dir, "in.json")) ->
            :ok

          true ->
            {:ok, docs} = parsed
            want = dir |> Path.join("in.json") |> File.read!() |> json_stream()
            got = Enum.map(docs, &plain/1)
            if got == want, do: :ok, else: {:json_mismatch, id, got, want}
        end
      end

    failures = Enum.reject(results, &(&1 == :ok))

    assert failures == [],
           "#{length(failures)} cases read otherwise than YAML: #{inspect(failures)}"
  end

  # The values of a stream of JSON texts, one or more lines each.
  defp json_stream(text) do
    {values, rest} =
      text
      |> String.split("\n")
      |> Enum.reduce({[], ""}, fn line, {values, buffer} ->
        buffer = buffer <> line <> "\n"

        case Jason.decode(buffer) do
          {:ok, value} -> {[value | values], ""}
          {:error, _} -> {values, buffer}
        end
      end)

    if String.trim(rest) != "", do: flunk("unreadable in.json: #{inspect(rest)}")
    Enum.reverse(values)
  end

  # The JSON image of a node: keys as JSON keys, numbers as numbers.
  defp plain(node) do
    case Kyaml.deref(node) do
      %{kind: :map, entries: es} -> Map.new(es, &{json_key(&1.key), plain(&1.value)})
      %{kind: :seq, items: items} -> Enum.map(items, &plain/1)
      %{kind: :float, value: v} when is_atom(v) -> {:non_finite, v}
      %{value: value} -> value
    end
  end

  defp json_key(%{kind: :string, value: v}), do: v
  defp json_key(key), do: {:non_string_key, key.value}
end
