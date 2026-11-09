# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Rules.ResearchExtensionsEquivalenceTest do
  @moduledoc """
  RE001 and RE002 read the parsed workflow (#971), so they must not depend on
  how a workflow is written. Each seeded random workflow is rendered four
  ways (block YAML with plain scalars, KYAML flow as `yq -o kyaml` writes it,
  block with every key and value single-quoted, and block with each `run:` as
  a literal block scalar), and every rendering must give the same findings,
  apart from line numbers.

  A failure names the seed and the case, so it reproduces with
  `HYPATIA_EQUIV_SEED=<seed>`.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ResearchExtensions

  @cases 150
  @sha "1234567890abcdef1234567890abcdef12345678"

  setup_all do
    seed =
      case System.get_env("HYPATIA_EQUIV_SEED") do
        nil -> 20_261_104
        text -> String.to_integer(text)
      end

    {:ok, seed: seed}
  end

  test "RE001 and RE002 findings do not depend on how a workflow is written", %{seed: seed} do
    :rand.seed(:exsss, {seed, seed + 1, seed + 2})

    for n <- 1..@cases do
      workflow = gen_workflow()

      [first | rest] =
        for style <- [:block, :flow, :quoted, :literal] do
          {style, findings(render(workflow, style))}
        end

      {_, expected} = first

      for {style, got} <- rest do
        assert got == expected, """
        seed #{seed}, case #{n}: #{style} differs from block
        block:
        #{render(workflow, :block)}
        #{style}:
        #{render(workflow, style)}
        """
      end
    end
  end

  test "the generator reaches every outcome the harness is meant to compare", %{seed: seed} do
    :rand.seed(:exsss, {seed, seed + 1, seed + 2})

    rules =
      for _ <- 1..@cases, f <- findings(render(gen_workflow(), :block)), into: MapSet.new() do
        {f.rule, f.severity}
      end

    assert {"RE001", :high} in rules or {"RE001", :critical} in rules
    assert {"RE002", :high} in rules
  end

  # ─── findings, line-free ────────────────────────────────────────────

  defp findings(yaml) do
    dir = Path.join(System.tmp_dir!(), "hyp-equiv-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, ".github/workflows"))
    File.write!(Path.join(dir, ".github/workflows/w.yml"), yaml)

    try do
      (ResearchExtensions.re001_missing_harden_runner(dir) ++
         ResearchExtensions.re002_harden_runner_audit_mode(dir))
      |> Enum.map(&strip_lines/1)
      |> Enum.sort()
    after
      File.rm_rf!(dir)
    end
  end

  defp strip_lines(%{} = map) do
    for {k, v} <- map,
        not (is_atom(k) and String.contains?(Atom.to_string(k), "line")),
        into: %{} do
      {k, strip_lines(v)}
    end
  end

  defp strip_lines(list) when is_list(list), do: Enum.map(list, &strip_lines/1)
  defp strip_lines(text) when is_binary(text), do: Regex.replace(~r/\bline \d+|:\d+\b/, text, "")
  defp strip_lines(other), do: other

  # ─── generator ─────────────────────────────────────────────────────
  # A workflow is ordered pairs, so every rendering keeps one key order.

  defp gen_workflow do
    events =
      Enum.take_random(["push", "pull_request", "pull_request_target", "release"], pick(1..3))

    on = if coin(), do: {:map, Enum.map(events, &{&1, {:map, []}})}, else: {:seq, events}

    jobs = for i <- 1..pick(1..3), do: {"job#{i}", gen_job()}
    top = [{"on", on}, {"jobs", {:map, jobs}}]

    top =
      if chance(0.2),
        do: [{"name", "w"}, {"env", {:map, [{"TOP", secret()}]}} | top],
        else: [{"name", "w"} | top]

    {:map, top}
  end

  defp gen_job do
    steps = Enum.shuffle(for _ <- 1..pick(1..4), do: gen_step())
    steps = if chance(0.4), do: [harden_step() | steps], else: steps

    entries =
      [{"steps", {:seq, steps}}]
      |> then(&if(chance(0.25), do: [{"env", {:map, [{"JOB_KEY", secret()}]}} | &1], else: &1))
      |> then(&if(chance(0.9), do: [{"runs-on", "ubuntu-latest"} | &1], else: &1))

    {:map, entries}
  end

  defp harden_step do
    owner = Enum.random(["step-security", "Step-Security"])
    uses = {"uses", "#{owner}/harden-runner@#{@sha}"}

    case Enum.random([:audit, :block, :none]) do
      :none -> {:map, [uses]}
      policy -> {:map, [uses, {"with", {:map, [{"egress-policy", Atom.to_string(policy)}]}}]}
    end
  end

  defp gen_step do
    case pick(1..4) do
      1 ->
        {:map, [{"run", "echo #{secret()}"}]}

      2 ->
        {:map,
         [{"uses", "actions/upload-artifact@#{@sha}"}, {"with", {:map, [{"token", secret()}]}}]}

      3 ->
        ref = if coin(), do: [{"ref", "${{ github.event.pull_request.head.sha }}"}], else: []
        {:map, [{"uses", "actions/checkout@#{@sha}"}, {"with", {:map, ref}}]}

      4 ->
        {:map, [{"run", "make build"}]}
    end
  end

  defp secret, do: "${{ secrets.#{Enum.random(["API_KEY", "DEPLOY_TOKEN", "GITHUB_TOKEN"])} }}"
  defp pick(range), do: Enum.random(range)
  defp coin, do: chance(0.5)
  defp chance(p), do: :rand.uniform() < p

  # ─── renderers ─────────────────────────────────────────────────────

  defp render(node, :flow), do: flow(node, 0) <> "\n"
  defp render(node, style), do: Enum.join(block(node, 0, style), "\n") <> "\n"

  defp block({:map, []}, indent, _), do: [pad(indent) <> "{}"]

  defp block({:map, entries}, indent, style) do
    Enum.flat_map(entries, fn {k, v} ->
      key = pad(indent) <> scalar(k, style) <> ":"

      case v do
        {:map, []} -> [key <> " {}"]
        {:seq, []} -> [key <> " []"]
        {_, _} -> [key | block(v, indent + 2, style)]
        text -> value_lines(key, k, text, indent, style)
      end
    end)
  end

  defp block({:seq, items}, indent, style) do
    Enum.flat_map(items, fn
      {:map, _} = item ->
        [first | rest] = block(item, indent + 2, style)
        [pad(indent) <> "- " <> String.trim_leading(first) | rest]

      text ->
        [pad(indent) <> "- " <> scalar(text, style)]
    end)
  end

  defp value_lines(key, "run", text, indent, :literal),
    do: [key <> " |", pad(indent + 2) <> text]

  defp value_lines(key, _, text, _indent, style), do: [key <> " " <> scalar(text, style)]

  defp scalar(text, :quoted), do: "'" <> String.replace(text, "'", "''") <> "'"
  defp scalar(text, _), do: text

  defp flow({:map, entries}, indent) do
    if entries == [] do
      "{}"
    else
      inner =
        Enum.map_join(entries, "", fn {k, v} ->
          pad(indent + 2) <> flow_key(k) <> ": " <> flow(v, indent + 2) <> ",\n"
        end)

      "{\n" <> inner <> pad(indent) <> "}"
    end
  end

  defp flow({:seq, items}, indent) do
    if items == [] do
      "[]"
    else
      inner = Enum.map_join(items, "", &(pad(indent + 2) <> flow(&1, indent + 2) <> ",\n"))
      "[\n" <> inner <> pad(indent) <> "]"
    end
  end

  defp flow(text, _), do: inspect(text)

  # yq -o kyaml leaves a key unquoted where it can.
  defp flow_key(key), do: if(key =~ ~r/\A[A-Za-z_][A-Za-z0-9_-]*\z/, do: key, else: inspect(key))

  defp pad(n), do: String.duplicate(" ", n)
end
