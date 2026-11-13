# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.CiTriage do
  @moduledoc """
  Triage a failed CI job with `Hypatia.Rules.CiFailureTriage`. Offline: reads
  files, calls no API.

  ## Usage

      mix hypatia.ci_triage log JOB.log [--exit-code N] [--conclusion C]
                                        [--pr-state open|merged|closed] [--attempts N]
      mix hypatia.ci_triage gate --required "A,B" --reported "B,C"
      mix hypatia.ci_triage pins WORKFLOW.yml...
      mix hypatia.ci_triage rules

  Prints one JSON object per finding (JSONL). For `log`, the first line is the
  primary finding after the loop limit (`--attempts` reruns so far).

  ## Exit codes

    0 — nothing found, or the primary action is `none`
    1 — bad arguments or an unreadable file
    3 — a finding (the caller reads the JSON for the action)
  """
  use Mix.Task

  alias Hypatia.Rules.CiFailureTriage, as: T

  @shortdoc "Classify a failed CI job: layer, action, loop limit (offline)"

  @switches [
    exit_code: :integer,
    conclusion: :string,
    pr_state: :string,
    attempts: :integer,
    required: :string,
    reported: :string
  ]

  @impl Mix.Task
  def run(argv) do
    {opts, args, invalid} = OptionParser.parse(argv, strict: @switches)
    if invalid != [], do: die("unknown option(s): #{inspect(invalid)}")

    case args do
      ["log", path] ->
        log(path, opts)

      ["gate"] ->
        gate(opts)

      ["pins" | paths] when paths != [] ->
        pins(paths)

      ["rules"] ->
        Enum.each(T.rules(), &emit/1)

      _ ->
        die("usage: mix hypatia.ci_triage log|gate|pins|rules (see `mix help hypatia.ci_triage`)")
    end
  end

  defp log(path, opts) do
    text = read!(path)

    ctx =
      %{}
      |> put_opt(:exit_code, opts[:exit_code])
      |> put_opt(:conclusion, opts[:conclusion])
      |> put_opt(:pr_state, pr_state(opts[:pr_state]))

    %{primary: primary, matches: matches} = T.classify(text, ctx)

    case T.decide(primary, Keyword.get(opts, :attempts, 0)) do
      nil ->
        :ok

      decided ->
        emit(decided)
        matches |> Enum.drop(1) |> Enum.each(&emit/1)
        if decided.action != :none, do: exit({:shutdown, 3})
    end
  end

  defp gate(opts) do
    split = fn s -> (s || "") |> String.split(",", trim: true) |> Enum.map(&String.trim/1) end
    findings = T.check_gate(split.(opts[:required]), split.(opts[:reported]))
    Enum.each(findings, &emit/1)
    if findings != [], do: exit({:shutdown, 3})
  end

  defp pins(paths) do
    findings = paths |> Map.new(&{&1, read!(&1)}) |> T.check_scanner_pin_drift()
    Enum.each(findings, &emit/1)
    if findings != [], do: exit({:shutdown, 3})
  end

  defp pr_state(nil), do: nil
  defp pr_state(s) when s in ["open", "merged", "closed"], do: String.to_atom(s)
  defp pr_state(s), do: die("--pr-state must be open, merged or closed, not #{inspect(s)}")

  defp put_opt(map, _key, nil), do: map
  defp put_opt(map, key, value), do: Map.put(map, key, value)

  defp read!(path) do
    case File.read(path) do
      {:ok, text} -> text
      {:error, reason} -> die("cannot read #{path}: #{:file.format_error(reason)}")
    end
  end

  defp emit(map), do: IO.puts(Jason.encode!(map))

  defp die(msg) do
    Mix.shell().error(msg)
    exit({:shutdown, 1})
  end
end
