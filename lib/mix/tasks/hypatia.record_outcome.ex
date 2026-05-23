# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.RecordOutcome do
  @moduledoc """
  Record the outcome of an applied fix, with default-on re-scan
  verification. Designed to be called from gitbot-fleet's bash
  dispatch-runner after a fix is committed.

  Usage:

      mix hypatia.record_outcome \\
          --recipe recipe-pin-action-sha \\
          --repo  hyperpolymath/007-lang \\
          --file  .github/workflows/ci.yml \\
          --outcome success

  Optional:
      --pattern-id  PA-013-pin-deps           (defaults to recipe id)
      --category    DependencyPinning         (auto-derived from recipe
                                               if omitted)
      --repo-path   /path/to/local/clone      (defaults to
                                               $HYPATIA_REPOS_DIR/<repo>)
      --no-verify                             (record without re-scanning)
      --format      text|json                 (default: text)

  Exit codes:
      0   outcome recorded, verification verified-clean OR not attempted
      0   outcome recorded, verification scan_unavailable (env didn't
          have panic-attack; counted distinctly in recipe_health)
      2   outcome recorded, verification still_present (the fix was
          claimed but the re-scan still finds the weak point — the
          dispatch-runner SHOULD treat this as a failed batch and
          consider rollback)
      1   bad arguments or unrecoverable error

  The non-zero exit on still_present is the contract that lets the bash
  runner notice a false-fix without reading JSON.
  """

  use Mix.Task

  @shortdoc "Record a fix outcome (default-verifies via panic-attack)"

  @switches [
    recipe: :string,
    repo: :string,
    file: :string,
    outcome: :string,
    pattern_id: :string,
    category: :string,
    repo_path: :string,
    no_verify: :boolean,
    format: :string
  ]

  @impl Mix.Task
  def run(argv) do
    {opts, _, _} = OptionParser.parse(argv, switches: @switches)

    required = [:recipe, :repo, :file, :outcome]
    missing = Enum.filter(required, &(Keyword.get(opts, &1) in [nil, ""]))

    if missing != [] do
      Mix.shell().error("missing required option(s): #{Enum.map_join(missing, ", ", &"--#{&1}")}")
      exit({:shutdown, 1})
    end

    outcome_atom =
      case Keyword.fetch!(opts, :outcome) do
        "success" -> :success
        "failure" -> :failure
        "false_positive" -> :false_positive
        other ->
          Mix.shell().error("invalid --outcome '#{other}' (use success|failure|false_positive)")
          exit({:shutdown, 1})
      end

    recipe = Keyword.fetch!(opts, :recipe)
    repo = Keyword.fetch!(opts, :repo)
    file = Keyword.fetch!(opts, :file)

    verify? = not Keyword.get(opts, :no_verify, false)

    call_opts =
      []
      |> maybe_put(:pattern_id, Keyword.get(opts, :pattern_id))
      |> maybe_put(:category, Keyword.get(opts, :category))
      |> maybe_put(:repo_path, Keyword.get(opts, :repo_path))

    {record, verification} =
      if verify? do
        {:ok, record, v} =
          Hypatia.OutcomeTracker.record_outcome_for_fix(recipe, repo, file, outcome_atom, call_opts)

        {record, v}
      else
        {:ok, record} =
          Hypatia.OutcomeTracker.record_outcome(recipe, repo, file, outcome_atom, %{
            "verification" => "unverified"
          })

        {record, :not_verified}
      end

    case Keyword.get(opts, :format, "text") do
      "json" -> emit_json(record, verification)
      _ -> emit_text(record, verification)
    end

    if verification == :false_positive do
      exit({:shutdown, 2})
    end
  end

  defp maybe_put(opts, _key, nil), do: opts
  defp maybe_put(opts, _key, ""), do: opts
  defp maybe_put(opts, key, value), do: Keyword.put(opts, key, value)

  defp emit_text(record, verification) do
    Mix.shell().info(
      "recorded: #{record["recipe_id"]} in #{record["repo"]}/#{record["file"]} " <>
        "outcome=#{record["outcome"]} verification=#{verification}"
    )
  end

  defp emit_json(record, verification) do
    payload = Map.put(record, "verification_result", Atom.to_string(verification))
    IO.puts(Jason.encode!(payload))
  end
end
