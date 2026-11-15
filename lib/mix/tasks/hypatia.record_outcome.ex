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
      --pattern-id  PA-013-pin-deps           (the pattern of the dispatch; written
                                               on the row. The rescan uses the
                                               recipe id when it is omitted, and
                                               then the row has no pattern id)
      --match-kind  exact|category|fuzzy      (the kind of match that chose the
                                               recipe; written on the row. Omitted,
                                               the row says "unknown")
      --category    DependencyPinning         (auto-derived from recipe
                                               if omitted)
      --repo-path   /path/to/local/clone      (defaults to
                                               $HYPATIA_REPOS_DIR/<repo>)
      --no-verify                             (record without re-scanning)
      --verdict     verified|no_op|incomplete|unverified|failed
                                              (the postcondition verdict of
                                               the fleet; see below)
      --format      text|json                 (default: text)

  `--verdict` is the postcondition judgement that the fleet made for the fix.
  It can lower trust. It never raises trust by itself. Only these pairs are accepted;
  any other pair, an invalid verdict, `--outcome false_positive` with a verdict,
  and an option that this task does not know exit 1 and write no row:

      --verdict    --outcome  behaviour
      verified     success    the rescan of Hypatia decides "verification"
      no_op        success    no rescan; "verification" is "no_op"
      unverified   success    no rescan; "verification" is "unverified"
      incomplete   failure    no rescan; "verification" is "incomplete"
      failed       failure    no rescan; "verification" is "failed"

  Each row that the call writes carries `"verdict"`. Without `--verdict` the
  row has no `"verdict"` key. Only `verified` with `success`, and `incomplete`
  or `failed` with `failure`, can count as evidence: see
  `Hypatia.OutcomeTracker.qualified_outcome_counts/1`.

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
    match_kind: :string,
    category: :string,
    repo_path: :string,
    no_verify: :boolean,
    verdict: :string,
    format: :string
  ]

  # The postcondition verdicts of the fleet, each with the one outcome that it
  # can accompany.
  @verdict_outcomes %{
    "verified" => "success",
    "no_op" => "success",
    "unverified" => "success",
    "incomplete" => "failure",
    "failed" => "failure"
  }

  @impl Mix.Task
  def run(argv) do
    {opts, _, invalid} = OptionParser.parse(argv, strict: @switches)

    if invalid != [] do
      names = Enum.map_join(invalid, ", ", fn {name, _value} -> name end)
      Mix.shell().error("unknown or invalid option(s): #{names}")
      exit({:shutdown, 1})
    end

    required = [:recipe, :repo, :file, :outcome]
    missing = Enum.filter(required, &(Keyword.get(opts, &1) in [nil, ""]))

    if missing != [] do
      Mix.shell().error("missing required option(s): #{Enum.map_join(missing, ", ", &"--#{&1}")}")
      exit({:shutdown, 1})
    end

    outcome_atom =
      case Keyword.fetch!(opts, :outcome) do
        "success" ->
          :success

        "failure" ->
          :failure

        "false_positive" ->
          :false_positive

        other ->
          Mix.shell().error("invalid --outcome '#{other}' (use success|failure|false_positive)")
          exit({:shutdown, 1})
      end

    verdict = validate_verdict!(Keyword.get(opts, :verdict), Keyword.fetch!(opts, :outcome))
    labels = labels!(opts)

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
      cond do
        verdict in [nil, "verified"] and verify? ->
          call_opts =
            Keyword.put(call_opts, :metadata, put_verdict(labels, verdict))

          {:ok, record, v} =
            Hypatia.OutcomeTracker.record_outcome_for_fix(
              recipe,
              repo,
              file,
              outcome_atom,
              call_opts
            )

          {record, v}

        verdict in [nil, "verified"] ->
          # No rescan was asked for, so there is no verification, whatever the
          # verdict says.
          {:ok, record} =
            Hypatia.OutcomeTracker.record_outcome(
              recipe,
              repo,
              file,
              outcome_atom,
              put_verdict(Map.put(labels, "verification", "unverified"), verdict)
            )

          {record, :not_verified}

        true ->
          # no_op, unverified, incomplete and failed: the fleet already
          # judged the postcondition. Record it without a rescan.
          {:ok, record} =
            Hypatia.OutcomeTracker.record_outcome(
              recipe,
              repo,
              file,
              outcome_atom,
              Map.merge(labels, %{"verification" => verdict, "verdict" => verdict})
            )

          {record, :not_verified}
      end

    case Keyword.get(opts, :format, "text") do
      "json" -> emit_json(record, verification, verdict)
      _ -> emit_text(record, verification)
    end

    if verification == :false_positive do
      exit({:shutdown, 2})
    end
  end

  defp validate_verdict!(nil, _outcome), do: nil

  defp validate_verdict!(verdict, outcome) do
    expected = Map.get(@verdict_outcomes, verdict)

    cond do
      expected == nil ->
        Mix.shell().error(
          "invalid --verdict '#{verdict}' (use #{Enum.join(Map.keys(@verdict_outcomes) |> Enum.sort(), "|")})"
        )

        exit({:shutdown, 1})

      outcome == "false_positive" ->
        Mix.shell().error("--verdict cannot be combined with --outcome false_positive")
        exit({:shutdown, 1})

      expected != outcome ->
        Mix.shell().error(
          "--verdict #{verdict} goes with --outcome #{expected}, not --outcome #{outcome}"
        )

        exit({:shutdown, 1})

      true ->
        verdict
    end
  end

  # The pattern and the kind of match of the dispatch (E0-18). The kind must be one
  # that the matcher makes; anything else is refused, and no kind is no label.
  @match_kinds ~w(exact category fuzzy)

  defp labels!(opts) do
    kind = Keyword.get(opts, :match_kind)

    if kind not in [nil, ""] and kind not in @match_kinds do
      Mix.shell().error("invalid --match-kind '#{kind}' (use exact|category|fuzzy)")
      exit({:shutdown, 1})
    end

    %{}
    |> put_label("pattern_id", Keyword.get(opts, :pattern_id))
    |> put_label("match_kind", kind)
  end

  defp put_label(labels, _key, nil), do: labels
  defp put_label(labels, _key, ""), do: labels
  defp put_label(labels, key, value), do: Map.put(labels, key, value)

  defp put_verdict(metadata, nil), do: metadata
  defp put_verdict(metadata, verdict), do: Map.put(metadata, "verdict", verdict)

  defp maybe_put(opts, _key, nil), do: opts
  defp maybe_put(opts, _key, ""), do: opts
  defp maybe_put(opts, key, value), do: Keyword.put(opts, key, value)

  defp emit_text(record, verification) do
    Mix.shell().info(
      "recorded: #{record["recipe_id"]} in #{record["repo"]}/#{record["file"]} " <>
        "outcome=#{record["outcome"]} verification=#{verification}"
    )
  end

  defp emit_json(record, verification, verdict) do
    payload =
      record
      |> Map.put("verification_result", Atom.to_string(verification))
      |> put_verdict(verdict)

    IO.puts(Jason.encode!(payload))
  end
end
