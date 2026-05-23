# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.RecipeHealth do
  @moduledoc """
  Per-recipe health report driven by `Hypatia.OutcomeTracker.recipe_health/1`.

  Surfaces recipes whose re-scan verification rate is low (potential
  false-fix candidates) or insufficient (verification was not attempted
  often enough to draw conclusions). Output is sorted so the most
  actionable rows -- quarantine candidates and degraded recipes -- are
  at the top.

  Status legend:
    healthy           -- verification rate >= 0.70
    degraded          -- verification rate < 0.70 (review)
    quarantine_cand   -- verification rate < 0.30 (auto-quarantine candidate)
    insufficient      -- fewer than --min-attempts verifiable outcomes
    no_data           -- recipe has outcomes but none were verified

  Options:
    --format text|json     (default: text)
    --min-attempts N       fewer than this and the recipe is "insufficient"
    --degraded N.NN        threshold below "healthy" (default 0.70)
    --quarantine N.NN      threshold below "degraded" (default 0.30)
    --only-actionable      hide healthy + insufficient + no_data rows

  ## Examples

      mix hypatia.recipe_health
      mix hypatia.recipe_health --only-actionable
      mix hypatia.recipe_health --format json > recipe-health.json
  """

  use Mix.Task

  @shortdoc "Show per-recipe success + verification health"

  @switches [
    format: :string,
    min_attempts: :integer,
    degraded: :float,
    quarantine: :float,
    only_actionable: :boolean
  ]

  @impl Mix.Task
  def run(argv) do
    {opts, _, _} = OptionParser.parse(argv, switches: @switches)

    format = Keyword.get(opts, :format, "text")
    min_attempts = Keyword.get(opts, :min_attempts, 5)
    degraded = Keyword.get(opts, :degraded, 0.70)
    quarantine = Keyword.get(opts, :quarantine, 0.30)
    only_actionable = Keyword.get(opts, :only_actionable, false)

    rows =
      Hypatia.OutcomeTracker.recipe_health(
        min_attempts: min_attempts,
        degraded_threshold: degraded,
        quarantine_threshold: quarantine
      )

    rows =
      if only_actionable do
        Enum.filter(rows, fn r -> r.status in [:degraded, :quarantine_candidate] end)
      else
        rows
      end

    case format do
      "json" -> emit_json(rows)
      _ -> emit_text(rows)
    end
  end

  defp emit_text([]) do
    Mix.shell().info("No recipes match the filter (or no outcomes recorded yet).")
  end

  defp emit_text(rows) do
    headers = ["recipe_id", "disp", "succ", "fail", "fp", "verified", "still", "scan_fail", "rate", "status"]
    width = column_widths(rows, headers)

    Mix.shell().info(format_row(headers, width))
    Mix.shell().info(format_row(Enum.map(width, fn w -> String.duplicate("-", w) end), width))

    Enum.each(rows, fn r ->
      row = [
        r.recipe_id,
        Integer.to_string(r.dispatches),
        Integer.to_string(r.successes),
        Integer.to_string(r.failures),
        Integer.to_string(r.false_positives),
        Integer.to_string(r.verification.verified),
        Integer.to_string(r.verification.still_present),
        Integer.to_string(r.verification.scan_failed),
        format_rate(r.verification.rate),
        Atom.to_string(r.status)
      ]

      Mix.shell().info(format_row(row, width))
    end)

    Mix.shell().info("")

    Mix.shell().info(
      "#{length(rows)} recipe(s). " <>
        "Quarantine threshold #{quarantine_msg(rows)}, " <>
        "degraded threshold #{degraded_msg(rows)}."
    )
  end

  defp emit_json(rows) do
    payload = %{
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "rows" =>
        Enum.map(rows, fn r ->
          %{
            "recipe_id" => r.recipe_id,
            "dispatches" => r.dispatches,
            "successes" => r.successes,
            "failures" => r.failures,
            "false_positives" => r.false_positives,
            "success_rate" => to_jsonable(r.success_rate),
            "verification" => %{
              "verified" => r.verification.verified,
              "still_present" => r.verification.still_present,
              "scan_failed" => r.verification.scan_failed,
              "unverified" => r.verification.unverified,
              "verifiable" => r.verification.verifiable,
              "rate" => to_jsonable(r.verification.rate)
            },
            "status" => Atom.to_string(r.status)
          }
        end)
    }

    IO.puts(Jason.encode!(payload, pretty: true))
  end

  defp column_widths(rows, headers) do
    initial = Enum.map(headers, &String.length/1)

    Enum.reduce(rows, initial, fn r, widths ->
      lengths = [
        String.length(r.recipe_id),
        String.length(Integer.to_string(r.dispatches)),
        String.length(Integer.to_string(r.successes)),
        String.length(Integer.to_string(r.failures)),
        String.length(Integer.to_string(r.false_positives)),
        String.length(Integer.to_string(r.verification.verified)),
        String.length(Integer.to_string(r.verification.still_present)),
        String.length(Integer.to_string(r.verification.scan_failed)),
        String.length(format_rate(r.verification.rate)),
        String.length(Atom.to_string(r.status))
      ]

      Enum.zip_with([widths, lengths], fn [a, b] -> max(a, b) end)
    end)
  end

  defp format_row(cells, widths) do
    Enum.zip(cells, widths)
    |> Enum.map_join("  ", fn {cell, w} -> String.pad_trailing(cell, w) end)
  end

  defp format_rate(:no_data), do: "—"
  defp format_rate(:insufficient_data), do: "?"
  defp format_rate(r) when is_float(r), do: :erlang.float_to_binary(r, decimals: 2)

  defp to_jsonable(:no_data), do: nil
  defp to_jsonable(:insufficient_data), do: "insufficient_data"
  defp to_jsonable(r) when is_float(r), do: r

  defp quarantine_msg(rows) do
    count = Enum.count(rows, &(&1.status == :quarantine_candidate))
    "#{count} recipe(s)"
  end

  defp degraded_msg(rows) do
    count = Enum.count(rows, &(&1.status == :degraded))
    "#{count} recipe(s)"
  end
end
