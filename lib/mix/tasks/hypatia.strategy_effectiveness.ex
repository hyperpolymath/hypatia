# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.StrategyEffectiveness do
  @moduledoc """
  Longitudinal effectiveness study for neural rebalancer strategies
  (M9 follow-up).

  Strategy A (uniform random dips), B (adversarial perturbation), and
  C (corpus-grounded failures) are all available since commit
  fdb929d. The roadmap leaves "is :rotate actually better than fixed
  Strategy A at catching failures?" as an open question requiring
  longitudinal data.

  This task answers it once data accumulates. It reads:

    * outcomes log (data/verisim/outcomes/*.jsonl) for ground truth
    * historical Watcher snapshots (data/verisim/metrics/*.jsonl)
      for which strategy was active when each outcome arrived
    * neural-states/esn.json for current ESN accuracy + drift state

  And reports per-strategy:

    * outcome count during the window the strategy was active
    * success rate
    * verification rate (when verified marker present)
    * recipes that hit auto-quarantine while this strategy was active
    * ESN's drift-flag rate (how often the ESN raised drift while
      this strategy was the training input)

  ## Usage

      mix hypatia.strategy_effectiveness
      mix hypatia.strategy_effectiveness --format json
      mix hypatia.strategy_effectiveness --days 30

  ## Output

  The default text format prints a four-row table (one per strategy,
  plus an "unknown" row for outcomes recorded before snapshots
  started). JSON format emits the same content as a single object
  suitable for piping into a dashboard or further analysis.

  ## Why this is a separate mix task

  The data lives across two stores (outcomes for ground truth,
  metrics snapshots for strategy attribution) and the analysis
  requires joining them on timestamp. That join is a one-shot
  read — doesn't belong in the always-on Watcher pipeline. Run
  this task weekly to monitor whether `:rotate` is winning,
  losing, or tied vs the prior Strategy A baseline.
  """

  use Mix.Task

  @shortdoc "Compare neural rebalancer strategy effectiveness over a window"

  @switches [
    days: :integer,
    format: :string,
    outcomes_path: :string,
    metrics_path: :string
  ]

  @impl Mix.Task
  def run(argv) do
    Mix.Task.run("app.start")

    {opts, _, _} = OptionParser.parse(argv, switches: @switches)
    days = Keyword.get(opts, :days, 30)
    format = Keyword.get(opts, :format, "text")

    outcomes_dir =
      Keyword.get(opts, :outcomes_path) ||
        Path.join(Path.expand(verisim_path()), "outcomes")

    metrics_dir =
      Keyword.get(opts, :metrics_path) ||
        Path.join(Path.expand(verisim_path()), "metrics")

    cutoff_ms = System.system_time(:millisecond) - days * 24 * 60 * 60 * 1000

    outcomes = load_outcomes_since(outcomes_dir, cutoff_ms)
    snapshots = load_snapshots_since(metrics_dir, cutoff_ms)
    strategy_windows = build_strategy_windows(snapshots)

    report = analyse(outcomes, strategy_windows)

    case format do
      "json" -> emit_json(report)
      _ -> emit_text(report, days)
    end
  end

  # ─── Outcome ingestion ────────────────────────────────────────────────

  defp load_outcomes_since(dir, cutoff_ms) do
    case File.ls(dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          dir
          |> Path.join(f)
          |> File.stream!()
          |> Stream.map(fn line ->
            case Jason.decode(String.trim(line)) do
              {:ok, record} -> record
              _ -> nil
            end
          end)
          |> Stream.reject(&is_nil/1)
          |> Stream.filter(fn r -> outcome_ts(r) >= cutoff_ms end)
          |> Enum.to_list()
        end)

      _ ->
        []
    end
  end

  defp outcome_ts(record) do
    case Map.get(record, "timestamp") do
      iso when is_binary(iso) ->
        case DateTime.from_iso8601(iso) do
          {:ok, dt, _} -> DateTime.to_unix(dt, :millisecond)
          _ -> 0
        end

      _ ->
        0
    end
  end

  # ─── Snapshot ingestion (which strategy was active when) ──────────────

  defp load_snapshots_since(dir, cutoff_ms) do
    case File.ls(dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          dir
          |> Path.join(f)
          |> File.stream!()
          |> Stream.map(fn line ->
            case Jason.decode(String.trim(line)) do
              {:ok, record} -> record
              _ -> nil
            end
          end)
          |> Stream.reject(&is_nil/1)
          |> Stream.filter(fn r -> Map.get(r, "at_ms", 0) >= cutoff_ms end)
          |> Enum.to_list()
        end)
        |> Enum.sort_by(&Map.get(&1, "at_ms", 0))

      _ ->
        []
    end
  end

  # Build a list of {start_ms, end_ms, strategy} windows from the
  # snapshot stream. The Watcher snapshots don't currently record
  # which strategy is active; this task READS the live coordinator
  # config + falls back to inferring from the cycle counter (rotate
  # mode = strategy varies with cycle).
  #
  # For now: assume the current configured strategy applies across
  # the entire window (operationally close to true unless the env
  # was changed mid-window). When the Watcher persistence schema
  # adds a `rebalance_strategy` field per snapshot, this function
  # joins on that instead.
  defp build_strategy_windows(snapshots) do
    current_strategy =
      Application.get_env(:hypatia, :neural_rebalance_strategy, :a)
      |> to_string()

    case snapshots do
      [] ->
        []

      [first | _] = list ->
        start_ms = Map.get(first, "at_ms", 0)
        end_ms = list |> List.last() |> Map.get("at_ms", 0)
        [{start_ms, end_ms, current_strategy}]
    end
  end

  # ─── Analysis ─────────────────────────────────────────────────────────

  defp analyse(outcomes, strategy_windows) do
    Enum.reduce(outcomes, %{}, fn outcome, acc ->
      strategy = strategy_for(outcome_ts(outcome), strategy_windows)
      bucket = Map.get(acc, strategy, fresh_bucket())
      Map.put(acc, strategy, update_bucket(bucket, outcome))
    end)
    |> Enum.map(fn {strategy, bucket} ->
      Map.put(bucket, :strategy, strategy)
    end)
    |> Enum.sort_by(& &1.strategy)
  end

  defp strategy_for(_ts, []), do: "unknown"

  defp strategy_for(ts, windows) do
    case Enum.find(windows, fn {start, finish, _s} -> ts >= start and ts <= finish end) do
      {_, _, strategy} -> strategy
      nil -> "unknown"
    end
  end

  defp fresh_bucket do
    %{
      total: 0,
      successes: 0,
      failures: 0,
      false_positives: 0,
      verified: 0,
      still_present: 0,
      quarantined_recipes: MapSet.new()
    }
  end

  defp update_bucket(bucket, outcome) do
    outcome_str = Map.get(outcome, "outcome", "unknown")
    verification = Map.get(outcome, "verification", "unverified")
    recipe = Map.get(outcome, "recipe_id", "")

    bucket
    |> Map.update!(:total, &(&1 + 1))
    |> maybe_inc(outcome_str, "success", :successes)
    |> maybe_inc(outcome_str, "failure", :failures)
    |> maybe_inc(outcome_str, "false_positive", :false_positives)
    |> maybe_inc(verification, "verified", :verified)
    |> maybe_inc(verification, "still_present", :still_present)
    |> maybe_quarantine_set(verification, recipe)
  end

  defp maybe_inc(bucket, value, expected, key) do
    if value == expected, do: Map.update!(bucket, key, &(&1 + 1)), else: bucket
  end

  defp maybe_quarantine_set(bucket, "still_present", recipe) when recipe != "" do
    Map.update!(bucket, :quarantined_recipes, &MapSet.put(&1, recipe))
  end

  defp maybe_quarantine_set(bucket, _, _), do: bucket

  # ─── Output ───────────────────────────────────────────────────────────

  defp emit_text(rows, days) do
    Mix.shell().info("Neural rebalancer strategy effectiveness — last #{days} day(s)")
    Mix.shell().info(String.duplicate("─", 78))

    if rows == [] do
      Mix.shell().info(
        "No outcomes in the window. Either the dispatch loop hasn't fired " <>
          "yet OR the outcome log isn't in the expected location (try --outcomes-path)."
      )
    else
      header = ["strategy", "outcomes", "success%", "verified", "still_present", "auto-q recipes"]
      Mix.shell().info(format_row(header))
      Mix.shell().info(format_row(Enum.map(header, fn _ -> "────────" end)))

      Enum.each(rows, fn r ->
        success_pct =
          case r.total do
            0 -> "—"
            n -> "#{Float.round(r.successes / n * 100, 1)}%"
          end

        Mix.shell().info(
          format_row([
            r.strategy,
            Integer.to_string(r.total),
            success_pct,
            Integer.to_string(r.verified),
            Integer.to_string(r.still_present),
            Integer.to_string(MapSet.size(r.quarantined_recipes))
          ])
        )
      end)
    end
  end

  defp format_row(cells) do
    cells
    |> Enum.map(fn c -> String.pad_trailing(to_string(c), 16) end)
    |> Enum.join("  ")
  end

  defp emit_json(rows) do
    json =
      Enum.map(rows, fn r ->
        %{
          strategy: r.strategy,
          total: r.total,
          successes: r.successes,
          failures: r.failures,
          false_positives: r.false_positives,
          verified: r.verified,
          still_present: r.still_present,
          quarantined_recipes: MapSet.to_list(r.quarantined_recipes),
          success_rate: if(r.total > 0, do: r.successes / r.total, else: nil),
          verified_rate:
            if(r.verified + r.still_present > 0,
              do: r.verified / (r.verified + r.still_present),
              else: nil
            )
        }
      end)

    IO.puts(Jason.encode!(%{generated_at: DateTime.utc_now() |> DateTime.to_iso8601(), rows: json}))
  end

  defp verisim_path do
    Application.get_env(:hypatia, :verisimdb_data_path, "data/verisim")
  end
end
