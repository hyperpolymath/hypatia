# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.HistoricalTrends do
  @moduledoc """
  Tracks how scan results change across scan cycles.

  The day-to-day pipeline produces snapshots — one scan JSON per repo,
  overwriting the previous — which is fine for "what's broken now" but
  can't answer:

    * Is weak-point count improving or worsening for repo X?
    * Is a PA category spreading or receding across the estate?
    * Are we hitting a CVE wave?

  This module keeps a tamper-resistant history of scan snapshots in
  append-only JSONL, keyed by date. It exposes trend queries that
  compose with the VCL Query layer.

  Writes to:

      data/verisim/scan-history/YYYY-MM-DD.jsonl

  One line per scan cycle:

      {
        "scan_id": "scan-20260422164200",
        "scanned_at": "2026-04-22T16:42:00Z",
        "repos": {
          "repo-0": {"weak_point_count": 5, "categories": {"injection": 2}},
          ...
        },
        "total_weak_points": 3385,
        "total_repos": 302
      }

  Pure module. No GenServer. Writes are append-only; if two scan cycles
  record on the same day their lines simply stack.
  """

  require Logger

  @scan_history_path Application.compile_env(
                      :hypatia,
                      :scan_history_path,
                      "data/verisim/scan-history"
                    )

  @doc """
  Record a scan snapshot. Call this at the tail of
  `PatternAnalyzer.analyze_all_scans/0`.

  `scans` must be in the shape produced by
  `VerisimConnector.fetch_all_scans/0`: a list of
  `%{repo: repo_name, scan: %{"weak_points" => [...], ...}}`.

  Returns `{:ok, scan_id, record}` where `record` is the full JSON line
  written to disk.
  """
  def record_scan_snapshot(scans, opts \\ []) when is_list(scans) do
    now = Keyword.get(opts, :at, DateTime.utc_now())
    scan_id = Keyword.get(opts, :scan_id, default_scan_id(now))

    repos_map =
      scans
      |> Enum.map(fn %{repo: repo, scan: scan} ->
        {to_string(repo), summarize_scan(scan)}
      end)
      |> Map.new()

    total_weak_points =
      repos_map
      |> Map.values()
      |> Enum.reduce(0, fn %{"weak_point_count" => n}, acc -> acc + n end)

    record = %{
      "scan_id" => scan_id,
      "scanned_at" => DateTime.to_iso8601(now),
      "repos" => repos_map,
      "total_weak_points" => total_weak_points,
      "total_repos" => map_size(repos_map)
    }

    path = path_for(now)
    File.mkdir_p!(Path.dirname(path))
    line = Jason.encode!(record) <> "\n"
    File.write!(path, line, [:append])

    Logger.info(
      "HistoricalTrends recorded #{scan_id}: #{record["total_repos"]} repos, " <>
        "#{total_weak_points} weak points"
    )

    {:ok, scan_id, record}
  end

  @doc """
  Return the weak-point trajectory for a repo between `from_date` and
  `to_date` (inclusive).

  One row per recorded scan cycle. Each row carries a `:delta` against
  the previous recorded count (0 for the first row in the window).
  Sorted ascending by `:scanned_at`.

  Dates may be `Date.t()` or ISO-8601 strings.
  """
  def weak_point_trend(repo, from_date, to_date) do
    points =
      from_date
      |> load_range(to_date)
      |> Enum.flat_map(fn rec ->
        case get_in(rec, ["repos", to_string(repo)]) do
          %{"weak_point_count" => n} ->
            [
              %{
                scan_id: rec["scan_id"],
                scanned_at: rec["scanned_at"],
                date: date_slice(rec["scanned_at"]),
                count: n
              }
            ]

          _ ->
            []
        end
      end)
      |> Enum.sort_by(& &1.scanned_at)

    with_deltas(points)
  end

  @doc """
  Frequency of a single category across the recorded scan history,
  within the trailing `window_days` window.

  Returns a list of `%{date, count}` sorted ascending. `count` is the
  estate-wide occurrence count on that scan cycle.
  """
  def category_frequency(category, window_days \\ 30) do
    to_date = Date.utc_today()
    from_date = Date.add(to_date, -window_days)

    from_date
    |> load_range(to_date)
    |> Enum.map(fn rec ->
      count =
        rec
        |> Map.get("repos", %{})
        |> Map.values()
        |> Enum.reduce(0, fn repo_summary, acc ->
          acc + get_category_count(repo_summary, category)
        end)

      %{date: date_slice(rec["scanned_at"]), count: count}
    end)
  end

  @doc """
  Detect repos whose weak-point count has spiked by more than
  `z_threshold` standard deviations from their own trailing mean.

  Options:
    * `:lookback_days` — trailing window (default 7)
    * `:z_threshold` — sigma threshold (default 2.0)
    * `:min_points` — minimum samples to score (default 3)

  Returns `[%{repo, anomaly_type, z_score, latest_count, trailing_mean,
  trailing_stdev}]` sorted by descending `:z_score`.
  """
  def detect_anomalies(opts \\ []) do
    lookback = Keyword.get(opts, :lookback_days, 7)
    z_threshold = Keyword.get(opts, :z_threshold, 2.0)
    min_points = Keyword.get(opts, :min_points, 3)

    to_date = Date.utc_today()
    from_date = Date.add(to_date, -lookback)

    from_date
    |> load_range(to_date)
    |> Enum.flat_map(fn rec ->
      Enum.map(Map.get(rec, "repos", %{}), fn {repo, summary} ->
        {repo,
         %{scanned_at: rec["scanned_at"], count: Map.get(summary, "weak_point_count", 0)}}
      end)
    end)
    |> Enum.group_by(fn {repo, _} -> repo end, fn {_, point} -> point end)
    |> Enum.flat_map(fn {repo, points} ->
      points = Enum.sort_by(points, & &1.scanned_at)

      cond do
        length(points) < min_points ->
          []

        true ->
          counts = Enum.map(points, & &1.count)
          {mean, stdev} = mean_and_stdev(counts)
          latest = List.last(points)

          if stdev > 0 and abs(latest.count - mean) / stdev >= z_threshold do
            [
              %{
                repo: repo,
                anomaly_type: :weak_point_spike,
                z_score: (latest.count - mean) / stdev,
                latest_count: latest.count,
                trailing_mean: mean,
                trailing_stdev: stdev
              }
            ]
          else
            []
          end
      end
    end)
    |> Enum.sort_by(& &1.z_score, :desc)
  end

  # ── helpers ──────────────────────────────────────────────────────────────

  defp summarize_scan(scan) do
    weak_points = Map.get(scan, "weak_points") || Map.get(scan, :weak_points) || []

    categories =
      weak_points
      |> Enum.map(fn wp ->
        Map.get(wp, "category") || Map.get(wp, :category) || "unknown"
      end)
      |> Enum.frequencies()

    %{"weak_point_count" => length(weak_points), "categories" => categories}
  end

  defp get_category_count(%{"categories" => cats}, category) when is_map(cats) do
    Map.get(cats, to_string(category), 0)
  end

  defp get_category_count(_, _), do: 0

  defp path_for(%DateTime{} = dt) do
    date_str = dt |> DateTime.to_date() |> Date.to_iso8601()
    Path.join(@scan_history_path, "#{date_str}.jsonl")
  end

  defp default_scan_id(%DateTime{} = dt) do
    compact =
      dt
      |> DateTime.to_iso8601()
      |> String.replace(~r/[-:T.Z]/, "")
      |> String.slice(0, 14)

    "scan-#{compact}"
  end

  defp date_slice(iso) when is_binary(iso), do: String.slice(iso, 0, 10)
  defp date_slice(_), do: ""

  defp with_deltas([]), do: []

  defp with_deltas(points) do
    {result, _final} =
      Enum.map_reduce(points, nil, fn pt, prev ->
        delta = if prev, do: pt.count - prev, else: 0
        {Map.put(pt, :delta, delta), pt.count}
      end)

    result
  end

  defp load_range(from_date, to_date) do
    from_date = coerce_date(from_date)
    to_date = coerce_date(to_date)

    @scan_history_path
    |> list_jsonl_files()
    |> Enum.filter(fn file ->
      case file_date(file) do
        {:ok, d} ->
          Date.compare(d, from_date) != :lt and Date.compare(d, to_date) != :gt

        _ ->
          false
      end
    end)
    |> Enum.flat_map(&read_jsonl/1)
  end

  defp coerce_date(%Date{} = d), do: d
  defp coerce_date(s) when is_binary(s), do: Date.from_iso8601!(s)

  defp list_jsonl_files(dir) do
    case File.ls(dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.map(&Path.join(dir, &1))
        |> Enum.sort()

      _ ->
        []
    end
  end

  defp file_date(path) do
    path
    |> Path.basename(".jsonl")
    |> Date.from_iso8601()
  end

  defp read_jsonl(path) do
    path
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.reject(&(&1 == ""))
    |> Enum.map(&Jason.decode/1)
    |> Enum.flat_map(fn
      {:ok, v} -> [v]
      _ -> []
    end)
  end

  defp mean_and_stdev(xs) do
    n = length(xs)
    mean = Enum.sum(xs) / n
    var = Enum.reduce(xs, 0.0, fn x, acc -> acc + (x - mean) * (x - mean) end) / n
    {mean, :math.sqrt(var)}
  end
end
