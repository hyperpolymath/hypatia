# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.PatternAnalyzer do
  @moduledoc """
  Analyzes scan results from verisimdb and detects patterns.
  """

  alias Hypatia.VerisimdbConnector
  alias Hypatia.FleetDispatcher

  require Logger

  def analyze_all_scans do
    scans = VerisimdbConnector.fetch_all_scans()

    Logger.info("Loaded #{length(scans)} scan results")

    # Write facts to temp file
    facts = Enum.map_join(scans, "\n", &VerisimdbConnector.to_logtalk_facts/1)
    facts_file = "/tmp/scan_facts.lgt"
    File.write!(facts_file, facts)

    Logger.info("Wrote Logtalk facts to #{facts_file}")

    # TODO: Integrate with actual Logtalk interpreter
    # For now, just return the scans
    {:ok, %{
      scan_count: length(scans),
      facts_file: facts_file,
      scans: scans
    }}
  end

  def generate_summary(scans) do
    total_weak_points =
      scans
      |> Enum.map(fn scan ->
        Map.get(scan.scan, "weak_points", []) |> length()
      end)
      |> Enum.sum()

    repos_by_severity =
      scans
      |> Enum.map(fn scan ->
        summary = Map.get(scan.scan, "summary", %{})
        {scan.repo, summary}
      end)
      |> Enum.into(%{})

    %{
      total_repos: length(scans),
      total_weak_points: total_weak_points,
      repos_by_severity: repos_by_severity
    }
  end

  def process_findings(findings) do
    Enum.each(findings, fn finding ->
      case FleetDispatcher.dispatch_finding(finding) do
        {:ok, _} -> :ok
        {:error, reason} -> Logger.error("Dispatch failed: #{inspect(reason)}")
      end
    end)
  end
end
