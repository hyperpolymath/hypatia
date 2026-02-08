# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.VerisimdbConnector do
  @moduledoc """
  Reads scan results from verisimdb-data repo and transforms into Logtalk facts.
  """

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  def fetch_all_scans do
    scans_path = Path.join(expand_path(@verisimdb_data_path), "scans")

    case File.ls(scans_path) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.map(&load_scan/1)
        |> Enum.reject(&is_nil/1)

      {:error, reason} ->
        require Logger
        Logger.error("Failed to read scans directory: #{inspect(reason)}")
        []
    end
  end

  defp load_scan(filename) do
    path = Path.join([expand_path(@verisimdb_data_path), "scans", filename])
    repo_name = String.replace(filename, ".json", "")

    with {:ok, content} <- File.read(path),
         {:ok, data} <- Jason.decode(content) do
      %{repo: repo_name, scan: data}
    else
      {:error, reason} ->
        require Logger
        Logger.error("Failed to load scan #{filename}: #{inspect(reason)}")
        nil
    end
  end

  def to_logtalk_facts(scan_data) do
    # Transform weak points into Logtalk facts
    weak_points = Map.get(scan_data.scan, "weak_points", [])

    weak_points
    |> Enum.map(fn wp ->
      file = Map.get(wp, "file", "unknown")
      category = Map.get(wp, "category", "unknown")
      severity = Map.get(wp, "severity", "unknown")

      """
      weak_point('#{scan_data.repo}', '#{file}', '#{category}', '#{severity}').
      """
    end)
    |> Enum.join("\n")
  end

  defp expand_path(path) do
    Path.expand(path)
  end
end
