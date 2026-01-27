# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Scanner do
  @moduledoc """
  Core scanning logic with concurrent file processing.
  """

  alias Hypatia.{Finding, Patterns}

  require Logger

  @doc """
  Scan a directory or file and return findings.
  """
  def scan(path) do
    if File.dir?(path) do
      scan_directory(path)
    else
      scan_file(path)
    end
  end

  @doc """
  Scan a single file and return findings.
  """
  def scan_file(file) do
    ext = Path.extname(file)
    patterns = Patterns.patterns_for_extension(ext) ++ Patterns.universal_patterns()

    case File.read(file) do
      {:ok, content} ->
        content
        |> String.split("\n", trim: false)
        |> Enum.with_index(1)
        |> Enum.flat_map(fn {line, line_num} ->
          Patterns.scan_line(file, line_num, line, patterns)
        end)

      {:error, reason} ->
        Logger.warning("Failed to read #{file}: #{reason}")
        []
    end
  end

  @doc """
  Scan a directory recursively with concurrent file processing.
  """
  def scan_directory(dir) do
    files = find_source_files(dir)

    Logger.info("Scanning #{length(files)} files in #{dir}")

    # Scan files concurrently
    files
    |> Task.async_stream(&scan_file/1,
      max_concurrency: System.schedulers_online() * 2,
      timeout: 60_000
    )
    |> Enum.flat_map(fn
      {:ok, findings} -> findings
      {:exit, reason} ->
        Logger.error("Task crashed: #{inspect(reason)}")
        []
    end)
  end

  @doc """
  Find all source files in a directory recursively.
  """
  def find_source_files(dir) do
    extensions = [".rs", ".res", ".ml"]

    Path.wildcard(Path.join(dir, "**/*"))
    |> Enum.filter(fn path ->
      File.regular?(path) and Path.extname(path) in extensions and
        not String.contains?(path, "/target/") and
        not String.contains?(path, "/node_modules/") and
        not String.contains?(path, "/.git/")
    end)
  end

  @doc """
  Generate a complete scan report.
  """
  def generate_report(target) do
    start_time = DateTime.utc_now()
    findings = scan(target)
    end_time = DateTime.utc_now()

    files_scanned =
      if File.dir?(target) do
        length(find_source_files(target))
      else
        1
      end

    %{
      scan_info: %{
        timestamp: DateTime.to_iso8601(start_time),
        repository: Path.basename(target),
        scanner_version: "2.0.0",
        scanned_files: files_scanned,
        total_findings: length(findings),
        scan_duration_ms: DateTime.diff(end_time, start_time, :millisecond)
      },
      findings: Enum.map(findings, &Finding.to_json/1)
    }
  end
end
