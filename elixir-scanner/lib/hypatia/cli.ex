# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.CLI do
  @moduledoc """
  Command-line interface for Hypatia scanner.
  """

  alias Hypatia.Scanner

  def main(args) do
    # Configure logger to write to stderr only
    :logger.remove_handler(:default)
    :logger.add_handler(:default, :logger_std_h, %{
      config: %{type: :standard_error}
    })

    {opts, args, _invalid} =
      OptionParser.parse(args,
        switches: [
          help: :boolean,
          version: :boolean,
          format: :string,
          severity: :string
        ],
        aliases: [h: :help, v: :version]
      )

    cond do
      opts[:help] -> print_help()
      opts[:version] -> print_version()
      length(args) > 0 -> run_scan(List.first(args), opts)
      true -> print_help()
    end
  end

  defp run_scan(target, opts) do
    unless File.exists?(target) do
      IO.puts(:stderr, "Error: #{target} not found")
      System.halt(1)
    end

    report = Scanner.generate_report(target)

    # Filter by severity if specified
    report =
      if severity = opts[:severity] do
        severity_atom = String.to_existing_atom(severity)
        severities = severity_levels_from(severity_atom)

        update_in(report.findings, fn findings ->
          Enum.filter(findings, fn f ->
            String.to_existing_atom(f.severity) in severities
          end)
        end)
      else
        report
      end

    # Output in requested format
    case opts[:format] || "json" do
      "json" ->
        Jason.encode!(report, pretty: true)
        |> IO.puts()

      "text" ->
        print_text_report(report)

      "github" ->
        print_github_actions_format(report)

      format ->
        IO.puts(:stderr, "Unknown format: #{format}")
        System.halt(1)
    end
  end

  defp severity_levels_from(:critical), do: [:critical]
  defp severity_levels_from(:high), do: [:critical, :high]
  defp severity_levels_from(:medium), do: [:critical, :high, :medium]
  defp severity_levels_from(:low), do: [:critical, :high, :medium, :low]

  defp print_text_report(report) do
    IO.puts("\n=== Hypatia Security Scan Report ===")
    IO.puts("Scanned: #{report.scan_info.scanned_files} files")
    IO.puts("Found: #{report.scan_info.total_findings} issues")
    IO.puts("Duration: #{report.scan_info.scan_duration_ms}ms\n")

    report.findings
    |> Enum.group_by(& &1.severity)
    |> Enum.each(fn {severity, findings} ->
      IO.puts("#{String.upcase(severity)} (#{length(findings)} issues):")

      Enum.each(findings, fn f ->
        IO.puts("  #{f.file}:#{f.line}")
        IO.puts("    Pattern: #{f.pattern}")
        IO.puts("    Fix: #{f.fix}\n")
      end)
    end)
  end

  defp print_github_actions_format(report) do
    Enum.each(report.findings, fn f ->
      IO.puts("::#{f.severity} file=#{f.file},line=#{f.line}::#{f.pattern} - #{f.fix}")
    end)
  end

  defp print_help do
    IO.puts("""
    Hypatia v2.0.0 - Security Scanner

    USAGE:
        hypatia [OPTIONS] <path>

    OPTIONS:
        --help, -h              Show this help
        --version, -v           Show version
        --format <format>       Output format: json, text, github (default: json)
        --severity <level>      Minimum severity: critical, high, medium, low

    EXAMPLES:
        hypatia .                             # Scan current directory
        hypatia src/auth/JWT.res              # Scan specific file
        hypatia --format=text .               # Text output
        hypatia --severity=critical .         # Only critical issues
        hypatia --format=github . >> $GITHUB_OUTPUT  # GitHub Actions
    """)
  end

  defp print_version do
    IO.puts("Hypatia Scanner v2.0.0 (Elixir)")
  end
end
