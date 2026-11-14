# SPDX-License-Identifier: MPL-2.0
defmodule Mix.Tasks.Hypatia.Coverage.Report do
  @moduledoc """
  SHADOW report comparing *proven* coverage against *raw* coverage.

  Not a gate: no PR comment, no check-run, no ruleset. See
  `docs/design/proven-coverage.adoc` (§2, §5.3, §8, §10).

  ## Usage

      mix hypatia.coverage.report \\
        --raw cov-all.json --proven cov-counted.json --evidence evidence.json \\
        --repo OWNER/NAME --sha SHA \\
        [--raw-meta PATH] [--proven-meta PATH] [--root DIR] [--min-cases N] [--format markdown|json] [--out PATH] [--shadow]

  ## Options

    * `--raw PATH`      coverage of the run of ALL tests (llvm-cov JSON or LCOV)
    * `--proven PATH`   coverage of the run of only the COUNTED tests
    * `--evidence PATH` evidence list (`hypatia.proven-coverage.evidence/1`)
    * `--raw-meta PATH`, `--proven-meta PATH`  sidecars (`hypatia.proven-coverage.report-meta/1`) binding
                        each report to repo, commit, run and role; default `<report path>.meta.json`.
                        Missing: COV-E013; mismatching: COV-E007 (both UNJUDGED)
    * `--repo OWNER/NAME`, `--sha SHA`   used for permalinks
    * `--root DIR`      repo checkout (expanded to an absolute path); strips it from report paths and checks
                        that every reported file exists under it (COV-E003)
    * `--min-cases N`   property-test case floor (integer, default and floor 100)
    * `--format`        `markdown` (default) or `json`
    * `--out PATH`      write the report there instead of stdout
    * `--shadow`        always exit 0; the true verdict is still printed

  ## Exit codes

  0 judged pass, 1 judged fail, 2 could not judge (also: bad arguments).
  With `--shadow` the exit code is 0 in every judged or unjudged case;
  the printed outcome is still the true one.
  """

  use Mix.Task

  alias Hypatia.Coverage.{Report, ReportMeta}

  @shortdoc "SHADOW proven-vs-raw coverage report (not a gate)"

  @switches [
    raw: :string,
    proven: :string,
    evidence: :string,
    raw_meta: :string,
    proven_meta: :string,
    repo: :string,
    sha: :string,
    root: :string,
    min_cases: :integer,
    format: :string,
    out: :string,
    shadow: :boolean
  ]

  @impl true
  def run(argv) do
    case execute(argv) do
      {:ok, 0, _output} -> :ok
      {:ok, code, _output} -> exit({:shutdown, code})
      {:usage, message} -> usage_error(message)
    end
  end

  @doc """
  Run the task without printing or halting. Returns `{:ok, exit_code, output}`
  (the output is also written/printed by `run/1`) or `{:usage, message}`.
  """
  @spec execute([String.t()]) :: {:ok, 0 | 1 | 2, String.t()} | {:usage, String.t()}
  def execute(argv) do
    {opts, _rest, invalid} = OptionParser.parse(argv, strict: @switches)

    invalid =
      Enum.map(invalid, fn
        {"--min-cases", _} -> {"--min-cases (must be an integer >= 100)", nil}
        other -> other
      end)

    with :ok <- check_invalid(invalid),
         :ok <- check_required(opts),
         :ok <- check_repo(opts[:repo]),
         :ok <- check_min_cases(opts[:min_cases]),
         {:ok, format} <- check_format(opts[:format]) do
      root = if opts[:root], do: Path.expand(opts[:root])
      load_opts = if root, do: [root: root], else: []

      verdict =
        Report.build(%{
          raw: Report.load_coverage(opts[:raw], load_opts),
          proven: Report.load_coverage(opts[:proven], load_opts),
          evidence: Report.load_evidence(opts[:evidence]),
          raw_meta: Report.load_meta(opts[:raw_meta] || ReportMeta.default_path(opts[:raw])),
          proven_meta:
            Report.load_meta(opts[:proven_meta] || ReportMeta.default_path(opts[:proven])),
          repo: opts[:repo],
          sha: opts[:sha],
          root: root,
          min_cases: opts[:min_cases] || Hypatia.Coverage.Evidence.default_min_cases()
        })

      output =
        if format == "json",
          do: Report.to_json(verdict) <> "\n",
          else: Report.to_markdown(verdict)

      code = if opts[:shadow], do: 0, else: Report.exit_code(verdict)
      emit(output, opts[:out], verdict)
      {:ok, code, output}
    end
  end

  defp emit(output, nil, _verdict), do: IO.write(output)

  defp emit(output, path, verdict) do
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, output)
    IO.puts("hypatia.coverage.report: #{verdict["outcome"]} (shadow) -> #{path}")
  end

  defp check_invalid([]), do: :ok

  defp check_invalid(invalid),
    do: {:usage, "unknown or malformed option(s): " <> Enum.map_join(invalid, ", ", &elem(&1, 0))}

  defp check_required(opts) do
    missing = for k <- [:raw, :proven, :evidence, :repo, :sha], is_nil(opts[k]), do: "--#{k}"

    if missing == [],
      do: :ok,
      else: {:usage, "missing required option(s): " <> Enum.join(missing, ", ")}
  end

  defp check_repo(repo) do
    if Regex.match?(~r{\A[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\z}, repo),
      do: :ok,
      else: {:usage, "--repo must look like OWNER/NAME, got #{inspect(repo)}"}
  end

  defp check_min_cases(nil), do: :ok
  defp check_min_cases(n) when is_integer(n) and n >= 100, do: :ok

  defp check_min_cases(n),
    do:
      {:usage, "--min-cases must be an integer >= 100 (the floor, design Q8), got #{inspect(n)}"}

  defp check_format(nil), do: {:ok, "markdown"}
  defp check_format(f) when f in ["markdown", "json"], do: {:ok, f}
  defp check_format(f), do: {:usage, "--format must be markdown or json, got #{inspect(f)}"}

  defp usage_error(message) do
    Mix.shell().error("""
    #{message}

    Usage: mix hypatia.coverage.report --raw PATH --proven PATH --evidence PATH \\
             --repo OWNER/NAME --sha SHA [--raw-meta PATH] [--proven-meta PATH] [--root DIR] [--min-cases N] \\
             [--format markdown|json] [--out PATH] [--shadow]
    """)

    exit({:shutdown, 2})
  end
end
