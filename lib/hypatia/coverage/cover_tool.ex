# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.CoverTool do
  @moduledoc """
  A Mix test-coverage tool that also writes LCOV (design §4.1, Elixir row).

  Opt in from any Mix project:

      # mix.exs
      def project do
        [
          test_coverage: [tool: Hypatia.Coverage.CoverTool, export_lcov: "cover/lcov.info"],
          ...
        ]
      end

  then run `mix test --cover`. The tool is the one Mix calls for `--cover`
  (`start(compile_path, opts)`). It cover-compiles the project exactly as Mix does,
  and after the run it writes the LCOV file, then runs Mix's own summary and
  HTML output (`Mix.Tasks.Test.Coverage`), so the usual behaviour is kept, including
  `:summary`, `:threshold`, `:ignore_modules`, `:output` and `:export`.

  Options besides Mix's own:

    * `:export_lcov` - path of the LCOV file. Without it nothing extra is written.
    * `:repo`, `:commit`, `:run_id`, `:role` - fill the metadata sidecar (below). Defaults:
      `GITHUB_REPOSITORY`, `GITHUB_SHA`, `GITHUB_RUN_ID` (else a generated `local-...` id) and
      `HYPATIA_COVERAGE_ROLE` (`raw` or `proven`).
    * `:ignore_modules` - modules (or regexes, as in Mix) left out of the LCOV file too.

  Next to the LCOV file it writes `<path>.meta.json`, schema
  `hypatia.proven-coverage.report-meta/1`: `repo`, `commit`, `run_id`, `role`
  (`"raw"` for the run of all tests, `"proven"` for the counted subset), `tool`
  (`"erlang-cover"`) and `tool_version` (the `:tools` app version). A value that is
  not known is left out (with a warning for repo, commit and role), never invented.

  Only lines `:cover` reports are written, with the source path each module was
  compiled from, made relative to the current directory. There are no branch
  records: Erlang `:cover` cannot measure branches. See `Hypatia.Coverage.CoverExport`.
  """

  alias Hypatia.Coverage.CoverExport

  # `:cover` lives in OTP's `:tools`, a dev-time app. Mix loads it for `--cover`
  # (`Mix.ensure_application!(:tools)`); it is deliberately not a runtime dependency.
  @compile {:no_warn_undefined, :cover}

  @doc "Mix's `--cover` entry point."
  @spec start(String.t(), keyword()) :: (-> any())
  def start(compile_path, opts) do
    mix_callback = Mix.Tasks.Test.Coverage.start(compile_path, opts)

    fn ->
      case opts[:export_lcov] do
        path when is_binary(path) -> write_lcov(path, opts)
        _ -> :ok
      end

      mix_callback.()
    end
  end

  @doc "Analyse every cover-compiled module and write the LCOV file."
  @spec write_lcov(Path.t(), keyword()) :: :ok
  def write_lcov(path, opts \\ []) do
    ignore = opts[:ignore_modules] || []
    modules = Enum.reject(:cover.modules(), &ignored?(&1, ignore))

    analyses = Map.new(modules, fn m -> {m, :cover.analyse(m, :calls, :line)} end)

    sources =
      for m <- modules, src = source(m), into: %{}, do: {m, CoverExport.relative_source(src)}

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, CoverExport.to_lcov(analyses, sources))
    meta = meta(opts)
    warn_missing(meta)
    File.write!(path <> ".meta.json", Jason.encode!(meta, pretty: true) <> "\n")
    Mix.shell().info("Wrote LCOV coverage to #{inspect(path)}")
    :ok
  end

  @meta_schema "hypatia.proven-coverage.report-meta/1"

  @doc """
  The sidecar map (string keys) for `opts`. Unknown values are left out.
  """
  @spec meta(keyword()) :: map()
  def meta(opts \\ []) do
    %{
      "schema" => @meta_schema,
      "repo" => opts[:repo] || env("GITHUB_REPOSITORY"),
      "commit" => opts[:commit] || env("GITHUB_SHA"),
      "run_id" => opts[:run_id] || env("GITHUB_RUN_ID") || local_run_id(),
      "role" => role(opts[:role] || env("HYPATIA_COVERAGE_ROLE")),
      "tool" => "erlang-cover",
      "tool_version" => tools_version()
    }
    |> Enum.reject(fn {_, v} -> is_nil(v) end)
    |> Map.new()
  end

  defp role(r) when r in ["raw", "proven"], do: r
  defp role(r) when r in [:raw, :proven], do: Atom.to_string(r)
  defp role(_), do: nil

  defp tools_version do
    Mix.ensure_application!(:tools)
    _ = Application.load(:tools)

    case Application.spec(:tools, :vsn) do
      nil -> nil
      vsn -> to_string(vsn)
    end
  end

  defp local_run_id do
    "local-" <>
      Integer.to_string(System.os_time(:second)) <>
      "-" <> Base.encode16(:crypto.strong_rand_bytes(3), case: :lower)
  end

  defp env(name) do
    case System.get_env(name) do
      v when v in [nil, ""] -> nil
      v -> v
    end
  end

  defp warn_missing(meta) do
    for {key, hint} <- [
          {"repo", "GITHUB_REPOSITORY or :repo"},
          {"commit", "GITHUB_SHA or :commit"},
          {"role", "HYPATIA_COVERAGE_ROLE=raw|proven or :role"}
        ],
        not Map.has_key?(meta, key) do
      Mix.shell().info("CoverTool: sidecar has no #{key}; set #{hint}")
    end

    :ok
  end

  defp source(mod) do
    case :cover.is_compiled(mod) do
      {:file, _beam} ->
        mod.module_info(:compile)[:source]

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  defp ignored?(mod, ignores), do: Enum.any?(ignores, &ignored_one?(mod, &1))
  defp ignored_one?(mod, %Regex{} = re), do: Regex.match?(re, inspect(mod))
  defp ignored_one?(mod, other), do: mod == other
end
