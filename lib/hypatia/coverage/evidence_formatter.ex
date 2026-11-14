# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.EvidenceFormatter do
  @moduledoc """
  ExUnit formatter that writes an evidence file for every test (design §3.1).

  The file is `{"schema": "hypatia.proven-coverage.evidence/1", "evidence": [...]}`
  and loads with `Hypatia.Coverage.Evidence.load/1`.

  ## Use

      # test/test_helper.exs
      ExUnit.configure(formatters: [ExUnit.CLIFormatter, Hypatia.Coverage.EvidenceFormatter])
      ExUnit.start()

      HYPATIA_EVIDENCE_OUT=cover/evidence.json GITHUB_REPOSITORY=o/n GITHUB_SHA=<40 hex> mix test

  Without `HYPATIA_EVIDENCE_OUT` (or the `:out` option) the formatter writes nothing.
  Options come from `Application.get_env(:hypatia, Hypatia.Coverage.EvidenceFormatter)`:
  `:out`, `:repo`, `:commit`, `:run_id`. Environment: `HYPATIA_EVIDENCE_OUT`,
  `GITHUB_REPOSITORY`, `GITHUB_SHA`, `GITHUB_RUN_ID`, `RUNNER_OS`. When repo or commit
  is unknown the key is left out; the report then does not count the record
  (`:wrong_repo`, `:no_commit`). Nothing is invented.

  ## Tagging

  An untagged test is kind `E` (an example). Tag a stronger test with a map:

      @tag :proven
      @tag evidence: %{
             kind: "R",
             generator: "int in 1..50",
             seed: 42,
             cases: 500,
             fixtures: %{silence: "n in 1..50", firing: %{source: "...", defect_class: "wrong-branch"}},
             mutation: %{tool: "muex", killed: 7, survived: 0}
           }

  Map keys:

    * `kind` - `P R C B E M` (default `E`)
    * `fixtures` - copied as given. Default `%{cannot_fail_by_design: "example test"}`
    * `property` (or the flat keys `generator`, `seed`, `cases`, `shrunk`), `conformance`,
      `benchmark`, `proof`, `mutation` - copied into `run`
    * `subjects`, `tier`, `category`, `aspect` - copied to the record top level

  Select the counted subset with a second tag, `@tag :proven`, run with
  `mix test --only proven`. The `evidence` map alone also works with `--only evidence`
  (ExUnit matches a bare key against any value) but then selects every tagged test,
  including kind `E`, so the evidence tag does not say "counted". Use `:proven` for that.

  ## Outcome

  `passed` for a pass; `failed` for an assertion failure; `error` for any other
  exception, exit, throw or a failed `setup_all`; `skipped` for skipped and excluded tests.
  Excluded tests (for example under `--only proven`) are recorded as skipped, never counted.
  """

  use GenServer

  @schema "hypatia.proven-coverage.evidence/1"
  @top_level ~w(subjects tier category aspect)a
  @run_sections ~w(conformance benchmark proof mutation)a
  @property_flat ~w(generator seed cases shrunk)a

  ## GenServer

  @impl true
  def init(_opts) do
    config = Application.get_env(:hypatia, __MODULE__, [])
    {:ok, %{config: config, records: [], aborted: false}}
  end

  @impl true
  def handle_cast({:test_finished, %ExUnit.Test{} = test}, state) do
    {:noreply, %{state | records: [record(test, state.config) | state.records]}}
  end

  # `--max-failures` stops the run before every test is scheduled. An evidence file
  # from such a run would look complete while missing tests, so none is written; the
  # report then says the evidence is missing (COV-E001) instead of judging a partial run.
  def handle_cast(:max_failures_reached, state), do: {:noreply, Map.put(state, :aborted, true)}

  def handle_cast({:suite_finished, _times}, %{aborted: true} = state) do
    IO.warn(
      "Hypatia.Coverage.EvidenceFormatter: run stopped at --max-failures; no evidence file written"
    )

    {:noreply, state}
  end

  def handle_cast({:suite_finished, _times}, state) do
    write(state)
    {:noreply, state}
  end

  def handle_cast(_event, state), do: {:noreply, state}

  defp write(%{config: config, records: records}) do
    case config[:out] || System.get_env("HYPATIA_EVIDENCE_OUT") do
      path when is_binary(path) and path != "" ->
        File.mkdir_p!(Path.dirname(path))
        File.write!(path, document(Enum.reverse(records)))

      _ ->
        :ok
    end
  end

  ## Pure part

  @doc "The evidence file text for a list of records."
  @spec document([map()]) :: String.t()
  def document(records) do
    Jason.encode!(%{"schema" => @schema, "evidence" => records}, pretty: true) <> "\n"
  end

  @doc """
  The evidence record (string keys) for one finished `ExUnit.Test`. `config` takes `:repo`,
  `:commit`, `:run_id`, falling back to the environment.
  """
  @spec record(ExUnit.Test.t(), keyword()) :: map()
  def record(%ExUnit.Test{} = test, config \\ []) do
    tag = evidence_tag(test.tags[:evidence])
    kind = kind(tag)

    test_id =
      %{
        "module_path" => inspect(test.module),
        "name" => Atom.to_string(test.name),
        "file" => relative(test.tags[:file]),
        "line" => test.tags[:line],
        "parameters" => parameters(test)
      }
      |> drop_nils()

    repo = config[:repo] || env("GITHUB_REPOSITORY")
    commit = config[:commit] || env("GITHUB_SHA")

    %{
      "schema" => @schema,
      "id" => id(repo, test_id, kind),
      "repo" => repo,
      "commit" => commit,
      "run_id" =>
        config[:run_id] || env("HYPATIA_COVERAGE_RUN_ID") || env("GITHUB_RUN_ID") ||
          (commit && "local-" <> commit),
      "language" => "elixir",
      "kind" => kind,
      "test_id" => test_id,
      "fixtures" => fixtures(tag),
      "run" => run(test, tag)
    }
    |> Map.merge(top_level(tag))
    |> drop_nils()
  end

  @doc "The outcome string for an `ExUnit.Test.state`."
  @spec outcome(ExUnit.Test.state()) :: String.t()
  def outcome(nil), do: "passed"
  def outcome({:skipped, _}), do: "skipped"
  def outcome({:excluded, _}), do: "skipped"
  def outcome({:invalid, _}), do: "error"

  def outcome({:failed, failures}) do
    if Enum.any?(failures, &assertion?/1), do: "failed", else: "error"
  end

  def outcome(_), do: "error"

  defp assertion?({:error, %ExUnit.AssertionError{}, _}), do: true

  defp assertion?({:error, %ExUnit.MultiError{errors: errors}, _}),
    do: Enum.any?(errors, &assertion?/1)

  defp assertion?(_), do: false

  defp evidence_tag(%{} = map), do: Map.new(map, fn {k, v} -> {to_key(k), v} end)
  defp evidence_tag(_), do: %{}

  defp to_key(k) when is_atom(k), do: k
  defp to_key(k) when is_binary(k), do: String.to_atom(k)
  defp to_key(k), do: k

  defp kind(tag) do
    case tag[:kind] do
      k when k in ["P", "R", "C", "B", "E", "M"] -> k
      k when k in [:P, :R, :C, :B, :E, :M] -> Atom.to_string(k)
      nil -> "E"
      other -> jsonable(other)
    end
  end

  # Only kinds that never count (E, M, unknown) get a default; a counted kind (P/R/C/B)
  # with no fixtures stays
  # inadmissible, so Evidence.counted?/2 reports :no_fixtures instead of counting it.
  # Elixir 1.18+ `parameterize:` runs one test per parameter set with the same name and
  # line; the parameters keep their records (and ids) apart.
  defp parameters(test) do
    case Map.get(test, :parameters) do
      p when p in [nil, %{}] -> nil
      p -> %{"display" => jsonable(p), "term_sha256" => term_sha256(p)}
    end
  end

  # The display form is lossy (atoms and strings both become JSON strings); the hash of
  # the exact Erlang term keeps parameter sets such as %{n: :one} and %{n: "one"} apart.
  defp term_sha256(term),
    do:
      :crypto.hash(:sha256, :erlang.term_to_binary(term, [:deterministic]))
      |> Base.encode16(case: :lower)

  defp fixtures(tag) do
    case {tag[:fixtures], kind(tag)} do
      {nil, k} when k in ["P", "R", "C", "B"] -> nil
      {nil, _} -> %{"cannot_fail_by_design" => "example test"}
      {f, _} -> jsonable(f)
    end
  end

  defp top_level(tag) do
    for k <- @top_level,
        Map.has_key?(tag, k),
        into: %{},
        do: {Atom.to_string(k), jsonable(tag[k])}
  end

  defp run(test, tag) do
    base = %{
      "outcome" => outcome(test.state),
      "duration_ms" => div(test.time || 0, 1000),
      "toolchain" => "elixir #{System.version()} / otp #{System.otp_release()}",
      "runner" => env("RUNNER_OS")
    }

    property =
      case tag[:property] do
        %{} = p -> jsonable(p)
        _ -> flat_property(tag)
      end

    sections =
      for k <- @run_sections,
          Map.has_key?(tag, k),
          into: %{},
          do: {Atom.to_string(k), jsonable(tag[k])}

    base
    |> Map.merge(sections)
    |> Map.put("property", property)
    |> drop_nils()
  end

  defp flat_property(tag) do
    flat =
      for k <- @property_flat,
          Map.has_key?(tag, k),
          into: %{},
          do: {Atom.to_string(k), jsonable(tag[k])}

    if flat == %{}, do: nil, else: flat
  end

  defp id(repo, test_id, kind) do
    :crypto.hash(:sha256, :erlang.term_to_binary({repo, test_id, kind}, [:deterministic]))
    |> Base.encode16(case: :lower)
  end

  defp relative(nil), do: nil
  defp relative(file), do: Hypatia.Coverage.CoverExport.relative_source(file)

  defp env(name) do
    case System.get_env(name) do
      v when v in [nil, ""] -> nil
      v -> v
    end
  end

  defp drop_nils(map), do: map |> Enum.reject(fn {_, v} -> is_nil(v) end) |> Map.new()

  # JSON-safe copy: atoms become strings; maps get string keys; anything else odd is inspected.
  defp jsonable(v) when is_binary(v) or is_number(v) or is_boolean(v) or is_nil(v), do: v
  defp jsonable(v) when is_atom(v), do: Atom.to_string(v)
  defp jsonable(v) when is_list(v), do: Enum.map(v, &jsonable/1)

  defp jsonable(%{__struct__: _} = v), do: inspect(v)

  defp jsonable(v) when is_map(v),
    do: Map.new(v, fn {k, x} -> {to_string_key(k), jsonable(x)} end)

  defp jsonable(v), do: inspect(v)

  defp to_string_key(k) when is_binary(k), do: k
  defp to_string_key(k) when is_atom(k), do: Atom.to_string(k)
  defp to_string_key(k), do: inspect(k)
end
