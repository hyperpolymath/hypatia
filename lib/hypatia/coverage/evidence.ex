# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.Evidence do
  @moduledoc """
  Evidence records and the "does this count?" decision (design §2, §3.1).

  Records are the §3.1 `EvidenceRecord` shape as decoded JSON (string keys).
  Only the fields the decision reads are needed; the rest is a free subset.

  ## File format

  Either a JSON array of records, or an object
  `{"schema": "hypatia.proven-coverage.evidence/1", "evidence": [...]}`.
  A record may carry its own `"schema"`; any value other than
  `hypatia.proven-coverage.evidence/1` is rejected.

  ## The decision

  `counted?/2` returns `:counted` or `{:not_counted, reason}`. Checks run in
  this order and the first failure wins:

  1. `run.outcome == "passed"` (else `:not_passed`; a `run` that is not an object
     is simply not passed)
  2. the record is admissible: `fixtures` is `{"debt": ...}` -> `:fixture_debt`;
     otherwise it must hold both `silence` and `firing`, or a non-empty
     `cannot_fail_by_design` reason (else `:no_fixtures`)
  3. evidence binding (kinds `P R C B`; E and M are never counted anyway): when `:repo` / `:commit` options are given (the report
     always gives them), the record's `repo` must equal the requested repo
     (`:wrong_repo`, case-insensitive) and its `commit` must be present
     (`:no_commit`) and be the full 40-hex sha equal to the requested one
     (`:wrong_commit`, case-insensitive)
  4. kind `E` -> `:example_test`, kind `M` -> `:mock_test`; any kind other than
     `P R C B` -> `:unknown_kind`
  5. the kind's requirements (the *overclaim* reasons, COV-E010: the record
     claims a kind it cannot back):

     * `R`: `run.property.seed` present (`:no_seed`) and
       `run.property.cases >= min_cases` (`:too_few_cases`); default 100,
       set per repo with the `:min_cases` option. The effective minimum is
       `max(min_cases, 100)`: the floor (design Q8) cannot be lowered
     * `C`: `spec`, `version`, `vectors_sha256` all present under
       `run.conformance` (`:no_spec`, `:no_version`, `:no_vectors_sha256`) and
       `failed == 0` (`:conformance_failures`)
     * `B`: `run.benchmark.checksum == true` (`:no_checksum`)
     * `P`: `run.proof.ladder` a non-empty list (`:empty_ladder`); every rung
       has `theorem`, `file`, `line` (integer), `checker`, `checker_version`
       and `statement_sha256` (64 hex chars) (`:incomplete_rung`) and a passed
       check (`:proof_check_not_passed`): `"check": "passed"`,
       `"check": {"outcome": "passed"}` or `"passed": true`; and
       `run.proof.covers_decls` a non-empty list of non-empty strings
       (`:no_covers_decls`)

  6. mutation (owner ruling Q7), for kinds `R`, `C`, `B` only; `P` is exempt.
     When the record's `language` has a mutation tool (rust, js, ts, javascript, typescript, elixir, julia),
     `run.mutation` must carry `tool` (non-empty string), `killed` (integer >= 1)
     and `survived == 0`; otherwise `:no_mutation_result` (missing or malformed)
     or `:mutants_survived`. For other languages the condition does not apply
     and the report notes "mutation: not measurable for <lang>". These are
     ordinary not-counted reasons, not overclaims.

  The mutation check is a deliberate simplification: design §2 states it per
  line (a mutant at that location must be killed by counted evidence); this
  increment applies it per record.

  Malformed-but-valid JSON never crashes the decision: fields of the wrong
  type are treated as absent, and a record whose `kind` or `run` has the wrong
  type is simply not counted with a reason.

  Nothing is silently dropped: every record gets a decision.
  """

  @schema "hypatia.proven-coverage.evidence/1"
  @default_min_cases 100

  @type record :: map()
  @type decision :: :counted | {:not_counted, atom()}

  @overclaim_reasons [
    :no_seed,
    :too_few_cases,
    :no_spec,
    :no_version,
    :no_vectors_sha256,
    :conformance_failures,
    :no_checksum,
    :empty_ladder,
    :incomplete_rung,
    :proof_check_not_passed,
    :no_covers_decls
  ]

  @mutation_languages ~w(rust js ts javascript typescript elixir julia)

  @doc "The schema string records must carry (when they carry one)."
  def schema, do: @schema

  @doc "Default minimum property-test case count (design Q8: floor 100)."
  def default_min_cases, do: @default_min_cases

  @doc "Languages with a mutation tool (design §2.1)."
  def mutation_languages, do: @mutation_languages

  @doc "Normalised language of a record (`\"unknown\"` when absent or not a string)."
  @spec language(record()) :: String.t()
  def language(record) do
    case record["language"] do
      l when is_binary(l) ->
        if String.trim(l) == "", do: "unknown", else: String.downcase(String.trim(l))

      _ ->
        "unknown"
    end
  end

  @doc "The record's kind when it is a string, else nil."
  @spec kind(record()) :: String.t() | nil
  def kind(record), do: if(is_binary(record["kind"]), do: record["kind"])

  @doc "Reasons that mean a record claims a kind it cannot back (COV-E010)."
  @spec overclaim_reasons() :: [atom()]
  def overclaim_reasons, do: @overclaim_reasons

  @spec overclaim?(decision()) :: boolean()
  def overclaim?({:not_counted, reason}), do: reason in @overclaim_reasons
  def overclaim?(:counted), do: false

  @doc """
  Read an evidence file. Errors: `{:error, {:missing, path}}`,
  `{:error, {:parse, path, where}}`.
  """
  @spec load(Path.t()) :: {:ok, [record()]} | {:error, term()}
  def load(path) do
    case File.read(path) do
      {:ok, text} ->
        case decode(text) do
          {:ok, list} -> {:ok, list}
          {:error, where} -> {:error, {:parse, path, where}}
        end

      {:error, _} ->
        {:error, {:missing, path}}
    end
  end

  @doc "Decode evidence JSON text. `{:error, where}` carries a human location."
  @spec decode(String.t()) :: {:ok, [record()]} | {:error, String.t()}
  def decode(text) do
    case Jason.decode(text) do
      {:ok, list} when is_list(list) ->
        validate(list)

      {:ok, %{"evidence" => list} = doc} when is_list(list) ->
        case doc do
          %{"schema" => s} when s != @schema -> {:error, "unsupported schema #{inspect(s)}"}
          _ -> validate(list)
        end

      {:ok, _} ->
        {:error, "expected a JSON array of records or {\"schema\": ..., \"evidence\": [...]}"}

      {:error, %Jason.DecodeError{position: pos}} ->
        {:error, "invalid JSON at byte #{pos}"}
    end
  end

  defp validate(list) do
    list
    |> Enum.with_index()
    |> Enum.find_value({:ok, list}, fn
      {%{"schema" => s}, i} when s != @schema ->
        {:error, "evidence[#{i}]: unsupported schema #{inspect(s)}"}

      {r, _i} when is_map(r) ->
        nil

      {_r, i} ->
        {:error, "evidence[#{i}] is not an object"}
    end)
  end

  @doc """
  Decide whether a record counts. Options: `:min_cases` (default #{@default_min_cases}, never
  below it), `:repo` and `:commit` (bind the record to the judged run).
  """
  @spec counted?(record(), keyword()) :: decision()
  def counted?(record, opts \\ []) when is_map(record) do
    min_cases =
      case Keyword.get(opts, :min_cases) do
        n when is_integer(n) -> max(n, @default_min_cases)
        _ -> @default_min_cases
      end

    cond do
      dig(record, ["run", "outcome"]) != "passed" -> {:not_counted, :not_passed}
      debt?(record) -> {:not_counted, :fixture_debt}
      not admissible?(record) -> {:not_counted, :no_fixtures}
      true -> bound_by_kind(record, opts, min_cases)
    end
  end

  defp bound_by_kind(record, opts, min_cases) do
    with true <- record["kind"] in ["P", "R", "C", "B"],
         why when why != :ok <- binding(record, opts) do
      {:not_counted, why}
    else
      _ -> by_kind(record["kind"], record, min_cases)
    end
  end

  defp binding(record, opts) do
    repo = Keyword.get(opts, :repo)
    commit = Keyword.get(opts, :commit)

    cond do
      repo != nil and not same?(record["repo"], repo) -> :wrong_repo
      commit == nil -> :ok
      not present_string?(record["commit"]) -> :no_commit
      not full_sha?(record["commit"]) or not same?(record["commit"], commit) -> :wrong_commit
      true -> :ok
    end
  end

  defp same?(a, b) when is_binary(a) and is_binary(b),
    do: String.downcase(String.trim(a)) == String.downcase(String.trim(b))

  defp same?(_, _), do: false

  defp full_sha?(s), do: is_binary(s) and Regex.match?(~r/\A[0-9a-fA-F]{40}\z/, String.trim(s))

  @doc "Classify every record: `[{record, decision}]`, in input order (same options as `counted?/2`)."
  @spec classify_all([record()], keyword()) :: [{record(), decision()}]
  def classify_all(records, opts \\ []), do: Enum.map(records, &{&1, counted?(&1, opts)})

  @doc "A short display name for a record."
  @spec name(record()) :: String.t()
  def name(record) do
    tid = record["test_id"]

    cond do
      is_map(tid) and present_string?(tid["name"]) ->
        [tid["module_path"], tid["name"]] |> Enum.filter(&present_string?/1) |> Enum.join("::")

      present_string?(record["id"]) ->
        record["id"]

      true ->
        "(unnamed record)"
    end
  end

  @doc "Human text for a not-counted reason."
  @spec reason_text(atom()) :: String.t()
  def reason_text(:not_passed), do: "did not pass in this run"
  def reason_text(:fixture_debt), do: "FixtureDebt (no silence/firing fixtures)"

  def reason_text(:no_fixtures),
    do: "not admissible: needs silence and firing fixtures, or a cannot-fail-by-design reason"

  def reason_text(:example_test), do: "example test (kind E): measured, never counted"
  def reason_text(:mock_test), do: "runs against a mock or stub (kind M): never counted"
  def reason_text(:unknown_kind), do: "unknown evidence kind"
  def reason_text(:no_seed), do: "property test with no seed recorded"
  def reason_text(:too_few_cases), do: "property test below the minimum case count"
  def reason_text(:no_spec), do: "conformance test with no spec id"
  def reason_text(:no_version), do: "conformance test with no spec version"
  def reason_text(:no_vectors_sha256), do: "conformance test with no vector-set hash"
  def reason_text(:conformance_failures), do: "conformance vectors failed"
  def reason_text(:no_checksum), do: "benchmark whose checksum does not assert a property"
  def reason_text(:empty_ladder), do: "proof claim with an empty proof ladder"

  def reason_text(:incomplete_rung),
    do: "proof rung missing theorem, file, line, checker, checker version or statement hash"

  def reason_text(:proof_check_not_passed), do: "proof rung with no passed checker result"
  def reason_text(:no_covers_decls), do: "proof claim with no covered declarations (covers_decls)"

  def reason_text(:no_mutation_result),
    do: "no mutation result (tool, killed >= 1, survived 0) recorded"

  def reason_text(:mutants_survived), do: "mutants survived the test"
  def reason_text(:wrong_repo), do: "evidence belongs to a different repository"
  def reason_text(:no_commit), do: "evidence record carries no commit"
  def reason_text(:wrong_commit), do: "evidence was produced for a different commit"
  def reason_text(other), do: to_string(other)

  # -- kind rules -----------------------------------------------------------

  defp by_kind("E", _r, _m), do: {:not_counted, :example_test}
  defp by_kind("M", _r, _m), do: {:not_counted, :mock_test}

  defp by_kind("R", r, min_cases) do
    seed = dig(r, ["run", "property", "seed"])
    cases = dig(r, ["run", "property", "cases"])

    cond do
      not present?(seed) -> {:not_counted, :no_seed}
      not (is_integer(cases) and cases >= min_cases) -> {:not_counted, :too_few_cases}
      true -> mutation(r)
    end
  end

  defp by_kind("C", r, _m) do
    c = dig(r, ["run", "conformance"])

    cond do
      not present?(dig(c, ["spec"])) -> {:not_counted, :no_spec}
      not present?(dig(c, ["version"])) -> {:not_counted, :no_version}
      not present?(dig(c, ["vectors_sha256"])) -> {:not_counted, :no_vectors_sha256}
      dig(c, ["failed"]) != 0 -> {:not_counted, :conformance_failures}
      true -> mutation(r)
    end
  end

  defp by_kind("B", r, _m) do
    if dig(r, ["run", "benchmark", "checksum"]) == true,
      do: mutation(r),
      else: {:not_counted, :no_checksum}
  end

  defp by_kind("P", r, _m) do
    case dig(r, ["run", "proof", "ladder"]) do
      [_ | _] = ladder ->
        cond do
          not Enum.all?(ladder, &complete_rung?/1) -> {:not_counted, :incomplete_rung}
          not Enum.all?(ladder, &rung_passed?/1) -> {:not_counted, :proof_check_not_passed}
          not covers_decls?(r) -> {:not_counted, :no_covers_decls}
          true -> :counted
        end

      _ ->
        {:not_counted, :empty_ladder}
    end
  end

  defp by_kind(_other, _r, _m), do: {:not_counted, :unknown_kind}

  defp complete_rung?(rung) when is_map(rung) do
    Enum.all?(~w(theorem file checker checker_version), &present_string?(rung[&1])) and
      is_integer(rung["line"]) and
      is_binary(rung["statement_sha256"]) and
      Regex.match?(~r/\A[0-9a-fA-F]{64}\z/, rung["statement_sha256"])
  end

  defp complete_rung?(_), do: false

  defp covers_decls?(r) do
    case dig(r, ["run", "proof", "covers_decls"]) do
      [_ | _] = l -> Enum.all?(l, &present_string?/1)
      _ -> false
    end
  end

  # Mutation condition (Q7), record level.
  defp mutation(r) do
    if language(r) in @mutation_languages do
      m = dig(r, ["run", "mutation"])
      killed = dig(m, ["killed"])
      survived = dig(m, ["survived"])

      cond do
        is_integer(survived) and survived > 0 ->
          {:not_counted, :mutants_survived}

        present_string?(dig(m, ["tool"])) and is_integer(killed) and killed >= 1 and survived == 0 ->
          :counted

        true ->
          {:not_counted, :no_mutation_result}
      end
    else
      :counted
    end
  end

  defp rung_passed?(%{"check" => "passed"}), do: true
  defp rung_passed?(%{"check" => %{"outcome" => "passed"}}), do: true
  defp rung_passed?(%{"passed" => true}), do: true
  defp rung_passed?(_), do: false

  defp admissible?(record) do
    case record["fixtures"] do
      %{"cannot_fail_by_design" => why} when is_binary(why) ->
        present?(why)

      %{"silence" => s, "firing" => f} ->
        present?(s) and present?(f)

      _ ->
        false
    end
  end

  defp debt?(record) do
    case record["fixtures"] do
      %{"debt" => d} -> d != nil and d != false
      _ -> false
    end
  end

  # -- helpers --------------------------------------------------------------

  defp dig(nil, _), do: nil
  defp dig(v, []), do: v
  defp dig(%{} = m, [k | rest]), do: dig(Map.get(m, k), rest)
  defp dig(_, _), do: nil

  defp present_string?(s), do: is_binary(s) and String.trim(s) != ""

  defp present?(nil), do: false
  defp present?(s) when is_binary(s), do: String.trim(s) != ""
  defp present?(_), do: true
end
