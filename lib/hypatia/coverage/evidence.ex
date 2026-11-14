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

  1. `run.outcome == "passed"` (else `:not_passed`)
  2. the record is admissible: `fixtures` is `{"debt": ...}` -> `:fixture_debt`;
     otherwise it must hold both `silence` and `firing`, or a non-empty
     `cannot_fail_by_design` reason (else `:no_fixtures`)
  3. kind `E` -> `:example_test`, kind `M` -> `:mock_test`; any kind other than
     `P R C B` -> `:unknown_kind`
  4. the kind's requirements (the *overclaim* reasons, COV-E010: the record
     claims a kind it cannot back):

     * `R`: `run.property.seed` present (`:no_seed`) and
       `run.property.cases >= min_cases` (`:too_few_cases`); default 100,
       set per repo with the `:min_cases` option
     * `C`: `spec`, `version`, `vectors_sha256` all present under
       `run.conformance` (`:no_spec`, `:no_version`, `:no_vectors_sha256`) and
       `failed == 0` (`:conformance_failures`)
     * `B`: `run.benchmark.checksum == true` (`:no_checksum`)
     * `P`: `run.proof.ladder` a non-empty list (`:empty_ladder`); every rung
       has `theorem`, `file`, `checker` (`:incomplete_rung`) and a passed
       check (`:proof_check_not_passed`): `"check": "passed"`,
       `"check": {"outcome": "passed"}` or `"passed": true`

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
    :proof_check_not_passed
  ]

  @doc "The schema string records must carry (when they carry one)."
  def schema, do: @schema

  @doc "Default minimum property-test case count (design Q8: floor 100)."
  def default_min_cases, do: @default_min_cases

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
  Decide whether a record counts. Options: `:min_cases` (default #{@default_min_cases}).
  """
  @spec counted?(record(), keyword()) :: decision()
  def counted?(record, opts \\ []) when is_map(record) do
    min_cases = Keyword.get(opts, :min_cases, @default_min_cases)

    cond do
      dig(record, ["run", "outcome"]) != "passed" -> {:not_counted, :not_passed}
      debt?(record) -> {:not_counted, :fixture_debt}
      not admissible?(record) -> {:not_counted, :no_fixtures}
      true -> by_kind(record["kind"], record, min_cases)
    end
  end

  @doc "Classify every record: `[{record, decision}]`, in input order."
  @spec classify_all([record()], keyword()) :: [{record(), decision()}]
  def classify_all(records, opts \\ []), do: Enum.map(records, &{&1, counted?(&1, opts)})

  @doc "A short display name for a record."
  @spec name(record()) :: String.t()
  def name(record) do
    tid = record["test_id"]

    cond do
      is_map(tid) and present?(tid["name"]) ->
        [tid["module_path"], tid["name"]] |> Enum.filter(&present?/1) |> Enum.join("::")

      present?(record["id"]) ->
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
  def reason_text(:incomplete_rung), do: "proof rung missing theorem, file or checker"
  def reason_text(:proof_check_not_passed), do: "proof rung with no passed checker result"
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
      true -> :counted
    end
  end

  defp by_kind("C", r, _m) do
    c = dig(r, ["run", "conformance"])

    cond do
      not present?(dig(c, ["spec"])) -> {:not_counted, :no_spec}
      not present?(dig(c, ["version"])) -> {:not_counted, :no_version}
      not present?(dig(c, ["vectors_sha256"])) -> {:not_counted, :no_vectors_sha256}
      dig(c, ["failed"]) != 0 -> {:not_counted, :conformance_failures}
      true -> :counted
    end
  end

  defp by_kind("B", r, _m) do
    if dig(r, ["run", "benchmark", "checksum"]) == true,
      do: :counted,
      else: {:not_counted, :no_checksum}
  end

  defp by_kind("P", r, _m) do
    case dig(r, ["run", "proof", "ladder"]) do
      [_ | _] = ladder ->
        cond do
          not Enum.all?(ladder, &complete_rung?/1) -> {:not_counted, :incomplete_rung}
          not Enum.all?(ladder, &rung_passed?/1) -> {:not_counted, :proof_check_not_passed}
          true -> :counted
        end

      _ ->
        {:not_counted, :empty_ladder}
    end
  end

  defp by_kind(_other, _r, _m), do: {:not_counted, :unknown_kind}

  defp complete_rung?(rung) when is_map(rung),
    do: Enum.all?(["theorem", "file", "checker"], &present?(rung[&1]))

  defp complete_rung?(_), do: false

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

  defp present?(nil), do: false
  defp present?(s) when is_binary(s), do: String.trim(s) != ""
  defp present?(_), do: true
end
