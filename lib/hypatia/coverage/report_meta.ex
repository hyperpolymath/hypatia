# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Coverage.ReportMeta do
  @moduledoc """
  Sidecar metadata binding one coverage report to the commit it was produced for.

  Schema `hypatia.proven-coverage.report-meta/1`:

      {"schema": "hypatia.proven-coverage.report-meta/1",
       "repo": "OWNER/NAME", "commit": "<40 hex>", "run_id": "<non-empty string>",
       "role": "raw" | "proven", "tool": "<string>", "tool_version": "<string>"}

  Collectors write it next to the report (default `<report>.meta.json`) with `write!/2`;
  `Hypatia.Coverage.Report` reads it with `read/1` and refuses to judge unbound reports
  (COV-E013) or reports from another repo, commit, role or run (COV-E007).
  """

  @schema "hypatia.proven-coverage.report-meta/1"
  @roles ["raw", "proven"]
  @string_keys ~w(repo run_id tool tool_version)

  @spec schema() :: String.t()
  def schema, do: @schema

  @doc "Default sidecar path for a report path."
  @spec default_path(Path.t()) :: Path.t()
  def default_path(report_path), do: report_path <> ".meta.json"

  @doc "Read and validate a sidecar. Returns `{:ok, map}` or `{:error, {:missing | :invalid, path, why}}`."
  @spec read(Path.t()) :: {:ok, map()} | {:error, {:missing | :invalid, Path.t(), String.t()}}
  def read(path) do
    with {:ok, text} <- File.read(path) |> wrap(path, :missing),
         {:ok, doc} <- Jason.decode(text) |> wrap(path, :invalid),
         :ok <- validate(doc) |> wrap_v(path) do
      {:ok, doc}
    end
  end

  @doc "Validate a metadata map (string keys). Returns `:ok` or `{:error, why}`."
  @spec validate(term()) :: :ok | {:error, String.t()}
  def validate(%{} = m) do
    cond do
      m["schema"] != @schema ->
        {:error, "schema must be #{@schema}"}

      not (is_binary(m["commit"]) and Regex.match?(~r/\A[0-9a-fA-F]{40}\z/, m["commit"])) ->
        {:error, "commit must be 40 hex characters"}

      m["role"] not in @roles ->
        {:error, "role must be raw or proven"}

      bad = Enum.find(@string_keys, &(not nonblank?(m[&1]))) ->
        {:error, "#{bad} must be a non-empty string"}

      true ->
        :ok
    end
  end

  def validate(_), do: {:error, "not a JSON object"}

  @doc """
  Write a sidecar. `map` may use atom or string keys; `schema` is filled in. Raises
  `ArgumentError` if the result is invalid.
  """
  @spec write!(Path.t(), map()) :: :ok
  def write!(path, map) do
    doc = map |> Map.new(fn {k, v} -> {to_string(k), v} end) |> Map.put("schema", @schema)

    case validate(doc) do
      :ok ->
        File.mkdir_p!(Path.dirname(path))
        File.write!(path, Jason.encode!(doc, pretty: true) <> "\n")

      {:error, why} ->
        raise ArgumentError, "invalid report metadata: #{why}"
    end
  end

  defp nonblank?(s), do: is_binary(s) and String.trim(s) != ""

  defp wrap({:ok, v}, _path, _kind), do: {:ok, v}

  defp wrap({:error, %Jason.DecodeError{}}, path, kind),
    do: {:error, {kind, path, "invalid JSON"}}

  defp wrap({:error, e}, path, kind), do: {:error, {kind, path, inspect(e)}}

  defp wrap_v(:ok, _path), do: :ok
  defp wrap_v({:error, why}, path), do: {:error, {:invalid, path, why}}
end
