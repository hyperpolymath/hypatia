# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.VCL.ProofResolver do
  @moduledoc """
  V4f: resolves `PROOF PROVEN(class, prover)` and `PROOF SANCTIFY(class)`
  expressions inside VCL statements against the Verisim certificate views.

  These two proof types are special — unlike EXISTENCE/INTEGRITY/etc.
  which operate on octad modalities, PROVEN and SANCTIFY consult the
  `proof_attempts` learning loop via the `/api/v1/proof_attempts/certificates`
  endpoint and return `status: "proven" | "sanctified" | "pending"`
  annotations alongside the evidence cursor.

  This resolver is called by the VCL executor when a statement contains a
  PROOF clause with the PROVEN or SANCTIFY keyword. Other proof types
  are handled by the existing `proven_bridge.rs` pipeline unchanged.

  ## Syntax (recognised patterns)

      PROOF PROVEN(class='linearity', prover='coq')
      PROOF PROVEN(class=linearity, prover=coq)       -- unquoted identifiers allowed
      PROOF SANCTIFY(class='equiv')
      PROOF SANCTIFY(class=equiv)

  ## Return shape

      {:proven, status, cert_id, success_rate, total_attempts}
      {:sanctified, status, cert_id, proven_provers, combined_attempts}
      {:pending, cert_type, reason}
      {:error, reason}
  """

  require Logger

  @default_base_url "http://localhost:8080"
  @timeout_ms 5_000

  # ── Public API ───────────────────────────────────────────────────────────

  @doc """
  Parse a single `PROOF PROVEN(...)` or `PROOF SANCTIFY(...)` expression
  and resolve it against Verisim's certificate endpoint.

  Returns `{:ok, resolution}` on success, `{:error, reason}` otherwise.
  """
  def resolve(expression, opts \\ []) when is_binary(expression) do
    case parse_expression(expression) do
      {:proven, class, prover}  -> lookup_proven(class, prover, opts)
      {:sanctify, class}         -> lookup_sanctify(class, opts)
      {:error, reason}           -> {:error, reason}
    end
  end

  @doc """
  Walk a whole VCL statement string and extract every `PROOF PROVEN/SANCTIFY`
  expression in it, resolving each. Returns a list of
  `{expression_text, resolution}` tuples.
  """
  def resolve_all(vcl_statement, opts \\ []) when is_binary(vcl_statement) do
    regex = ~r/PROOF\s+(?:PROVEN|SANCTIFY)\s*\([^)]*\)/i

    Regex.scan(regex, vcl_statement)
    |> List.flatten()
    |> Enum.uniq()
    |> Enum.map(fn expr -> {expr, resolve(expr, opts)} end)
  end

  # ── Expression parsing ────────────────────────────────────────────────────

  @doc false
  def parse_expression(expression) do
    stripped = String.trim(expression)

    cond do
      String.match?(stripped, ~r/^PROOF\s+PROVEN\s*\(/i) ->
        parse_proven_args(stripped)

      String.match?(stripped, ~r/^PROOF\s+SANCTIFY\s*\(/i) ->
        parse_sanctify_args(stripped)

      true ->
        {:error, {:not_a_proven_or_sanctify, stripped}}
    end
  end

  defp parse_proven_args(expr) do
    case Regex.run(
           ~r/PROOF\s+PROVEN\s*\(\s*class\s*=\s*['"]?([a-zA-Z_\-]+)['"]?\s*,\s*prover\s*=\s*['"]?([a-zA-Z0-9_]+)['"]?\s*\)/i,
           expr
         ) do
      [_, class, prover] -> {:proven, class, String.downcase(prover)}
      _ -> {:error, {:malformed_proven, expr}}
    end
  end

  defp parse_sanctify_args(expr) do
    case Regex.run(
           ~r/PROOF\s+SANCTIFY\s*\(\s*class\s*=\s*['"]?([a-zA-Z_\-]+)['"]?\s*\)/i,
           expr
         ) do
      [_, class] -> {:sanctify, class}
      _ -> {:error, {:malformed_sanctify, expr}}
    end
  end

  # ── Verisim endpoint lookups ─────────────────────────────────────────────

  defp lookup_proven(class, prover, opts) do
    url =
      base_url(opts) <>
        "/api/v1/proof_attempts/certificates?class=" <>
        URI.encode_www_form(class) <> "&prover=" <> URI.encode_www_form(prover)

    case http_get(url, opts) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"proven" => [row | _]}} ->
            {:ok,
             {:proven, Map.get(row, "status", "pending"), Map.get(row, "cert_id"),
              Map.get(row, "success_rate"), Map.get(row, "total_attempts")}}

          {:ok, %{"proven" => []}} ->
            {:ok, {:pending, :proven, "no data for (#{class}, #{prover})"}}

          _ ->
            {:error, {:malformed_response, body}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp lookup_sanctify(class, opts) do
    url =
      base_url(opts) <>
        "/api/v1/proof_attempts/certificates?class=" <> URI.encode_www_form(class)

    case http_get(url, opts) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"sanctify" => [row | _]}} ->
            {:ok,
             {:sanctified, Map.get(row, "status", "pending"), Map.get(row, "cert_id"),
              Map.get(row, "proven_provers"), Map.get(row, "combined_attempts")}}

          {:ok, %{"sanctify" => []}} ->
            {:ok, {:pending, :sanctify, "no data for class=#{class}"}}

          _ ->
            {:error, {:malformed_response, body}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # ── HTTP transport ────────────────────────────────────────────────────────

  defp http_get(url, opts) do
    timeout_ms = Keyword.get(opts, :timeout, @timeout_ms)

    Application.ensure_all_started(:inets)
    Application.ensure_all_started(:ssl)

    case :httpc.request(
           :get,
           {String.to_charlist(url), [{~c"accept", ~c"application/json"}]},
           [{:timeout, timeout_ms}],
           []
         ) do
      {:ok, {{_, status, _}, _headers, body}} when status in 200..299 ->
        {:ok, List.to_string(body)}

      {:ok, {{_, status, _}, _headers, _}} ->
        {:error, {:http_status, status}}

      {:error, reason} ->
        {:error, {:transport, reason}}
    end
  end

  defp base_url(opts) do
    case Keyword.get(opts, :base_url) do
      nil -> System.get_env("HYPATIA_VERISIM_URL") || @default_base_url
      url -> url
    end
  end
end
