# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.EchidnabotObligation do
  @moduledoc """
  The one place hypatia builds echidnabot's `submitProofObligation` request.

  Two senders use it: `Hypatia.FleetDispatcher` (proof obligations routed
  from findings) and `Hypatia.LearningScheduler` (re-queues after a strategy
  shift). Both send the same static GraphQL document and pass every value as
  a GraphQL variable, so a claim or context holding a newline, a backslash or
  a quote reaches echidnabot byte for byte and cannot change the document.

  The contract is echidnabot's `SubmitProofObligationInput` in
  `src/api/graphql.rs` (added by hyperpolymath/echidnabot#169, on `main` at
  `faeb280`): `repo`, `claim` and `context` are strings, `prover` is an
  optional `ProverKind` enum value, and `inline` is an optional boolean.

  `HYPATIA_ECHIDNABOT_URL` has one meaning for both senders: the base URL of
  echidnabot's HTTP server. Each sender appends `/graphql` (see
  `graphql_url/1`).
  """

  @mutation """
  mutation SubmitProofObligation($input: SubmitProofObligationInput!) {
    submitProofObligation(input: $input) {
      success
      proofId
    }
  }
  """

  # VeriSimDB / ProofStrategySelection prover names (lowercase snake) mapped
  # to echidnabot's ProverKind enum values. Anything else has no ProverKind.
  @prover_kinds %{
    "coq" => "COQ",
    "lean" => "LEAN",
    "agda" => "AGDA",
    "isabelle" => "ISABELLE",
    "z3" => "Z3",
    "cvc5" => "CVC5",
    "metamath" => "METAMATH",
    "hol_light" => "HOL_LIGHT",
    "mizar" => "MIZAR",
    "pvs" => "PVS",
    "acl2" => "ACL2",
    "hol4" => "HOL4"
  }

  @doc """
  The static `submitProofObligation` GraphQL document. Every value travels in
  the `variables` map built by `variables/5`; nothing is interpolated here.
  """
  @spec mutation() :: String.t()
  def mutation, do: @mutation

  @doc """
  Build the GraphQL `variables` map for `mutation/0`.

  `prover_hint` is passed through `normalise_prover_hint/1`; when it has no
  `ProverKind` the `prover` field is omitted, and echidnabot falls back to its
  default prover. Pass `inline: true` (or `false`) in `opts` to set the
  optional `inline` field; it is omitted otherwise.
  """
  @spec variables(String.t(), String.t(), String.t(), String.t() | nil, keyword()) :: map()
  def variables(repo, claim, context, prover_hint, opts \\ []) do
    input =
      %{"repo" => repo, "claim" => claim, "context" => context}
      |> put_prover(normalise_prover_hint(prover_hint))
      |> put_inline(Keyword.fetch(opts, :inline))

    %{"input" => input}
  end

  @doc """
  Map a lowercase prover name (`"lean"`, `"hol_light"`, ...) to echidnabot's
  `ProverKind` enum value (`"LEAN"`, `"HOL_LIGHT"`, ...).

  Returns `nil` for `nil` and for any name that is not a `ProverKind`, such as
  `"lean4"` or `"idris2"`. Unknown names are dropped, not guessed at, because
  echidnabot rejects the whole request when an enum value is invalid.
  """
  @spec normalise_prover_hint(term()) :: String.t() | nil
  def normalise_prover_hint(hint) when is_binary(hint), do: Map.get(@prover_kinds, hint)
  def normalise_prover_hint(_hint), do: nil

  @doc """
  The GraphQL endpoint for an echidnabot base URL: `base_url <> "/graphql"`.

  This is the rule `Hypatia.FleetDispatcher` applies to every
  `HYPATIA_<BOT>_URL`, so `HYPATIA_ECHIDNABOT_URL` means the same thing to
  both senders.
  """
  @spec graphql_url(String.t()) :: String.t()
  def graphql_url(base_url) when is_binary(base_url), do: base_url <> "/graphql"

  # Add the optional `prover` field when there is a ProverKind for it.
  defp put_prover(input, nil), do: input
  defp put_prover(input, kind), do: Map.put(input, "prover", kind)

  # Add the optional `inline` field when the caller passed a boolean for it.
  defp put_inline(input, {:ok, inline}) when is_boolean(inline),
    do: Map.put(input, "inline", inline)

  defp put_inline(input, _), do: input
end
