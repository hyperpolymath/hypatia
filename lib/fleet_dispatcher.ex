# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.FleetDispatcher do
  @moduledoc """
  Routes findings to appropriate gitbot-fleet bots via GraphQL.
  """

  require Logger

  @doc """
  Dispatch a finding to the appropriate bot based on type.

  Finding types:
  - :eco_score -> sustainabot
  - :proof_obligation -> echidnabot
  - :fix_suggestion -> rhodibot
  """
  def dispatch_finding(finding) do
    case finding.type do
      :eco_score -> dispatch_to_sustainabot(finding)
      :proof_obligation -> dispatch_to_echidnabot(finding)
      :fix_suggestion -> dispatch_to_rhodibot(finding)
      _ -> {:error, :unknown_finding_type}
    end
  end

  defp dispatch_to_sustainabot(finding) do
    # GraphQL mutation to sustainabot
    mutation = """
    mutation {
      reportEcoScore(
        repo: "#{finding.repo}",
        score: #{finding.score},
        details: "#{escape_quotes(finding.details)}"
      ) {
        success
      }
    }
    """

    execute_graphql(mutation, "sustainabot")
  end

  defp dispatch_to_echidnabot(finding) do
    # GraphQL mutation to echidnabot
    mutation = """
    mutation {
      submitProofObligation(
        repo: "#{finding.repo}",
        claim: "#{escape_quotes(finding.claim)}",
        context: "#{escape_quotes(finding.context)}"
      ) {
        success
        proofId
      }
    }
    """

    execute_graphql(mutation, "echidnabot")
  end

  defp dispatch_to_rhodibot(finding) do
    # GraphQL mutation to rhodibot
    mutation = """
    mutation {
      suggestFix(
        repo: "#{finding.repo}",
        file: "#{finding.file}",
        issue: "#{escape_quotes(finding.issue)}",
        suggestion: "#{escape_quotes(finding.suggestion)}"
      ) {
        success
        prNumber
      }
    }
    """

    execute_graphql(mutation, "rhodibot")
  end

  defp execute_graphql(query, bot_name) do
    # TODO: Actual GraphQL client call
    # For now, just log
    Logger.info("Would dispatch to #{bot_name}: #{query}")
    {:ok, :logged}
  end

  defp escape_quotes(str) when is_binary(str) do
    String.replace(str, "\"", "\\\"")
  end

  defp escape_quotes(_), do: ""
end
