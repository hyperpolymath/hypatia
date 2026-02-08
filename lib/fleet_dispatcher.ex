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
  - :presentation_finding -> glambot (visual, SEO, machine-readability, git-seo)
  - :accessibility_violation -> accessibilitybot
  """
  def dispatch_finding(finding) do
    case finding.type do
      :eco_score -> dispatch_to_sustainabot(finding)
      :proof_obligation -> dispatch_to_echidnabot(finding)
      :fix_suggestion -> dispatch_to_rhodibot(finding)
      :presentation_finding -> dispatch_to_glambot(finding)
      :accessibility_violation -> dispatch_to_accessibilitybot(finding)
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

  defp dispatch_to_glambot(finding) do
    # GraphQL mutation to glambot
    category = Map.get(finding, :category, "presentation")

    mutation = """
    mutation {
      reportPresentationFinding(
        repo: "#{finding.repo}",
        file: "#{escape_quotes(Map.get(finding, :file, ""))}",
        category: "#{escape_quotes(category)}",
        issue: "#{escape_quotes(finding.issue)}",
        suggestion: "#{escape_quotes(Map.get(finding, :suggestion, ""))}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "glambot")
  end

  defp dispatch_to_accessibilitybot(finding) do
    # GraphQL mutation to accessibilitybot
    mutation = """
    mutation {
      reportAccessibilityViolation(
        repo: "#{finding.repo}",
        file: "#{finding.file}",
        wcagLevel: "#{escape_quotes(finding.wcag_level)}",
        criterion: "#{escape_quotes(finding.criterion)}",
        element: "#{escape_quotes(finding.element)}",
        issue: "#{escape_quotes(finding.issue)}",
        suggestion: "#{escape_quotes(finding.suggestion)}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "accessibilitybot")
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
