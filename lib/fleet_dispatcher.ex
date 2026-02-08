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
  - :seam_finding -> seambot (seam analysis, drift detection, hidden channels, forge integration)
  - :auto_fix_request -> robot-repo-automaton (Tier 3 Executor, confidence-gated)
  - :fix_outcome -> learning engine (feeds neurosymbolic loop)
  """
  def dispatch_finding(finding) do
    case finding.type do
      :eco_score -> dispatch_to_sustainabot(finding)
      :proof_obligation -> dispatch_to_echidnabot(finding)
      :fix_suggestion -> dispatch_to_rhodibot(finding)
      :presentation_finding -> dispatch_to_glambot(finding)
      :accessibility_violation -> dispatch_to_accessibilitybot(finding)
      :seam_finding -> dispatch_to_seambot(finding)
      :auto_fix_request -> dispatch_to_robot_repo_automaton(finding)
      :fix_outcome -> ingest_fix_outcome(finding)
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

  defp dispatch_to_seambot(finding) do
    # GraphQL mutation to seambot
    category = Map.get(finding, :category, "seam-analysis")

    mutation = """
    mutation {
      reportSeamFinding(
        repo: "#{finding.repo}",
        file: "#{escape_quotes(Map.get(finding, :file, ""))}",
        category: "#{escape_quotes(category)}",
        issue: "#{escape_quotes(finding.issue)}",
        driftScore: #{Map.get(finding, :drift_score, 0.0)},
        suggestion: "#{escape_quotes(Map.get(finding, :suggestion, ""))}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "seambot")
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

  defp dispatch_to_robot_repo_automaton(finding) do
    # GraphQL mutation to robot-repo-automaton (Tier 3 Executor)
    # RRA applies fixes with confidence thresholds: high=auto, medium=review, low=skip
    confidence = Map.get(finding, :confidence, "high")
    fix_type = Map.get(finding, :fix_type, "compliance")

    mutation = """
    mutation {
      requestAutoFix(
        repo: "#{finding.repo}",
        file: "#{escape_quotes(Map.get(finding, :file, ""))}",
        issue: "#{escape_quotes(finding.issue)}",
        fixType: "#{escape_quotes(fix_type)}",
        confidence: "#{escape_quotes(confidence)}",
        suggestion: "#{escape_quotes(Map.get(finding, :suggestion, ""))}"
      ) {
        success
        fixApplied
        fixDecision
      }
    }
    """

    execute_graphql(mutation, "robot-repo-automaton")
  end

  @doc """
  Ingest a fix outcome from robot-repo-automaton into the neurosymbolic
  learning loop. Records whether an auto-fix succeeded or failed so the
  learning engine can adjust confidence thresholds and propose new rules.
  """
  defp ingest_fix_outcome(finding) do
    pattern = Map.get(finding, :pattern, "unknown")
    success = Map.get(finding, :success, false)
    repo = Map.get(finding, :repo, "unknown")

    Logger.info(
      "Learning loop: fix outcome for pattern '#{pattern}' in #{repo} - " <>
      "success=#{success}"
    )

    # Write observation to the shared-context learning pipeline
    observation = %{
      type: pattern,
      repo: repo,
      success: success,
      fix_type: Map.get(finding, :fix_type, "unknown"),
      confidence: Map.get(finding, :confidence, "unknown"),
      observed: DateTime.utc_now() |> DateTime.to_iso8601()
    }

    learning_file = Path.expand(
      "~/Documents/hyperpolymath-repos/gitbot-fleet/shared-context/learning/fix-outcomes.jsonl"
    )

    case Jason.encode(observation) do
      {:ok, json_line} ->
        File.write(learning_file, json_line <> "\n", [:append, :utf8])
        Logger.info("Fix outcome recorded to learning pipeline: #{learning_file}")
        {:ok, :recorded}

      {:error, reason} ->
        Logger.error("Failed to encode fix outcome: #{inspect(reason)}")
        {:error, reason}
    end
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
