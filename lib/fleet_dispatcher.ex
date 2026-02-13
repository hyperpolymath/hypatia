# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.FleetDispatcher do
  @moduledoc """
  Routes findings to appropriate gitbot-fleet bots via GraphQL.

  Supports both:
  - Legacy dispatch_finding/1 — routes by finding type
  - Triangle-aware dispatch_routed_action/1 — routes by safety triangle tier
  """

  alias Hypatia.TriangleRouter

  require Logger

  # ====================================================================
  # Triangle-Aware Dispatch (new — from safety triangle router)
  # ====================================================================

  @doc """
  Dispatch a routed action from the triangle router.

  Handles:
  - {:eliminate, recipe, pattern} — auto-fix or PR depending on confidence
  - {:substitute, recipe, pattern} — PR with proven module + proof obligation
  - {:control, pattern} — advisory report to sustainabot
  """
  def dispatch_routed_action({:eliminate, recipe, pattern}) do
    confidence = Map.get(recipe, "confidence", 0.0)
    strategy = TriangleRouter.dispatch_strategy(confidence)

    case strategy do
      :auto_execute ->
        dispatch_to_robot_repo_automaton(%{
          type: :auto_fix_request,
          repo: get_pattern_repo(pattern),
          file: Map.get(pattern, "file", ""),
          issue: Map.get(pattern, "description", ""),
          fix_type: "eliminate",
          confidence: confidence,
          recipe_id: Map.get(recipe, "id"),
          suggestion: Map.get(recipe, "description", "")
        })

      :review ->
        dispatch_to_rhodibot(%{
          type: :fix_suggestion,
          repo: get_pattern_repo(pattern),
          file: Map.get(pattern, "file", ""),
          issue: Map.get(pattern, "description", ""),
          suggestion: Map.get(recipe, "description", "")
        })

      :report_only ->
        dispatch_to_sustainabot(%{
          type: :eco_score,
          repo: get_pattern_repo(pattern),
          score: Map.get(pattern, "severity_score", 0.5),
          details: "Low-confidence eliminate: #{Map.get(pattern, "description", "")}"
        })
    end
  end

  def dispatch_routed_action({:substitute, recipe, pattern}) do
    proven_module = Map.get(recipe, "proven_module", "unknown")

    # Create a fix suggestion PR via rhodibot
    dispatch_to_rhodibot(%{
      type: :fix_suggestion,
      repo: get_pattern_repo(pattern),
      file: Map.get(pattern, "file", ""),
      issue: Map.get(pattern, "description", ""),
      suggestion: "Replace with proven/#{proven_module}"
    })

    # Request proof obligation from echidnabot
    dispatch_to_echidnabot(%{
      type: :proof_obligation,
      repo: get_pattern_repo(pattern),
      claim: "Substitution with #{proven_module} preserves behavior",
      context: Map.get(pattern, "description", "")
    })
  end

  def dispatch_routed_action({:control, finding}) do
    dispatch_to_sustainabot(%{
      type: :eco_score,
      repo: Map.get(finding, "routed_repo", Map.get(finding, "repo", "unknown")),
      score: Map.get(finding, "severity_score", 0.5),
      details: "Unresolved: #{Map.get(finding, "description", "")} (no safe fix available)"
    })
  end

  # ====================================================================
  # Legacy Finding Dispatch (preserved for backward compatibility)
  # ====================================================================

  @doc """
  Dispatch a finding to the appropriate bot based on type.

  Finding types:
  - :eco_score -> sustainabot
  - :proof_obligation -> echidnabot
  - :fix_suggestion -> rhodibot
  - :presentation_finding -> glambot
  - :accessibility_violation -> accessibilitybot
  - :seam_finding -> seambot
  - :crypto_finding -> cipherbot
  - :auto_fix_request -> robot-repo-automaton
  - :completeness_finding -> finishbot
  - :fix_outcome -> learning engine
  """
  def dispatch_finding(finding) do
    case finding.type do
      :eco_score -> dispatch_to_sustainabot(finding)
      :proof_obligation -> dispatch_to_echidnabot(finding)
      :fix_suggestion -> dispatch_to_rhodibot(finding)
      :presentation_finding -> dispatch_to_glambot(finding)
      :accessibility_violation -> dispatch_to_accessibilitybot(finding)
      :seam_finding -> dispatch_to_seambot(finding)
      :crypto_finding -> dispatch_to_cipherbot(finding)
      :completeness_finding -> dispatch_to_finishbot(finding)
      :auto_fix_request -> dispatch_to_robot_repo_automaton(finding)
      :fix_outcome -> ingest_fix_outcome(finding)
      _ -> {:error, :unknown_finding_type}
    end
  end

  # ====================================================================
  # Bot-specific dispatch functions
  # ====================================================================

  defp dispatch_to_sustainabot(finding) do
    mutation = """
    mutation {
      reportEcoScore(
        repo: "#{finding_field(finding, :repo)}",
        score: #{finding_field(finding, :score, 0.0)},
        details: "#{escape_quotes(finding_field(finding, :details, ""))}"
      ) {
        success
      }
    }
    """

    execute_graphql(mutation, "sustainabot")
  end

  defp dispatch_to_echidnabot(finding) do
    mutation = """
    mutation {
      submitProofObligation(
        repo: "#{finding_field(finding, :repo)}",
        claim: "#{escape_quotes(finding_field(finding, :claim, ""))}",
        context: "#{escape_quotes(finding_field(finding, :context, ""))}"
      ) {
        success
        proofId
      }
    }
    """

    execute_graphql(mutation, "echidnabot")
  end

  defp dispatch_to_rhodibot(finding) do
    mutation = """
    mutation {
      suggestFix(
        repo: "#{finding_field(finding, :repo)}",
        file: "#{finding_field(finding, :file, "")}",
        issue: "#{escape_quotes(finding_field(finding, :issue, ""))}",
        suggestion: "#{escape_quotes(finding_field(finding, :suggestion, ""))}"
      ) {
        success
        prNumber
      }
    }
    """

    execute_graphql(mutation, "rhodibot")
  end

  defp dispatch_to_glambot(finding) do
    category = finding_field(finding, :category, "presentation")

    mutation = """
    mutation {
      reportPresentationFinding(
        repo: "#{finding_field(finding, :repo)}",
        file: "#{escape_quotes(finding_field(finding, :file, ""))}",
        category: "#{escape_quotes(category)}",
        issue: "#{escape_quotes(finding_field(finding, :issue, ""))}",
        suggestion: "#{escape_quotes(finding_field(finding, :suggestion, ""))}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "glambot")
  end

  defp dispatch_to_seambot(finding) do
    category = finding_field(finding, :category, "seam-analysis")

    mutation = """
    mutation {
      reportSeamFinding(
        repo: "#{finding_field(finding, :repo)}",
        file: "#{escape_quotes(finding_field(finding, :file, ""))}",
        category: "#{escape_quotes(category)}",
        issue: "#{escape_quotes(finding_field(finding, :issue, ""))}",
        driftScore: #{finding_field(finding, :drift_score, 0.0)},
        suggestion: "#{escape_quotes(finding_field(finding, :suggestion, ""))}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "seambot")
  end

  defp dispatch_to_cipherbot(finding) do
    category = finding_field(finding, :category, "crypto/hashing")

    mutation = """
    mutation {
      reportCryptoFinding(
        repo: "#{finding_field(finding, :repo)}",
        file: "#{escape_quotes(finding_field(finding, :file, ""))}",
        category: "#{escape_quotes(category)}",
        algorithm: "#{escape_quotes(finding_field(finding, :algorithm, ""))}",
        issue: "#{escape_quotes(finding_field(finding, :issue, ""))}",
        pqReady: #{finding_field(finding, :pq_ready, false)},
        suggestion: "#{escape_quotes(finding_field(finding, :suggestion, ""))}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "cipherbot")
  end

  defp dispatch_to_finishbot(finding) do
    category = finding_field(finding, :category, "completeness/release")

    mutation = """
    mutation {
      reportCompletenessFinding(
        repo: "#{finding_field(finding, :repo)}",
        file: "#{escape_quotes(finding_field(finding, :file, ""))}",
        category: "#{escape_quotes(category)}",
        issue: "#{escape_quotes(finding_field(finding, :issue, ""))}",
        suggestion: "#{escape_quotes(finding_field(finding, :suggestion, ""))}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "finishbot")
  end

  defp dispatch_to_accessibilitybot(finding) do
    mutation = """
    mutation {
      reportAccessibilityViolation(
        repo: "#{finding_field(finding, :repo)}",
        file: "#{finding_field(finding, :file, "")}",
        wcagLevel: "#{escape_quotes(finding_field(finding, :wcag_level, ""))}",
        criterion: "#{escape_quotes(finding_field(finding, :criterion, ""))}",
        element: "#{escape_quotes(finding_field(finding, :element, ""))}",
        issue: "#{escape_quotes(finding_field(finding, :issue, ""))}",
        suggestion: "#{escape_quotes(finding_field(finding, :suggestion, ""))}"
      ) {
        success
        findingId
      }
    }
    """

    execute_graphql(mutation, "accessibilitybot")
  end

  defp dispatch_to_robot_repo_automaton(finding) do
    confidence = finding_field(finding, :confidence, "high")
    fix_type = finding_field(finding, :fix_type, "compliance")

    mutation = """
    mutation {
      requestAutoFix(
        repo: "#{finding_field(finding, :repo)}",
        file: "#{escape_quotes(finding_field(finding, :file, ""))}",
        issue: "#{escape_quotes(finding_field(finding, :issue, ""))}",
        fixType: "#{escape_quotes(fix_type)}",
        confidence: "#{escape_quotes(to_string(confidence))}",
        recipeId: "#{escape_quotes(finding_field(finding, :recipe_id, ""))}",
        suggestion: "#{escape_quotes(finding_field(finding, :suggestion, ""))}"
      ) {
        success
        fixApplied
        fixDecision
      }
    }
    """

    execute_graphql(mutation, "robot-repo-automaton")
  end

  defp ingest_fix_outcome(finding) do
    pattern = finding_field(finding, :pattern, "unknown")
    success = finding_field(finding, :success, false)
    repo = finding_field(finding, :repo, "unknown")

    Logger.info(
      "Learning loop: fix outcome for pattern '#{pattern}' in #{repo} - " <>
        "success=#{success}"
    )

    observation = %{
      type: pattern,
      repo: repo,
      success: success,
      fix_type: finding_field(finding, :fix_type, "unknown"),
      confidence: finding_field(finding, :confidence, "unknown"),
      observed: DateTime.utc_now() |> DateTime.to_iso8601()
    }

    learning_file =
      Path.expand(
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

  # ====================================================================
  # Helpers
  # ====================================================================

  defp execute_graphql(query, bot_name) do
    # Dual dispatch: file-based (immediate) + HTTP (when fleet API available)
    dispatch_record = %{
      "bot" => bot_name,
      "query" => query,
      "dispatched_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "status" => "pending"
    }

    # 1. Always write to dispatch manifest (dispatch-runner.sh reads this)
    manifest_path = Path.join([
      Path.expand("~/Documents/hyperpolymath-repos/verisimdb-data"),
      "dispatch",
      "pending.jsonl"
    ])

    case Jason.encode(dispatch_record) do
      {:ok, json} ->
        File.mkdir_p!(Path.dirname(manifest_path))
        File.write(manifest_path, json <> "\n", [:append, :utf8])

      {:error, reason} ->
        Logger.error("Failed to write dispatch manifest: #{inspect(reason)}")
    end

    # 2. Attempt HTTP dispatch to fleet API (graceful degradation if unavailable)
    fleet_url = System.get_env("HYPATIA_FLEET_URL")

    if fleet_url do
      try do
        # Use Finch if available, otherwise fall back to file-only dispatch
        case http_post(fleet_url <> "/dispatch/#{bot_name}", query) do
          {:ok, _response} ->
            Logger.info("Live dispatch to #{bot_name} succeeded")
            {:ok, :dispatched}

          {:error, reason} ->
            Logger.warning("Live dispatch to #{bot_name} failed (#{inspect(reason)}), file dispatch used")
            {:ok, :file_dispatched}
        end
      rescue
        e ->
          Logger.warning("HTTP dispatch error: #{inspect(e)}, file dispatch used")
          {:ok, :file_dispatched}
      end
    else
      Logger.info("Dispatched to #{bot_name} via manifest (no fleet URL configured)")
      {:ok, :file_dispatched}
    end
  end

  defp http_post(url, body) do
    # Attempt HTTP POST — works if :httpc is available (OTP built-in)
    case :httpc.request(
           :post,
           {String.to_charlist(url), [{'content-type', 'application/json'}],
            'application/json', String.to_charlist(body)},
           [{:timeout, 10_000}],
           []
         ) do
      {:ok, {{_, status, _}, _, _response_body}} when status in 200..299 ->
        {:ok, status}

      {:ok, {{_, status, _}, _, _}} ->
        {:error, {:http_status, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp escape_quotes(str) when is_binary(str) do
    String.replace(str, "\"", "\\\"")
  end

  defp escape_quotes(_), do: ""

  # Unified field accessor that works with both maps and keyword structs
  defp finding_field(finding, key, default \\ nil) when is_map(finding) do
    Map.get(finding, key) || Map.get(finding, Atom.to_string(key)) || default
  end

  defp get_pattern_repo(pattern) do
    # Pattern may have repos_affected_list — take first repo as representative
    case Map.get(pattern, "repos_affected_list", []) do
      [repo | _] -> repo
      _ -> Map.get(pattern, "routed_repo", "unknown")
    end
  end
end
