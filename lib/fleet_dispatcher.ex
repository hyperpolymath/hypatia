# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.FleetDispatcher do
  @moduledoc """
  Routes findings to appropriate gitbot-fleet bots via GraphQL.

  Supports both:
  - Legacy dispatch_finding/1 -- routes by finding type
  - Triangle-aware dispatch_routed_action/1 -- routes by safety triangle tier
  """

  alias Hypatia.TriangleRouter
  alias Hypatia.DirectGitHubPR
  alias Hypatia.Rules.ProofObligation

  require Logger

  # When HYPATIA_DIRECT_PR=true, scorecard findings with supported fix scripts
  # bypass the fleet dispatch pipeline and create PRs directly via `gh` CLI.
  @direct_pr_enabled System.get_env("HYPATIA_DIRECT_PR") == "true"

  # ====================================================================
  # Triangle-Aware Dispatch (new -- from safety triangle router)
  # ====================================================================

  @doc """
  Dispatch a routed action from the triangle router.

  Handles:
  - {:eliminate, recipe, pattern} -- auto-fix or PR depending on confidence
  - {:substitute, recipe, pattern} -- PR with proven module + proof obligation
  - {:control, pattern} -- advisory report to sustainabot
  """
  def dispatch_routed_action({:eliminate, recipe, pattern}) do
    # Direct PR path: when HYPATIA_DIRECT_PR=true and the finding is a
    # scorecard check with a supported fix script, bypass the fleet pipeline
    # and create a PR directly via `gh` CLI. This is the fast path for
    # well-understood scorecard fixes (SC-013, SC-018).
    if direct_pr_enabled?() and is_direct_pr_candidate?(pattern) do
      Logger.info("Direct PR path: #{Map.get(pattern, "id", "unknown")}")
      DirectGitHubPR.create_fix_pr(pattern)
    else
      dispatch_eliminate_via_fleet(recipe, pattern)
    end
  end

  # Standard fleet dispatch path for eliminate tier.
  defp dispatch_eliminate_via_fleet(recipe, pattern) do
    confidence = Map.get(recipe, "confidence", 0.0)
    strategy = TriangleRouter.dispatch_strategy(confidence)

    bot_id =
      case strategy do
        :auto_execute -> "robot-repo-automaton"
        :review -> "rhodibot"
        :report_only -> "sustainabot"
      end

    action_type =
      case strategy do
        :auto_execute -> :commit_push
        :review -> :pr_create
        :report_only -> :advisory
      end

    # Gate review -- every action must pass through the Kin Gate
    gate_action = %{
      bot_id: bot_id,
      repo: get_pattern_repo(pattern),
      action_type: action_type,
      confidence: confidence,
      pattern_id: Map.get(pattern, "id", Map.get(pattern, "description", "")),
      scan_timestamp: Map.get(pattern, "scan_timestamp"),
      dispatch_tier: strategy
    }

    case gate_review(gate_action) do
      {:approved, _} ->
        do_eliminate_dispatch(strategy, recipe, pattern, confidence)

      {:held, reason} ->
        Logger.warning("Gate held eliminate dispatch: #{reason}")
        {:ok, :held}

      {:rejected, reason} ->
        Logger.warning("Gate rejected eliminate dispatch: #{reason}")
        {:error, :gate_rejected, reason}

      {:deferred, wait_ms} ->
        Logger.info("Gate deferred eliminate dispatch -- retry in #{div(wait_ms, 1000)}s")
        {:ok, :deferred}
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

  @doc """
  Dispatch a ProofObligation recipe through the Safety Triangle.

  Called by `ProofObligation.obligations_from_patterns/2` and any code
  that constructs `{:proof_obligation, recipe, pattern}` tuples.

  Triangle routing for proof obligations:
  - `:eliminate` (auto-provable, confidence >= 0.90) →
      robot-repo-automaton applies tactic inline
  - `:eliminate` (confidence < 0.90) →
      echidnabot with eliminate-tier hint
  - `:substitute` →
      echidnabot with VeriSimDB-recommended prover hint
  - `:control` →
      sustainabot advisory (sorry/Admitted present, human required)
  """
  def dispatch_routed_action({:proof_obligation, recipe, pattern}) do
    tier = Map.get(recipe, "triangle_tier", "substitute")
    claim = Map.get(recipe, "claim", Map.get(pattern, "description", ""))
    context = Map.get(recipe, "context", "")
    repo = Map.get(recipe, "repo", get_pattern_repo(pattern))
    prover_hint = Map.get(recipe, "prover_hint")
    tactic_hint = Map.get(recipe, "tactic_hint")
    confidence = Map.get(recipe, "confidence", 0.75)

    Logger.info("ProofObligation dispatch: #{ProofObligation.summary(recipe)}")

    case tier do
      "eliminate" when confidence >= 0.90 ->
        # Auto-provable: robot-repo-automaton applies the tactic inline
        dispatch_to_robot_repo_automaton(%{
          type: :auto_fix_request,
          repo: repo,
          file: Map.get(pattern, "file", ""),
          issue: claim,
          fix_type: "proof_tactic",
          confidence: confidence,
          recipe_id: Map.get(recipe, "id"),
          suggestion: "Apply tactic: #{tactic_hint}"
        })

      "eliminate" ->
        # Eliminate-tier but below auto-execute threshold -- ask echidnabot
        dispatch_to_echidnabot(%{
          type: :proof_obligation,
          repo: repo,
          claim: claim,
          context: context,
          prover_hint_override: prover_hint,
          tactic_hint: tactic_hint
        })

      "substitute" ->
        # Standard ECHIDNA routing -- prover hint from VeriSimDB history
        dispatch_to_echidnabot(%{
          type: :proof_obligation,
          repo: repo,
          claim: claim,
          context: context,
          prover_hint_override: prover_hint
        })

      "control" ->
        # Genuinely hard -- sorry/Admitted present, human review required
        dispatch_to_sustainabot(%{
          type: :eco_score,
          repo: repo,
          score: 0.3,
          details:
            "Proof obligation requires human review: #{claim} " <>
              "(sorry/Admitted detected -- formal proof not yet possible)"
        })

      unknown ->
        Logger.warning("ProofObligation: unknown tier '#{unknown}', routing to sustainabot")
        {:error, :unknown_proof_tier}
    end
  end

  # --- Eliminate dispatch helpers (called after Gate approval) ---

  defp do_eliminate_dispatch(:auto_execute, recipe, pattern, confidence) do
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
  end

  defp do_eliminate_dispatch(:review, recipe, pattern, _confidence) do
    dispatch_to_rhodibot(%{
      type: :fix_suggestion,
      repo: get_pattern_repo(pattern),
      file: Map.get(pattern, "file", ""),
      issue: Map.get(pattern, "description", ""),
      suggestion: Map.get(recipe, "description", "")
    })
  end

  defp do_eliminate_dispatch(:report_only, _recipe, pattern, _confidence) do
    dispatch_to_sustainabot(%{
      type: :eco_score,
      repo: get_pattern_repo(pattern),
      score: Map.get(pattern, "severity_score", 0.5),
      details: "Low-confidence eliminate: #{Map.get(pattern, "description", "")}"
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
  - :panic_finding -> panicbot (fixable → auto-fix pipeline, unfixable → A2ML debt register)
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
      :panic_finding -> dispatch_to_panicbot(finding)
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
    # Closes the proof learning loop: classify this obligation, ask VeriSimDB
    # which prover has historically worked best on that class, and pass the
    # recommendation as a hint to echidnabot so it runs the winning prover
    # first instead of always defaulting to Lean.
    #
    # If `prover_hint_override` is already set (from ProofObligation.to_recipe/2,
    # which pre-fetches the hint), skip the VeriSimDB lookup to avoid a
    # redundant round trip.
    #
    # If VeriSimDB is unreachable or has no data for this class, dispatch
    # proceeds without a prover hint (echidnabot falls back to Lean default).
    claim = finding_field(finding, :claim, "")
    context = finding_field(finding, :context, "")

    prover_hint =
      case Map.get(finding, :prover_hint_override) do
        nil ->
          obligation_class = Hypatia.Rules.ProofStrategySelection.classify_obligation(claim)
          lookup_prover_hint(obligation_class)

        override ->
          # Already resolved by ProofObligation.to_recipe/2 -- pass through
          # the lowercase string; normalise_prover_hint/1 handles the mapping.
          override
      end

    mutation =
      build_proof_obligation_mutation(
        finding_field(finding, :repo),
        claim,
        context,
        prover_hint
      )

    execute_graphql(mutation, "echidnabot")
  end

  # Look up the historically-best prover for an obligation class from
  # VeriSimDB's proof_attempts MV. Returns the lowercase prover name
  # (e.g. "coq") or nil if no data / VeriSimDB unreachable.
  defp lookup_prover_hint(obligation_class) do
    case Hypatia.Rules.ProofStrategySelection.recommend(obligation_class, limit: 3) do
      {:ok, [top | _]} ->
        prover = Map.get(top, "prover")
        explanation = Hypatia.Rules.ProofStrategySelection.explain_strategy([top])
        Logger.info("echidnabot strategy (#{obligation_class}): #{explanation}")
        prover

      {:ok, []} ->
        Logger.debug("echidnabot strategy (#{obligation_class}): no historical data")
        nil

      {:error, reason} ->
        Logger.debug(
          "echidnabot strategy lookup failed (#{obligation_class}): #{inspect(reason)}"
        )

        nil
    end
  end

  # Build the submitProofObligation mutation using the input-object syntax
  # that ProofObligationInput expects. The prover field is omitted when
  # prover_hint is nil OR the hint names a prover echidnabot's GraphQL
  # schema doesn't know about (VeriSimDB tracks more provers than echidnabot
  # currently exposes). In either case echidnabot uses its own default.
  defp build_proof_obligation_mutation(repo, claim, context, prover_hint) do
    prover_line =
      case normalise_prover_hint(prover_hint) do
        nil -> ""
        enum_value -> "        prover: #{enum_value},\n"
      end

    """
    mutation {
      submitProofObligation(input: {
        repo: "#{repo}",
        claim: "#{escape_quotes(claim)}",
        context: "#{escape_quotes(context)}",
    #{prover_line}  }) {
        success
        proofId
      }
    }
    """
  end

  # Map a VeriSimDB prover string to echidnabot's GraphQL ProverKind enum
  # variant name. Returns nil for provers echidnabot doesn't expose (idris2,
  # fstar, altergo, dafny, why3, tlaps, vampire, eprover, other) -- caller
  # falls back to echidnabot's own default (Lean).
  defp normalise_prover_hint(nil), do: nil

  defp normalise_prover_hint(hint) when is_binary(hint) do
    case hint do
      "coq" -> "COQ"
      "lean" -> "LEAN"
      "agda" -> "AGDA"
      "isabelle" -> "ISABELLE"
      "z3" -> "Z3"
      "cvc5" -> "CVC5"
      "metamath" -> "METAMATH"
      "hol_light" -> "HOL_LIGHT"
      "mizar" -> "MIZAR"
      "pvs" -> "PVS"
      "acl2" -> "ACL2"
      "hol4" -> "HOL4"
      _ -> nil
    end
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

  defp dispatch_to_panicbot(finding) do
    # Route fixable findings to robot-repo-automaton via standard auto-fix pipeline.
    # Route unfixable findings back to panicbot for A2ML debt register documentation.
    fixable = finding_field(finding, :fixable, false)
    confidence = finding_field(finding, :confidence, 0.0)

    if fixable do
      strategy = TriangleRouter.dispatch_strategy(confidence)

      case strategy do
        :auto_execute ->
          dispatch_to_robot_repo_automaton(%{
            type: :auto_fix_request,
            repo: finding_field(finding, :repo),
            file: finding_field(finding, :file, ""),
            issue: finding_field(finding, :issue, ""),
            fix_type: "panic-finding",
            confidence: confidence,
            recipe_id: finding_field(finding, :rule_id, ""),
            suggestion: finding_field(finding, :suggestion, "")
          })

        :review ->
          dispatch_to_rhodibot(%{
            type: :fix_suggestion,
            repo: finding_field(finding, :repo),
            file: finding_field(finding, :file, ""),
            issue: finding_field(finding, :issue, ""),
            suggestion: finding_field(finding, :suggestion, "")
          })

        :report_only ->
          dispatch_to_sustainabot(%{
            type: :eco_score,
            repo: finding_field(finding, :repo),
            score: 0.5,
            details: "Low-confidence panic finding: #{finding_field(finding, :issue, "")}"
          })
      end
    else
      # Unfixable: log as advisory via sustainabot
      dispatch_to_sustainabot(%{
        type: :eco_score,
        repo: finding_field(finding, :repo),
        score: Map.get(finding, :severity_score, 0.5),
        details:
          "Unfixable (panicbot): #{finding_field(finding, :issue, "")} -- documented in .panicbot/PANICBOT-FINDINGS.a2ml"
      })
    end
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

    # Feed outcome to neural coordinator for continuous learning.
    # The coordinator updates MoE, LSM, and ESN networks with this data.
    outcome_str = if success, do: "success", else: "failure"

    try do
      Hypatia.Neural.Coordinator.record_outcome(finding, outcome_str)
    catch
      :exit, _ -> Logger.warning("Neural coordinator unavailable for outcome recording")
    end

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
    manifest_path =
      Path.join([
        Application.get_env(:hypatia, :verisimdb_data_path, "data/verisim"),
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

    # 2. Attempt HTTP dispatch (graceful degradation if unavailable).
    #
    # URL resolution precedence:
    #   a. HYPATIA_{BOT}_URL      → POST {url}/graphql directly (per-bot override)
    #   b. HYPATIA_FLEET_URL      → POST {url}/dispatch/{bot_name} (fleet coordinator)
    #   c. (neither set)          → file-only dispatch
    #
    # Per-bot URLs are preferred: they let hypatia talk directly to each bot's
    # GraphQL endpoint instead of routing through a fleet coordinator that may
    # not exist. The fleet-coordinator path is kept as a fallback for
    # deployments that front multiple bots behind one dispatcher.
    {target_url, description, body} = resolve_dispatch_url(bot_name, query)

    if target_url do
      try do
        case http_post(target_url, body) do
          {:ok, _response} ->
            Logger.info("Live dispatch to #{bot_name} succeeded (#{description})")
            {:ok, :dispatched}

          {:error, reason} ->
            Logger.warning(
              "Live dispatch to #{bot_name} failed (#{inspect(reason)}), file dispatch used"
            )

            {:ok, :file_dispatched}
        end
      rescue
        e ->
          Logger.warning("HTTP dispatch error: #{inspect(e)}, file dispatch used")
          {:ok, :file_dispatched}
      end
    else
      Logger.info("Dispatched to #{bot_name} via manifest (no URL configured)")
      {:ok, :file_dispatched}
    end
  end

  # Resolve the HTTP dispatch target for a bot.
  # Returns {url, description, body} on success, {nil, _, _} when no URL configured.
  #
  # - Per-bot URLs hit the bot's own GraphQL endpoint, so we wrap the query in
  #   a GraphQL-over-HTTP JSON envelope: {"query": "..."}.
  # - Fleet-coordinator URLs hit /dispatch/{bot_name} with the raw query as
  #   body, preserving legacy fleet-coordinator semantics.
  defp resolve_dispatch_url(bot_name, query) do
    per_bot_env = "HYPATIA_" <> String.upcase(bot_name) <> "_URL"

    case System.get_env(per_bot_env) do
      nil ->
        case System.get_env("HYPATIA_FLEET_URL") do
          nil ->
            {nil, :none, nil}

          fleet_url ->
            {fleet_url <> "/dispatch/" <> bot_name, "via fleet coordinator", query}
        end

      bot_url ->
        envelope = Jason.encode!(%{"query" => query})
        {bot_url <> "/graphql", "via per-bot URL #{per_bot_env}", envelope}
    end
  end

  defp http_post(url, body) do
    # Attempt HTTP POST -- works if :httpc is available (OTP built-in)
    case :httpc.request(
           :post,
           {String.to_charlist(url), [{~c"content-type", ~c"application/json"}],
            ~c"application/json", String.to_charlist(body)},
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
    # Pattern may have repos_affected_list -- take first repo as representative
    case Map.get(pattern, "repos_affected_list", []) do
      [repo | _] -> repo
      _ -> Map.get(pattern, "routed_repo", "unknown")
    end
  end

  # --- Kin Gate Integration ---

  defp gate_review(gate_action) do
    # Check contingency level first -- if system is frozen/shutdown, reject immediately
    case contingency_check(gate_action.dispatch_tier) do
      :permitted ->
        # Check if bot is isolated
        case bot_isolation_check(gate_action.bot_id) do
          :clear ->
            # Full Gate review
            if Process.whereis(Hypatia.Kin.Gate) do
              Hypatia.Kin.Gate.review(gate_action)
            else
              # Gate not running = pass through
              {:approved, gate_action}
            end

          {:isolated, reason} ->
            {:rejected, "bot #{gate_action.bot_id} is isolated: #{reason}"}
        end

      {:blocked, reason} ->
        {:rejected, reason}
    end
  end

  defp contingency_check(dispatch_tier) do
    if Process.whereis(Hypatia.Kin.Contingency) do
      if Hypatia.Kin.Contingency.action_permitted?(dispatch_tier) do
        :permitted
      else
        level = Hypatia.Kin.Contingency.level()
        {:blocked, "contingency level :#{level} -- actions not permitted"}
      end
    else
      :permitted
    end
  end

  defp bot_isolation_check(bot_id) do
    if Process.whereis(Hypatia.Kin.Contingency) do
      if Hypatia.Kin.Contingency.bot_isolated?(bot_id) do
        {:isolated, "quarantined by contingency system"}
      else
        :clear
      end
    else
      :clear
    end
  end

  # --- Direct PR Helpers ---

  # Check if direct PR mode is enabled (compile-time default + runtime override).
  defp direct_pr_enabled? do
    @direct_pr_enabled or System.get_env("HYPATIA_DIRECT_PR") == "true"
  end

  # A pattern is a direct PR candidate if it has a scorecard source and a
  # supported fix script in DirectGitHubPR.
  defp is_direct_pr_candidate?(pattern) do
    source = Map.get(pattern, "source", "")
    check_id = Map.get(pattern, "pa_rule", "")

    # Normalize compact format (SC013 -> SC-013) for lookup
    normalized_id =
      if Regex.match?(~r/^SC\d{3}$/, check_id) do
        String.replace(check_id, ~r/^SC(\d{3})$/, "SC-\\1")
      else
        check_id
      end

    source == "scorecard" and Map.has_key?(DirectGitHubPR.supported_checks(), normalized_id)
  end
end
