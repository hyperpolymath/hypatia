# SPDX-License-Identifier: EUPL-1.2
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ProofObligation do
  @moduledoc """
  ProofObligation recipe type for Hypatia.

  Classifies proof obligations through the Safety Triangle and generates
  structured recipe maps that the `FleetDispatcher` routes to echidnabot.

  ## Safety Triangle tiers for proof obligations

  | Tier        | Meaning                                       | Action                                    |
  |-------------|-----------------------------------------------|-------------------------------------------|
  | `:eliminate`  | Auto-provable: `simp`/`omega`/`decide`/`ring` | Robot-repo-automaton applies tactic inline |
  | `:substitute` | Needs premise selection — ECHIDNA routes this | echidnabot with prover hint from VeriSimDB |
  | `:control`    | Genuinely hard: `sorry`/`Admitted` present    | sustainabot advisory + human review required |

  The tier assignment is prover-agnostic: the same `classify/2` function
  handles obligations from all 9 provers without modification. Prover
  preference is resolved downstream by `ProofStrategySelection.recommend/2`
  against historical VeriSimDB outcomes.

  ## Data flow

  ```
  scan finding with proof_obligation type
      → ProofObligation.classify(claim)
      → {:eliminate, confidence} | {:substitute, class} | {:control, reason}
      → FleetDispatcher.dispatch_routed_action({:proof_obligation, obligation, pattern})
      → dispatch based on tier:
          eliminate  → robot-repo-automaton (auto-apply tactic)
          substitute → echidnabot (ECHIDNA proves, records to VeriSimDB)
          control    → sustainabot (human review advisory)
  ```

  Rule IDs: PO001–PO006
  """

  require Logger

  alias Hypatia.Rules.ProofStrategySelection

  # ─── Tactic keywords that indicate auto-provability ───────────────────────

  # Tactics that decision procedures can discharge without premise search.
  # Extend this list as new provers / automation tactics are confirmed safe.
  @eliminate_tactics ~w(
    simp omega decide ring norm_num linarith
    trivial tauto auto exact rfl refl
    native_decide decide! norm_cast push_cast
  )

  # Keywords in a claim or file that flag genuinely hard obligations
  # requiring human judgment. A single match → :control tier.
  @control_markers ~w(
    sorry Admitted admit SORRY ADMIT
    postulate axiom assume axiom:
    believe_me assert_total
    unsafeCoerce unsafe
  )

  # ─── Public API ───────────────────────────────────────────────────────────

  @doc """
  PO001: Classify a proof obligation claim through the Safety Triangle.

  Returns one of:
  - `{:eliminate, confidence}` — auto-provable by a decision tactic
  - `{:substitute, obligation_class}` — needs premise selection via ECHIDNA
  - `{:control, reason}` — genuinely hard, `sorry` or human review required

  ## Options

  - `:context` — surrounding code text for richer classification (default: `""`)

  ## Examples

      iex> ProofObligation.classify("omega proves this bound")
      {:eliminate, 0.92}

      iex> ProofObligation.classify("linear type invariant for allocation")
      {:substitute, "linearity"}

      iex> ProofObligation.classify("Admitted -- TODO fix later")
      {:control, "sorry or Admitted found in claim"}
  """
  @spec classify(String.t(), keyword()) ::
          {:eliminate, float()}
          | {:substitute, String.t()}
          | {:control, String.t()}
  def classify(claim, opts \\ []) when is_binary(claim) do
    context = Keyword.get(opts, :context, "")
    combined = "#{claim} #{context}"

    cond do
      control_marker_present?(combined) ->
        marker = first_control_marker(combined)
        {:control, "#{marker} found — human review required"}

      eliminate_candidate?(combined) ->
        confidence = eliminate_confidence(combined)
        {:eliminate, confidence}

      true ->
        class = ProofStrategySelection.classify_obligation(claim)
        {:substitute, class}
    end
  end

  @doc """
  PO002: Convert a scan finding into a ProofObligation recipe map.

  The returned map is compatible with `FleetDispatcher.dispatch_routed_action/1`
  when wrapped as `{:proof_obligation, recipe, pattern}`.

  Fields set by this function:
  - `"id"` — deterministic recipe ID from claim hash
  - `"type"` — `"proof_obligation"`
  - `"triangle_tier"` — `"eliminate"` | `"substitute"` | `"control"`
  - `"obligation_class"` — mathematical obligation class
  - `"prover_hint"` — best prover from VeriSimDB history (or `nil`)
  - `"confidence"` — routing confidence
  - `"claim"` — the obligation text
  - `"context"` — surrounding code context
  - `"repo"` — repository slug
  - `"auto_fixable"` — `true` only for eliminate tier
  - `"tactic_hint"` — tactic suggestion for eliminate tier (`nil` otherwise)
  - `"requires_human"` — `true` for control tier
  """
  @spec to_recipe(map(), keyword()) :: map()
  def to_recipe(finding, opts \\ []) when is_map(finding) do
    claim = Map.get(finding, "claim", Map.get(finding, :claim, ""))
    context = Map.get(finding, "context", Map.get(finding, :context, ""))
    repo = Map.get(finding, "repo", Map.get(finding, :repo, "unknown"))

    classification = classify(claim, context: context)

    {tier_str, obligation_class, confidence, auto_fixable, tactic_hint, requires_human} =
      expand_classification(classification, claim, opts)

    %{
      "id" => recipe_id(claim, repo),
      "type" => "proof_obligation",
      "triangle_tier" => tier_str,
      "obligation_class" => obligation_class,
      "prover_hint" => prover_hint_for(obligation_class, opts),
      "confidence" => confidence,
      "claim" => claim,
      "context" => context,
      "repo" => repo,
      "auto_fixable" => auto_fixable,
      "tactic_hint" => tactic_hint,
      "requires_human" => requires_human
    }
  end

  @doc """
  PO003: Generate proof obligation findings from a list of scan patterns.

  Filters patterns whose category or description suggests a formal proof
  is required (e.g. linearity invariants, termination bounds, memory safety
  obligations), then converts each to a ProofObligation recipe.

  Returns a list of `{:proof_obligation, recipe, pattern}` tuples ready
  for `FleetDispatcher.dispatch_routed_action/1`.
  """
  @spec obligations_from_patterns([map()], keyword()) :: [
          {:proof_obligation, map(), map()}
        ]
  def obligations_from_patterns(patterns, opts \\ []) when is_list(patterns) do
    patterns
    |> Enum.filter(&proof_relevant?/1)
    |> Enum.map(fn pattern ->
      recipe = to_recipe(pattern, opts)
      {:proof_obligation, recipe, pattern}
    end)
  end

  @doc """
  PO004: Check whether a pattern is proof-relevant.

  A pattern is proof-relevant if its category or description contains
  keywords that indicate a formal correctness obligation (not merely a
  style or security issue resolvable by a fix script).

  This is the gate before `to_recipe/2` — non-proof patterns are
  silently skipped so the proof obligation pipeline stays focused.
  """
  @spec proof_relevant?(map()) :: boolean()
  def proof_relevant?(pattern) when is_map(pattern) do
    text =
      [
        Map.get(pattern, "category", ""),
        Map.get(pattern, "description", ""),
        Map.get(pattern, "claim", "")
      ]
      |> Enum.join(" ")
      |> String.downcase()

    Enum.any?(
      [
        "linearity",
        "linear type",
        "termination",
        "terminating",
        "memory safety",
        "type safety",
        "invariant",
        "proof obligation",
        "formal verification",
        "sorry",
        "admitted",
        "postulate",
        "axiom",
        "correctness",
        "soundness"
      ],
      &String.contains?(text, &1)
    )
  end

  @doc """
  PO005: Dispatch strategy for a given classification.

  Returns `{bot_id, action_type}` for the FleetDispatcher to route to.
  Mirrors the eliminate/substitute/control dispatch strategy, but for
  proof obligations rather than fix recipes.
  """
  @spec dispatch_strategy({:eliminate | :substitute | :control, any()}) ::
          {String.t(), atom()}
  def dispatch_strategy({:eliminate, confidence}) when confidence >= 0.90,
    do: {"robot-repo-automaton", :auto_fix_request}

  def dispatch_strategy({:eliminate, _}),
    do: {"echidnabot", :proof_obligation}

  def dispatch_strategy({:substitute, _class}),
    do: {"echidnabot", :proof_obligation}

  def dispatch_strategy({:control, _reason}),
    do: {"sustainabot", :advisory}

  @doc """
  PO006: Human-readable summary of a ProofObligation recipe for logs.
  """
  @spec summary(map()) :: String.t()
  def summary(recipe) when is_map(recipe) do
    tier = Map.get(recipe, "triangle_tier", "unknown")
    class = Map.get(recipe, "obligation_class", "unknown")
    prover = Map.get(recipe, "prover_hint", "any")
    repo = Map.get(recipe, "repo", "unknown")
    "ProofObligation[#{tier}] class=#{class} prover=#{prover} repo=#{repo}"
  end

  # ─── Private helpers ──────────────────────────────────────────────────────

  defp control_marker_present?(text) do
    Enum.any?(@control_markers, &String.contains?(text, &1))
  end

  defp first_control_marker(text) do
    Enum.find(@control_markers, "unknown", &String.contains?(text, &1))
  end

  defp eliminate_candidate?(text) do
    lower = String.downcase(text)
    Enum.any?(@eliminate_tactics, &String.contains?(lower, &1))
  end

  # Higher confidence when multiple auto-tactics appear.
  defp eliminate_confidence(text) do
    lower = String.downcase(text)
    count = Enum.count(@eliminate_tactics, &String.contains?(lower, &1))

    cond do
      count >= 3 -> 0.96
      count == 2 -> 0.93
      true -> 0.90
    end
  end

  # Expand a classification into the full set of recipe fields.
  defp expand_classification({:eliminate, confidence}, _claim, _opts) do
    tactic = best_tactic_hint()
    {"eliminate", "decidable", confidence, true, tactic, false}
  end

  defp expand_classification({:substitute, class}, _claim, _opts) do
    {"substitute", class, 0.75, false, nil, false}
  end

  defp expand_classification({:control, reason}, _claim, _opts) do
    Logger.info("ProofObligation :control — #{reason}")
    {"control", "hard_obligation", 0.50, false, nil, true}
  end

  # Look up the historically best prover for an obligation class.
  # Returns nil if VeriSimDB is unreachable or has no data for this class.
  defp prover_hint_for(obligation_class, opts) do
    base_url =
      Keyword.get(
        opts,
        :verisim_url,
        System.get_env("HYPATIA_VERISIM_URL") || "http://localhost:8080"
      )

    case ProofStrategySelection.recommend(obligation_class, base_url: base_url) do
      {:ok, [%{"prover" => p} | _]} -> p
      _ -> nil
    end
  end

  # Deterministic recipe ID: "po-" + first 12 chars of claim hash.
  defp recipe_id(claim, repo) do
    hash =
      :crypto.hash(:sha256, "#{repo}:#{claim}")
      |> Base.encode16(case: :lower)
      |> String.slice(0, 12)

    "po-#{hash}"
  end

  # Best tactic hint for the eliminate tier (generic auto-tactic).
  defp best_tactic_hint, do: "simp; omega; decide"
end
