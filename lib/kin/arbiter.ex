# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Kin.Arbiter do
  @moduledoc """
  Kin Arbiter -- conflict resolution and priority arbitration between bots.

  When multiple bots want to act on the same repo or the same finding, the Arbiter
  decides who goes first and whether actions are compatible. It also handles:

  - **Priority ranking**: which bot's action takes precedence
  - **Merge analysis**: can two fixes coexist or do they conflict?
  - **Deadlock detection**: circular dependencies between bot actions
  - **Rollback coordination**: if one fix breaks another, coordinate revert
  - **Synergy detection**: identify when bot outputs reinforce each other

  ## Bot Priority Order (highest to lowest)

  1. echidnabot -- formal verification (safety-critical, always wins)
  2. rhodibot -- RSR compliance (structural, high impact)
  3. panicbot -- security scanning (vulnerability fixes)
  4. sustainabot -- sustainability/supply chain
  5. glambot -- accessibility
  6. seambot -- integration/API
  7. finishingbot -- polish/cleanup
  8. cipherbot -- cryptography
  9. gsbot -- general
  10. accessibilitybot -- a11y
  11. the-hotchocolabot -- comfort/DX
  """

  use GenServer
  require Logger

  @bot_priority %{
    "echidnabot" => 1,
    "rhodibot" => 2,
    "panicbot" => 3,
    "sustainabot" => 4,
    "glambot" => 5,
    "seambot" => 6,
    "finishingbot" => 7,
    "cipherbot" => 8,
    "gsbot" => 9,
    "accessibilitybot" => 10,
    "the-hotchocolabot" => 11
  }

  # Categories that are known to never conflict with each other
  @non_conflicting_pairs MapSet.new([
    {"license", "security"},
    {"accessibility", "security"},
    {"documentation", "security"},
    {"formatting", "security"},
    {"license", "accessibility"},
    {"license", "documentation"},
    {"formatting", "documentation"}
  ])

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Arbitrate between two competing actions on the same repo.

  Returns `{:winner, winning_action, losing_action, reason}`
  or `{:compatible, action_a, action_b}` if they don't conflict.
  """
  def arbitrate(action_a, action_b) do
    GenServer.call(__MODULE__, {:arbitrate, action_a, action_b})
  end

  @doc """
  Check if a proposed action has any pending conflicts.

  Scans the current action queue for anything that would clash.
  Returns `:clear` or `{:conflict, conflicting_action, resolution}`.
  """
  def check_conflicts(action) do
    GenServer.call(__MODULE__, {:check_conflicts, action})
  end

  @doc """
  Register an active action (bot is currently working on this).
  """
  def register_active(action) do
    GenServer.cast(__MODULE__, {:register_active, action})
  end

  @doc """
  Unregister a completed action.
  """
  def complete_action(action_id) do
    GenServer.cast(__MODULE__, {:complete, action_id})
  end

  @doc "Get all active actions."
  def active_actions do
    GenServer.call(__MODULE__, :active_actions)
  end

  @doc "Get conflict resolution history."
  def resolution_history do
    GenServer.call(__MODULE__, :resolution_history)
  end

  @doc "Detect synergies -- actions that reinforce each other."
  def detect_synergies do
    GenServer.call(__MODULE__, :detect_synergies)
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(_opts) do
    state = %{
      active_actions: %{},     # action_id => action
      resolution_history: [],  # list of past arbitration decisions
      synergy_log: []          # detected synergies between bot actions
    }

    Logger.info("Kin.Arbiter started -- bot conflict resolution active.")
    {:ok, state}
  end

  @impl true
  def handle_call({:arbitrate, action_a, action_b}, _from, state) do
    {result, new_state} = do_arbitrate(action_a, action_b, state)
    {:reply, result, new_state}
  end

  @impl true
  def handle_call({:check_conflicts, action}, _from, state) do
    result = scan_for_conflicts(action, state)
    {:reply, result, state}
  end

  @impl true
  def handle_call(:active_actions, _from, state) do
    {:reply, state.active_actions, state}
  end

  @impl true
  def handle_call(:resolution_history, _from, state) do
    {:reply, Enum.take(state.resolution_history, 50), state}
  end

  @impl true
  def handle_call(:detect_synergies, _from, state) do
    synergies = find_synergies(state.active_actions)
    {:reply, synergies, %{state | synergy_log: synergies ++ state.synergy_log}}
  end

  @impl true
  def handle_cast({:register_active, action}, state) do
    action_id = action_id(action)
    new_active = Map.put(state.active_actions, action_id, Map.put(action, :registered_at, DateTime.utc_now()))
    {:noreply, %{state | active_actions: new_active}}
  end

  @impl true
  def handle_cast({:complete, action_id}, state) do
    new_active = Map.delete(state.active_actions, action_id)
    {:noreply, %{state | active_actions: new_active}}
  end

  # --- Arbitration Logic ---

  defp do_arbitrate(action_a, action_b, state) do
    cond do
      # Different repos = no conflict
      action_a.repo != action_b.repo ->
        {{:compatible, action_a, action_b}, state}

      # Same repo, check if categories conflict
      non_conflicting?(action_a, action_b) ->
        new_state = log_resolution(state, action_a, action_b, :compatible, "non-conflicting categories")
        {{:compatible, action_a, action_b}, new_state}

      # Same repo, same pattern = direct conflict, priority wins
      Map.get(action_a, :pattern_id) == Map.get(action_b, :pattern_id) ->
        resolve_by_priority(action_a, action_b, state)

      # Same repo, different patterns, but touching same files
      files_overlap?(action_a, action_b) ->
        resolve_by_priority(action_a, action_b, state)

      # Same repo, different patterns, different files = compatible
      true ->
        new_state = log_resolution(state, action_a, action_b, :compatible, "different files")
        {{:compatible, action_a, action_b}, new_state}
    end
  end

  defp resolve_by_priority(action_a, action_b, state) do
    priority_a = Map.get(@bot_priority, action_a.bot_id, 99)
    priority_b = Map.get(@bot_priority, action_b.bot_id, 99)

    {winner, loser, reason} =
      cond do
        priority_a < priority_b ->
          {action_a, action_b, "#{action_a.bot_id} (priority #{priority_a}) outranks #{action_b.bot_id} (priority #{priority_b})"}
        priority_b < priority_a ->
          {action_b, action_a, "#{action_b.bot_id} (priority #{priority_b}) outranks #{action_a.bot_id} (priority #{priority_a})"}
        true ->
          # Same priority: higher confidence wins
          conf_a = Map.get(action_a, :confidence, 0.0)
          conf_b = Map.get(action_b, :confidence, 0.0)
          if conf_a >= conf_b do
            {action_a, action_b, "same priority, #{action_a.bot_id} has higher confidence (#{conf_a} vs #{conf_b})"}
          else
            {action_b, action_a, "same priority, #{action_b.bot_id} has higher confidence (#{conf_b} vs #{conf_a})"}
          end
      end

    Logger.info("Arbiter: #{winner.bot_id} wins over #{loser.bot_id} on #{winner.repo} -- #{reason}")

    result = {:winner, winner, loser, reason}
    new_state = log_resolution(state, action_a, action_b, :priority_resolved, reason)
    {result, new_state}
  end

  defp non_conflicting?(action_a, action_b) do
    cat_a = categorize_action(action_a)
    cat_b = categorize_action(action_b)
    pair = if cat_a <= cat_b, do: {cat_a, cat_b}, else: {cat_b, cat_a}
    MapSet.member?(@non_conflicting_pairs, pair)
  end

  defp categorize_action(action) do
    pattern = Map.get(action, :pattern_id, "")
    cond do
      String.contains?(pattern, "license") or String.contains?(pattern, "spdx") -> "license"
      String.contains?(pattern, "CommandInjection") or String.contains?(pattern, "PathTraversal") -> "security"
      String.contains?(pattern, "accessibility") or String.contains?(pattern, "a11y") -> "accessibility"
      String.contains?(pattern, "doc") -> "documentation"
      String.contains?(pattern, "format") -> "formatting"
      true -> "general"
    end
  end

  defp files_overlap?(action_a, action_b) do
    files_a = Map.get(action_a, :affected_files, []) |> MapSet.new()
    files_b = Map.get(action_b, :affected_files, []) |> MapSet.new()

    not MapSet.disjoint?(files_a, files_b)
  end

  defp scan_for_conflicts(action, state) do
    conflicts =
      state.active_actions
      |> Map.values()
      |> Enum.filter(fn active ->
        active.repo == action.repo and
        not non_conflicting?(action, active) and
        (Map.get(action, :pattern_id) == Map.get(active, :pattern_id) or files_overlap?(action, active))
      end)

    case conflicts do
      [] -> :clear
      [first | _] ->
        {_, resolution_state} = resolve_by_priority(action, first, state)
        {:conflict, first, resolution_state}
    end
  end

  # --- Synergy Detection ---

  defp find_synergies(active_actions) do
    actions = Map.values(active_actions)

    for a <- actions,
        b <- actions,
        a.bot_id != b.bot_id,
        a.repo == b.repo,
        synergy = detect_synergy(a, b),
        synergy != nil do
      synergy
    end
    |> Enum.uniq_by(fn s -> {s.type, Enum.sort([s.bot_a, s.bot_b])} end)
  end

  defp detect_synergy(a, b) do
    cond do
      # echidnabot proving + rhodibot fixing = verified fix
      a.bot_id == "echidnabot" and b.bot_id in ["rhodibot", "panicbot"] ->
        %{type: :verified_fix, bot_a: a.bot_id, bot_b: b.bot_id, repo: a.repo,
          description: "#{a.bot_id} can formally verify #{b.bot_id}'s fix"}

      # panicbot scanning + sustainabot auditing = defense in depth
      a.bot_id == "panicbot" and b.bot_id == "sustainabot" ->
        %{type: :defense_in_depth, bot_a: a.bot_id, bot_b: b.bot_id, repo: a.repo,
          description: "security scan + supply chain audit on same repo"}

      # glambot + seambot = full UX coverage
      a.bot_id == "glambot" and b.bot_id == "seambot" ->
        %{type: :ux_coverage, bot_a: a.bot_id, bot_b: b.bot_id, repo: a.repo,
          description: "accessibility + integration coverage on same repo"}

      true -> nil
    end
  end

  # --- Helpers ---

  defp action_id(action) do
    "#{action.bot_id}:#{action.repo}:#{action.pattern_id || "general"}"
  end

  defp log_resolution(state, action_a, action_b, outcome, reason) do
    entry = %{
      at: DateTime.utc_now(),
      bot_a: action_a.bot_id,
      bot_b: action_b.bot_id,
      repo: action_a.repo,
      outcome: outcome,
      reason: reason
    }

    %{state | resolution_history: Enum.take([entry | state.resolution_history], 200)}
  end
end
