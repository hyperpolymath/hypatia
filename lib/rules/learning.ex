# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.Learning do
  @moduledoc """
  Learning engine rules absorbed from Logtalk engine/rules/learning.lgt
  and engine/rules/rule_distiller.lgt.

  Tracks fix outcomes, adjusts confidence, promotes patterns to rules,
  and manages cross-repo learning.
  """

  use GenServer

  require Logger

  defstruct fixes: %{},
            patterns: %{},
            feedback: [],
            promoted_rules: %{},
            false_positives: %{}

  # ---------------------------------------------------------------------------
  # GenServer API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %__MODULE__{}, Keyword.put_new(opts, :name, __MODULE__))
  end

  @impl true
  def init(state), do: {:ok, state}

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  def learn_from_fix(issue_type, fix, outcome) when outcome in [:success, :failure, :partial] do
    GenServer.cast(__MODULE__, {:learn_fix, issue_type, fix, outcome})
  end

  def record_false_positive(issue_type, context) do
    GenServer.cast(__MODULE__, {:false_positive, issue_type, context})
  end

  def get_confidence(issue_type) do
    GenServer.call(__MODULE__, {:get_confidence, issue_type})
  end

  def get_success_rate(issue_type) do
    GenServer.call(__MODULE__, {:get_success_rate, issue_type})
  end

  def recommend_fix(issue_type) do
    GenServer.call(__MODULE__, {:recommend_fix, issue_type})
  end

  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  def promote_patterns do
    GenServer.call(__MODULE__, :promote_patterns)
  end

  # ---------------------------------------------------------------------------
  # GenServer Callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def handle_cast({:learn_fix, issue_type, fix, outcome}, state) do
    entry = %{fix: fix, outcome: outcome, timestamp: DateTime.utc_now()}
    fixes = Map.update(state.fixes, issue_type, [entry], &[entry | &1])
    {:noreply, %{state | fixes: fixes}}
  end

  @impl true
  def handle_cast({:false_positive, issue_type, context}, state) do
    fps = Map.update(state.false_positives, issue_type, [context], &[context | &1])
    {:noreply, %{state | false_positives: fps}}
  end

  @impl true
  def handle_call({:get_confidence, issue_type}, _from, state) do
    fixes = Map.get(state.fixes, issue_type, [])
    total = length(fixes)

    confidence =
      if total == 0 do
        0.5
      else
        successes = Enum.count(fixes, &(&1.outcome == :success))
        fp_count = length(Map.get(state.false_positives, issue_type, []))
        
        # Apply recency weighting
        weighted_successes = Enum.reduce(fixes, 0, fn fix, acc ->
          age_hours = calculate_age_hours(fix.timestamp)
          weight = :math.exp(-age_hours / 24.0)
          if fix.outcome == :success, do: acc + weight, else: acc
        end)
        
        base = weighted_successes / total
        penalty = min(fp_count * 0.15, 0.5)
        max(base - penalty, 0.0)
      end

    {:reply, confidence, state}
  end

  defp calculate_age_hours(timestamp) do
    now = DateTime.utc_now()
    diff = DateTime.diff(now, timestamp)
    diff.hour + diff.minute / 60 + diff.second / 3600
  end

  @impl true
  def handle_call({:get_success_rate, issue_type}, _from, state) do
    fixes = Map.get(state.fixes, issue_type, [])
    total = length(fixes)

    rate =
      if total == 0 do
        0.0
      else
        Enum.count(fixes, &(&1.outcome == :success)) / total
      end

    {:reply, rate, state}
  end

  @impl true
  def handle_call({:recommend_fix, issue_type}, _from, state) do
    fixes = Map.get(state.fixes, issue_type, [])
    successes = Enum.filter(fixes, &(&1.outcome == :success))

    recommendation =
      successes
      |> Enum.group_by(& &1.fix)
      |> Enum.max_by(fn {_fix, entries} -> length(entries) end, fn -> nil end)
      |> case do
        nil -> nil
        {fix, _entries} -> fix
      end

    {:reply, recommendation, state}
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    total_fixes = state.fixes |> Map.values() |> List.flatten() |> length()
    total_patterns = map_size(state.patterns)
    total_feedback = length(state.feedback)
    promoted = map_size(state.promoted_rules)
    total_fps = state.false_positives |> Map.values() |> List.flatten() |> length()

    stats = %{
      total_fixes: total_fixes,
      total_patterns: total_patterns,
      total_feedback: total_feedback,
      promoted_rules: promoted,
      false_positives: total_fps
    }

    {:reply, stats, state}
  end

  @impl true
  def handle_call(:promote_patterns, _from, state) do
    # Promote patterns with confidence >= 0.75 and occurrences >= 5
    promotable =
      state.fixes
      |> Enum.filter(fn {_type, fixes} ->
        total = length(fixes)
        successes = Enum.count(fixes, &(&1.outcome == :success))
        total >= 5 and successes / total >= 0.75
      end)
      |> Enum.map(fn {type, fixes} ->
        successes = Enum.count(fixes, &(&1.outcome == :success))
        {type, %{confidence: successes / length(fixes), occurrences: length(fixes)}}
      end)
      |> Map.new()

    # Check for disqualifying conditions
    promotable =
      Enum.reject(promotable, fn {type, _info} ->
        fp_count = length(Map.get(state.false_positives, type, []))
        total = length(Map.get(state.fixes, type, []))
        fp_count > 0 and fp_count / total > 0.3
      end)
      |> Map.new()

    new_promoted = Map.merge(state.promoted_rules, promotable)
    {:reply, {:ok, map_size(promotable)}, %{state | promoted_rules: new_promoted}}
  end

  # ====================================================================
  # False Positive Decay
  # ====================================================================

  defp decay_false_positives(state) do
    thirty_days_ago = DateTime.add(DateTime.utc_now(), -30)
    
    Map.map(state.false_positives, fn {issue_type, contexts} ->
      {issue_type, Enum.reject(contexts, fn ctx ->
        # Assume context has timestamp if it's a map
        # If not, keep it (backward compatibility)
        if is_map(ctx) && Map.has_key?(ctx, :timestamp) do
          ctx.timestamp < thirty_days_ago
        else
          false  # Keep non-timestamped contexts
        end
      end)}
    end)
  end
end
