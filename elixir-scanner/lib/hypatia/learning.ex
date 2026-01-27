# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Learning do
  @moduledoc """
  Learning engine for autonomous security pattern discovery.

  Tracks pattern observations and auto-generates rules when thresholds are reached:
  - 5 observations: Generate rule proposal
  - 10 observations + 3 successful fixes: Auto-approve and integrate
  """

  use GenServer

  require Logger

  @proposal_threshold 5
  @auto_approve_threshold 10
  @min_successful_fixes 3

  # Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Observe a security pattern.
  Returns {:ok, :tracked} or {:ok, :proposal_generated}.
  """
  def observe_pattern(pattern, context) do
    GenServer.call(__MODULE__, {:observe, pattern, context})
  end

  @doc """
  Record a fix outcome (success or failure).
  """
  def record_fix(pattern, file, outcome) when outcome in [:success, :failure] do
    GenServer.cast(__MODULE__, {:record_fix, pattern, file, outcome})
  end

  @doc """
  Get current observation counts.
  """
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    state = %{
      observations: %{},  # %{pattern => count}
      contexts: %{},      # %{pattern => [contexts]}
      fixes: %{},         # %{pattern => %{success: N, failure: N}}
      proposals: MapSet.new()  # Set of patterns with generated proposals
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:observe, pattern, context}, _from, state) do
    count = Map.get(state.observations, pattern, 0) + 1
    observations = Map.put(state.observations, pattern, count)

    # Track context
    contexts = Map.update(state.contexts, pattern, [context], &[context | &1])

    state = %{state | observations: observations, contexts: contexts}

    cond do
      # Already proposed
      MapSet.member?(state.proposals, pattern) ->
        Logger.debug("Pattern #{pattern} already proposed (#{count} observations)")
        {:reply, {:ok, :tracked}, state}

      # Check for auto-approval threshold
      count >= @auto_approve_threshold and should_auto_approve?(pattern, state) ->
        Logger.info("Pattern #{pattern} reached auto-approval threshold!")
        auto_approve_rule(pattern, state)
        proposals = MapSet.put(state.proposals, pattern)
        {:reply, {:ok, :auto_approved}, %{state | proposals: proposals}}

      # Check for proposal threshold
      count >= @proposal_threshold ->
        Logger.info("Pattern #{pattern} reached proposal threshold (#{count} observations)")
        generate_rule_proposal(pattern, state)
        proposals = MapSet.put(state.proposals, pattern)
        {:reply, {:ok, :proposal_generated}, %{state | proposals: proposals}}

      # Just track
      true ->
        Logger.debug("Tracking pattern #{pattern} (#{count} observations)")
        {:reply, {:ok, :tracked}, state}
    end
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    stats = %{
      total_patterns: map_size(state.observations),
      observations: state.observations,
      proposals: MapSet.size(state.proposals),
      fix_success_rates: calculate_success_rates(state.fixes)
    }

    {:reply, stats, state}
  end

  @impl true
  def handle_cast({:record_fix, pattern, _file, outcome}, state) do
    fixes =
      Map.update(state.fixes, pattern, %{success: 0, failure: 0}, fn counts ->
        Map.update!(counts, outcome, &(&1 + 1))
      end)

    {:noreply, %{state | fixes: fixes}}
  end

  # Private Functions

  defp should_auto_approve?(pattern, state) do
    case Map.get(state.fixes, pattern) do
      %{success: success, failure: failure} ->
        success >= @min_successful_fixes and success > failure

      _ ->
        false
    end
  end

  defp generate_rule_proposal(pattern, state) do
    contexts = Map.get(state.contexts, pattern, [])
    count = Map.get(state.observations, pattern, 0)

    # Extract common characteristics from contexts
    sample_context = List.first(contexts) || %{}

    proposal = """
    # Auto-generated rule proposal
    # Pattern: #{pattern}
    # Observations: #{count}
    # Generated: #{DateTime.utc_now() |> DateTime.to_iso8601()}

    # TODO: Review and integrate into patterns.ex

    %{
      name: "#{pattern}",
      regex: ~r/#{sample_context[:code_snippet] || pattern}/,
      severity: :high,
      type: "#{sample_context[:type] || "auto_detected"}",
      cwe: "#{sample_context[:cwe] || "CWE-TODO"}",
      fix: "Auto-generated fix suggestion - NEEDS REVIEW"
    }
    """

    # Write proposal to file
    proposal_dir = "/var/mnt/eclipse/repos/gitbot-fleet/shared-context/learning/rule-proposals"
    File.mkdir_p!(proposal_dir)

    proposal_file = Path.join(proposal_dir, "#{pattern}.ex")
    File.write!(proposal_file, proposal)

    Logger.info("✓ Rule proposal created: #{proposal_file}")
  end

  defp auto_approve_rule(pattern, state) do
    fixes = Map.get(state.fixes, pattern, %{})

    Logger.info("""
    Pattern #{pattern} AUTO-APPROVED:
    - Observations: #{Map.get(state.observations, pattern, 0)}
    - Success rate: #{fixes[:success] || 0}/#{(fixes[:success] || 0) + (fixes[:failure] || 0)}

    TODO: Integrate into Hypatia.Patterns automatically
    """)

    # In production, this would add to patterns.ex programmatically
    # For now, just log the recommendation
  end

  defp calculate_success_rates(fixes) do
    Enum.map(fixes, fn {pattern, counts} ->
      total = counts.success + counts.failure
      rate = if total > 0, do: counts.success / total * 100, else: 0

      {pattern, %{
        success: counts.success,
        failure: counts.failure,
        rate: Float.round(rate, 1)
      }}
    end)
    |> Enum.into(%{})
  end
end
