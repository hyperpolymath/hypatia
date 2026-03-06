# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Kin.Gate do
  @moduledoc """
  Kin Gate — the second-opinion checkpoint before any bot action reaches production.

  Every PR, commit, or push from the gitbot-fleet must pass through the Gate.
  The Gate enforces:

  - **Repo lock**: only one bot operates on a repo at a time
  - **Staleness check**: rejects actions based on scanner data older than threshold
  - **Conflict detection**: blocks fixes that contradict pending fixes
  - **Confidence floor**: enforces minimum confidence for auto-execute tier
  - **Rate guard**: prevents flood of PRs to a single repo
  - **Neural consensus**: if available, checks neural network agreement on severity

  ## Decision outcomes

  - `:approved` — action proceeds immediately
  - `:held` — action queued for human review (written to held.jsonl)
  - `:rejected` — action blocked with reason (written to rejected.jsonl)
  - `:deferred` — action delayed until a condition clears (e.g. repo lock released)

  ## Usage

      case Hypatia.Kin.Gate.review(action) do
        {:approved, action} -> execute(action)
        {:held, reason} -> log_for_human_review(action, reason)
        {:rejected, reason} -> log_rejection(action, reason)
        {:deferred, wait_ms} -> requeue_after(action, wait_ms)
      end
  """

  use GenServer
  require Logger

  @lock_timeout_ms 30 * 60 * 1_000  # 30 minutes max lock
  @stale_scan_threshold_hours 48
  @min_auto_execute_confidence 0.95
  @max_prs_per_repo_per_hour 3
  @held_file "held.jsonl"
  @rejected_file "rejected.jsonl"

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Submit an action for Gate review. Returns the decision.

  Action map must contain:
  - `bot_id` — which bot is requesting (e.g. "rhodibot")
  - `repo` — target repository name
  - `action_type` — :pr_create, :commit_push, :issue_create, :advisory
  - `confidence` — recipe confidence (0.0-1.0)
  - `pattern_id` — the pattern being fixed (e.g. "PA-CommandInjection-shell_unquoted_var")
  - `scan_timestamp` — when the underlying scan data was generated
  - `dispatch_tier` — :auto_execute, :review, :report_only
  """
  def review(action) do
    GenServer.call(__MODULE__, {:review, action}, 10_000)
  end

  @doc "Release a repo lock after action completes."
  def release_lock(repo) do
    GenServer.cast(__MODULE__, {:release_lock, repo})
  end

  @doc "Get current lock state for all repos."
  def locks do
    GenServer.call(__MODULE__, :locks)
  end

  @doc "Get Gate statistics."
  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  @doc "Get actions currently held for human review."
  def held_actions do
    GenServer.call(__MODULE__, :held_actions)
  end

  @doc "Human approves a held action (by held_id)."
  def approve_held(held_id) do
    GenServer.call(__MODULE__, {:approve_held, held_id})
  end

  @doc "Human rejects a held action (by held_id)."
  def reject_held(held_id, reason) do
    GenServer.call(__MODULE__, {:reject_held, held_id, reason})
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(_opts) do
    state = %{
      repo_locks: %{},         # repo => %{bot_id, locked_at, action_type}
      pr_rate: %{},            # repo => [timestamps of recent PRs]
      held: %{},               # held_id => action
      stats: %{
        approved: 0,
        held: 0,
        rejected: 0,
        deferred: 0,
        conflicts_prevented: 0
      }
    }

    # Periodic cleanup of stale locks
    Process.send_after(self(), :cleanup_locks, 60_000)
    Logger.info("Kin.Gate started — all bot actions require Gate approval.")
    {:ok, state}
  end

  @impl true
  def handle_call({:review, action}, _from, state) do
    {decision, new_state} = evaluate(action, state)

    case decision do
      {:approved, _} ->
        Logger.info("Gate APPROVED: #{action.bot_id} -> #{action.repo} (#{action.action_type})")
      {:held, reason} ->
        Logger.warning("Gate HELD: #{action.bot_id} -> #{action.repo}: #{reason}")
      {:rejected, reason} ->
        Logger.warning("Gate REJECTED: #{action.bot_id} -> #{action.repo}: #{reason}")
      {:deferred, wait_ms} ->
        Logger.info("Gate DEFERRED: #{action.bot_id} -> #{action.repo} (retry in #{div(wait_ms, 1000)}s)")
    end

    {:reply, decision, new_state}
  end

  @impl true
  def handle_call(:locks, _from, state) do
    {:reply, state.repo_locks, state}
  end

  @impl true
  def handle_call(:stats, _from, state) do
    {:reply, state.stats, state}
  end

  @impl true
  def handle_call(:held_actions, _from, state) do
    {:reply, state.held, state}
  end

  @impl true
  def handle_call({:approve_held, held_id}, _from, state) do
    case Map.pop(state.held, held_id) do
      {nil, _} ->
        {:reply, {:error, :not_found}, state}
      {action, new_held} ->
        Logger.info("Gate: human approved held action #{held_id} for #{action.repo}")
        {:reply, {:approved, action}, %{state | held: new_held}}
    end
  end

  @impl true
  def handle_call({:reject_held, held_id, reason}, _from, state) do
    case Map.pop(state.held, held_id) do
      {nil, _} ->
        {:reply, {:error, :not_found}, state}
      {action, new_held} ->
        Logger.info("Gate: human rejected held action #{held_id}: #{reason}")
        write_to_log(@rejected_file, action, reason)
        {:reply, :ok, %{state | held: new_held}}
    end
  end

  @impl true
  def handle_cast({:release_lock, repo}, state) do
    new_locks = Map.delete(state.repo_locks, repo)
    if Map.has_key?(state.repo_locks, repo) do
      Logger.debug("Gate: released lock on #{repo}")
    end
    {:noreply, %{state | repo_locks: new_locks}}
  end

  @impl true
  def handle_info(:cleanup_locks, state) do
    now = System.monotonic_time(:millisecond)

    expired =
      state.repo_locks
      |> Enum.filter(fn {_repo, lock} ->
        now - lock.locked_at > @lock_timeout_ms
      end)

    new_locks =
      Enum.reduce(expired, state.repo_locks, fn {repo, lock}, locks ->
        Logger.warning("Gate: expired stale lock on #{repo} (held by #{lock.bot_id} for >30min)")
        Map.delete(locks, repo)
      end)

    Process.send_after(self(), :cleanup_locks, 60_000)
    {:noreply, %{state | repo_locks: new_locks}}
  end

  # --- Evaluation Pipeline ---

  defp evaluate(action, state) do
    checks = [
      &check_repo_lock/2,
      &check_scan_staleness/2,
      &check_confidence_floor/2,
      &check_pr_rate/2,
      &check_conflict/2,
      &check_neural_consensus/2
    ]

    result =
      Enum.reduce_while(checks, {:approved, action}, fn check_fn, _acc ->
        case check_fn.(action, state) do
          :pass -> {:cont, {:approved, action}}
          {:reject, reason} -> {:halt, {:rejected, reason}}
          {:hold, reason} -> {:halt, {:held, reason}}
          {:defer, wait_ms} -> {:halt, {:deferred, wait_ms}}
        end
      end)

    case result do
      {:approved, action} ->
        new_state = state
        |> acquire_lock(action)
        |> record_pr(action)
        |> update_stats(:approved)
        {{:approved, action}, new_state}

      {:held, reason} ->
        held_id = generate_held_id()
        new_held = Map.put(state.held, held_id, action)
        write_to_log(@held_file, action, reason)
        new_state = %{state | held: new_held} |> update_stats(:held)
        {{:held, reason}, new_state}

      {:rejected, reason} ->
        write_to_log(@rejected_file, action, reason)
        new_state = update_stats(state, :rejected)
        {{:rejected, reason}, new_state}

      {:deferred, wait_ms} ->
        new_state = update_stats(state, :deferred)
        {{:deferred, wait_ms}, new_state}
    end
  end

  # Check 1: Is another bot already working on this repo?
  defp check_repo_lock(action, state) do
    case Map.get(state.repo_locks, action.repo) do
      nil -> :pass
      %{bot_id: same_bot} when same_bot == action.bot_id -> :pass  # same bot can re-enter
      %{bot_id: _other_bot} ->
        {:defer, 60_000}  # wait 1 minute, another bot has the lock
    end
  end

  # Check 2: Is the scan data too old?
  defp check_scan_staleness(action, _state) do
    case Map.get(action, :scan_timestamp) do
      nil -> :pass  # no timestamp = trust the caller
      ts when is_binary(ts) ->
        case DateTime.from_iso8601(ts) do
          {:ok, dt, _} ->
            age_hours = DateTime.diff(DateTime.utc_now(), dt, :hour)
            if age_hours > @stale_scan_threshold_hours do
              {:hold, "scan data is #{age_hours}h old (threshold: #{@stale_scan_threshold_hours}h) — rescan recommended"}
            else
              :pass
            end
          _ -> :pass
        end
      _ -> :pass
    end
  end

  # Check 3: Does confidence meet the floor for auto-execute?
  defp check_confidence_floor(action, _state) do
    if action.dispatch_tier == :auto_execute do
      confidence = Map.get(action, :confidence, 0.0)
      if confidence >= @min_auto_execute_confidence do
        :pass
      else
        {:hold, "confidence #{confidence} below auto-execute threshold #{@min_auto_execute_confidence}"}
      end
    else
      :pass
    end
  end

  # Check 4: Too many PRs to this repo recently?
  defp check_pr_rate(action, state) do
    if action.action_type in [:pr_create, :commit_push] do
      now = System.monotonic_time(:millisecond)
      hour_ago = now - 3_600_000

      recent =
        state.pr_rate
        |> Map.get(action.repo, [])
        |> Enum.filter(&(&1 > hour_ago))

      if length(recent) >= @max_prs_per_repo_per_hour do
        {:hold, "#{length(recent)} PRs to #{action.repo} in last hour (max: #{@max_prs_per_repo_per_hour})"}
      else
        :pass
      end
    else
      :pass
    end
  end

  # Check 5: Does this fix conflict with a pending fix?
  defp check_conflict(action, state) do
    conflicting =
      state.held
      |> Map.values()
      |> Enum.find(fn held ->
        held.repo == action.repo and held.pattern_id == action.pattern_id and held.bot_id != action.bot_id
      end)

    case conflicting do
      nil -> :pass
      held ->
        {:reject, "conflicts with held action from #{held.bot_id} on same pattern #{action.pattern_id}"}
    end
  end

  # Check 6: If neural networks are available, check consensus
  defp check_neural_consensus(action, _state) do
    if action.dispatch_tier == :auto_execute do
      case check_neural_available() do
        false -> :pass  # neural layer not available, don't block
        true ->
          # Ask the neural coordinator for a confidence assessment
          case neural_agrees?(action) do
            true -> :pass
            false -> {:hold, "neural network disagrees with dispatch confidence — human review recommended"}
          end
      end
    else
      :pass
    end
  end

  defp check_neural_available do
    case Process.whereis(Hypatia.Neural.Coordinator) do
      nil -> false
      pid -> Process.alive?(pid)
    end
  end

  defp neural_agrees?(action) do
    try do
      finding = %{"id" => action.pattern_id, "category" => "unknown"}
      case Hypatia.Neural.Coordinator.dispatch_recommendation(finding) do
        %{confidence: neural_conf} ->
          # Neural agrees if its confidence is within 0.15 of the recipe confidence
          abs(neural_conf - Map.get(action, :confidence, 0.0)) < 0.15
        _ -> true  # if prediction fails, don't block
      end
    rescue
      _ -> true
    catch
      :exit, _ -> true
    end
  end

  # --- Helpers ---

  defp acquire_lock(state, action) do
    if action.action_type in [:pr_create, :commit_push] do
      lock = %{
        bot_id: action.bot_id,
        locked_at: System.monotonic_time(:millisecond),
        action_type: action.action_type
      }
      %{state | repo_locks: Map.put(state.repo_locks, action.repo, lock)}
    else
      state
    end
  end

  defp record_pr(state, action) do
    if action.action_type in [:pr_create, :commit_push] do
      now = System.monotonic_time(:millisecond)
      existing = Map.get(state.pr_rate, action.repo, [])
      %{state | pr_rate: Map.put(state.pr_rate, action.repo, [now | existing])}
    else
      state
    end
  end

  defp update_stats(state, decision) do
    new_stats = Map.update!(state.stats, decision, &(&1 + 1))
    %{state | stats: new_stats}
  end

  defp generate_held_id do
    :crypto.strong_rand_bytes(8) |> Base.hex_encode32(case: :lower, padding: false)
  end

  defp write_to_log(filename, action, reason) do
    dispatch_dir = Hypatia.Paths.dispatch()
    File.mkdir_p!(dispatch_dir)
    path = Path.join(dispatch_dir, filename)

    entry = %{
      "timestamp" => DateTime.to_iso8601(DateTime.utc_now()),
      "bot_id" => action.bot_id,
      "repo" => action.repo,
      "action_type" => to_string(action.action_type),
      "pattern_id" => action.pattern_id,
      "reason" => reason
    }

    case Jason.encode(entry) do
      {:ok, json} ->
        File.write!(path, json <> "\n", [:append])
      _ -> :ok
    end
  end
end
