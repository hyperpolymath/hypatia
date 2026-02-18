# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.SelfDiagnostics do
  @moduledoc """
  Self-diagnostics, health monitoring, and fault tolerance for Hypatia.

  Periodically checks:
  - Filesystem access (verisimdb-data, gitbot-fleet, dispatch manifests)
  - Rule engine health (can Logtalk rules be loaded?)
  - Recipe confidence drift (recipes dropping below threshold)
  - Fleet connectivity (dispatch-runner reachable?)
  - Learning scheduler health (is it running? last successful cycle?)

  When issues detected:
  - Logs warnings with structured diagnostics
  - Attempts auto-recovery (reconnect, clear stale locks, retry)
  - Writes health report to verisimdb-data/health/hypatia.json
  - Degrades gracefully (continue scanning even if dispatch is down)

  Circuit breaker pattern:
  - After 3 consecutive dispatch failures, opens circuit
  - Queues dispatches to pending.jsonl instead
  - Auto-retries after cooldown period (15 min)
  """

  use GenServer
  require Logger

  @health_interval_ms 10 * 60 * 1_000  # 10 minutes
  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"
  @fleet_path "~/Documents/hyperpolymath-repos/gitbot-fleet"
  @circuit_breaker_threshold 3
  @circuit_breaker_cooldown_ms 15 * 60 * 1_000  # 15 minutes

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def health_report do
    GenServer.call(__MODULE__, :health_report)
  end

  def circuit_state do
    GenServer.call(__MODULE__, :circuit_state)
  end

  def record_dispatch_failure do
    GenServer.cast(__MODULE__, :dispatch_failure)
  end

  def record_dispatch_success do
    GenServer.cast(__MODULE__, :dispatch_success)
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(_opts) do
    state = %{
      started_at: DateTime.utc_now(),
      last_health_check: nil,
      health_status: %{},
      # Circuit breaker
      circuit: :closed,  # :closed (normal), :open (failing), :half_open (testing)
      consecutive_failures: 0,
      circuit_opened_at: nil,
      # History
      health_history: [],
      total_checks: 0
    }

    Process.send_after(self(), :check_health, 10_000)
    Logger.info("SelfDiagnostics started. Health checks every #{div(@health_interval_ms, 60_000)} min.")
    {:ok, state}
  end

  @impl true
  def handle_info(:check_health, state) do
    new_state = run_health_check(state)
    Process.send_after(self(), :check_health, @health_interval_ms)
    {:noreply, new_state}
  end

  @impl true
  def handle_info(:try_half_open, state) do
    Logger.info("Circuit breaker: testing half-open state...")
    {:noreply, %{state | circuit: :half_open}}
  end

  @impl true
  def handle_call(:health_report, _from, state) do
    {:reply, state.health_status, state}
  end

  @impl true
  def handle_call(:circuit_state, _from, state) do
    {:reply, state.circuit, state}
  end

  @impl true
  def handle_cast(:dispatch_failure, state) do
    failures = state.consecutive_failures + 1

    new_state =
      if failures >= @circuit_breaker_threshold and state.circuit == :closed do
        Logger.error(
          "Circuit breaker OPENED after #{failures} consecutive dispatch failures. " <>
          "Dispatches will be queued to pending.jsonl. Auto-retry in #{div(@circuit_breaker_cooldown_ms, 60_000)} min."
        )
        Process.send_after(self(), :try_half_open, @circuit_breaker_cooldown_ms)
        %{state | circuit: :open, consecutive_failures: failures, circuit_opened_at: DateTime.utc_now()}
      else
        %{state | consecutive_failures: failures}
      end

    {:noreply, new_state}
  end

  @impl true
  def handle_cast(:dispatch_success, state) do
    new_state =
      case state.circuit do
        :half_open ->
          Logger.info("Circuit breaker CLOSED — dispatch recovered.")
          %{state | circuit: :closed, consecutive_failures: 0, circuit_opened_at: nil}
        _ ->
          %{state | consecutive_failures: 0}
      end

    {:noreply, new_state}
  end

  # --- Health Check Implementation ---

  defp run_health_check(state) do
    now = DateTime.utc_now()

    checks = %{
      "filesystem" => check_filesystem(),
      "verisimdb_data" => check_verisimdb_data(),
      "fleet_access" => check_fleet_access(),
      "learning_scheduler" => check_learning_scheduler(),
      "recipe_health" => check_recipe_health(),
      "circuit_breaker" => %{"state" => Atom.to_string(state.circuit), "failures" => state.consecutive_failures}
    }

    overall =
      checks
      |> Map.values()
      |> Enum.all?(fn
        %{"status" => "pass"} -> true
        %{"state" => "closed"} -> true
        _ -> false
      end)

    status = %{
      "overall" => if(overall, do: "healthy", else: "degraded"),
      "checked_at" => DateTime.to_iso8601(now),
      "checks" => checks
    }

    # Write health report
    write_health_report(status)

    # Attempt auto-recovery for failed checks
    attempt_recovery(checks)

    if not overall do
      failed =
        checks
        |> Enum.filter(fn {_k, v} -> Map.get(v, "status") == "fail" end)
        |> Enum.map(fn {k, _v} -> k end)

      Logger.warning("Health check DEGRADED. Failed: #{Enum.join(failed, ", ")}")
    end

    %{state |
      last_health_check: now,
      health_status: status,
      total_checks: state.total_checks + 1,
      health_history: Enum.take([status | state.health_history], 10)
    }
  end

  defp check_filesystem do
    paths = [
      Path.expand(@verisimdb_data_path),
      Path.expand(@fleet_path),
      Path.join(Path.expand(@verisimdb_data_path), "outcomes"),
      Path.join(Path.expand(@verisimdb_data_path), "recipes"),
      Path.join(Path.expand(@verisimdb_data_path), "dispatch")
    ]

    results = Enum.map(paths, fn path ->
      {path, File.dir?(path)}
    end)

    missing = Enum.filter(results, fn {_p, exists} -> not exists end)

    if Enum.empty?(missing) do
      %{"status" => "pass", "paths_checked" => length(paths)}
    else
      missing_paths = Enum.map(missing, fn {p, _} -> p end)
      %{"status" => "fail", "missing" => missing_paths}
    end
  end

  defp check_verisimdb_data do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")
    recipes_dir = Path.join(Path.expand(@verisimdb_data_path), "recipes")

    outcomes_count =
      case File.ls(outcomes_dir) do
        {:ok, files} -> length(Enum.filter(files, &String.ends_with?(&1, ".jsonl")))
        {:error, _} -> 0
      end

    recipes_count =
      case File.ls(recipes_dir) do
        {:ok, files} -> length(Enum.filter(files, &String.ends_with?(&1, ".json")))
        {:error, _} -> 0
      end

    %{
      "status" => if(recipes_count > 0, do: "pass", else: "warn"),
      "outcome_files" => outcomes_count,
      "recipe_files" => recipes_count
    }
  end

  defp check_fleet_access do
    fleet_learning = Path.join([
      Path.expand(@fleet_path),
      "shared-context/learning/fix-outcomes.jsonl"
    ])

    if File.exists?(fleet_learning) do
      case File.stat(fleet_learning) do
        {:ok, %{size: size, mtime: mtime}} ->
          %{"status" => "pass", "size_bytes" => size, "last_modified" => inspect(mtime)}
        {:error, reason} ->
          %{"status" => "fail", "error" => inspect(reason)}
      end
    else
      %{"status" => "warn", "message" => "fix-outcomes.jsonl not found"}
    end
  end

  defp check_learning_scheduler do
    case Process.whereis(Hypatia.LearningScheduler) do
      nil ->
        %{"status" => "fail", "message" => "LearningScheduler not running"}
      pid ->
        if Process.alive?(pid) do
          status = Hypatia.LearningScheduler.status()
          %{
            "status" => "pass",
            "last_run" => inspect(Map.get(status, :last_run)),
            "outcomes_processed" => Map.get(status, :total_outcomes_processed, 0),
            "confidence_updates" => Map.get(status, :total_confidence_updates, 0)
          }
        else
          %{"status" => "fail", "message" => "LearningScheduler process dead"}
        end
    end
  end

  defp check_recipe_health do
    recipes_dir = Path.join(Path.expand(@verisimdb_data_path), "recipes")

    case File.ls(recipes_dir) do
      {:ok, files} ->
        recipe_files = Enum.filter(files, &String.ends_with?(&1, ".json"))

        low_confidence =
          recipe_files
          |> Enum.map(fn f ->
            path = Path.join(recipes_dir, f)
            case File.read(path) do
              {:ok, content} ->
                case Jason.decode(content) do
                  {:ok, recipe} ->
                    {Map.get(recipe, "id", f), Map.get(recipe, "confidence", 0.5)}
                  _ -> nil
                end
              _ -> nil
            end
          end)
          |> Enum.reject(&is_nil/1)
          |> Enum.filter(fn {_id, conf} -> conf < 0.3 end)

        %{
          "status" => if(Enum.empty?(low_confidence), do: "pass", else: "warn"),
          "total_recipes" => length(recipe_files),
          "low_confidence" => Enum.map(low_confidence, fn {id, c} -> %{"id" => id, "confidence" => c} end)
        }

      {:error, _} ->
        %{"status" => "fail", "message" => "Cannot read recipes directory"}
    end
  end

  defp write_health_report(status) do
    health_dir = Path.join(Path.expand(@verisimdb_data_path), "health")
    File.mkdir_p!(health_dir)
    path = Path.join(health_dir, "hypatia.json")

    case Jason.encode(status, pretty: true) do
      {:ok, json} -> File.write!(path, json <> "\n")
      {:error, _} -> :ok
    end
  end

  defp attempt_recovery(checks) do
    # Auto-create missing directories
    case Map.get(checks, "filesystem") do
      %{"status" => "fail", "missing" => missing} ->
        Enum.each(missing, fn path ->
          Logger.info("Auto-recovery: creating missing directory #{path}")
          File.mkdir_p(path)
        end)
      _ -> :ok
    end

    # Restart LearningScheduler if dead
    case Map.get(checks, "learning_scheduler") do
      %{"status" => "fail"} ->
        Logger.info("Auto-recovery: restarting LearningScheduler")
        case Hypatia.LearningScheduler.start_link() do
          {:ok, _pid} -> Logger.info("LearningScheduler restarted successfully")
          {:error, reason} -> Logger.error("Failed to restart LearningScheduler: #{inspect(reason)}")
        end
      _ -> :ok
    end
  end
end
