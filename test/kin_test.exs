# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.KinTest do
  use ExUnit.Case, async: false

  alias Hypatia.Kin.{Protocol, Gate, Arbiter, Contingency}

  # ===================================================================
  # Protocol
  # ===================================================================

  describe "Protocol" do
    test "kin_dir returns expanded path" do
      dir = Protocol.kin_dir()
      assert String.contains?(dir, ".hypatia/kin")
    end

    test "heartbeat_path returns file path for kin_id" do
      path = Protocol.heartbeat_path("panic-attacker")
      assert String.ends_with?(path, "panic-attacker.heartbeat.json")
    end

    test "stale?/1 returns true for nil timestamp" do
      assert Protocol.stale?(%{})
    end

    test "stale?/1 returns false for recent timestamp" do
      ts = DateTime.utc_now() |> DateTime.to_iso8601()
      refute Protocol.stale?(%{"timestamp" => ts})
    end

    test "stale?/1 returns true for old timestamp" do
      old = DateTime.utc_now() |> DateTime.add(-48 * 3600, :second) |> DateTime.to_iso8601()
      assert Protocol.stale?(%{"timestamp" => old})
    end

    test "heartbeat_age/1 returns human-readable age" do
      recent = DateTime.utc_now() |> DateTime.to_iso8601()
      assert String.ends_with?(Protocol.heartbeat_age(%{"timestamp" => recent}), "s ago")

      assert Protocol.heartbeat_age(%{}) == "never"
    end

    test "known_kin has 4 entries" do
      kin = Protocol.known_kin()
      assert map_size(kin) == 4
      assert Map.has_key?(kin, "hypatia")
      assert Map.has_key?(kin, "panic-attacker")
      assert Map.has_key?(kin, "gitbot-fleet")
      assert Map.has_key?(kin, "auto-fix")
    end
  end

  # ===================================================================
  # Gate
  # ===================================================================

  describe "Gate" do
    setup do
      # Gate runs as a named GenServer, started by the application
      # Verify it's running
      assert Process.whereis(Gate) != nil
      :ok
    end

    test "stats/0 returns initial stats" do
      stats = Gate.stats()
      assert is_map(stats)
      assert Map.has_key?(stats, :approved)
    end

    test "locks/0 returns empty map initially" do
      locks = Gate.locks()
      assert is_map(locks)
    end

    test "review/1 approves a well-formed action" do
      action = %{
        bot_id: "rhodibot",
        repo: "test-repo",
        action_type: :advisory,
        confidence: 0.99,
        pattern_id: "PA-test-001",
        scan_timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
        dispatch_tier: :report_only
      }

      result = Gate.review(action)
      assert match?({:approved, _}, result)
    end

    test "review/1 holds auto_execute with low confidence" do
      action = %{
        bot_id: "robot-repo-automaton",
        repo: "test-repo-2",
        action_type: :commit_push,
        confidence: 0.80,
        pattern_id: "PA-test-002",
        scan_timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
        dispatch_tier: :auto_execute
      }

      result = Gate.review(action)
      assert match?({:held, _reason}, result)
    end

    test "review/1 holds action with stale scan data" do
      old_ts = DateTime.utc_now() |> DateTime.add(-72 * 3600, :second) |> DateTime.to_iso8601()

      action = %{
        bot_id: "rhodibot",
        repo: "stale-repo",
        action_type: :pr_create,
        confidence: 0.99,
        pattern_id: "PA-test-003",
        scan_timestamp: old_ts,
        dispatch_tier: :review
      }

      result = Gate.review(action)
      assert match?({:held, _reason}, result)
    end
  end

  # ===================================================================
  # Arbiter
  # ===================================================================

  describe "Arbiter" do
    test "arbitrate/2 returns compatible for different repos" do
      a = %{bot_id: "rhodibot", repo: "repo-a", pattern_id: "PA-001", confidence: 0.95}
      b = %{bot_id: "panicbot", repo: "repo-b", pattern_id: "PA-002", confidence: 0.90}

      result = Arbiter.arbitrate(a, b)
      assert match?({:compatible, _, _}, result)
    end

    test "arbitrate/2 resolves conflict by bot priority" do
      a = %{bot_id: "echidnabot", repo: "same-repo", pattern_id: "PA-001", confidence: 0.90}
      b = %{bot_id: "finishingbot", repo: "same-repo", pattern_id: "PA-001", confidence: 0.95}

      result = Arbiter.arbitrate(a, b)
      assert match?({:winner, _, _, _}, result)
      {:winner, winner, _loser, _reason} = result
      assert winner.bot_id == "echidnabot"
    end

    test "active_actions/0 returns map" do
      actions = Arbiter.active_actions()
      assert is_map(actions)
    end

    test "detect_synergies/0 returns list" do
      synergies = Arbiter.detect_synergies()
      assert is_list(synergies)
    end
  end

  # ===================================================================
  # Contingency
  # ===================================================================

  describe "Contingency" do
    test "level/0 returns :normal by default" do
      level = Contingency.level()
      assert level == :normal
    end

    test "action_permitted?/1 allows all tiers at :normal" do
      assert Contingency.action_permitted?(:auto_execute)
      assert Contingency.action_permitted?(:review)
      assert Contingency.action_permitted?(:report_only)
    end

    test "set_level/2 changes emergency level" do
      Contingency.set_level(:caution, "test")
      assert Contingency.level() == :caution
      refute Contingency.action_permitted?(:auto_execute)
      assert Contingency.action_permitted?(:review)

      # Reset to normal
      Contingency.set_level(:normal, "test complete")
      assert Contingency.level() == :normal
    end

    test "isolate_bot/2 and restore_bot/1" do
      Contingency.isolate_bot("test-bot", "misbehaving")
      assert Contingency.bot_isolated?("test-bot")

      Contingency.restore_bot("test-bot")
      refute Contingency.bot_isolated?("test-bot")
    end

    test "freeze blocks all actions" do
      Contingency.set_level(:freeze, "test freeze")
      refute Contingency.action_permitted?(:auto_execute)
      refute Contingency.action_permitted?(:review)
      refute Contingency.action_permitted?(:report_only)

      Contingency.set_level(:normal, "unfreeze")
    end
  end
end
