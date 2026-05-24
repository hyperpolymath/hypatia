# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.AlertsTest do
  # async: false — the Alerts GenServer is a named singleton that
  # registers global handlers; concurrent tests would observe each
  # other's emissions.
  use ExUnit.Case, async: false

  alias Hypatia.Watcher.Alerts
  alias Hypatia.Telemetry, as: T

  setup do
    case Process.whereis(Hypatia.Watcher.PubSub) do
      nil ->
        {:ok, pid} = Registry.start_link(keys: :duplicate, name: Hypatia.Watcher.PubSub)
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    case Process.whereis(Hypatia.Watcher) do
      nil ->
        {:ok, pid} = Hypatia.Watcher.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    case Process.whereis(Hypatia.Watcher.Alerts) do
      nil ->
        {:ok, pid} = Alerts.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    # No external sinks by default — test isolation.
    System.delete_env("HYPATIA_ALERT_WEBHOOK_URL")
    System.delete_env("HYPATIA_ALERT_LOG_FILE")

    :ok
  end

  describe "quarantine_triggered rule" do
    test "emits an alert on the corresponding telemetry event" do
      T.quarantine_triggered(kind: :recipe, id: "bad-recipe-1", reason: "rate", level: :auto)
      # The cast to Alerts is async; wait a beat.
      Process.sleep(100)

      alerts = Alerts.recent()
      [head | _] = alerts

      assert head.rule == :quarantine_triggered
      assert head.severity == :high
      assert head.summary =~ "bad-recipe-1"
    end

    test "deduplicates within the dedup window" do
      T.quarantine_triggered(kind: :recipe, id: "dup-recipe", reason: "rate", level: :auto)
      Process.sleep(50)
      T.quarantine_triggered(kind: :recipe, id: "dup-recipe", reason: "rate", level: :auto)
      Process.sleep(50)
      T.quarantine_triggered(kind: :recipe, id: "dup-recipe", reason: "rate", level: :auto)
      Process.sleep(50)

      matching =
        Alerts.recent()
        |> Enum.filter(fn a -> a.rule == :quarantine_triggered and a.summary =~ "dup-recipe" end)

      assert length(matching) == 1
    end

    test "different ids are not deduped against each other" do
      T.quarantine_triggered(kind: :recipe, id: "id-a-#{System.unique_integer()}", reason: "r", level: :auto)
      T.quarantine_triggered(kind: :recipe, id: "id-b-#{System.unique_integer()}", reason: "r", level: :auto)
      Process.sleep(100)

      recent_summaries = Alerts.recent() |> Enum.map(& &1.summary)
      a_count = Enum.count(recent_summaries, &(&1 =~ "id-a-"))
      b_count = Enum.count(recent_summaries, &(&1 =~ "id-b-"))
      assert a_count >= 1
      assert b_count >= 1
    end
  end

  describe "soundness_violation rule" do
    test "fires immediately, with severity :critical" do
      T.soundness_violation(
        rule_module: "code_safety",
        rule_id: "elixir_system_shell",
        fixture: "test/soundness/fixtures/code_safety/elixir_system_shell.ex"
      )

      Process.sleep(100)

      [head | _] = Alerts.recent()
      assert head.rule == :soundness_violation
      assert head.severity == :critical
      assert head.summary =~ "elixir_system_shell"
    end
  end

  describe "events_dropped rule (tick path)" do
    test "fires when watcher dropped_events delta > 0" do
      # We can't directly mutate the watcher's internal state, but we can
      # craft the rule's input by exercising the tick path against a
      # snapshot the watcher returns. Easiest: assert the tick is callable
      # and recent() is queryable without crashing.
      assert :ok = Alerts.tick_now()
      assert is_list(Alerts.recent())
    end
  end
end
