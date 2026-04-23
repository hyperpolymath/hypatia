# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.ReflexiveTest do
  @moduledoc """
  Reflexive tests: verify Hypatia can introspect and report its own state.

  Covers:
  - `Hypatia.SelfDiagnostics` health reports and circuit-breaker transitions
  - `Hypatia.VCL.Client.stats/0` (cache + query-count introspection)
  - `Hypatia.Safety.RateLimiter.stats/0` (dispatch + queue counters)
  - `Hypatia.Safety.Quarantine.list_quarantined/0` (roster introspection)
  - Justfile `doctor` recipe presence and structure

  Closes TESTING-TAXONOMY § 1 REF (Reflexive) — previously "weak" with
  evidence in test/safety_test.exs + test/blackboard_test.exs only and
  an explicit gap for `just doctor not tested`.

  Run in isolation: mix test --only reflexive
  """
  use ExUnit.Case, async: false
  @moduletag :reflexive

  alias Hypatia.SelfDiagnostics
  alias Hypatia.VCL.Client, as: VCLClient
  alias Hypatia.Safety.{RateLimiter, Quarantine}

  # ---------------------------------------------------------------------------
  # SelfDiagnostics — the primary reflexive surface
  # ---------------------------------------------------------------------------

  describe "SelfDiagnostics — health report introspection" do
    setup do
      start_supervised!(SelfDiagnostics)
      :ok
    end

    test "health_report/0 returns a map" do
      assert is_map(SelfDiagnostics.health_report())
    end

    test "circuit_state/0 starts at :closed" do
      assert SelfDiagnostics.circuit_state() == :closed
    end
  end

  describe "SelfDiagnostics — circuit breaker contract" do
    setup do
      start_supervised!(SelfDiagnostics)
      :ok
    end

    test "3 consecutive dispatch failures open the circuit" do
      assert SelfDiagnostics.circuit_state() == :closed

      SelfDiagnostics.record_dispatch_failure()
      SelfDiagnostics.record_dispatch_failure()
      SelfDiagnostics.record_dispatch_failure()
      :timer.sleep(50)

      assert SelfDiagnostics.circuit_state() == :open
    end

    test "2 dispatch failures do not open the circuit" do
      SelfDiagnostics.record_dispatch_failure()
      SelfDiagnostics.record_dispatch_failure()
      :timer.sleep(50)

      assert SelfDiagnostics.circuit_state() == :closed
    end

    test "dispatch_success resets consecutive failures while closed" do
      SelfDiagnostics.record_dispatch_failure()
      SelfDiagnostics.record_dispatch_failure()
      SelfDiagnostics.record_dispatch_success()
      :timer.sleep(50)

      # Two more failures should still not open the circuit — counter was reset
      SelfDiagnostics.record_dispatch_failure()
      SelfDiagnostics.record_dispatch_failure()
      :timer.sleep(50)

      assert SelfDiagnostics.circuit_state() == :closed
    end

    test "half_open → closed on dispatch_success (recovery path)" do
      # Open the circuit
      for _ <- 1..3, do: SelfDiagnostics.record_dispatch_failure()
      :timer.sleep(50)
      assert SelfDiagnostics.circuit_state() == :open

      # Simulate the cooldown timer firing early by sending the internal msg
      send(Process.whereis(SelfDiagnostics), :try_half_open)
      :timer.sleep(30)
      assert SelfDiagnostics.circuit_state() == :half_open

      # A dispatch success in half_open closes the circuit
      SelfDiagnostics.record_dispatch_success()
      :timer.sleep(30)
      assert SelfDiagnostics.circuit_state() == :closed
    end
  end

  # ---------------------------------------------------------------------------
  # VCL.Client — stats introspection
  # ---------------------------------------------------------------------------

  describe "VCL.Client — stats introspection" do
    setup do
      start_supervised!(VCLClient)
      :ok
    end

    test "stats/0 returns query_count, cache_size, cache_ttl_ms" do
      stats = VCLClient.stats()

      assert Map.has_key?(stats, :query_count)
      assert Map.has_key?(stats, :cache_size)
      assert Map.has_key?(stats, :cache_ttl_ms)
      assert is_integer(stats.query_count) and stats.query_count >= 0
      assert is_integer(stats.cache_size) and stats.cache_size >= 0
      assert is_integer(stats.cache_ttl_ms) and stats.cache_ttl_ms > 0
    end
  end

  # ---------------------------------------------------------------------------
  # RateLimiter — stats introspection
  # ---------------------------------------------------------------------------

  describe "RateLimiter — stats introspection" do
    setup do
      start_supervised!(RateLimiter)
      :ok
    end

    test "stats/0 exposes expected counter keys" do
      stats = RateLimiter.stats()

      for key <- [:total_dispatched, :total_queued, :total_rate_limited,
                  :queue_size, :active_bots, :global_window_size] do
        assert Map.has_key?(stats, key), "stats/0 missing :#{key}"
      end
    end

    test "total_dispatched increments after record_dispatch/1" do
      before = RateLimiter.stats().total_dispatched

      RateLimiter.record_dispatch("reflex-bot-#{unique_id()}")
      :timer.sleep(30)

      after_ = RateLimiter.stats().total_dispatched
      assert after_ == before + 1
    end
  end

  # ---------------------------------------------------------------------------
  # Quarantine — roster introspection
  # ---------------------------------------------------------------------------

  describe "Quarantine — roster introspection" do
    setup do
      start_supervised!(Quarantine)
      :ok
    end

    test "list_quarantined/0 starts empty" do
      assert Quarantine.list_quarantined() == %{}
    end

    test "list_quarantined/0 includes manually quarantined bots" do
      bot = "reflex-q-#{unique_id()}"
      Quarantine.quarantine(bot, :soft, "reflexive test")
      :timer.sleep(30)

      roster = Quarantine.list_quarantined()
      assert Map.has_key?(roster, bot)
      assert roster[bot].level == :soft
      assert roster[bot].reason == "reflexive test"
    end
  end

  # ---------------------------------------------------------------------------
  # Justfile `doctor` recipe — structural test
  # ---------------------------------------------------------------------------

  describe "Justfile — doctor recipe" do
    setup do
      justfile = Path.join(File.cwd!(), "Justfile")
      {:ok, content: File.read!(justfile)}
    end

    test "doctor recipe exists", %{content: content} do
      assert content =~ ~r/^doctor:/m
    end

    test "doctor checks required tools (elixir, cargo, just)", %{content: content} do
      assert content =~ ~r/check\s+"Elixir"/
      assert content =~ ~r/check\s+"Rust\/cargo"/
      assert content =~ ~r/check\s+"just"/
    end

    test "doctor checks optional FFI/ABI tools (Idris2, Zig)", %{content: content} do
      assert content =~ ~r/check_optional\s+"Idris2"/
      assert content =~ ~r/check_optional\s+"Zig"/
    end

    test "doctor exits non-zero on failure", %{content: content} do
      assert content =~ ~r/exit 1/
    end

    test "heal recipe follows doctor (install instructions)", %{content: content} do
      assert content =~ ~r/^heal:/m
    end
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp unique_id, do: :erlang.unique_integer([:positive]) |> to_string()
end
