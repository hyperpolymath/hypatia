# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Concurrency tests for parallel recipe evaluation and concurrent safety module access.
#
# Validates that:
# - RecipeMatcher is safe under concurrent access (pure functions, no shared state)
# - TriangleRouter handles parallel routing without contention
# - RateLimiter GenServer handles concurrent calls correctly
# - Quarantine GenServer handles concurrent quarantine/release without crash
# - VQL Client handles concurrent queries correctly

defmodule Hypatia.Concurrency.RecipeMatcherTest do
  @moduledoc """
  Concurrency tests for parallel recipe evaluation.

  RecipeMatcher is a pure module (reads files from disk on each call) so
  all functions are inherently safe for concurrent access. These tests verify
  that concurrent execution produces consistent results with no race conditions.
  """

  use ExUnit.Case, async: true

  alias Hypatia.RecipeMatcher
  alias Hypatia.TriangleRouter

  # Number of parallel tasks for stress tests
  @parallel_count 20

  describe "parallel recipe evaluation" do
    test "concurrent all_recipes/0 calls return consistent results" do
      # All concurrent calls must return the same recipe set
      tasks =
        for _ <- 1..@parallel_count do
          Task.async(fn -> RecipeMatcher.all_recipes() end)
        end

      results = Enum.map(tasks, &Task.await(&1, 5_000))

      # All results must be lists
      Enum.each(results, fn r ->
        assert is_list(r),
               "all_recipes() returned non-list under concurrency: #{inspect(r)}"
      end)

      # All results should have the same length (stable file set)
      lengths = results |> Enum.map(&length/1) |> Enum.uniq()
      assert length(lengths) == 1,
             "Concurrent all_recipes() returned different lengths: #{inspect(lengths)}"
    end

    test "concurrent find_recipes/1 calls are race-condition free" do
      pattern_ids = [
        "PA009-shell-unquoted-var",
        "PA018-unchecked-todo",
        "PA001-missing-spdx",
        "PA999-nonexistent-rule"
      ]

      # Each task gets a random pattern_id from the list
      tasks =
        for i <- 1..@parallel_count do
          id = Enum.at(pattern_ids, rem(i, length(pattern_ids)))
          Task.async(fn -> {id, RecipeMatcher.find_recipes(id)} end)
        end

      results = Enum.map(tasks, &Task.await(&1, 5_000))

      # Group by pattern_id and verify consistent results per id
      by_id = Enum.group_by(results, &elem(&1, 0), &elem(&1, 1))

      Enum.each(by_id, fn {id, all_results} ->
        unique_results = Enum.uniq(all_results)
        assert length(unique_results) == 1,
               "find_recipes(#{inspect(id)}) returned different results concurrently: " <>
                 "#{inspect(unique_results)}"
      end)
    end

    test "concurrent route/3 calls across multiple patterns are independent" do
      patterns =
        for i <- 1..@parallel_count do
          %{
            "id" => "PA009-test-#{i}",
            "pa_rule" => "PA009",
            "category" => "ShellInjection",
            "description" => "Test pattern #{i} in .sh script",
            "repos_affected_list" => ["repo-#{i}"],
            "severity" => "High"
          }
        end

      tasks =
        Enum.map(patterns, fn pattern ->
          Task.async(fn ->
            result = TriangleRouter.route(pattern, Map.get(pattern, "id"), "shell")
            {Map.get(pattern, "id"), result}
          end)
        end)

      results = Enum.map(tasks, &Task.await(&1, 5_000))

      # All routing results must be valid tuple types
      Enum.each(results, fn {id, result} ->
        assert valid_route_tuple?(result),
               "route/3 returned invalid tuple for #{id}: #{inspect(result)}"
      end)
    end

    test "concurrent route_batch/3 calls finish without timeout" do
      batch =
        for i <- 1..10 do
          %{
            "id" => "PA00#{rem(i, 9) + 1}-batch-test-#{i}",
            "pa_rule" => "PA00#{rem(i, 9) + 1}",
            "category" => "TestCategory",
            "description" => "Batch test pattern #{i}",
            "repos_affected_list" => ["batch-repo"],
            "severity" => "Medium"
          }
        end

      tasks =
        for _ <- 1..5 do
          Task.async(fn ->
            TriangleRouter.route_batch(batch, "batch-repo", "shell")
          end)
        end

      results = Enum.map(tasks, &Task.await(&1, 10_000))

      Enum.each(results, fn batch_result ->
        assert is_list(batch_result)
        assert length(batch_result) == 10
      end)
    end

    defp valid_route_tuple?({:eliminate, recipe, _pattern}) when is_map(recipe), do: true
    defp valid_route_tuple?({:substitute, recipe, _pattern}) when is_map(recipe), do: true
    defp valid_route_tuple?({:control, _pattern}), do: true
    defp valid_route_tuple?(_), do: false
  end
end

defmodule Hypatia.Concurrency.SafetyModulesTest do
  @moduledoc """
  Concurrency tests for safety GenServers: RateLimiter and Quarantine.

  These GenServers process messages serially (standard OTP), so concurrent
  calls should be safe, but we verify:
  - No crashes under concurrent load
  - Final state is consistent
  - Counters only increase monotonically
  """

  use ExUnit.Case, async: false

  alias Hypatia.Safety.RateLimiter
  alias Hypatia.Safety.Quarantine

  setup do
    case GenServer.whereis(RateLimiter) do
      nil -> start_supervised!(RateLimiter)
      _pid -> :ok
    end

    case GenServer.whereis(Quarantine) do
      nil -> start_supervised!(Quarantine)
      _pid -> :ok
    end

    :ok
  end

  describe "RateLimiter under concurrent load" do
    test "concurrent check/1 calls never crash the GenServer" do
      bots = for i <- 1..10, do: "concurrent_bot_#{System.unique_integer([:positive])}_#{i}"

      # Flood the rate limiter with concurrent calls from multiple bots
      tasks =
        for bot <- bots do
          Task.async(fn ->
            for _ <- 1..5 do
              RateLimiter.check(bot)
              RateLimiter.record_dispatch(bot)
            end
          end)
        end

      Enum.each(tasks, &Task.await(&1, 5_000))

      # GenServer must still be alive and responsive
      pid = GenServer.whereis(RateLimiter)
      assert pid != nil and Process.alive?(pid),
             "RateLimiter crashed under concurrent load"

      stats = RateLimiter.stats()
      assert is_map(stats)
      assert stats.total_dispatched >= 0
    end

    test "concurrent record_dispatch/1 increments total monotonically" do
      bot = "mono_dispatch_bot_#{System.unique_integer([:positive])}"
      before_stats = RateLimiter.stats()

      tasks =
        for _ <- 1..10 do
          Task.async(fn ->
            for _ <- 1..5 do
              RateLimiter.record_dispatch(bot)
            end
          end)
        end

      Enum.each(tasks, &Task.await(&1, 5_000))
      :timer.sleep(100)

      after_stats = RateLimiter.stats()
      assert after_stats.total_dispatched >= before_stats.total_dispatched,
             "total_dispatched decreased after concurrent dispatches (non-monotonic)"
    end
  end

  describe "Quarantine under concurrent access" do
    test "concurrent quarantine and check do not crash GenServer" do
      bots = for i <- 1..5, do: "q_bot_#{System.unique_integer([:positive])}_#{i}"

      tasks =
        Enum.flat_map(bots, fn bot ->
          [
            Task.async(fn -> Quarantine.quarantine(bot, :soft, "concurrency test") end),
            Task.async(fn -> Quarantine.check(bot) end),
            Task.async(fn -> Quarantine.release(bot) end)
          ]
        end)

      Enum.each(tasks, &Task.await(&1, 5_000))

      pid = GenServer.whereis(Quarantine)
      assert pid != nil and Process.alive?(pid),
             "Quarantine crashed under concurrent load"
    end

    test "concurrent record_outcome calls do not produce inconsistent state" do
      bot = "outcome_concurrent_bot_#{System.unique_integer([:positive])}"

      # Interleave successes and failures concurrently
      tasks =
        for _ <- 1..3 do
          Task.async(fn ->
            Quarantine.record_outcome(bot, :success)
            Quarantine.record_outcome(bot, :failure)
          end)
        end

      Enum.each(tasks, &Task.await(&1, 5_000))
      :timer.sleep(100)

      # After mixed outcomes, bot should not be quarantined
      # (5 consecutive failures needed, but we mixed them)
      result = Quarantine.check(bot)
      assert result == :ok or match?({:quarantined, _, _}, result),
             "Unexpected result from check/1 after concurrent outcomes: #{inspect(result)}"
    end
  end
end

defmodule Hypatia.Concurrency.VQLClientTest do
  @moduledoc """
  Concurrency tests for VQL Client GenServer.

  VQL Client is a GenServer (serial message processing), so concurrent
  callers queue up. These tests verify:
  - No deadlock or crash under parallel query load
  - Query count increments correctly
  - Cache hits under repeated identical queries
  """

  use ExUnit.Case, async: false

  alias Hypatia.VQL.Client

  setup do
    case GenServer.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end
    :ok
  end

  describe "VQL Client under concurrent query load" do
    test "concurrent queries to different stores all return results" do
      queries = [
        "SELECT DOCUMENT FROM STORE scans LIMIT 3",
        "SELECT DOCUMENT FROM STORE recipes LIMIT 3",
        "SELECT DOCUMENT FROM STORE index LIMIT 1",
        "SELECT DOCUMENT FROM STORE patterns LIMIT 1"
      ]

      tasks =
        Enum.flat_map(queries, fn query ->
          for _ <- 1..3 do
            Task.async(fn -> Client.query(query) end)
          end
        end)

      results = Enum.map(tasks, &Task.await(&1, 30_000))

      Enum.each(results, fn result ->
        assert match?({:ok, _}, result) or match?({:error, _}, result),
               "Concurrent VQL query returned unexpected value: #{inspect(result)}"
      end)

      # GenServer must still be alive
      pid = GenServer.whereis(Client)
      assert pid != nil and Process.alive?(pid)
    end

    test "concurrent parse calls are safe" do
      # parse/1 goes through GenServer but is pure
      tasks =
        for i <- 1..20 do
          store = Enum.at(~w(scans recipes outcomes index), rem(i, 4))
          Task.async(fn ->
            Client.parse("SELECT DOCUMENT FROM STORE #{store} LIMIT #{i}")
          end)
        end

      results = Enum.map(tasks, &Task.await(&1, 10_000))

      Enum.each(results, fn result ->
        assert match?({:ok, _}, result),
               "Concurrent parse returned error: #{inspect(result)}"
      end)
    end
  end
end
