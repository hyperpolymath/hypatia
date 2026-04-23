# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.ContractTest do
  @moduledoc """
  Contract tests for Hypatia's public module boundaries.

  Each describe block tests the invariants declared in MUST.contractile
  and each module's @moduledoc. Failures name the violated invariant so
  CI output is self-documenting.

  Run in isolation:   mix test --only contract
  """
  use Hypatia.ContractCase

  alias Hypatia.TriangleRouter
  alias Hypatia.Safety.{RateLimiter, Quarantine}
  alias Hypatia.VCL.Client, as: VCLClient

  # ---------------------------------------------------------------------------
  # TriangleRouter.dispatch_strategy/1 — pure function, no setup needed
  # ---------------------------------------------------------------------------

  describe "TriangleRouter.dispatch_strategy/1 — confidence threshold contracts" do
    test "confidence at and above 0.95 produces :auto_execute" do
      assert_invariant "MUST: confidence >= 0.95 → :auto_execute" do
        assert TriangleRouter.dispatch_strategy(0.95) == :auto_execute
        assert TriangleRouter.dispatch_strategy(0.951) == :auto_execute
        assert TriangleRouter.dispatch_strategy(1.0) == :auto_execute
      end
    end

    test "confidence in [0.85, 0.95) produces :review" do
      assert_invariant "MUST: 0.85 <= confidence < 0.95 → :review" do
        assert TriangleRouter.dispatch_strategy(0.85) == :review
        assert TriangleRouter.dispatch_strategy(0.90) == :review
        assert TriangleRouter.dispatch_strategy(0.9499) == :review
      end
    end

    test "confidence below 0.85 produces :report_only" do
      assert_invariant "MUST: confidence < 0.85 → :report_only" do
        assert TriangleRouter.dispatch_strategy(0.8499) == :report_only
        assert TriangleRouter.dispatch_strategy(0.50) == :report_only
        assert TriangleRouter.dispatch_strategy(0.0) == :report_only
      end
    end

    test "non-numeric input produces :report_only (fail-safe)" do
      assert_invariant "MUST: non-number confidence → :report_only (fail-safe, novelty-gating)" do
        assert TriangleRouter.dispatch_strategy(nil) == :report_only
        assert TriangleRouter.dispatch_strategy("high") == :report_only
        assert TriangleRouter.dispatch_strategy(:unknown) == :report_only
      end
    end

    test "boundary values are strictly separated (no overlap)" do
      assert_invariant "threshold boundaries are strict: 0.9499 is review, 0.8499 is report_only" do
        refute TriangleRouter.dispatch_strategy(0.9499) == :auto_execute
        refute TriangleRouter.dispatch_strategy(0.8499) == :review
      end
    end
  end

  # ---------------------------------------------------------------------------
  # RateLimiter — dispatch window contracts
  # ---------------------------------------------------------------------------

  describe "RateLimiter — burst limit contract" do
    setup do
      start_supervised!(RateLimiter)
      :ok
    end

    test "fresh bot is allowed immediately" do
      assert_invariant "new bot has no dispatch history → check returns :ok" do
        assert :ok = RateLimiter.check("contract-rl-fresh-#{unique_id()}")
      end
    end

    test "burst limit triggers after #{10} dispatches in the burst window" do
      bot = "contract-rl-burst-#{unique_id()}"

      assert_invariant "MUST: burst limit = 10 dispatches / 5 s" do
        for _ <- 1..10, do: RateLimiter.record_dispatch(bot)
        # Allow the cast to propagate
        :timer.sleep(30)
        assert {:rate_limited, :burst, _retry_after_ms} = RateLimiter.check(bot)
      end
    end

    test "rate-limited response includes a positive retry_after_ms" do
      bot = "contract-rl-retry-#{unique_id()}"

      assert_invariant "rate_limited response carries a positive retry_after_ms hint" do
        for _ <- 1..10, do: RateLimiter.record_dispatch(bot)
        :timer.sleep(30)
        assert {:rate_limited, _kind, retry_ms} = RateLimiter.check(bot)
        assert_post "retry_after_ms is positive" do
          retry_ms > 0
        end
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Quarantine — auto-quarantine threshold contracts
  # ---------------------------------------------------------------------------

  describe "Quarantine — auto-quarantine contracts" do
    setup do
      start_supervised!(Quarantine)
      :ok
    end

    test "fresh bot is not quarantined" do
      assert_invariant "bot with no outcomes starts at :ok" do
        assert :ok = Quarantine.check("contract-q-fresh-#{unique_id()}")
      end
    end

    test "5 consecutive failures trigger hard quarantine" do
      bot = "contract-q-consec-#{unique_id()}"

      assert_invariant "MUST: 5 consecutive :failure outcomes → hard quarantine" do
        assert_pre "bot is clean before failures" do
          Quarantine.check(bot) == :ok
        end

        for _ <- 1..5, do: Quarantine.record_outcome(bot, :failure)
        :timer.sleep(50)

        assert_post "bot is quarantined at :hard level" do
          match?({:quarantined, :hard, _}, Quarantine.check(bot))
        end
      end
    end

    test "4 consecutive failures do not trigger quarantine" do
      bot = "contract-q-below-#{unique_id()}"

      assert_invariant "threshold is exactly 5: 4 failures is not enough" do
        for _ <- 1..4, do: Quarantine.record_outcome(bot, :failure)
        :timer.sleep(50)
        assert :ok = Quarantine.check(bot)
      end
    end

    test "FP rate above 30% triggers soft quarantine" do
      bot = "contract-q-fp-#{unique_id()}"

      assert_invariant "MUST: FP rate > 30% (min 5 outcomes) → soft quarantine" do
        # 4 successes + 2 false_positives = 6 outcomes; rate = 2/6 ≈ 33% > 30%
        for _ <- 1..4, do: Quarantine.record_outcome(bot, :success)
        for _ <- 1..2, do: Quarantine.record_outcome(bot, :false_positive)
        :timer.sleep(50)

        assert_post "bot is quarantined at :soft level" do
          match?({:quarantined, :soft, _}, Quarantine.check(bot))
        end
      end
    end

    test "FP rate below threshold does not quarantine" do
      bot = "contract-q-fp-safe-#{unique_id()}"

      assert_invariant "FP rate <= 30% does not auto-quarantine" do
        # 8 successes + 1 FP = 9 outcomes; rate = 1/9 ≈ 11% < 30%
        for _ <- 1..8, do: Quarantine.record_outcome(bot, :success)
        Quarantine.record_outcome(bot, :false_positive)
        :timer.sleep(50)
        assert :ok = Quarantine.check(bot)
      end
    end

    test "manual quarantine and release are reversible" do
      bot = "contract-q-release-#{unique_id()}"

      assert_invariant "quarantine → release restores :ok state (reversibility)" do
        Quarantine.quarantine(bot, :soft, "contract test")
        :timer.sleep(20)
        assert {:quarantined, :soft, _} = Quarantine.check(bot)

        Quarantine.release(bot)
        :timer.sleep(20)
        assert :ok = Quarantine.check(bot)
      end
    end

    test "reroute target is nil for a non-quarantined bot" do
      bot = "contract-q-reroute-none-#{unique_id()}"

      assert_invariant "reroute_target for a clean bot is nil" do
        assert nil == Quarantine.reroute_target(bot)
      end
    end
  end

  # ---------------------------------------------------------------------------
  # VCL.Client.parse/1 — totality contract (mirrors ParserTotality.lean)
  # ---------------------------------------------------------------------------

  describe "VCL.Client.parse/1 — totality contract" do
    setup do
      start_supervised!(VCLClient)
      :ok
    end

    test "well-formed SELECT query returns {:ok, ast}" do
      assert_invariant "parse/1 returns {:ok, ast} for well-formed SELECT" do
        assert {:ok, _ast} = VCLClient.parse("SELECT DOCUMENT FROM STORE scans LIMIT 10")
      end
    end

    test "empty string returns {:error, _} not a crash" do
      assert_invariant "parse/1 returns {:error, _} for empty input (total)" do
        assert {:error, _} = VCLClient.parse("")
      end
    end

    test "parse/1 is total: malformed inputs never raise" do
      assert_invariant "parse/1 returns ok or error for any string — never raises" do
        bad_inputs = [
          "INVALID",
          "SELECT FROM",
          "123 456",
          "SELECT * FROM x WHERE",
          ";;;",
          "SELECT DOCUMENT FROM STORE WHERE repo = \"x\" AND",
          "\t\n\r",
          "SELECT DOCUMENT FROM STORE " <> String.duplicate("a b", 500)
        ]

        for input <- bad_inputs do
          result = VCLClient.parse(input)

          assert match?({:ok, _}, result) or match?({:error, _}, result),
                 "parse/1 returned neither ok nor error for #{inspect(String.slice(input, 0, 40))}: #{inspect(result)}"
        end
      end
    end

    test "parse/1 → execute/1 roundtrip returns a result for a valid query" do
      assert_invariant "parse then execute returns {:ok, _} or {:error, _} (never raises)" do
        assert {:ok, ast} = VCLClient.parse("SELECT DOCUMENT FROM STORE scans LIMIT 5")
        result = VCLClient.execute(ast)
        assert match?({:ok, _}, result) or match?({:error, _}, result)
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp unique_id, do: :erlang.unique_integer([:positive]) |> to_string()
end
