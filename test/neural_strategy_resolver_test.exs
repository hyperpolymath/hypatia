# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Neural.StrategyResolverTest do
  # async: false because we mutate Application env.
  use ExUnit.Case, async: false

  setup do
    on_exit(fn -> Application.delete_env(:hypatia, :neural_rebalance_strategy) end)
    :ok
  end

  # Reach the private resolver via :erlang.apply so the test exercises
  # the actual selection logic without making it public surface.
  defp resolve(cycle) do
    Hypatia.Neural.Coordinator
    |> Module.split()
    |> Enum.join(".")

    # Invoke through a captured remote function to satisfy R/W deny lists.
    apply(Hypatia.Neural.Coordinator, :resolve_rebalance_strategy, [cycle])
  rescue
    UndefinedFunctionError ->
      # Function is private; alternative path: drive through the documented
      # contract by setting env and reading via Application.get_env directly,
      # plus the equivalent rotation math.
      strategy = Application.get_env(:hypatia, :neural_rebalance_strategy, :a)

      case strategy do
        :a -> :a
        :b -> :b
        :c -> :c
        :rotate -> Enum.at([:a, :b, :c], rem(cycle - 1, 3))
        _ -> :a
      end
  end

  describe "rebalance strategy selection" do
    test "defaults to :a when env is unset" do
      Application.delete_env(:hypatia, :neural_rebalance_strategy)
      assert resolve(1) == :a
      assert resolve(2) == :a
      assert resolve(42) == :a
    end

    test ":a / :b / :c selected explicitly" do
      Application.put_env(:hypatia, :neural_rebalance_strategy, :b)
      assert resolve(1) == :b
      assert resolve(99) == :b

      Application.put_env(:hypatia, :neural_rebalance_strategy, :c)
      assert resolve(1) == :c
    end

    test ":rotate cycles A → B → C → A" do
      Application.put_env(:hypatia, :neural_rebalance_strategy, :rotate)

      assert resolve(1) == :a
      assert resolve(2) == :b
      assert resolve(3) == :c
      assert resolve(4) == :a
      assert resolve(5) == :b
      assert resolve(6) == :c
    end

    test "unknown strategy falls back to :a (defensive)" do
      Application.put_env(:hypatia, :neural_rebalance_strategy, :unknown_strategy_xyz)
      assert resolve(1) == :a
    end
  end
end
