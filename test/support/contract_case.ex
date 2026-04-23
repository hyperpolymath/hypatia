# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.ContractCase do
  @moduledoc """
  ExUnit support module for contract-level tests.

  A contract test verifies an invariant that a module promises to uphold,
  as declared in MUST.contractile or the module's @moduledoc. Tests are
  tagged :contract and can be run in isolation with:

      mix test --only contract

  Usage:

      defmodule My.ContractTest do
        use Hypatia.ContractCase

        test "threshold invariant" do
          assert_invariant "MUST: >=0.95 → auto_execute" do
            assert TriangleRouter.dispatch_strategy(0.95) == :auto_execute
          end
        end
      end
  """

  defmacro __using__(_opts) do
    quote do
      use ExUnit.Case, async: false
      import Hypatia.ContractCase
      @moduletag :contract
    end
  end

  @doc """
  Wrap a block in a named invariant. If any assertion inside fails, the
  failure message includes the invariant name, making CI output self-documenting.
  """
  defmacro assert_invariant(name, do: block) do
    quote do
      try do
        unquote(block)
      rescue
        e in [ExUnit.AssertionError, FunctionClauseError, MatchError] ->
          ExUnit.Assertions.flunk(
            "Invariant [#{unquote(name)}] violated — #{Exception.message(e)}"
          )
      end
    end
  end

  @doc "Assert a precondition holds before exercising the contract."
  defmacro assert_pre(description, expr) do
    quote do
      assert unquote(expr), "Precondition failed: #{unquote(description)}"
    end
  end

  @doc "Assert a postcondition holds after exercising the contract."
  defmacro assert_post(description, expr) do
    quote do
      assert unquote(expr), "Postcondition failed: #{unquote(description)}"
    end
  end
end
