# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.FleetDispatcherTest do
  use ExUnit.Case, async: true

  alias Hypatia.FleetDispatcher

  @sample_recipe %{
    "id" => "recipe-shell-quote-vars",
    "confidence" => 0.99,
    "description" => "Quote shell variables",
    "fix_script" => "fix-shell-quoting.sh",
    "action" => "shell"
  }

  @sample_pattern %{
    "id" => "PA009-shell-unquoted-var",
    "category" => "CommandInjection",
    "description" => "Unquoted shell variable",
    "severity" => "High",
    "routed_repo" => "test-repo",
    "repos_affected_list" => ["test-repo"]
  }

  describe "dispatch_routed_action/1" do
    test "dispatches eliminate action" do
      result =
        FleetDispatcher.dispatch_routed_action(
          {:eliminate, @sample_recipe, @sample_pattern}
        )

      # Fleet dispatcher logs dispatch; returns :ok or {:ok, _}
      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end

    test "dispatches substitute action" do
      recipe = %{
        "id" => "recipe-safe-command",
        "confidence" => 0.90,
        "proven_module" => "SafeCommand"
      }

      result =
        FleetDispatcher.dispatch_routed_action(
          {:substitute, recipe, @sample_pattern}
        )

      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end

    test "dispatches control action" do
      result = FleetDispatcher.dispatch_routed_action({:control, @sample_pattern})

      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end
  end

  describe "dispatch_finding/1 (legacy)" do
    test "dispatches by finding type" do
      finding = %{
        type: :fix_suggestion,
        repo: "test-repo",
        file: "scripts/deploy.sh",
        issue: "Unquoted variable",
        suggestion: "Add double quotes"
      }

      result = FleetDispatcher.dispatch_finding(finding)
      assert result in [:ok, {:ok, :dispatched}, {:ok, :logged}] or
               match?({:ok, _}, result)
    end
  end
end
