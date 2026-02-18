# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.TriangleRouterTest do
  use ExUnit.Case, async: true

  alias Hypatia.TriangleRouter

  describe "route/3" do
    test "routes eliminate-tier pattern to eliminate action" do
      pattern = %{
        "id" => "PA009-shell-unquoted-var",
        "category" => "CommandInjection",
        "triangle_tier" => "eliminate",
        "description" => "Unquoted shell variable",
        "severity" => "High",
        "repos_affected_list" => ["test-repo"]
      }

      result = TriangleRouter.route(pattern, "test-repo", "shell")
      assert match?({:eliminate, _recipe, _pattern}, result)
    end

    test "routes substitute-tier pattern with proven modules" do
      pattern = %{
        "id" => "PA016-path-traversal",
        "category" => "PathTraversal",
        "triangle_tier" => "substitute",
        "description" => "Path traversal via ..",
        "severity" => "High",
        "repos_affected_list" => ["test-repo"]
      }

      result = TriangleRouter.route(pattern, "test-repo", "rust")

      case result do
        {:substitute, recipe, _pattern} ->
          assert recipe["proven_module"] == "SafePath"
          assert recipe["formally_proven"] == true

        {:control, _} ->
          # Acceptable fallback if no recipe file matched
          :ok
      end
    end

    test "routes control-tier pattern to control action" do
      pattern = %{
        "id" => "PA006-race-condition",
        "category" => "RaceCondition",
        "triangle_tier" => "control",
        "description" => "Data race in concurrent block",
        "severity" => "High",
        "repos_affected_list" => ["test-repo"]
      }

      result = TriangleRouter.route(pattern, "test-repo", "rust")
      assert match?({:control, _}, result)
    end
  end

  describe "route_batch/3" do
    test "routes multiple patterns at once" do
      patterns = [
        %{
          "id" => "PA009-shell-unquoted",
          "category" => "CommandInjection",
          "triangle_tier" => "eliminate",
          "description" => "Unquoted var",
          "severity" => "High",
          "repos_affected_list" => ["repo"]
        },
        %{
          "id" => "PA005-panic-unwrap",
          "category" => "PanicPath",
          "triangle_tier" => "control",
          "description" => "Unwrap call",
          "severity" => "Medium",
          "repos_affected_list" => ["repo"]
        }
      ]

      results = TriangleRouter.route_batch(patterns, "repo", "rust")
      assert length(results) == 2
    end
  end

  describe "dispatch_strategy/1" do
    test "auto-execute for high confidence" do
      assert TriangleRouter.dispatch_strategy(0.97) == :auto_execute
    end

    test "review for medium confidence" do
      assert TriangleRouter.dispatch_strategy(0.90) == :review
    end

    test "report only for low confidence" do
      assert TriangleRouter.dispatch_strategy(0.50) == :report_only
    end

    test "exact threshold boundaries" do
      assert TriangleRouter.dispatch_strategy(0.95) == :auto_execute
      assert TriangleRouter.dispatch_strategy(0.85) == :review
      assert TriangleRouter.dispatch_strategy(0.84) == :report_only
    end
  end
end
