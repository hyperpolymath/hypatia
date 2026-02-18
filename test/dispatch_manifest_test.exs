# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.DispatchManifestTest do
  use ExUnit.Case, async: true

  alias Hypatia.DispatchManifest

  describe "write/1" do
    test "writes manifest from routed actions" do
      recipe = %{
        "id" => "recipe-shell-quote-vars",
        "confidence" => 0.99,
        "fix_script" => "fix-shell-quoting.sh",
        "action" => "shell",
        "auto_fixable" => true,
        "match" => "$VAR",
        "replacement" => "\"${VAR}\""
      }

      pattern = %{
        "id" => "PA009-shell-unquoted-var",
        "category" => "CommandInjection",
        "description" => "Unquoted shell variable",
        "severity" => "High",
        "routed_repo" => "test-repo"
      }

      actions = [
        {:eliminate, recipe, pattern},
        {:control, %{"id" => "PA006-race", "category" => "RaceCondition",
                     "description" => "Data race", "severity" => "High",
                     "routed_repo" => "test-repo"}}
      ]

      {:ok, path, stats} = DispatchManifest.write(actions)

      assert String.ends_with?(path, "pending.jsonl")
      assert stats.total >= 2
      assert stats.auto_execute >= 1
      assert stats.report_only >= 1
    end
  end
end
