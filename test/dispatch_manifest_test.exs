# SPDX-License-Identifier: MPL-2.0

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
        {:control,
         %{
           "id" => "PA006-race",
           "category" => "RaceCondition",
           "description" => "Data race",
           "severity" => "High",
           "routed_repo" => "test-repo"
         }}
      ]

      {:ok, path, stats} = DispatchManifest.write(actions)

      assert String.ends_with?(path, "pending.jsonl")
      assert stats.total >= 2
      assert stats.auto_execute >= 1
      assert stats.report_only >= 1
    end
  end

  # HYP-01: facts derive workflows. The manifest is the workflow hypatia
  # derives for dispatch-runner.sh; every step names its fact, and a step
  # that names none refuses the whole manifest.
  describe "facts (HYP-01)" do
    @eliminate_recipe %{"id" => "recipe-x", "confidence" => 0.99, "auto_fixable" => true}

    defp pattern(id, extra \\ %{}) do
      Map.merge(
        %{
          "id" => id,
          "category" => "CommandInjection",
          "severity" => "High",
          "routed_repo" => "test-repo"
        },
        extra
      )
    end

    test "every step names the verdict it derives from" do
      routed = [
        {:eliminate, @eliminate_recipe, pattern("PA009-x", %{"occurrences" => 7})},
        {:substitute, %{"id" => "r2", "confidence" => 0.9}, pattern("PA010-y")},
        {:control, pattern("PA006-z")}
      ]

      assert {:ok, steps} = DispatchManifest.compile(routed)
      assert length(steps) == 3

      [elim, subst, ctrl] = steps

      assert [
               %{
                 "pattern_id" => "PA009-x",
                 "support" => %{"kind" => "observed", "observations" => 7},
                 "level" => "epi",
                 "evidence" => [%{"rule" => "pattern_registry/PA009-x"}]
               }
             ] = elim["facts"]

      assert [%{"pattern_id" => "PA010-y", "support" => %{"kind" => "pattern"}}] =
               subst["facts"]

      assert [%{"pattern_id" => "PA006-z", "level" => "epi"}] = ctrl["facts"]

      # Confidence is belief and stays in its own field: no fact carries it,
      # and no fact is ever "sound".
      for step <- steps, fact <- step["facts"] do
        refute Map.has_key?(fact, "confidence")
        assert fact["level"] != "sound"
      end
    end

    test "the written manifest shows each step's facts", %{} do
      dir = Path.join(System.tmp_dir!(), "hypatia-manifest-#{System.unique_integer([:positive])}")
      on_exit(fn -> File.rm_rf(dir) end)

      assert {:ok, path, %{total: 1}} =
               DispatchManifest.write([{:control, pattern("PA006-z")}], dispatch_dir: dir)

      [line] = path |> File.read!() |> String.split("\n", trim: true)
      assert %{"facts" => [%{"pattern_id" => "PA006-z"}]} = Jason.decode!(line)
    end

    test "a factless step refuses the whole manifest, and nothing is written" do
      dir = Path.join(System.tmp_dir!(), "hypatia-manifest-#{System.unique_integer([:positive])}")
      on_exit(fn -> File.rm_rf(dir) end)

      routed = [
        {:control, pattern("PA006-z")},
        {:control, Map.delete(pattern("ignored"), "id")},
        {:eliminate, @eliminate_recipe, pattern("unknown")}
      ]

      assert {:error, {:factless_steps, steps}} = DispatchManifest.compile(routed)

      assert [%{"step" => 1, "tier" => "control"}, %{"step" => 2, "tier" => "eliminate"}] =
               steps

      assert {:error, {:factless_steps, ^steps}} =
               DispatchManifest.write(routed, dispatch_dir: dir)

      refute File.exists?(Path.join(dir, "pending.jsonl"))
    end
  end
end
