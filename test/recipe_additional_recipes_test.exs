# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.RecipeAdditionalRecipesTest do
  use ExUnit.Case, async: true

  alias Hypatia.RecipeMatcher

  describe "additional recipes loaded" do
    test "all_recipes includes the 7 new recipes" do
      recipes = RecipeMatcher.all_recipes()
      ids = Enum.map(recipes, &Map.get(&1, "id"))

      assert "recipe-unbounded-loop" in ids
      assert "recipe-safe-deserialize" in ids
      assert "recipe-eval-to-safe-exec" in ids
      assert "recipe-dynamic-apply-to-dispatch" in ids
      assert "recipe-deno-least-privilege" in ids
      assert "recipe-secret-to-env" in ids
      assert "recipe-unchecked-error" in ids
    end

    test "total recipe count is at least 22" do
      recipes = RecipeMatcher.all_recipes()
      assert length(recipes) >= 22
    end
  end

  describe "PA002 recipe" do
    test "unbounded-loop targets all languages" do
      recipe = RecipeMatcher.get_recipe("recipe-unbounded-loop")
      assert recipe != nil
      assert "PA002-unbounded-while-true-loop" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "control"
      assert recipe["confidence"] == 0.55
      assert recipe["auto_fixable"] == false
      assert "*" in recipe["languages"]
    end
  end

  describe "PA010 recipe" do
    test "safe-deserialize covers rescript and javascript" do
      recipe = RecipeMatcher.get_recipe("recipe-safe-deserialize")
      assert recipe != nil
      assert "PA010-json-parseexn" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "substitute"
      assert recipe["confidence"] == 0.70
      assert recipe["auto_fixable"] == false
      assert "rescript" in recipe["languages"]
      assert "javascript" in recipe["languages"]
    end
  end

  describe "PA011 eval recipe" do
    test "eval-to-safe-exec targets shell and javascript" do
      recipe = RecipeMatcher.get_recipe("recipe-eval-to-safe-exec")
      assert recipe != nil
      assert "PA011-eval-usage" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "eliminate"
      assert recipe["confidence"] == 0.60
      assert "javascript" in recipe["languages"]
      assert "shell" in recipe["languages"]
      assert "bash" in recipe["languages"]
    end
  end

  describe "PA011 dynamic apply recipe" do
    test "dynamic-apply-to-dispatch targets elixir and erlang" do
      recipe = RecipeMatcher.get_recipe("recipe-dynamic-apply-to-dispatch")
      assert recipe != nil
      assert "PA011-dynamic-apply" in recipe["pattern_ids"]
      assert "PA011-dynamic-apply-3" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "substitute"
      assert recipe["confidence"] == 0.65
      assert "elixir" in recipe["languages"]
      assert "erlang" in recipe["languages"]
    end
  end

  describe "PA015 recipe" do
    test "deno-least-privilege replaces -A flag" do
      recipe = RecipeMatcher.get_recipe("recipe-deno-least-privilege")
      assert recipe != nil
      assert "PA015-deno-a-all-permissions" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "substitute"
      assert recipe["confidence"] == 0.75
      assert recipe["auto_fixable"] == false
      assert "javascript" in recipe["languages"]
      assert "shell" in recipe["languages"]
    end
  end

  describe "PA017 recipe" do
    test "secret-to-env is eliminate tier and covers all languages" do
      recipe = RecipeMatcher.get_recipe("recipe-secret-to-env")
      assert recipe != nil
      assert "PA017-possible-hardcoded-secret" in recipe["pattern_ids"]
      assert "PA017-hardcoded-api-key" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "eliminate"
      assert recipe["confidence"] == 0.55
      assert recipe["auto_fixable"] == false
      assert "*" in recipe["languages"]
    end
  end

  describe "PA018 recipe" do
    test "unchecked-error covers multiple languages" do
      recipe = RecipeMatcher.get_recipe("recipe-unchecked-error")
      assert recipe != nil
      assert "PA018-ignore-calls" in recipe["pattern_ids"]
      assert "PA018-unchecked-error" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "control"
      assert recipe["confidence"] == 0.60
      assert recipe["auto_fixable"] == false
      assert "rescript" in recipe["languages"]
      assert "elixir" in recipe["languages"]
      assert "rust" in recipe["languages"]
    end
  end

  describe "fuzzy matching for additional recipes" do
    test "PA002 fuzzy match finds unbounded-loop recipe" do
      pattern = %{
        "id" => "PA002-unbounded-while-true-loop-in-s",
        "pa_rule" => "PA002",
        "description" => "Unbounded while True loop in scripts/repo-reconcile.py"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "python")
      assert recipe != nil
      assert recipe["id"] == "recipe-unbounded-loop"
    end

    test "PA010 fuzzy match finds safe-deserialize for rescript" do
      pattern = %{
        "id" => "PA010-2-json-parseexn-calls-in-src-c",
        "pa_rule" => "PA010",
        "description" => "2 JSON.parseExn calls in src/crawlers/FindBible.res"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "rescript")
      assert recipe != nil
      assert recipe["id"] == "recipe-safe-deserialize"
    end

    test "PA011 fuzzy match finds eval recipe for shell" do
      pattern = %{
        "id" => "PA011-eval-usage-in-scripts-check-la",
        "pa_rule" => "PA011",
        "description" => "eval usage in scripts/check-languages.sh"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "shell")
      assert recipe != nil
      assert recipe["id"] == "recipe-eval-to-safe-exec"
    end

    test "PA011 fuzzy match finds apply recipe for elixir" do
      pattern = %{
        "id" => "PA011-dynamic-apply-3-in-src-elixir-",
        "pa_rule" => "PA011",
        "description" => "Dynamic apply/3 in src/elixir/lib/vordr/reversibility.ex"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "elixir")
      assert recipe != nil
      assert recipe["id"] == "recipe-dynamic-apply-to-dispatch"
    end

    test "PA015 fuzzy match finds deno-least-privilege for javascript" do
      pattern = %{
        "id" => "PA015-deno-a-all-permissions-in-src-",
        "pa_rule" => "PA015",
        "description" => "Deno -A (all permissions) in src/main.js"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "javascript")
      assert recipe != nil
      assert recipe["id"] == "recipe-deno-least-privilege"
    end

    test "PA017 fuzzy match finds secret-to-env recipe" do
      pattern = %{
        "id" => "PA017-possible-hardcoded-secret-in-b",
        "pa_rule" => "PA017",
        "description" => "Possible hardcoded secret in backend/api/tests/api.test.js"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "javascript")
      assert recipe != nil
      assert recipe["id"] == "recipe-secret-to-env"
    end

    test "PA018 fuzzy match finds unchecked-error for rescript" do
      pattern = %{
        "id" => "PA018-4-ignore-calls-in-packages-cor",
        "pa_rule" => "PA018",
        "description" => "4 ignore() calls in packages/core/compiler-source/tests (may discard important results)"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "rescript")
      assert recipe != nil
      assert recipe["id"] == "recipe-unchecked-error"
    end
  end

  describe "triangle tier distribution" do
    test "new recipes span all three tiers" do
      new_ids = [
        "recipe-unbounded-loop",
        "recipe-safe-deserialize",
        "recipe-eval-to-safe-exec",
        "recipe-dynamic-apply-to-dispatch",
        "recipe-deno-least-privilege",
        "recipe-secret-to-env",
        "recipe-unchecked-error"
      ]

      recipes = RecipeMatcher.all_recipes()
      new_recipes = Enum.filter(recipes, fn r -> Map.get(r, "id") in new_ids end)
      tiers = Enum.map(new_recipes, fn r -> Map.get(r, "triangle_tier") end) |> Enum.uniq() |> Enum.sort()

      assert "control" in tiers
      assert "eliminate" in tiers
      assert "substitute" in tiers
    end
  end

  describe "confidence ranges" do
    test "all new recipes have confidence between 0.50 and 0.80" do
      new_ids = [
        "recipe-unbounded-loop",
        "recipe-safe-deserialize",
        "recipe-eval-to-safe-exec",
        "recipe-dynamic-apply-to-dispatch",
        "recipe-deno-least-privilege",
        "recipe-secret-to-env",
        "recipe-unchecked-error"
      ]

      recipes = RecipeMatcher.all_recipes()
      new_recipes = Enum.filter(recipes, fn r -> Map.get(r, "id") in new_ids end)

      for recipe <- new_recipes do
        confidence = Map.get(recipe, "confidence", 0.0)
        assert confidence >= 0.50, "#{recipe["id"]} confidence #{confidence} below 0.50"
        assert confidence <= 0.80, "#{recipe["id"]} confidence #{confidence} above 0.80"
      end
    end

    test "none of the new recipes are auto_fixable" do
      new_ids = [
        "recipe-unbounded-loop",
        "recipe-safe-deserialize",
        "recipe-eval-to-safe-exec",
        "recipe-dynamic-apply-to-dispatch",
        "recipe-deno-least-privilege",
        "recipe-secret-to-env",
        "recipe-unchecked-error"
      ]

      recipes = RecipeMatcher.all_recipes()
      new_recipes = Enum.filter(recipes, fn r -> Map.get(r, "id") in new_ids end)

      for recipe <- new_recipes do
        assert recipe["auto_fixable"] == false,
               "#{recipe["id"]} should not be auto_fixable (new recipe, needs confidence buildup)"
      end
    end
  end
end
