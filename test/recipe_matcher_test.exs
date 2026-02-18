# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.RecipeMatcherTest do
  use ExUnit.Case, async: true

  alias Hypatia.RecipeMatcher

  describe "all_recipes/0" do
    test "loads recipe files from verisimdb-data" do
      recipes = RecipeMatcher.all_recipes()
      assert is_list(recipes)
      assert length(recipes) >= 4

      ids = Enum.map(recipes, &Map.get(&1, "id"))
      assert "recipe-shell-quote-vars" in ids
      assert "recipe-remove-believe-me" in ids
      assert "recipe-heredoc-to-install" in ids
      assert "recipe-todo-to-fill" in ids
    end
  end

  describe "find_recipes/1" do
    test "finds recipes matching a pattern_id" do
      recipes = RecipeMatcher.find_recipes("PA009-shell-unquoted-var")
      assert length(recipes) >= 1

      recipe = hd(recipes)
      assert recipe["id"] == "recipe-shell-quote-vars"
      assert recipe["triangle_tier"] == "eliminate"
      assert recipe["confidence"] >= 0.90
    end

    test "returns empty list for unknown pattern" do
      assert RecipeMatcher.find_recipes("PA999-nonexistent") == []
    end
  end

  describe "best_recipe/2" do
    test "returns best recipe for pattern and language" do
      recipe = RecipeMatcher.best_recipe("PA009-shell-unquoted-var", "shell")
      assert recipe != nil
      assert recipe["id"] == "recipe-shell-quote-vars"
    end

    test "matches wildcard language recipes" do
      recipe = RecipeMatcher.best_recipe("PA018-unchecked-todo", "rust")
      assert recipe != nil
      assert recipe["id"] == "recipe-todo-to-fill"
    end

    test "returns nil for unmatched language" do
      # recipe-shell-quote-vars only targets "shell"
      recipe = RecipeMatcher.best_recipe("PA009-shell-unquoted-var", "haskell")
      assert recipe == nil
    end
  end

  describe "recipes_by_tier/1" do
    test "filters recipes by triangle tier" do
      eliminate = RecipeMatcher.recipes_by_tier("eliminate")
      assert length(eliminate) >= 3

      Enum.each(eliminate, fn r ->
        assert r["triangle_tier"] == "eliminate"
      end)
    end
  end

  describe "load_substitutions/0" do
    test "loads proven-substitutions with all 20 PA rules" do
      subs = RecipeMatcher.load_substitutions()
      assert length(subs) == 20

      categories = Enum.map(subs, &Map.get(&1, "category"))
      assert "CommandInjection" in categories
      assert "PathTraversal" in categories
      assert "UnsafeDeserialization" in categories
    end
  end

  describe "substitution_for_category/1" do
    test "finds substitution for known category" do
      sub = RecipeMatcher.substitution_for_category("PathTraversal")
      assert sub != nil
      assert sub["pa_rule"] == "PA016"
      assert "SafePath" in sub["proven_modules"]
      assert sub["formally_proven"] == true
      assert sub["confidence"] == 0.98
    end

    test "returns nil for unknown category" do
      assert RecipeMatcher.substitution_for_category("FakeCategory") == nil
    end
  end
end
