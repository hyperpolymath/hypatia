# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.RecipeNewRecipesTest do
  use ExUnit.Case, async: true

  alias Hypatia.RecipeMatcher

  describe "new recipes loaded" do
    test "all_recipes includes the 5 new recipes" do
      recipes = RecipeMatcher.all_recipes()
      ids = Enum.map(recipes, &Map.get(&1, "id"))

      assert "recipe-unwrap-to-match" in ids
      assert "recipe-panic-to-result" in ids
      assert "recipe-unsafe-type-coercion" in ids
      assert "recipe-atom-exhaustion" in ids
      assert "recipe-unsafe-ffi-wrapper" in ids
    end

    test "total recipe count is at least 15" do
      recipes = RecipeMatcher.all_recipes()
      assert length(recipes) >= 15
    end
  end

  describe "PA005 recipes" do
    test "unwrap-to-match matches PA005 patterns" do
      recipe = RecipeMatcher.get_recipe("recipe-unwrap-to-match")
      assert recipe != nil
      assert "PA005-panic-unwrap" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "substitute"
      assert recipe["confidence"] == 0.75
      assert recipe["auto_fixable"] == false
    end

    test "panic-to-result matches PA005 patterns" do
      recipe = RecipeMatcher.get_recipe("recipe-panic-to-result")
      assert recipe != nil
      assert "PA005-panic-macro" in recipe["pattern_ids"]
      assert recipe["triangle_tier"] == "substitute"
      assert recipe["confidence"] == 0.70
    end
  end

  describe "PA020 recipe" do
    test "unsafe-type-coercion targets multiple languages" do
      recipe = RecipeMatcher.get_recipe("recipe-unsafe-type-coercion")
      assert recipe != nil
      assert recipe["triangle_tier"] == "eliminate"
      assert "ocaml" in recipe["languages"]
      assert "rust" in recipe["languages"]
      assert "haskell" in recipe["languages"]
      assert length(recipe["pattern_ids"]) == 4
    end
  end

  describe "PA013 recipe" do
    test "atom-exhaustion is auto-fixable" do
      recipe = RecipeMatcher.get_recipe("recipe-atom-exhaustion")
      assert recipe != nil
      assert recipe["auto_fixable"] == true
      assert recipe["confidence"] == 0.90
      assert recipe["triangle_tier"] == "eliminate"
      assert "elixir" in recipe["languages"]
    end
  end

  describe "PA012 recipe" do
    test "unsafe-ffi-wrapper covers rust, elixir, zig" do
      recipe = RecipeMatcher.get_recipe("recipe-unsafe-ffi-wrapper")
      assert recipe != nil
      assert recipe["triangle_tier"] == "substitute"
      assert "rust" in recipe["languages"]
      assert "elixir" in recipe["languages"]
      assert "zig" in recipe["languages"]
    end
  end

  describe "fuzzy matching for new recipes" do
    test "PA005 fuzzy match finds unwrap recipe for rust" do
      pattern = %{
        "id" => "PA005-potentially-unsafe-unwrap-call",
        "pa_rule" => "PA005",
        "description" => "unwrap call that could panic at runtime"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "rust")
      assert recipe != nil
      assert recipe["id"] in ["recipe-unwrap-to-match", "recipe-panic-to-result"]
    end

    test "PA013 fuzzy match finds atom recipe for elixir" do
      pattern = %{
        "id" => "PA013-dynamic-atom-creation",
        "pa_rule" => "PA013",
        "description" => "dynamic atom creation from user string input"
      }

      recipe = RecipeMatcher.best_recipe_for_pattern(pattern, "elixir")
      assert recipe != nil
      assert recipe["id"] == "recipe-atom-exhaustion"
    end
  end

  describe "updated substitutions" do
    test "PA013 substitution is now eliminate tier" do
      sub = RecipeMatcher.substitution_for_category("AtomExhaustion")
      assert sub != nil
      assert sub["triangle_tier"] == "eliminate"
    end

    test "PA020 substitution is now eliminate tier" do
      sub = RecipeMatcher.substitution_for_category("UnsafeTypeCoercion")
      assert sub != nil
      assert sub["triangle_tier"] == "eliminate"
    end
  end
end
