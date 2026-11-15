# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.RecipeFuzzyRankTest do
  @moduledoc """
  E0-21: among the recipes that match a description by keyword, the one with the most
  keyword hits wins; confidence decides only between equal hit counts; and a keyword
  matches a whole word, not a part of one (design of 2026-11-14 on merge honesty,
  decision 4). No `:verisim_data` tag: the recipes are the test's own.
  """
  use ExUnit.Case, async: true

  alias Hypatia.RecipeMatcher

  defp recipe(id, pattern_ids, confidence) do
    %{
      "id" => id,
      "pattern_ids" => pattern_ids,
      "target_categories" => [],
      "languages" => ["*"],
      "triangle_tier" => "eliminate",
      "confidence" => confidence
    }
  end

  defp finding(description),
    do: %{
      "id" => "PA500-not-a-recipe-id",
      "category" => "no-such-category",
      "pa_rule" => "PA500",
      "description" => description
    }

  describe "ranking, on a list of recipes" do
    test "5 keyword hits win over 1 hit with a higher confidence" do
      recipes = [
        recipe("recipe-rank-one-hit", ["PA500-alpha"], 0.99),
        recipe("recipe-rank-five-hits", ["PA500-alpha-beta-gamma-delta-epsilon"], 0.85)
      ]

      f = finding("alpha beta gamma delta epsilon all appear here")

      assert %{"id" => "recipe-rank-five-hits"} =
               RecipeMatcher.fuzzy_match_in(recipes, f, "elixir")

      assert %{"id" => "recipe-rank-five-hits"} =
               RecipeMatcher.fuzzy_match_in(Enum.reverse(recipes), f, "elixir")
    end

    test "confidence decides between equal hit counts" do
      recipes = [
        recipe("recipe-rank-low", ["PA500-alpha-beta"], 0.80),
        recipe("recipe-rank-high", ["PA500-alpha-beta"], 0.92)
      ]

      assert %{"id" => "recipe-rank-high"} =
               RecipeMatcher.fuzzy_match_in(recipes, finding("alpha and beta"), "elixir")
    end

    test "a keyword matches a whole word: var does not match invariant" do
      recipes = [recipe("recipe-rank-var", ["PA500-var"], 0.9)]

      assert RecipeMatcher.fuzzy_match_in(recipes, finding("an invariant is broken"), "elixir") ==
               nil

      assert %{"id" => "recipe-rank-var"} =
               RecipeMatcher.fuzzy_match_in(recipes, finding("an unquoted var is used"), "elixir")
    end

    test "a keyword is not matched inside a longer word at either end" do
      recipes = [recipe("recipe-rank-quote", ["PA500-quote"], 0.9)]

      for description <- ["unquoted values", "a quotedvalue", "quotes everywhere"] do
        assert RecipeMatcher.fuzzy_match_in(recipes, finding(description), "elixir") == nil
      end

      for description <- ["quote it", "please (quote) this", "do not quote, ever", "QUOTE"] do
        assert %{"id" => "recipe-rank-quote"} =
                 RecipeMatcher.fuzzy_match_in(recipes, finding(description), "elixir")
      end
    end
  end

  describe "fuzzy_match_in/3, on a list of recipes" do
    @recipes [
      %{
        "id" => "recipe-b",
        "pattern_ids" => ["PA500-alpha-beta"],
        "target_categories" => [],
        "languages" => ["*"],
        "confidence" => 0.9
      },
      %{
        "id" => "recipe-a",
        "pattern_ids" => ["PA500-alpha-beta"],
        "target_categories" => [],
        "languages" => ["*"],
        "confidence" => 0.9
      },
      %{
        "id" => "recipe-c",
        "pattern_ids" => ["PA500-alpha"],
        "target_categories" => [],
        "languages" => ["*"],
        "confidence" => 0.95
      }
    ]

    test "gives the same recipe for 2 orders of the list, and breaks a tie by id" do
      f = finding("alpha and beta")

      forward = RecipeMatcher.fuzzy_match_in(@recipes, f, "elixir")
      backward = RecipeMatcher.fuzzy_match_in(Enum.reverse(@recipes), f, "elixir")

      assert forward == backward
      # recipe-a and recipe-b tie on hits and on confidence; the lower id wins.
      assert forward["id"] == "recipe-a"
    end

    test "a repeated keyword is one hit, not several" do
      doubled = [
        recipe("recipe-doubled", ["PA500-alpha-x", "PA500-alpha-y", "PA500-alpha-z"], 0.99),
        recipe("recipe-two-words", ["PA500-alpha-beta"], 0.80)
      ]

      assert %{"id" => "recipe-two-words"} =
               RecipeMatcher.fuzzy_match_in(doubled, finding("alpha beta"), "elixir")
    end

    test "no PA rule, no match, and a language mismatch still give nil" do
      assert RecipeMatcher.fuzzy_match_in(
               @recipes,
               Map.delete(finding("alpha"), "pa_rule"),
               "elixir"
             ) ==
               nil

      assert RecipeMatcher.fuzzy_match_in(@recipes, finding("nothing relevant"), "elixir") == nil

      only_rust = [Map.put(hd(@recipes), "languages", ["rust"])]
      assert RecipeMatcher.fuzzy_match_in(only_rust, finding("alpha beta"), "elixir") == nil
    end
  end
end
