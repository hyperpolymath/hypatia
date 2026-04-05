# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.RecipeGenerator do
  @moduledoc """
  Generates new fix recipes from observed patterns that have no existing recipe.

  When the pipeline encounters a pattern category with no matching recipe,
  RecipeGenerator can create a starter recipe based on:

  1. The pattern's category, severity, and description
  2. Similar patterns that DO have recipes (template transfer)
  3. The pattern's frequency across repos (high frequency = higher priority)

  Generated recipes start at low confidence (0.50) and are marked
  auto_fixable: false until a fix script is written and tested.
  They route through the "control" tier (report only) until
  confidence rises through successful manual fixes being recorded.

  This closes the gap between "we detected 1021 patterns" and
  "we have recipes for 43 of them."
  """

  require Logger

  alias Hypatia.RecipeMatcher

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

  @doc """
  Scan the pattern registry for categories with no matching recipe.
  Returns a list of {category, pattern_count, sample_patterns}.
  """
  def find_uncovered_categories do
    recipes = RecipeMatcher.all_recipes()

    # Build set of all categories covered by recipes
    covered = recipes
    |> Enum.flat_map(fn r -> Map.get(r, "target_categories", []) end)
    |> MapSet.new()

    # Load pattern registry
    registry_path = Path.join(Path.expand(@verisimdb_data_path), "patterns/registry.json")
    case File.read(registry_path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, registry} ->
            patterns = Map.get(registry, "patterns", %{}) |> Map.values()

            # Group by category and filter uncovered
            patterns
            |> Enum.group_by(fn p -> Map.get(p, "category", "unknown") end)
            |> Enum.reject(fn {cat, _} -> MapSet.member?(covered, cat) end)
            |> Enum.map(fn {cat, pats} ->
              {cat, length(pats), Enum.take(pats, 3)}
            end)
            |> Enum.sort_by(fn {_, count, _} -> -count end)

          _ -> []
        end
      _ -> []
    end
  end

  @doc """
  Generate a recipe for an uncovered category.

  The generated recipe:
  - Starts at confidence 0.50 (low — needs evidence to rise)
  - Is NOT auto_fixable (requires manual fix script development)
  - Routes to "control" tier (report only)
  - Includes remediation guidance derived from pattern descriptions

  Returns {:ok, recipe_path} or {:error, reason}.
  """
  def generate_recipe(category, sample_patterns) when is_list(sample_patterns) do
    # Derive recipe properties from the patterns
    severity = most_common_severity(sample_patterns)
    pa_rule = extract_pa_rule(sample_patterns)
    description = synthesize_description(category, sample_patterns)
    remediation = synthesize_remediation(category, sample_patterns)

    # Find a similar existing recipe to use as template
    template = find_similar_recipe(category)

    recipe = %{
      "license" => "PMPL-1.0-or-later",
      "id" => "recipe-#{slugify(category)}",
      "name" => "Auto-generated: #{humanize(category)}",
      "description" => description,
      "category" => category,
      "target_categories" => [category],
      "pa_rule" => pa_rule,
      "languages" => ["*"],
      "confidence" => 0.50,
      "auto_fixable" => false,
      "action" => Map.get(template, "action", "report"),
      "match" => Map.get(List.first(sample_patterns) || %{}, "description", category),
      "replacement" => remediation,
      "fix_script" => "none",
      "proven_module" => nil,
      "formally_proven" => false,
      "generated" => true,
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "pattern_count" => length(sample_patterns),
      "severity" => severity,
      "triangle_tier" => "control"
    }

    recipe_path = Path.join(
      [Path.expand(@verisimdb_data_path), "recipes", "recipe-#{slugify(category)}.json"]
    )

    case Jason.encode(recipe, pretty: true) do
      {:ok, json} ->
        File.write!(recipe_path, json <> "\n")
        Logger.info("Generated recipe: #{recipe["id"]} for category #{category} (#{length(sample_patterns)} patterns)")
        {:ok, recipe_path}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Generate recipes for ALL uncovered categories.
  Returns {generated_count, skipped_count, categories}.
  """
  def generate_all do
    uncovered = find_uncovered_categories()

    results = Enum.map(uncovered, fn {category, _count, samples} ->
      recipe_id = "recipe-#{slugify(category)}"
      existing = RecipeMatcher.get_recipe(recipe_id)

      if existing do
        {:skipped, category}
      else
        case generate_recipe(category, samples) do
          {:ok, _path} -> {:generated, category}
          {:error, _} -> {:error, category}
        end
      end
    end)

    generated = Enum.count(results, fn {status, _} -> status == :generated end)
    skipped = Enum.count(results, fn {status, _} -> status == :skipped end)
    categories = Enum.filter(results, fn {status, _} -> status == :generated end)
                 |> Enum.map(fn {_, cat} -> cat end)

    Logger.info("Recipe generation: #{generated} new, #{skipped} skipped")
    {generated, skipped, categories}
  end

  # --- Private helpers ---

  defp most_common_severity(patterns) do
    patterns
    |> Enum.map(fn p -> Map.get(p, "severity", "Medium") end)
    |> Enum.frequencies()
    |> Enum.max_by(fn {_, count} -> count end, fn -> {"Medium", 0} end)
    |> elem(0)
  end

  defp extract_pa_rule(patterns) do
    case List.first(patterns) do
      nil -> "PA000"
      pattern ->
        id = Map.get(pattern, "id", "")
        case Regex.run(~r/^(PA\d+)/, id) do
          [_, rule] -> rule
          _ -> "PA000"
        end
    end
  end

  defp synthesize_description(category, patterns) do
    count = length(patterns)
    repos = patterns
    |> Enum.flat_map(fn p -> Map.get(p, "repos_affected_list", []) end)
    |> Enum.uniq()
    |> length()

    "#{humanize(category)} — #{count} patterns across #{repos} repos. " <>
    "Auto-generated recipe for triage and tracking."
  end

  defp synthesize_remediation(category, patterns) do
    # Extract common keywords from pattern descriptions
    descriptions = Enum.map(patterns, fn p -> Map.get(p, "description", "") end)
    common_words = descriptions
    |> Enum.flat_map(&String.split(&1, ~r/\s+/))
    |> Enum.map(&String.downcase/1)
    |> Enum.reject(fn w -> String.length(w) < 4 end)
    |> Enum.frequencies()
    |> Enum.sort_by(fn {_, c} -> -c end)
    |> Enum.take(5)
    |> Enum.map(fn {w, _} -> w end)
    |> Enum.join(", ")

    "Review #{humanize(category)} findings. Common terms: #{common_words}. " <>
    "Create targeted fix script once remediation pattern is established."
  end

  defp find_similar_recipe(category) do
    # Try to find a recipe with a similar category name
    all = RecipeMatcher.all_recipes()
    category_lower = String.downcase(category)

    Enum.find(all, %{}, fn recipe ->
      cats = Map.get(recipe, "target_categories", [])
      Enum.any?(cats, fn c ->
        String.jaro_distance(String.downcase(c), category_lower) > 0.6
      end)
    end)
  end

  defp slugify(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/, "-")
    |> String.trim("-")
  end

  defp humanize(text) do
    text
    |> String.replace(~r/([A-Z])/, " \\1")
    |> String.replace(~r/[-_]+/, " ")
    |> String.trim()
  end
end
