# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.RecipeMatcher do
  @moduledoc """
  Matches patterns to fix recipes from verisimdb-data/recipes/.

  For a given pattern_id, finds applicable recipe(s), ranks by confidence
  score, and checks if proven module bindings exist for the repo's language.
  """

  require Logger

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  @doc """
  Find all recipes matching a pattern_id.
  Returns a list of recipe maps sorted by confidence (highest first).
  """
  def find_recipes(pattern_id) do
    all_recipes()
    |> Enum.filter(fn recipe ->
      pattern_id in Map.get(recipe, "pattern_ids", [])
    end)
    |> Enum.sort_by(fn r -> Map.get(r, "confidence", 0) end, :desc)
  end

  @doc """
  Get the best recipe for a pattern+language combination.
  Returns the highest-confidence recipe whose languages list includes
  the given language (or "*" for any language).
  """
  def best_recipe(pattern_id, language) do
    find_recipes(pattern_id)
    |> Enum.find(fn recipe ->
      langs = Map.get(recipe, "languages", [])
      "*" in langs or language in langs
    end)
  end

  @doc """
  Get all recipes by triangle tier: "eliminate", "substitute", or "control".
  """
  def recipes_by_tier(tier) do
    all_recipes()
    |> Enum.filter(fn r -> Map.get(r, "triangle_tier") == tier end)
  end

  @doc """
  Get the current confidence score for a recipe.
  """
  def recipe_confidence(recipe_id) do
    case get_recipe(recipe_id) do
      nil -> 0.0
      recipe -> Map.get(recipe, "confidence", 0.0)
    end
  end

  @doc """
  Get a single recipe by its ID.
  """
  def get_recipe(recipe_id) do
    all_recipes()
    |> Enum.find(fn r -> Map.get(r, "id") == recipe_id end)
  end

  @doc """
  Load all recipe files from verisimdb-data/recipes/.
  Caches nothing — always reads fresh from disk.
  """
  def all_recipes do
    recipes_dir = Path.join(Path.expand(@verisimdb_data_path), "recipes")

    case File.ls(recipes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(fn f ->
          String.starts_with?(f, "recipe-") and String.ends_with?(f, ".json")
        end)
        |> Enum.map(fn f -> load_recipe(Path.join(recipes_dir, f)) end)
        |> Enum.reject(&is_nil/1)

      {:error, reason} ->
        Logger.error("Failed to read recipes directory: #{inspect(reason)}")
        []
    end
  end

  @doc """
  Load the proven-substitutions mapping.
  Returns list of substitution entries with pa_rule, category, proven_modules, etc.
  """
  def load_substitutions do
    path = Path.join(Path.expand(@verisimdb_data_path), "recipes/proven-substitutions.json")

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> Map.get(data, "substitutions", [])
          {:error, _} -> []
        end

      {:error, _} ->
        []
    end
  end

  @doc """
  Find the proven substitution entry for a given category.
  Returns nil if no substitution exists.
  """
  def substitution_for_category(category) do
    load_substitutions()
    |> Enum.find(fn s -> Map.get(s, "category") == category end)
  end

  # --- Private ---

  defp load_recipe(path) do
    with {:ok, content} <- File.read(path),
         {:ok, data} <- Jason.decode(content) do
      data
    else
      {:error, reason} ->
        Logger.error("Failed to load recipe #{path}: #{inspect(reason)}")
        nil
    end
  end
end
