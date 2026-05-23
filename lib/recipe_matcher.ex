# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.RecipeMatcher do
  @moduledoc """
  Matches patterns to fix recipes from verisim-data/recipes/.

  For a given pattern_id, finds applicable recipe(s), ranks by confidence
  score, and checks if proven module bindings exist for the repo's language.
  """

  require Logger

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

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
      langs_match?(Map.get(recipe, "languages", []), language)
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
  Load all recipe files from verisim-data/recipes/.
  Caches nothing -- always reads fresh from disk.
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

  @doc """
  Find the best recipe for a pattern map (with fuzzy matching).

  First tries exact pattern_id match. If that fails, falls back to
  fuzzy matching by PA rule prefix + description keyword overlap.
  This bridges fingerprinted registry IDs (e.g., PA009-potentially-unquoted-varia)
  to clean recipe IDs (e.g., PA009-shell-unquoted-var).
  """
  def best_recipe_for_pattern(pattern, language) when is_map(pattern) do
    pattern_id = Map.get(pattern, "id", "")

    case best_recipe(pattern_id, language) do
      nil ->
        # Try category-based match first (most reliable), then fuzzy
        case category_match_recipe(pattern, language) do
          nil -> fuzzy_match_recipe(pattern, language)
          recipe -> recipe
        end
      recipe -> recipe
    end
  end

  # --- Private ---

  # Match recipe by target_categories field -- most reliable match
  defp category_match_recipe(pattern, language) do
    category = Map.get(pattern, "category", "")
    effective_language = effective_language_for(pattern, language)

    all_recipes()
    |> Enum.filter(fn recipe ->
      cats = Map.get(recipe, "target_categories", [])
      lang_ok = langs_match?(Map.get(recipe, "languages", []), effective_language)
      lang_ok and category in cats
    end)
    |> Enum.sort_by(fn r -> Map.get(r, "confidence", 0) end, :desc)
    |> List.first()
  end

  defp fuzzy_match_recipe(pattern, language) do
    pa_rule = Map.get(pattern, "pa_rule", "")
    description = Map.get(pattern, "description", "") |> String.downcase()
    effective_language = effective_language_for(pattern, language)

    # Skip if no PA rule to match against
    if pa_rule == "" do
      nil
    else
      all_recipes()
      |> Enum.filter(fn recipe ->
        lang_ok = langs_match?(Map.get(recipe, "languages", []), effective_language)

        recipe_pattern_ids = Map.get(recipe, "pattern_ids", [])

        pa_match =
          Enum.any?(recipe_pattern_ids, fn rpid ->
            String.starts_with?(rpid, pa_rule <> "-")
          end)

        if lang_ok and pa_match do
          # Check keyword overlap between recipe pattern_ids and finding description
          recipe_keywords =
            recipe_pattern_ids
            |> Enum.flat_map(fn rpid ->
              rpid |> String.replace(~r/^PA\d+-/, "") |> String.split("-")
            end)
            |> Enum.reject(&(&1 == ""))

          keyword_hit_count =
            Enum.count(recipe_keywords, fn kw ->
              String.contains?(description, kw)
            end)

          keyword_hit_count > 0
        else
          false
        end
      end)
      |> Enum.sort_by(fn r -> Map.get(r, "confidence", 0) end, :desc)
      |> List.first()
    end
  end

  # Both "*" and "any" are language-agnostic sentinels. Historical recipes
  # use one or the other; treating them as synonyms keeps both groups
  # routable (without this, ~12 recipes declared "any" matched no patterns).
  defp langs_match?(langs, language) do
    "*" in langs or "any" in langs or language in langs
  end

  # Scorecard / workflow-file findings are about .github/workflows/*.yml,
  # not the repo's primary language. Without this remap, recipes declared
  # `languages: ["yaml"]` (pin-deps, token-permissions, etc.) never match
  # because no repo has yaml as its primary language, and every scorecard
  # finding falls through to :control "no safe fix available".
  defp effective_language_for(pattern, language) do
    cond do
      Map.get(pattern, "source") == "scorecard" -> "yaml"
      workflow_file_category?(Map.get(pattern, "category", "")) -> "yaml"
      true -> language
    end
  end

  defp workflow_file_category?(category) do
    category in [
      "DependencyPinning",
      "PinnedDependencies",
      "TokenPermissions",
      "DangerousWorkflow",
      "DependencyUpdateTool",
      "BranchProtection"
    ]
  end

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
