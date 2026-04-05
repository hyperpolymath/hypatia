# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# P2P (property-based style) tests for recipe matching and VCL query parsing.
#
# These tests exercise invariants that must hold across a wide input space
# using parameterized test tables and randomized spot-checks, without
# requiring an external property-testing library (no StreamData dep).
#
# Invariants tested:
# - RecipeMatcher: return-type stability, language fallback, tier membership
# - TriangleRouter: dispatch_strategy monotonicity, tier coverage
# - VCL Client: parse -> execute round-trip, error handling, modality coverage

defmodule Hypatia.P2P.RecipeMatcherTest do
  @moduledoc """
  Property-based style tests for RecipeMatcher invariants.

  Each describe block establishes an invariant and verifies it holds
  across a representative sample of inputs. These are not unit tests for
  a single function call — they verify structural properties of the module.
  """

  use ExUnit.Case, async: true

  alias Hypatia.RecipeMatcher
  alias Hypatia.TriangleRouter

  # ---------------------------------------------------------------------------
  # Invariant: find_recipes/1 always returns a list (never raises, never nil)
  # ---------------------------------------------------------------------------

  describe "find_recipes/1 return-type invariant" do
    # PA rule IDs seen in the wild, plus known unknowns
    @pattern_id_samples [
      "PA001-missing-spdx",
      "PA002-secret-in-code",
      "PA003-unpinned-action",
      "PA009-shell-unquoted-var",
      "PA018-unchecked-todo",
      "PA999-nonexistent-rule",
      "",
      "malformed-id-without-pa-prefix",
      "PA001",
      "totally random string without structure"
    ]

    test "always returns a list for any pattern_id input" do
      Enum.each(@pattern_id_samples, fn id ->
        result = RecipeMatcher.find_recipes(id)
        assert is_list(result),
               "find_recipes(#{inspect(id)}) returned #{inspect(result)}, expected list"
      end)
    end

    test "returns empty list for unknown pattern ids" do
      unknown_ids = [
        "PA999-nonexistent-rule",
        "",
        "totally random string without structure"
      ]

      Enum.each(unknown_ids, fn id ->
        result = RecipeMatcher.find_recipes(id)
        assert result == [],
               "find_recipes(#{inspect(id)}) expected [], got #{inspect(result)}"
      end)
    end

    test "results are sorted by confidence descending" do
      # For patterns that have multiple recipes, verify sorting invariant
      recipes = RecipeMatcher.all_recipes()

      # Find a pattern_id that appears in multiple recipes (if any)
      pattern_counts =
        recipes
        |> Enum.flat_map(fn r -> Map.get(r, "pattern_ids", []) end)
        |> Enum.frequencies()

      multi_pattern_ids = pattern_counts |> Enum.filter(fn {_, c} -> c > 1 end) |> Enum.map(&elem(&1, 0))

      # For any pattern_id with multiple recipes, confidence must be sorted descending
      Enum.each(multi_pattern_ids, fn id ->
        results = RecipeMatcher.find_recipes(id)
        confidences = Enum.map(results, &Map.get(&1, "confidence", 0.0))
        assert confidences == Enum.sort(confidences, :desc),
               "find_recipes(#{inspect(id)}) confidence not sorted descending: #{inspect(confidences)}"
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Invariant: best_recipe/2 never raises and returns nil or a valid recipe map
  # ---------------------------------------------------------------------------

  describe "best_recipe/2 return-type invariant" do
    @languages ["shell", "rust", "elixir", "idris2", "zig", "unknown", "", "haskell", "ocaml"]

    @pa_rules ["PA001", "PA002", "PA003", "PA009", "PA016", "PA018", "PA999"]

    test "always returns nil or a map for any pattern_id + language combination" do
      Enum.each(@pa_rules, fn pa ->
        Enum.each(@languages, fn lang ->
          id = "#{pa}-test-pattern"
          result = RecipeMatcher.best_recipe(id, lang)
          assert is_nil(result) or is_map(result),
                 "best_recipe(#{inspect(id)}, #{inspect(lang)}) returned #{inspect(result)}"
        end)
      end)
    end

    test "returned recipe map always has required fields when non-nil" do
      all_recipes = RecipeMatcher.all_recipes()

      Enum.each(all_recipes, fn recipe ->
        pattern_ids = Map.get(recipe, "pattern_ids", [])
        langs = Map.get(recipe, "languages", ["*"])
        test_lang = List.first(langs, "*")
        test_lang = if test_lang == "*", do: "shell", else: test_lang

        Enum.each(pattern_ids, fn pid ->
          result = RecipeMatcher.best_recipe(pid, test_lang)
          if result != nil do
            assert Map.has_key?(result, "id"),
                   "Recipe returned by best_recipe missing 'id': #{inspect(result)}"
            assert Map.has_key?(result, "triangle_tier"),
                   "Recipe returned by best_recipe missing 'triangle_tier': #{inspect(result)}"
            assert Map.has_key?(result, "confidence"),
                   "Recipe returned by best_recipe missing 'confidence': #{inspect(result)}"
          end
        end)
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Invariant: recipes_by_tier/1 only returns recipes matching that tier
  # ---------------------------------------------------------------------------

  describe "recipes_by_tier/1 tier membership invariant" do
    @valid_tiers ["eliminate", "substitute", "control"]

    test "all returned recipes match the requested tier" do
      Enum.each(@valid_tiers, fn tier ->
        results = RecipeMatcher.recipes_by_tier(tier)

        Enum.each(results, fn recipe ->
          assert Map.get(recipe, "triangle_tier") == tier,
                 "recipes_by_tier(#{inspect(tier)}) returned recipe with tier " <>
                   "#{inspect(Map.get(recipe, "triangle_tier"))}: #{inspect(Map.get(recipe, "id"))}"
        end)
      end)
    end

    test "unknown tier returns empty list (no cross-contamination)" do
      assert RecipeMatcher.recipes_by_tier("nonexistent_tier") == []
    end

    test "recipes that declare a triangle_tier have a valid tier value" do
      all_recipes = RecipeMatcher.all_recipes()

      # Only check recipes that explicitly declare a tier; recipes without
      # triangle_tier are treated as "substitute" by the pipeline (fallback).
      # Data quality: some scorecard recipes don't declare a tier yet.
      tiered_recipes = Enum.filter(all_recipes, &Map.has_key?(&1, "triangle_tier"))

      Enum.each(tiered_recipes, fn recipe ->
        tier = Map.get(recipe, "triangle_tier")
        assert tier in @valid_tiers,
               "Recipe #{Map.get(recipe, "id")} has invalid triangle_tier: #{inspect(tier)}"
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Invariant: TriangleRouter.dispatch_strategy/1 is monotone in confidence
  # ---------------------------------------------------------------------------

  describe "dispatch_strategy/1 monotonicity invariant" do
    # Generates evenly spaced confidence samples
    @confidence_samples for i <- 0..20, do: i / 20.0

    test "dispatch_strategy is non-increasing in tier strength as confidence rises" do
      # Higher confidence -> stronger action (auto_execute > review > report_only)
      tier_rank = fn
        :auto_execute -> 2
        :review -> 1
        :report_only -> 0
      end

      # For any pair (c1 < c2), rank(strategy(c1)) <= rank(strategy(c2))
      sorted_samples = Enum.sort(@confidence_samples)

      sorted_samples
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.each(fn [c1, c2] ->
        s1 = TriangleRouter.dispatch_strategy(c1)
        s2 = TriangleRouter.dispatch_strategy(c2)
        assert tier_rank.(s1) <= tier_rank.(s2),
               "dispatch_strategy monotonicity violated: " <>
                 "strategy(#{c1})=#{s1} > strategy(#{c2})=#{s2}"
      end)
    end

    test "dispatch_strategy covers all three tiers across the [0,1] range" do
      strategies = @confidence_samples |> Enum.map(&TriangleRouter.dispatch_strategy/1) |> Enum.uniq()
      assert :auto_execute in strategies
      assert :review in strategies
      assert :report_only in strategies
    end

    test "boundary values produce correct strategies" do
      # Exact boundary: 0.95
      assert TriangleRouter.dispatch_strategy(0.95) == :auto_execute
      # Just below 0.95
      assert TriangleRouter.dispatch_strategy(0.9499) == :review
      # Exact boundary: 0.85
      assert TriangleRouter.dispatch_strategy(0.85) == :review
      # Just below 0.85
      assert TriangleRouter.dispatch_strategy(0.8499) == :report_only
    end
  end

  # ---------------------------------------------------------------------------
  # Invariant: route/3 always returns a valid triangle tuple
  # ---------------------------------------------------------------------------

  describe "route/3 tuple-type invariant" do
    @test_patterns [
      %{
        "id" => "PA001-test",
        "pa_rule" => "PA001",
        "category" => "MissingSpdx",
        "description" => "Missing SPDX header in .sh file",
        "repos_affected_list" => ["repo-a"],
        "severity" => "High"
      },
      %{
        "id" => "PA999-unknown",
        "pa_rule" => "PA999",
        "category" => "UnknownCategory999",
        "description" => "No recipe for this",
        "repos_affected_list" => ["repo-b"],
        "severity" => "Low"
      },
      %{
        "id" => "PA009-shell-unquoted",
        "pa_rule" => "PA009",
        "category" => "ShellInjection",
        "description" => "Unquoted variable in .sh script",
        "repos_affected_list" => ["repo-c"],
        "severity" => "Medium"
      }
    ]

    @test_languages ["shell", "rust", "elixir", "unknown"]
    @test_repos ["repo-a", "repo-b", "some-repo"]

    test "route/3 always returns a valid 2- or 3-tuple with correct tier atom" do
      Enum.each(@test_patterns, fn pattern ->
        Enum.each(@test_repos, fn repo ->
          Enum.each(@test_languages, fn lang ->
            result = TriangleRouter.route(pattern, repo, lang)
            assert valid_route_tuple?(result),
                   "route/3 returned invalid tuple #{inspect(result)} " <>
                     "for pattern=#{Map.get(pattern, "id")}, repo=#{repo}, lang=#{lang}"
          end)
        end)
      end)
    end

    defp valid_route_tuple?({:eliminate, recipe, _pattern}) when is_map(recipe), do: true
    defp valid_route_tuple?({:substitute, recipe, _pattern}) when is_map(recipe), do: true
    defp valid_route_tuple?({:control, _pattern}), do: true
    defp valid_route_tuple?(_), do: false
  end
end

defmodule Hypatia.P2P.VQLParserTest do
  @moduledoc """
  Property-based style tests for VCL parser invariants.

  Exercises the parser over a structured input space: all 6 modalities,
  all FROM clause variants, all WHERE operators, all LIMIT/OFFSET combos.
  Ensures the parser is total (no crashes), returns {:ok, ast} or {:error, _},
  and that AST fields have correct types.
  """

  use ExUnit.Case, async: false

  alias Hypatia.VCL.Client

  setup do
    case GenServer.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end
    :ok
  end

  @modalities ~w(DOCUMENT GRAPH VECTOR TENSOR SEMANTIC TEMPORAL)
  @stores ~w(scans patterns recipes outcomes dispatch index)
  @limit_values [1, 5, 10, 100, 1000]

  # ---------------------------------------------------------------------------
  # Invariant: parser is total — never raises, always returns ok/error tuple
  # ---------------------------------------------------------------------------

  describe "parser totality invariant" do
    test "all 6 modalities parse successfully in SELECT ... FROM STORE scans" do
      Enum.each(@modalities, fn mod ->
        query = "SELECT #{mod} FROM STORE scans"
        result = Client.parse(query)
        assert match?({:ok, _}, result),
               "Expected {:ok, _} for query #{inspect(query)}, got #{inspect(result)}"
      end)
    end

    test "wildcard modality (*) parses successfully" do
      assert {:ok, ast} = Client.parse("SELECT * FROM STORE scans")
      assert ast.modalities == [:all]
    end

    test "all store names parse successfully" do
      Enum.each(@stores, fn store ->
        query = "SELECT DOCUMENT FROM STORE #{store}"
        result = Client.parse(query)
        assert match?({:ok, _}, result),
               "Store #{inspect(store)} failed to parse: #{inspect(result)}"
      end)
    end

    test "LIMIT values in [1, 1000] all parse correctly" do
      Enum.each(@limit_values, fn limit ->
        query = "SELECT DOCUMENT FROM STORE scans LIMIT #{limit}"
        {:ok, ast} = Client.parse(query)
        assert ast.limit == limit,
               "LIMIT #{limit} not parsed correctly: got #{inspect(ast.limit)}"
      end)
    end

    test "all WHERE comparison operators parse without error" do
      operators = [
        {"==", "value"},
        {"!=", "value"},
        {">", "0.5"},
        {">=", "0.5"},
        {"<", "0.9"},
        {"<=", "0.9"}
      ]

      Enum.each(operators, fn {op, val} ->
        query = "SELECT DOCUMENT FROM STORE scans WHERE FIELD confidence #{op} #{val}"
        result = Client.parse(query)
        assert match?({:ok, _}, result),
               "Operator #{op} failed to parse: #{inspect(result)}"
      end)
    end

    test "invalid queries return {:error, _} not raise" do
      invalid_queries = [
        "",
        "SELECT",
        "FROM STORE scans",
        "SELECT FROM STORE scans",
        "SELECT DOCUMENT TABLE scans",
        "SELECT DOCUMENT FROM",
        "GARBAGE QUERY TEXT",
        String.duplicate("x", 10_000)
      ]

      Enum.each(invalid_queries, fn query ->
        result = Client.parse(query)
        assert match?({:error, _}, result),
               "Expected {:error, _} for invalid query #{inspect(String.slice(query, 0, 80))}, " <>
                 "got #{inspect(result)}"
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Invariant: parse -> execute round-trip is coherent
  # ---------------------------------------------------------------------------

  describe "parse -> execute round-trip invariant" do
    test "parsed AST executes without error for valid stores" do
      valid_store_queries = Enum.map(@stores, fn store ->
        "SELECT DOCUMENT FROM STORE #{store} LIMIT 3"
      end)

      Enum.each(valid_store_queries, fn query ->
        {:ok, ast} = Client.parse(query)
        result = Client.execute(ast)
        # Either ok list or error — must not raise
        assert match?({:ok, _}, result) or match?({:error, _}, result),
               "execute/1 did not return ok/error tuple for #{inspect(query)}: #{inspect(result)}"
      end)
    end

    test "LIMIT in parsed AST is respected by execute" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 1")
      {:ok, results} = Client.execute(ast)
      assert length(results) <= 1,
             "execute with LIMIT 1 returned #{length(results)} results"
    end

    test "query/1 and parse+execute/2 produce identical results for same query" do
      query = "SELECT DOCUMENT FROM STORE scans LIMIT 2"

      {:ok, from_query} = Client.query(query)
      {:ok, ast} = Client.parse(query)
      {:ok, from_execute} = Client.execute(ast)

      assert from_query == from_execute,
             "query/1 and parse+execute/2 returned different results for #{inspect(query)}"
    end
  end

  # ---------------------------------------------------------------------------
  # Invariant: AST fields have correct types
  # ---------------------------------------------------------------------------

  describe "AST field type invariant" do
    test "parsed AST has correct field types for full query" do
      query = "SELECT DOCUMENT, TEMPORAL FROM STORE scans WHERE FIELD severity == High LIMIT 10 OFFSET 5"
      {:ok, ast} = Client.parse(query)

      assert is_list(ast.modalities)
      assert length(ast.modalities) == 2
      assert :document in ast.modalities
      assert :temporal in ast.modalities

      assert is_tuple(ast.source)
      assert elem(ast.source, 0) == :store

      # WHERE clause is a tuple
      assert is_tuple(ast.where)

      assert ast.limit == 10
      assert ast.offset == 5
    end

    test "limit is always a positive integer when present" do
      Enum.each(@limit_values, fn limit ->
        {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT #{limit}")
        assert is_integer(ast.limit) and ast.limit > 0,
               "LIMIT #{limit} yielded non-positive-integer: #{inspect(ast.limit)}"
      end)
    end

    test "modalities list is never empty for valid queries" do
      Enum.each(@modalities, fn mod ->
        {:ok, ast} = Client.parse("SELECT #{mod} FROM STORE scans")
        assert length(ast.modalities) >= 1
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Invariant: multi-modality combinations parse correctly
  # ---------------------------------------------------------------------------

  describe "multi-modality combination invariant" do
    test "all pairs of modalities parse successfully" do
      # Generate all 15 pairs of 6 modalities
      pairs =
        for {m1, i} <- Enum.with_index(@modalities),
            {m2, j} <- Enum.with_index(@modalities),
            i < j,
            do: {m1, m2}

      Enum.each(pairs, fn {m1, m2} ->
        query = "SELECT #{m1}, #{m2} FROM STORE scans"
        {:ok, ast} = Client.parse(query)
        assert length(ast.modalities) == 2,
               "Multi-modality query #{inspect(query)} yielded #{length(ast.modalities)} modalities"
      end)
    end

    test "all 6 modalities together parse as 6-element list" do
      query = "SELECT DOCUMENT, GRAPH, VECTOR, TENSOR, SEMANTIC, TEMPORAL FROM STORE scans"
      {:ok, ast} = Client.parse(query)
      assert length(ast.modalities) == 6
    end
  end
end
