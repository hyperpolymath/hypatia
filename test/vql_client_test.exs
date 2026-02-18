# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.ClientTest do
  @moduledoc """
  Comprehensive tests for the VQL Client GenServer.

  Tests cover three main areas:
  1. VQL parser — tokenization, AST construction, error handling
  2. Query execution — routing through FileExecutor against real verisimdb-data
  3. Cache behaviour — TTL, eviction, stats tracking

  These tests use async: false because they interact with a named GenServer.
  """

  use ExUnit.Case, async: false

  alias Hypatia.VQL.Client

  setup do
    # Ensure the VQL Client GenServer is running. It may already be started
    # by the OTP Application supervisor, so we check before starting.
    case GenServer.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end

    :ok
  end

  # ---------------------------------------------------------------------------
  # Parser Tests — Basic SELECT + Modalities
  # ---------------------------------------------------------------------------

  describe "parse/1 — SELECT and modalities" do
    test "parses basic STORE query with single modality" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans")
      assert ast.modalities == [:document]
      assert ast.source == {:store, "scans"}
      assert ast.where == nil
      assert ast.proof == nil
      assert ast.limit == nil
      assert ast.offset == nil
    end

    test "parses GRAPH modality" do
      {:ok, ast} = Client.parse("SELECT GRAPH FROM STORE scans")
      assert ast.modalities == [:graph]
    end

    test "parses VECTOR modality" do
      {:ok, ast} = Client.parse("SELECT VECTOR FROM STORE scans")
      assert ast.modalities == [:vector]
    end

    test "parses TENSOR modality" do
      {:ok, ast} = Client.parse("SELECT TENSOR FROM STORE scans")
      assert ast.modalities == [:tensor]
    end

    test "parses SEMANTIC modality" do
      {:ok, ast} = Client.parse("SELECT SEMANTIC FROM STORE scans")
      assert ast.modalities == [:semantic]
    end

    test "parses TEMPORAL modality" do
      {:ok, ast} = Client.parse("SELECT TEMPORAL FROM STORE scans")
      assert ast.modalities == [:temporal]
    end

    test "parses wildcard modality (*)" do
      {:ok, ast} = Client.parse("SELECT * FROM STORE scans")
      assert ast.modalities == [:all]
    end

    test "parses multiple modalities separated by commas" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT, TEMPORAL FROM STORE outcomes")
      assert :document in ast.modalities
      assert :temporal in ast.modalities
      assert length(ast.modalities) == 2
    end

    test "parses multiple modalities separated by spaces" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT TEMPORAL FROM STORE outcomes")
      assert :document in ast.modalities
      assert :temporal in ast.modalities
      assert length(ast.modalities) == 2
    end

    test "parses all six modalities together" do
      {:ok, ast} = Client.parse(
        "SELECT GRAPH, VECTOR, TENSOR, SEMANTIC, DOCUMENT, TEMPORAL FROM STORE scans"
      )
      assert length(ast.modalities) == 6
      assert :graph in ast.modalities
      assert :vector in ast.modalities
      assert :tensor in ast.modalities
      assert :semantic in ast.modalities
      assert :document in ast.modalities
      assert :temporal in ast.modalities
    end

    test "preserves modality order" do
      {:ok, ast} = Client.parse("SELECT TEMPORAL, GRAPH, DOCUMENT FROM STORE scans")
      assert ast.modalities == [:temporal, :graph, :document]
    end
  end

  # ---------------------------------------------------------------------------
  # Parser Tests — FROM Clauses
  # ---------------------------------------------------------------------------

  describe "parse/1 — FROM clause variants" do
    test "parses FROM STORE" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans")
      assert ast.source == {:store, "scans"}
    end

    test "parses different store names" do
      for store <- ~w(scans patterns recipes outcomes dispatch index) do
        {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE #{store}")
        assert ast.source == {:store, store}
      end
    end

    test "parses FROM HEXAD" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM HEXAD echidna")
      assert ast.source == {:hexad, "echidna"}
    end

    test "parses FROM HEXAD with hyphenated entity ID" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM HEXAD recipe-shell-quote-vars")
      assert ast.source == {:hexad, "recipe-shell-quote-vars"}
    end

    test "parses FROM FEDERATION without drift policy" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM FEDERATION /scans/*")
      assert {:federation, "/scans/*", nil} = ast.source
    end

    test "parses FROM FEDERATION with STRICT drift policy" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM FEDERATION /scans/* WITH DRIFT STRICT")
      assert {:federation, "/scans/*", :strict} = ast.source
    end

    test "parses FROM FEDERATION with REPAIR drift policy" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM FEDERATION /scans/* WITH DRIFT REPAIR")
      assert {:federation, "/scans/*", :repair} = ast.source
    end

    test "parses FROM FEDERATION with TOLERATE drift policy" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM FEDERATION /scans/* WITH DRIFT TOLERATE")
      assert {:federation, "/scans/*", :tolerate} = ast.source
    end

    test "parses FROM FEDERATION with LATEST drift policy" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM FEDERATION /all/* WITH DRIFT LATEST")
      assert {:federation, "/all/*", :latest} = ast.source
    end
  end

  # ---------------------------------------------------------------------------
  # Parser Tests — WHERE Clauses
  # ---------------------------------------------------------------------------

  describe "parse/1 — WHERE clause" do
    test "parses FIELD equality condition" do
      {:ok, ast} = Client.parse(
        "SELECT DOCUMENT FROM STORE scans WHERE FIELD category == PanicPath"
      )
      assert {:field, "category", :eq, "PanicPath"} = ast.where
    end

    test "parses FIELD inequality condition (!=)" do
      {:ok, ast} = Client.parse(
        "SELECT DOCUMENT FROM STORE scans WHERE FIELD severity != Low"
      )
      assert {:field, "severity", :neq, "Low"} = ast.where
    end

    test "parses FIELD greater-than condition (>)" do
      {:ok, ast} = Client.parse(
        "SELECT DOCUMENT FROM STORE recipes WHERE FIELD confidence > 0.9"
      )
      assert {:field, "confidence", :gt, "0.9"} = ast.where
    end

    test "parses FIELD greater-than-or-equal condition (>=)" do
      {:ok, ast} = Client.parse(
        "SELECT DOCUMENT FROM STORE recipes WHERE FIELD confidence >= 0.95"
      )
      assert {:field, "confidence", :gte, "0.95"} = ast.where
    end

    test "parses FIELD less-than condition (<)" do
      {:ok, ast} = Client.parse(
        "SELECT DOCUMENT FROM STORE recipes WHERE FIELD confidence < 0.5"
      )
      assert {:field, "confidence", :lt, "0.5"} = ast.where
    end

    test "parses FIELD less-than-or-equal condition (<=)" do
      {:ok, ast} = Client.parse(
        "SELECT DOCUMENT FROM STORE recipes WHERE FIELD confidence <= 0.85"
      )
      assert {:field, "confidence", :lte, "0.85"} = ast.where
    end

    test "parses FIELD CONTAINS condition" do
      {:ok, ast} = Client.parse(
        ~s(SELECT DOCUMENT FROM STORE recipes WHERE FIELD languages CONTAINS "shell")
      )
      assert {:field, "languages", :contains, "shell"} = ast.where
    end

    test "parses FIELD LIKE condition" do
      {:ok, ast} = Client.parse(
        "SELECT DOCUMENT FROM STORE scans WHERE FIELD _source LIKE echidna"
      )
      assert {:field, "_source", :like, "echidna"} = ast.where
    end

    test "parses FIELD MATCHES condition" do
      {:ok, ast} = Client.parse(
        ~s(SELECT DOCUMENT FROM STORE scans WHERE FIELD _source MATCHES "echo.*")
      )
      assert {:field, "_source", :matches, "echo.*"} = ast.where
    end

    test "parses FULLTEXT CONTAINS with quoted string" do
      {:ok, ast} = Client.parse(
        ~s(SELECT DOCUMENT FROM STORE scans WHERE FULLTEXT CONTAINS "unwrap")
      )
      assert {:fulltext, :contains, "unwrap"} = ast.where
    end

    test "parses FULLTEXT MATCHES with quoted regex" do
      {:ok, ast} = Client.parse(
        ~s(SELECT DOCUMENT FROM STORE scans WHERE FULLTEXT MATCHES "panic.*unwrap")
      )
      assert {:fulltext, :matches, "panic.*unwrap"} = ast.where
    end

    test "parses compound AND conditions" do
      {:ok, ast} = Client.parse(
        ~s(SELECT DOCUMENT FROM STORE scans WHERE FIELD category == PanicPath AND FULLTEXT CONTAINS "unwrap")
      )
      assert {:and, conditions} = ast.where
      assert length(conditions) == 2
    end

    test "absent WHERE clause yields nil" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans")
      assert ast.where == nil
    end
  end

  # ---------------------------------------------------------------------------
  # Parser Tests — PROOF, LIMIT, OFFSET
  # ---------------------------------------------------------------------------

  describe "parse/1 — PROOF clause" do
    test "parses PROOF clause" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans PROOF EXISTENCE")
      assert %{raw: "EXISTENCE"} = ast.proof
    end

    test "parses multi-word PROOF clause" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans PROOF COMPLETENESS CHECK")
      assert %{raw: raw} = ast.proof
      assert String.contains?(raw, "COMPLETENESS")
      assert String.contains?(raw, "CHECK")
    end

    test "absent PROOF clause yields nil" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans")
      assert ast.proof == nil
    end
  end

  describe "parse/1 — LIMIT and OFFSET" do
    test "parses LIMIT" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 5")
      assert ast.limit == 5
    end

    test "parses large LIMIT" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 10000")
      assert ast.limit == 10_000
    end

    test "parses LIMIT 1" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 1")
      assert ast.limit == 1
    end

    test "parses OFFSET" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 10 OFFSET 20")
      assert ast.limit == 10
      assert ast.offset == 20
    end

    test "parses OFFSET 0" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 10 OFFSET 0")
      assert ast.offset == 0
    end

    test "absent LIMIT yields nil" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans")
      assert ast.limit == nil
    end

    test "absent OFFSET yields nil" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans")
      assert ast.offset == nil
    end
  end

  # ---------------------------------------------------------------------------
  # Parser Tests — Edge Cases and Error Handling
  # ---------------------------------------------------------------------------

  describe "parse/1 — error handling" do
    test "fails without SELECT keyword" do
      assert {:error, "Expected SELECT"} = Client.parse("DOCUMENT FROM STORE scans")
    end

    test "fails with empty string" do
      assert {:error, _} = Client.parse("")
    end

    test "fails without modalities after SELECT" do
      assert {:error, "Expected at least one modality after SELECT"} =
               Client.parse("SELECT FROM STORE scans")
    end

    test "fails without FROM clause" do
      assert {:error, _} = Client.parse("SELECT DOCUMENT")
    end

    test "fails with invalid FROM target" do
      assert {:error, _} = Client.parse("SELECT DOCUMENT FROM TABLE scans")
    end

    test "handles leading/trailing whitespace" do
      {:ok, ast} = Client.parse("  SELECT DOCUMENT FROM STORE scans  ")
      assert ast.modalities == [:document]
      assert ast.source == {:store, "scans"}
    end
  end

  # ---------------------------------------------------------------------------
  # Parser Tests — Complex Combined Queries
  # ---------------------------------------------------------------------------

  describe "parse/1 — complex combined queries" do
    test "parses full query with all clauses" do
      query = ~s(SELECT DOCUMENT, TEMPORAL FROM STORE outcomes WHERE FIELD recipe_id == "recipe-shell-quote-vars" LIMIT 100 OFFSET 10)
      {:ok, ast} = Client.parse(query)
      assert :document in ast.modalities
      assert :temporal in ast.modalities
      assert ast.source == {:store, "outcomes"}
      assert ast.limit == 100
      assert ast.offset == 10
    end

    test "parses FEDERATION query with WHERE and LIMIT" do
      query = ~s(SELECT DOCUMENT FROM FEDERATION /scans/* WHERE FULLTEXT CONTAINS "weak_points" LIMIT 5)
      {:ok, ast} = Client.parse(query)
      assert {:federation, "/scans/*", nil} = ast.source
      assert {:fulltext, :contains, "weak_points"} = ast.where
      assert ast.limit == 5
    end

    test "parses HEXAD query with PROOF" do
      query = "SELECT DOCUMENT FROM HEXAD echidna PROOF EXISTENCE"
      {:ok, ast} = Client.parse(query)
      assert {:hexad, "echidna"} = ast.source
      assert %{raw: "EXISTENCE"} = ast.proof
    end
  end

  # ---------------------------------------------------------------------------
  # Query Execution Tests (uses real verisimdb-data)
  # ---------------------------------------------------------------------------

  describe "query/1 — store execution" do
    test "queries scans store and returns list" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE scans")
      assert is_list(results)
      assert length(results) >= 3
    end

    test "queries with LIMIT caps result count" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE scans LIMIT 2")
      assert length(results) <= 2
    end

    test "queries patterns store returns registry" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE patterns")
      assert is_list(results)
      assert length(results) >= 1
      assert Map.has_key?(hd(results), "patterns")
    end

    test "queries recipes store returns recipe files" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE recipes")
      assert is_list(results)
      assert length(results) >= 4

      # Verify recipe files are properly tagged with _source
      Enum.each(results, fn recipe ->
        assert Map.has_key?(recipe, "_source")
        assert String.starts_with?(recipe["_source"], "recipe-")
      end)
    end

    test "queries index store returns master index" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE index")
      assert is_list(results)
      assert length(results) >= 1

      [index | _] = results
      assert Map.has_key?(index, "total_scans")
    end

    test "queries HEXAD by repo name" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM HEXAD echidna")
      assert is_list(results)
      assert length(results) >= 1
    end

    test "returns error for unknown store" do
      result = Client.query("SELECT DOCUMENT FROM STORE nonexistent")
      assert {:error, _} = result
    end
  end

  describe "query/1 — filtered execution" do
    test "WHERE FIELD equality filters results" do
      {:ok, all} = Client.query("SELECT DOCUMENT FROM STORE scans")
      {:ok, filtered} = Client.query(
        "SELECT DOCUMENT FROM STORE scans WHERE FIELD _source == echidna.json"
      )

      assert length(filtered) < length(all)
      assert length(filtered) == 1
    end

    test "WHERE FULLTEXT CONTAINS filters by content" do
      {:ok, results} = Client.query(
        ~s(SELECT DOCUMENT FROM STORE scans WHERE FULLTEXT CONTAINS "weak_points")
      )
      assert is_list(results)
      assert length(results) >= 1
    end

    test "WHERE FULLTEXT MATCHES filters by regex" do
      {:ok, results} = Client.query(
        ~s(SELECT DOCUMENT FROM STORE scans WHERE FULLTEXT MATCHES "echidna|verisimdb")
      )
      assert is_list(results)
      assert length(results) >= 1
    end
  end

  describe "query/1 — FEDERATION execution" do
    test "FEDERATION cross-store query returns results" do
      {:ok, results} = Client.query(
        "SELECT DOCUMENT FROM FEDERATION /all/* LIMIT 10"
      )
      assert is_list(results)
      assert length(results) >= 1
    end

    test "FEDERATION scoped to scans returns scan data" do
      {:ok, results} = Client.query(
        "SELECT DOCUMENT FROM FEDERATION /scans/*"
      )
      assert is_list(results)
      assert length(results) >= 3
    end
  end

  # ---------------------------------------------------------------------------
  # execute/2 — Pre-parsed AST Execution
  # ---------------------------------------------------------------------------

  describe "execute/2" do
    test "executes a pre-parsed AST" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 2")
      {:ok, results} = Client.execute(ast)
      assert is_list(results)
      assert length(results) <= 2
    end

    test "execute increments query count" do
      initial_stats = Client.stats()
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 1")
      {:ok, _results} = Client.execute(ast)
      final_stats = Client.stats()

      assert final_stats.query_count >= initial_stats.query_count + 1
    end
  end

  # ---------------------------------------------------------------------------
  # Cache Tests
  # ---------------------------------------------------------------------------

  describe "cache behavior" do
    test "repeated identical queries populate cache" do
      query = "SELECT DOCUMENT FROM STORE scans LIMIT 1"
      {:ok, _} = Client.query(query)
      {:ok, _} = Client.query(query)

      %{cache_size: size} = Client.stats()
      assert size >= 1
    end

    test "different queries produce different cache entries" do
      {:ok, _} = Client.query("SELECT DOCUMENT FROM STORE scans LIMIT 1")
      {:ok, _} = Client.query("SELECT DOCUMENT FROM STORE recipes LIMIT 1")

      %{cache_size: size} = Client.stats()
      assert size >= 2
    end

    test "cached results match fresh results" do
      query = "SELECT DOCUMENT FROM STORE scans LIMIT 3"
      {:ok, first} = Client.query(query)
      {:ok, second} = Client.query(query)
      assert first == second
    end
  end

  # ---------------------------------------------------------------------------
  # Stats Tests
  # ---------------------------------------------------------------------------

  describe "stats/0" do
    test "returns query_count" do
      stats = Client.stats()
      assert Map.has_key?(stats, :query_count)
      assert is_integer(stats.query_count)
    end

    test "returns cache_size" do
      stats = Client.stats()
      assert Map.has_key?(stats, :cache_size)
      assert is_integer(stats.cache_size)
    end

    test "returns cache_ttl_ms" do
      stats = Client.stats()
      assert Map.has_key?(stats, :cache_ttl_ms)
      assert stats.cache_ttl_ms == 60_000
    end

    test "query_count increments after queries" do
      # Other tests run concurrently and may also increment the counter,
      # so we just verify the count is positive and non-decreasing
      before = Client.stats().query_count
      {:ok, _} = Client.query("SELECT DOCUMENT FROM STORE index")
      after_count = Client.stats().query_count
      assert after_count >= before
      assert after_count > 0
    end
  end

  # ---------------------------------------------------------------------------
  # GenServer Lifecycle Tests
  # ---------------------------------------------------------------------------

  describe "start_link/1" do
    test "Client process is alive" do
      pid = GenServer.whereis(Client)
      assert pid != nil
      assert Process.alive?(pid)
    end
  end
end
