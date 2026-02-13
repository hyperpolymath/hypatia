# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.ClientTest do
  use ExUnit.Case, async: false

  alias Hypatia.VQL.Client

  setup do
    # Client may already be started by Application supervisor
    case GenServer.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end

    :ok
  end

  # ---------------------------------------------------------------------------
  # Parser Tests
  # ---------------------------------------------------------------------------

  describe "parse/1" do
    test "parses basic STORE query" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans")
      assert ast.modalities == [:document]
      assert ast.source == {:store, "scans"}
      assert ast.where == nil
      assert ast.proof == nil
      assert ast.limit == nil
      assert ast.offset == nil
    end

    test "parses multiple modalities with commas" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT, TEMPORAL FROM STORE outcomes")
      assert :document in ast.modalities
      assert :temporal in ast.modalities
      assert ast.source == {:store, "outcomes"}
    end

    test "parses multiple modalities with spaces" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT TEMPORAL FROM STORE outcomes")
      assert :document in ast.modalities
      assert :temporal in ast.modalities
    end

    test "parses wildcard modality" do
      {:ok, ast} = Client.parse("SELECT * FROM STORE scans")
      assert ast.modalities == [:all]
    end

    test "parses all six modalities" do
      {:ok, ast} = Client.parse("SELECT GRAPH, VECTOR, TENSOR, SEMANTIC, DOCUMENT, TEMPORAL FROM STORE scans")
      assert length(ast.modalities) == 6
      assert :graph in ast.modalities
      assert :vector in ast.modalities
      assert :tensor in ast.modalities
      assert :semantic in ast.modalities
      assert :document in ast.modalities
      assert :temporal in ast.modalities
    end

    test "parses HEXAD query" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM HEXAD echidna")
      assert ast.source == {:hexad, "echidna"}
    end

    test "parses FEDERATION query" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM FEDERATION /scans/*")
      assert {:federation, "/scans/*", nil} = ast.source
    end

    test "parses FEDERATION with drift policy" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM FEDERATION /scans/* WITH DRIFT STRICT")
      assert {:federation, "/scans/*", :strict} = ast.source
    end

    test "parses WHERE FIELD condition" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans WHERE FIELD category == PanicPath")
      assert {:field, "category", :eq, "PanicPath"} = ast.where
    end

    test "parses WHERE FULLTEXT CONTAINS" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans WHERE FULLTEXT CONTAINS \"unwrap\"")
      assert {:fulltext, :contains, "unwrap"} = ast.where
    end

    test "parses WHERE FULLTEXT MATCHES" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans WHERE FULLTEXT MATCHES \"panic.*unwrap\"")
      assert {:fulltext, :matches, "panic.*unwrap"} = ast.where
    end

    test "parses PROOF clause" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans PROOF EXISTENCE")
      assert %{raw: "EXISTENCE"} = ast.proof
    end

    test "parses LIMIT" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 5")
      assert ast.limit == 5
    end

    test "parses OFFSET" do
      {:ok, ast} = Client.parse("SELECT DOCUMENT FROM STORE scans LIMIT 10 OFFSET 20")
      assert ast.limit == 10
      assert ast.offset == 20
    end

    test "fails without SELECT" do
      assert {:error, "Expected SELECT"} = Client.parse("DOCUMENT FROM STORE scans")
    end

    test "fails without modalities" do
      assert {:error, "Expected at least one modality after SELECT"} = Client.parse("SELECT FROM STORE scans")
    end

    test "fails without FROM" do
      assert {:error, _} = Client.parse("SELECT DOCUMENT")
    end
  end

  # ---------------------------------------------------------------------------
  # Query Execution Tests (uses real verisimdb-data)
  # ---------------------------------------------------------------------------

  describe "query/1" do
    test "queries scans store" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE scans")
      assert is_list(results)
      assert length(results) >= 3
    end

    test "queries with LIMIT" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE scans LIMIT 2")
      assert length(results) <= 2
    end

    test "queries patterns store" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE patterns")
      assert is_list(results)
      assert length(results) >= 1
      assert Map.has_key?(hd(results), "patterns")
    end

    test "queries recipes store" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE recipes")
      assert is_list(results)
      assert length(results) >= 4
    end

    test "queries index store" do
      {:ok, results} = Client.query("SELECT DOCUMENT FROM STORE index")
      assert is_list(results)
      assert length(results) >= 1
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

  # ---------------------------------------------------------------------------
  # Cache Tests
  # ---------------------------------------------------------------------------

  describe "cache behavior" do
    test "repeated queries use cache" do
      {:ok, _} = Client.query("SELECT DOCUMENT FROM STORE scans LIMIT 1")
      {:ok, _} = Client.query("SELECT DOCUMENT FROM STORE scans LIMIT 1")
      %{cache_size: size} = Client.stats()
      assert size >= 1
    end

    test "stats returns query count" do
      {:ok, _} = Client.query("SELECT DOCUMENT FROM STORE scans LIMIT 1")
      %{query_count: count} = Client.stats()
      assert count >= 1
    end

    test "stats returns cache TTL" do
      %{cache_ttl_ms: ttl} = Client.stats()
      assert ttl == 60_000
    end
  end
end
