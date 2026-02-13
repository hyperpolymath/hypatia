# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.FileExecutorTest do
  use ExUnit.Case, async: true

  alias Hypatia.VQL.FileExecutor

  # ---------------------------------------------------------------------------
  # Store Query Tests (real verisimdb-data)
  # ---------------------------------------------------------------------------

  describe "execute/2 — store queries" do
    test "loads scans from STORE" do
      ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 3

      Enum.each(results, fn scan ->
        assert is_map(scan)
        assert Map.has_key?(scan, "_source")
      end)
    end

    test "loads patterns from STORE" do
      ast = %{
        modalities: [:document],
        source: {:store, "patterns"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, [registry]} = FileExecutor.execute(ast)
      assert Map.has_key?(registry, "patterns")
      assert map_size(Map.get(registry, "patterns", %{})) >= 100
    end

    test "loads recipes from STORE" do
      ast = %{
        modalities: [:document],
        source: {:store, "recipes"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 4

      Enum.each(results, fn recipe ->
        assert is_map(recipe)
        assert Map.has_key?(recipe, "_source")
        assert String.starts_with?(recipe["_source"], "recipe-")
      end)
    end

    test "loads index from STORE" do
      ast = %{
        modalities: [:document],
        source: {:store, "index"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, [index]} = FileExecutor.execute(ast)
      assert Map.has_key?(index, "total_scans")
      assert Map.has_key?(index, "repos")
    end

    test "returns error for unknown store" do
      ast = %{
        modalities: [:document],
        source: {:store, "nonexistent"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      assert {:error, "Unknown store: nonexistent"} = FileExecutor.execute(ast)
    end
  end

  # ---------------------------------------------------------------------------
  # WHERE Clause Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — WHERE filtering" do
    test "FIELD equality filter" do
      ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: {:field, "_source", :eq, "echidna.json"},
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) == 1
      assert hd(results)["_source"] == "echidna.json"
    end

    test "FULLTEXT CONTAINS filter" do
      ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: {:fulltext, :contains, "weak_points"},
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      # All scan JSON files should contain "weak_points"
      assert length(results) >= 1
    end

    test "FULLTEXT MATCHES regex filter" do
      ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: {:fulltext, :matches, "echidna|verisimdb"},
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 2
    end

    test "AND conditions" do
      ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: {:and, [
          {:fulltext, :contains, "weak_points"},
          {:field, "_source", :eq, "echidna.json"}
        ]},
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) == 1
    end
  end

  # ---------------------------------------------------------------------------
  # Pagination Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — pagination" do
    test "LIMIT restricts result count" do
      ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: nil,
        proof: nil,
        limit: 2,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) <= 2
    end

    test "OFFSET skips results" do
      all_ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      offset_ast = %{all_ast | offset: 1}

      {:ok, all} = FileExecutor.execute(all_ast)
      {:ok, offset} = FileExecutor.execute(offset_ast)

      assert length(offset) == length(all) - 1
    end

    test "LIMIT and OFFSET together" do
      ast = %{
        modalities: [:document],
        source: {:store, "scans"},
        where: nil,
        proof: nil,
        limit: 1,
        offset: 1
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) == 1
    end
  end

  # ---------------------------------------------------------------------------
  # Hexad Query Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — HEXAD queries" do
    test "finds repo by name" do
      ast = %{
        modalities: [:document],
        source: {:hexad, "echidna"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "returns empty for unknown entity" do
      ast = %{
        modalities: [:document],
        source: {:hexad, "nonexistent-repo-xyz"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert results == []
    end
  end

  # ---------------------------------------------------------------------------
  # Federation Query Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — FEDERATION queries" do
    test "cross-store query returns results from multiple stores" do
      ast = %{
        modalities: [:document],
        source: {:federation, "/all/*", nil},
        where: nil,
        proof: nil,
        limit: 20,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "federation scoped to specific store" do
      ast = %{
        modalities: [:document],
        source: {:federation, "/scans/*", nil},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      }

      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 3
    end
  end
end
