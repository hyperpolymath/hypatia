# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.FileExecutorTest do
  @moduledoc """
  Comprehensive tests for the VQL FileExecutor module.

  Tests execute parsed VQL ASTs directly against verisimdb-data flat files,
  bypassing the Client GenServer. Covers store queries, WHERE filtering,
  pagination, HEXAD lookups, federation queries, and modality sorting.

  Uses async: true because FileExecutor is a stateless module (no GenServer).
  """

  use ExUnit.Case, async: true

  alias Hypatia.VQL.FileExecutor

  # ---------------------------------------------------------------------------
  # Helper — builds an AST map for concise test construction
  # ---------------------------------------------------------------------------

  defp make_ast(overrides \\ %{}) do
    Map.merge(
      %{
        modalities: [:document],
        source: {:store, "scans"},
        where: nil,
        proof: nil,
        limit: nil,
        offset: nil
      },
      overrides
    )
  end

  # ---------------------------------------------------------------------------
  # Store Query Tests (real verisimdb-data)
  # ---------------------------------------------------------------------------

  describe "execute/2 — STORE scans" do
    test "loads all scan files" do
      ast = make_ast()
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 3
    end

    test "each scan result is a map with _source metadata" do
      ast = make_ast()
      {:ok, results} = FileExecutor.execute(ast)

      Enum.each(results, fn scan ->
        assert is_map(scan)
        assert Map.has_key?(scan, "_source")
        assert String.ends_with?(scan["_source"], ".json")
      end)
    end

    test "scan files contain weak_points arrays" do
      ast = make_ast()
      {:ok, results} = FileExecutor.execute(ast)

      # At minimum, echidna and verisimdb have weak_points
      with_wp = Enum.filter(results, &Map.has_key?(&1, "weak_points"))
      assert length(with_wp) >= 1
    end
  end

  describe "execute/2 — STORE patterns" do
    test "loads pattern registry" do
      ast = make_ast(%{source: {:store, "patterns"}})
      {:ok, [registry]} = FileExecutor.execute(ast)
      assert Map.has_key?(registry, "patterns")
    end

    test "pattern registry has canonical patterns" do
      ast = make_ast(%{source: {:store, "patterns"}})
      {:ok, [registry]} = FileExecutor.execute(ast)
      patterns = Map.get(registry, "patterns", %{})
      assert map_size(patterns) >= 100
    end
  end

  describe "execute/2 — STORE recipes" do
    test "loads recipe files" do
      ast = make_ast(%{source: {:store, "recipes"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 4
    end

    test "recipe files have recipe- prefix in _source" do
      ast = make_ast(%{source: {:store, "recipes"}})
      {:ok, results} = FileExecutor.execute(ast)

      Enum.each(results, fn recipe ->
        assert is_map(recipe)
        assert String.starts_with?(recipe["_source"], "recipe-")
      end)
    end

    test "recipe files have required fields" do
      ast = make_ast(%{source: {:store, "recipes"}})
      {:ok, results} = FileExecutor.execute(ast)

      Enum.each(results, fn recipe ->
        assert Map.has_key?(recipe, "id"), "recipe missing 'id': #{inspect(recipe["_source"])}"
      end)
    end
  end

  describe "execute/2 — STORE index" do
    test "loads the master index" do
      ast = make_ast(%{source: {:store, "index"}})
      {:ok, [index]} = FileExecutor.execute(ast)
      assert Map.has_key?(index, "total_scans")
      assert Map.has_key?(index, "repos")
    end

    test "index total_scans matches actual scan file count" do
      ast = make_ast(%{source: {:store, "index"}})
      {:ok, [index]} = FileExecutor.execute(ast)

      scans_ast = make_ast(%{source: {:store, "scans"}})
      {:ok, scans} = FileExecutor.execute(scans_ast)

      # total_scans should match the number of scan JSON files
      assert index["total_scans"] == length(scans)
    end
  end

  describe "execute/2 — STORE outcomes" do
    test "loads JSONL outcome files" do
      ast = make_ast(%{source: {:store, "outcomes"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      # outcomes may be empty or populated depending on pipeline state
    end

    test "outcome records are maps (not raw strings)" do
      ast = make_ast(%{source: {:store, "outcomes"}})
      {:ok, results} = FileExecutor.execute(ast)

      Enum.each(results, fn outcome ->
        assert is_map(outcome)
      end)
    end
  end

  describe "execute/2 — STORE dispatch" do
    test "loads JSONL dispatch files" do
      ast = make_ast(%{source: {:store, "dispatch"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
    end
  end

  describe "execute/2 — unknown store" do
    test "returns error for unknown store name" do
      ast = make_ast(%{source: {:store, "nonexistent"}})
      assert {:error, "Unknown store: nonexistent"} = FileExecutor.execute(ast)
    end

    test "returns error for empty store name" do
      ast = make_ast(%{source: {:store, ""}})
      assert {:error, "Unknown store: "} = FileExecutor.execute(ast)
    end
  end

  # ---------------------------------------------------------------------------
  # WHERE Clause Tests — FIELD Conditions
  # ---------------------------------------------------------------------------

  describe "execute/2 — WHERE FIELD equality (:eq)" do
    test "filters scans by _source filename" do
      ast = make_ast(%{where: {:field, "_source", :eq, "echidna.json"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) == 1
      assert hd(results)["_source"] == "echidna.json"
    end

    test "returns empty list when no match" do
      ast = make_ast(%{where: {:field, "_source", :eq, "zzz-nonexistent.json"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert results == []
    end
  end

  describe "execute/2 — WHERE FIELD inequality (:neq)" do
    test "excludes matching records" do
      ast_all = make_ast()
      {:ok, all} = FileExecutor.execute(ast_all)

      ast_neq = make_ast(%{where: {:field, "_source", :neq, "echidna.json"}})
      {:ok, filtered} = FileExecutor.execute(ast_neq)

      assert length(filtered) == length(all) - 1
    end
  end

  describe "execute/2 — WHERE FIELD comparison operators" do
    test "greater-than (:gt) compares string values" do
      ast = make_ast(%{
        source: {:store, "scans"},
        where: {:field, "_source", :gt, "a"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      # All files starting with letters after "a" (alphabetically)
      assert is_list(results)
    end

    test "less-than (:lt) compares string values" do
      ast = make_ast(%{
        source: {:store, "scans"},
        where: {:field, "_source", :lt, "z"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) >= 1
    end
  end

  describe "execute/2 — WHERE FIELD :contains" do
    test "checks string containment" do
      ast = make_ast(%{
        where: {:field, "_source", :contains, "echidna"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) >= 1
    end
  end

  describe "execute/2 — WHERE FIELD :like" do
    test "case-insensitive substring match" do
      ast = make_ast(%{
        where: {:field, "_source", :like, "ECHIDNA"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) >= 1
    end
  end

  describe "execute/2 — WHERE FIELD :matches" do
    test "regex matching on field value" do
      ast = make_ast(%{
        where: {:field, "_source", :matches, "echidna|verisimdb"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) >= 1
    end

    test "invalid regex falls back gracefully" do
      # An invalid regex should cause :matches to return false rather than crash
      ast = make_ast(%{
        where: {:field, "_source", :matches, "[invalid"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
    end
  end

  # ---------------------------------------------------------------------------
  # WHERE Clause Tests — FULLTEXT Conditions
  # ---------------------------------------------------------------------------

  describe "execute/2 — WHERE FULLTEXT CONTAINS" do
    test "matches content across entire JSON serialization" do
      ast = make_ast(%{where: {:fulltext, :contains, "weak_points"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "case-insensitive fulltext search" do
      ast_lower = make_ast(%{where: {:fulltext, :contains, "weak_points"}})
      ast_upper = make_ast(%{where: {:fulltext, :contains, "WEAK_POINTS"}})

      {:ok, lower_results} = FileExecutor.execute(ast_lower)
      {:ok, upper_results} = FileExecutor.execute(ast_upper)

      assert length(lower_results) == length(upper_results)
    end

    test "returns empty for non-matching content" do
      ast = make_ast(%{where: {:fulltext, :contains, "zzz_nonexistent_content_xxyy"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert results == []
    end
  end

  describe "execute/2 — WHERE FULLTEXT MATCHES" do
    test "regex matching across serialized JSON" do
      ast = make_ast(%{where: {:fulltext, :matches, "echidna|verisimdb"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "invalid regex falls back to contains" do
      # The module falls back to :contains when regex compilation fails
      ast = make_ast(%{where: {:fulltext, :matches, "[invalid-regex"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
    end
  end

  # ---------------------------------------------------------------------------
  # WHERE Clause Tests — AND Conditions
  # ---------------------------------------------------------------------------

  describe "execute/2 — WHERE AND compound conditions" do
    test "both conditions must match" do
      ast = make_ast(%{
        where: {:and, [
          {:fulltext, :contains, "weak_points"},
          {:field, "_source", :eq, "echidna.json"}
        ]}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) == 1
      assert hd(results)["_source"] == "echidna.json"
    end

    test "no results when one condition is impossible" do
      ast = make_ast(%{
        where: {:and, [
          {:field, "_source", :eq, "echidna.json"},
          {:field, "_source", :eq, "verisimdb.json"}
        ]}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert results == []
    end

    test "multiple AND conditions progressively narrow results" do
      ast_one = make_ast(%{
        where: {:fulltext, :contains, "weak_points"}
      })
      {:ok, one_results} = FileExecutor.execute(ast_one)

      ast_two = make_ast(%{
        where: {:and, [
          {:fulltext, :contains, "weak_points"},
          {:field, "_source", :eq, "echidna.json"}
        ]}
      })
      {:ok, two_results} = FileExecutor.execute(ast_two)

      assert length(two_results) <= length(one_results)
    end
  end

  # ---------------------------------------------------------------------------
  # Deep Field Access Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — deep field access (dotted paths)" do
    test "WHERE on dotted field path accesses nested maps" do
      # Scans may have nested structures; this tests that deep_get works
      # Even if the path does not exist, it should not crash
      ast = make_ast(%{
        where: {:field, "nested.field.path", :eq, "value"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
    end
  end

  # ---------------------------------------------------------------------------
  # Modality Sorting Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — modality sorting" do
    test "TEMPORAL modality sorts by timestamp descending" do
      ast = make_ast(%{
        modalities: [:temporal],
        source: {:store, "outcomes"}
      })
      {:ok, results} = FileExecutor.execute(ast)

      if length(results) >= 2 do
        timestamps = Enum.map(results, fn item ->
          Map.get(item, "timestamp") || Map.get(item, "last_seen") ||
            Map.get(item, "last_scan") || ""
        end)

        # Check descending order
        pairs = Enum.zip(timestamps, Enum.drop(timestamps, 1))
        Enum.each(pairs, fn {a, b} ->
          assert a >= b, "Temporal sort not descending: #{a} < #{b}"
        end)
      end
    end

    test "SEMANTIC modality sorts by category" do
      ast = make_ast(%{
        modalities: [:semantic],
        source: {:store, "recipes"}
      })
      {:ok, results} = FileExecutor.execute(ast)

      if length(results) >= 2 do
        categories = Enum.map(results, fn item ->
          Map.get(item, "category") || ""
        end)

        # Check ascending alphabetical order
        assert categories == Enum.sort(categories)
      end
    end

    test "DOCUMENT modality preserves original order" do
      ast_doc = make_ast(%{modalities: [:document]})
      ast_none = make_ast(%{modalities: []})

      {:ok, doc_results} = FileExecutor.execute(ast_doc)
      {:ok, none_results} = FileExecutor.execute(ast_none)

      # Both should produce the same order (no sort applied)
      doc_sources = Enum.map(doc_results, & &1["_source"])
      none_sources = Enum.map(none_results, & &1["_source"])
      assert doc_sources == none_sources
    end
  end

  # ---------------------------------------------------------------------------
  # Pagination Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — LIMIT pagination" do
    test "LIMIT restricts result count" do
      ast = make_ast(%{limit: 2})
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) <= 2
    end

    test "LIMIT 1 returns exactly one result" do
      ast = make_ast(%{limit: 1})
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) == 1
    end

    test "LIMIT larger than total returns all results" do
      ast_all = make_ast()
      {:ok, all} = FileExecutor.execute(ast_all)

      ast_large = make_ast(%{limit: 99999})
      {:ok, large} = FileExecutor.execute(ast_large)

      assert length(large) == length(all)
    end
  end

  describe "execute/2 — OFFSET pagination" do
    test "OFFSET skips first N results" do
      ast_all = make_ast()
      {:ok, all} = FileExecutor.execute(ast_all)

      ast_offset = make_ast(%{offset: 1})
      {:ok, offset_results} = FileExecutor.execute(ast_offset)

      assert length(offset_results) == length(all) - 1
    end

    test "OFFSET equal to total returns empty" do
      ast_all = make_ast()
      {:ok, all} = FileExecutor.execute(ast_all)

      ast_offset = make_ast(%{offset: length(all)})
      {:ok, offset_results} = FileExecutor.execute(ast_offset)

      assert offset_results == []
    end

    test "OFFSET larger than total returns empty" do
      ast = make_ast(%{offset: 99999})
      {:ok, results} = FileExecutor.execute(ast)
      assert results == []
    end
  end

  describe "execute/2 — LIMIT and OFFSET combined" do
    test "LIMIT and OFFSET together window results" do
      ast = make_ast(%{limit: 1, offset: 1})
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) == 1
    end

    test "OFFSET + LIMIT acts as sliding window" do
      ast_all = make_ast()
      {:ok, all} = FileExecutor.execute(ast_all)

      if length(all) >= 3 do
        # First two
        ast_first = make_ast(%{limit: 2, offset: 0})
        {:ok, first_two} = FileExecutor.execute(ast_first)

        # Second and third
        ast_second = make_ast(%{limit: 2, offset: 1})
        {:ok, second_two} = FileExecutor.execute(ast_second)

        # The second result of the first page should be the first of the second
        assert Enum.at(first_two, 1)["_source"] == Enum.at(second_two, 0)["_source"]
      end
    end

    test "nil LIMIT and nil OFFSET returns all" do
      ast = make_ast(%{limit: nil, offset: nil})
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) >= 3
    end
  end

  # ---------------------------------------------------------------------------
  # HEXAD Query Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — HEXAD queries" do
    test "finds repo by name in scans directory" do
      ast = make_ast(%{source: {:hexad, "echidna"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "finds recipe by ID" do
      ast = make_ast(%{source: {:hexad, "recipe-shell-quote-vars"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "returns empty for unknown entity" do
      ast = make_ast(%{source: {:hexad, "nonexistent-repo-xyz"}})
      {:ok, results} = FileExecutor.execute(ast)
      assert results == []
    end

    test "HEXAD result respects WHERE clause" do
      ast = make_ast(%{
        source: {:hexad, "echidna"},
        where: {:fulltext, :contains, "weak_points"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) >= 1
    end

    test "HEXAD with non-matching WHERE returns empty" do
      ast = make_ast(%{
        source: {:hexad, "echidna"},
        where: {:fulltext, :contains, "zzz_absolutely_nonexistent_xxyy"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert results == []
    end
  end

  # ---------------------------------------------------------------------------
  # Federation Query Tests
  # ---------------------------------------------------------------------------

  describe "execute/2 — FEDERATION queries" do
    test "cross-store query returns results from multiple stores" do
      ast = make_ast(%{
        source: {:federation, "/all/*", nil},
        limit: 50
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "federation scoped to scans returns scan files" do
      ast = make_ast(%{source: {:federation, "/scans/*", nil}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 3
    end

    test "federation scoped to recipes returns recipe files" do
      ast = make_ast(%{source: {:federation, "/recipes/*", nil}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 4
    end

    test "federation scoped to patterns returns registry" do
      ast = make_ast(%{source: {:federation, "/patterns/*", nil}})
      {:ok, results} = FileExecutor.execute(ast)
      assert is_list(results)
      assert length(results) >= 1
    end

    test "federation with LIMIT caps results" do
      ast = make_ast(%{
        source: {:federation, "/scans/*", nil},
        limit: 2
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) <= 2
    end

    test "federation with WHERE filters results" do
      ast = make_ast(%{
        source: {:federation, "/scans/*", nil},
        where: {:field, "_source", :eq, "echidna.json"}
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) <= 1
    end
  end

  # ---------------------------------------------------------------------------
  # Integration: WHERE + Pagination + Modality Combined
  # ---------------------------------------------------------------------------

  describe "execute/2 — combined WHERE, LIMIT, and modality" do
    test "WHERE + LIMIT + OFFSET work together" do
      ast = make_ast(%{
        where: {:fulltext, :contains, "weak_points"},
        limit: 1,
        offset: 0
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) <= 1
    end

    test "TEMPORAL modality + LIMIT produces sorted limited results" do
      ast = make_ast(%{
        modalities: [:temporal],
        source: {:store, "outcomes"},
        limit: 5
      })
      {:ok, results} = FileExecutor.execute(ast)
      assert length(results) <= 5
    end
  end
end
