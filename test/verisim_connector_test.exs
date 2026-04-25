# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.VerisimConnectorTest do
  use ExUnit.Case, async: true
  @moduletag :verisim_data

  # JSONL and large JSON loading via Jason.Decoder is slow on big data files.
  # Allow 120s per test instead of the default 60s.
  @moduletag timeout: 120_000

  alias Hypatia.VerisimConnector

  describe "fetch_all_scans/0" do
    test "loads scan files from verisim-data/scans/" do
      scans = VerisimConnector.fetch_all_scans()
      assert is_list(scans)
      assert length(scans) >= 3

      # Each scan should have repo and scan fields
      Enum.each(scans, fn scan ->
        assert Map.has_key?(scan, :repo)
        assert Map.has_key?(scan, :scan)
      end)
    end

    test "scan data includes weak_points" do
      scans = VerisimConnector.fetch_all_scans()
      # At least one scan should have weak points
      has_wp = Enum.any?(scans, fn scan ->
        wp = Map.get(scan.scan, "weak_points", [])
        length(wp) > 0
      end)

      assert has_wp
    end
  end

  describe "fetch_pattern_registry/0" do
    test "loads pattern registry JSON" do
      {:ok, registry} = VerisimConnector.fetch_pattern_registry()
      assert is_map(registry)
      assert Map.has_key?(registry, "patterns")
      assert map_size(Map.get(registry, "patterns", %{})) >= 100
    end
  end

  describe "fetch_all_recipes/0" do
    test "loads recipe files" do
      recipes = VerisimConnector.fetch_all_recipes()
      assert is_list(recipes)
      assert length(recipes) >= 4
    end
  end

  describe "fetch_substitutions/0" do
    test "loads proven-substitutions.json" do
      subs = VerisimConnector.fetch_substitutions()
      assert is_list(subs)
      assert length(subs) == 20
    end
  end

  describe "fetch_index/0" do
    test "loads master index" do
      {:ok, index} = VerisimConnector.fetch_index()
      assert is_map(index)
      assert Map.has_key?(index, "total_scans")
      assert Map.has_key?(index, "repos")
    end
  end

end
