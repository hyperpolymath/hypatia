# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.CrossRepoLearningTest do
  use ExUnit.Case, async: true

  alias Hypatia.CrossRepoLearning

  # Cross-repo learning tests that don't require running GenServers
  # or real outcome data. Tests the pure logic functions.

  @moduletag timeout: 120_000

  describe "drift_discount/3" do
    test "conservative policy returns 0.5" do
      assert CrossRepoLearning.drift_discount(:conservative, "repo-a", "repo-b") == 0.5
    end

    test "moderate policy returns 0.75" do
      assert CrossRepoLearning.drift_discount(:moderate, "repo-a", "repo-b") == 0.75
    end

    test "aggressive policy returns 1.0" do
      assert CrossRepoLearning.drift_discount(:aggressive, "repo-a", "repo-b") == 1.0
    end

    test "language_aware returns value between 0 and 1" do
      discount = CrossRepoLearning.drift_discount(:language_aware, "repo-a", "repo-b")
      assert discount >= 0.0
      assert discount <= 1.0
    end
  end

  describe "available_policies/0" do
    test "returns all four policies" do
      policies = CrossRepoLearning.available_policies()

      assert :conservative in policies
      assert :moderate in policies
      assert :aggressive in policies
      assert :language_aware in policies
    end
  end

  describe "language_families/0" do
    test "contains expected language groupings" do
      families = CrossRepoLearning.language_families()

      assert families["rust"] == :systems
      assert families["zig"] == :systems
      assert families["rescript"] == :ml_family
      assert families["elixir"] == :beam
      assert families["gleam"] == :beam
      assert families["idris"] == :ml_family
    end

    test "all values are atoms" do
      families = CrossRepoLearning.language_families()

      Enum.each(families, fn {_lang, family} ->
        assert is_atom(family)
      end)
    end
  end

  describe "repos_with_fix/1" do
    test "returns empty list for nonexistent recipe" do
      result = CrossRepoLearning.repos_with_fix("recipe-does-not-exist-xyzzy")
      assert result == []
    end
  end

  describe "detect_ecosystem_anomalies/0" do
    test "returns a list" do
      result = CrossRepoLearning.detect_ecosystem_anomalies()
      assert is_list(result)
    end
  end

  describe "repo_confidence/3" do
    test "returns structured result for unknown recipe/repo" do
      result =
        CrossRepoLearning.repo_confidence(
          "recipe-nonexistent-xyzzy",
          "repo-nonexistent",
          :moderate
        )

      assert is_map(result)
      assert result.repo == "repo-nonexistent"
      assert result.recipe_id == "recipe-nonexistent-xyzzy"
      assert result.local_outcomes == 0
      assert is_float(result.blended_confidence)
    end

    test "supports all drift policies" do
      for policy <- [:conservative, :moderate, :aggressive, :language_aware] do
        result =
          CrossRepoLearning.repo_confidence(
            "recipe-nonexistent-xyzzy",
            "repo-nonexistent",
            policy
          )

        assert is_map(result)
        assert is_float(result.blended_confidence)
      end
    end
  end

  describe "primary_language_from_scan_data/1 (#676 determinism)" do
    test "declared primary_language wins over counts" do
      data = %{"primary_language" => "Idris", "languages" => %{"rust" => 99}}
      assert CrossRepoLearning.primary_language_from_scan_data(data) == "idris"
    end

    test "higher count wins regardless of priority (both directions)" do
      rust_dominant = %{"languages" => %{"rust" => 5, "idris" => 4}}
      idris_dominant = %{"languages" => %{"rust" => 4, "idris" => 5}}

      assert CrossRepoLearning.primary_language_from_scan_data(rust_dominant) == "rust"
      assert CrossRepoLearning.primary_language_from_scan_data(idris_dominant) == "idris"
    end

    test "equal counts break ties by fixed language priority, not enumeration order" do
      # DISCRIMINATING TEST: before the #676 fix this resolved via
      # Enum.max_by/3 — ties fell to map enumeration order ("idris" first
      # in term order), not the documented priority. Fails before, passes
      # after.
      tie = %{"languages" => %{"idris" => 10, "rust" => 10, "zig" => 10}}
      assert CrossRepoLearning.primary_language_from_scan_data(tie) == "rust"
    end

    test "equal counts for unlisted languages fall back to lexical order" do
      tie = %{"languages" => %{"zebra" => 3, "apple" => 3}}
      assert CrossRepoLearning.primary_language_from_scan_data(tie) == "apple"
    end

    test "resolution is identical whatever order the map was built in" do
      a = Enum.reduce(["idris", "rust", "zig"], %{}, fn l, acc -> Map.put(acc, l, 10) end)
      b = Enum.reduce(["zig", "rust", "idris"], %{}, fn l, acc -> Map.put(acc, l, 10) end)

      assert CrossRepoLearning.primary_language_from_scan_data(a) ==
               CrossRepoLearning.primary_language_from_scan_data(b)
    end

    test "absent or empty language data resolves to unknown" do
      assert CrossRepoLearning.primary_language_from_scan_data(%{}) == "unknown"
      assert CrossRepoLearning.primary_language_from_scan_data(%{"languages" => %{}}) == "unknown"
      assert CrossRepoLearning.primary_language_from_scan_data(nil) == "unknown"
    end
  end
end
