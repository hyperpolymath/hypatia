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

  describe "verified-evidence qualification" do
    test "unverified successes cannot become local or transferable confidence" do
      rows = [
        %{"outcome" => "success"},
        %{"outcome" => "success", "verification" => "scan_failed"},
        %{"outcome" => "success", "verification" => "verified"},
        %{"outcome" => "failure", "verification" => "failed"},
        %{"outcome" => "false_positive", "verification" => "still_present"}
      ]

      qualified = CrossRepoLearning.qualified_rows(rows)

      assert length(qualified) == 3

      assert Hypatia.OutcomeTracker.qualified_outcome_counts(qualified) ==
               %{successes: 1, failures: 1, false_positives: 1, ignored: 0}
    end

    test "three legacy successes do not satisfy the transfer evidence minimum" do
      legacy = List.duplicate(%{"outcome" => "success"}, 3)

      assert CrossRepoLearning.qualified_rows(legacy) == []
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

  # ── E0-14: what cross-repo learning does with a small sample and with a
  # qualified false positive. These tests PIN the present behaviour. They do
  # not say that it is right: see the ruling packet in the pull request.
  # The fixtures go into the test store (config/test.exs) under unique names
  # and are removed afterwards.

  describe "pinned present behaviour (E0-14)" do
    setup do
      data = Application.fetch_env!(:hypatia, :verisimdb_data_path) |> Path.expand()
      tag = "e0-14-#{System.unique_integer([:positive])}"
      outcomes_dir = Path.join(data, "outcomes")
      scans_dir = Path.join(data, "scans")
      File.mkdir_p!(outcomes_dir)
      File.mkdir_p!(scans_dir)
      log = Path.join(outcomes_dir, "#{tag}.jsonl")
      File.write!(log, "")

      on_exit(fn ->
        File.rm(log)

        for f <- Path.wildcard(Path.join(scans_dir, "#{tag}-*.json")), do: File.rm(f)
      end)

      %{tag: tag, log: log, scans_dir: scans_dir}
    end

    defp add_rows(ctx, recipe, rows) do
      now = DateTime.utc_now() |> DateTime.to_iso8601()

      lines =
        for {repo, outcome, verification} <- rows do
          %{
            "recipe_id" => recipe,
            "repo" => repo,
            "file" => "a.txt",
            "outcome" => outcome,
            "timestamp" => now,
            "bot" => "hypatia"
          }
          |> then(fn r ->
            if verification, do: Map.put(r, "verification", verification), else: r
          end)
          |> Jason.encode!()
        end

      File.write!(ctx.log, Enum.join(lines, "\n") <> "\n", [:append])
    end

    defp add_scan(ctx, repo, language) do
      File.write!(
        Path.join(ctx.scans_dir, "#{repo}.json"),
        Jason.encode!(%{"primary_language" => language})
      )
    end

    defp anomalies_for(pred),
      do: Enum.filter(CrossRepoLearning.detect_ecosystem_anomalies(), pred)

    test "case 1: a source repo with 3 verified successes and 0 failures transfers 1.0", ctx do
      recipe = "recipe-#{ctx.tag}-small"
      source = "#{ctx.tag}-source"
      target = "#{ctx.tag}-target"

      add_rows(ctx, recipe, List.duplicate({source, "success", "verified"}, 3))

      assert CrossRepoLearning.repos_with_fix(recipe) == [{source, 3, 3}]

      for policy <- [:moderate, :aggressive] do
        result = CrossRepoLearning.repo_confidence(recipe, target, policy)

        # One source, three rows, no failure, no prior: the raw rate is 1.0.
        assert result.transferred_confidence == 1.0
        assert result.transfer_sources == [source]
        assert result.local_outcomes == 0
        assert result.local_confidence == nil
        # With no local row the target takes the transferred value, before
        # annealing (blend_confidence/4).
        assert is_float(result.blended_confidence)
      end

      # Two verified successes are below the transfer minimum: nothing moves.
      recipe_2 = "recipe-#{ctx.tag}-two"
      add_rows(ctx, recipe_2, List.duplicate({source, "success", "verified"}, 2))
      assert CrossRepoLearning.repos_with_fix(recipe_2) == []
      assert CrossRepoLearning.repo_confidence(recipe_2, target).transferred_confidence == nil
    end

    test "case 2: a qualified false positive is a failure in the recipe anomaly rate", ctx do
      recipe = "recipe-#{ctx.tag}-fp"
      repos = for n <- 1..5, do: "#{ctx.tag}-r#{n}"
      [r1, r2, r3, r4, r5] = repos

      # 2 verified successes + 2 qualified false positives, in 4 repos. A
      # fifth repo has 1 verified success, because the rule needs 5 repos.
      add_rows(ctx, recipe, [
        {r1, "success", "verified"},
        {r2, "success", "verified"},
        {r3, "false_positive", "still_present"},
        {r4, "false_positive", "still_present"},
        {r5, "success", "verified"}
      ])

      assert [anomaly] = anomalies_for(&(&1[:recipe_id] == recipe))
      assert anomaly.type == :recipe_degradation
      # 2 of 5 rows are false positives: counted as failures (outcome != "success").
      assert anomaly.failure_rate == 0.4
      assert anomaly.total_outcomes == 5
      assert anomaly.affected_repos == 5
      assert anomaly.severity == :warning

      # The same rows without the 2 false positives raise no anomaly.
      recipe_ok = "recipe-#{ctx.tag}-fp-none"

      add_rows(ctx, recipe_ok, [
        {r1, "success", "verified"},
        {r2, "success", "verified"},
        {r3, "success", "verified"},
        {r4, "success", "verified"},
        {r5, "success", "verified"}
      ])

      assert anomalies_for(&(&1[:recipe_id] == recipe_ok)) == []
    end

    test "case 2: a qualified false positive is a failure in the language-family rate", ctx do
      recipe = "recipe-#{ctx.tag}-lang-fp"
      repos = for n <- 1..5, do: "#{ctx.tag}-g#{n}"
      Enum.each(repos, &add_scan(ctx, &1, "gleam"))
      [g1, g2, g3, g4, g5] = repos

      add_rows(ctx, recipe, [
        {g1, "success", "verified"},
        {g2, "success", "verified"},
        {g3, "success", "verified"},
        {g4, "false_positive", "still_present"},
        {g5, "false_positive", "still_present"}
      ])

      assert [anomaly] = anomalies_for(&(&1[:language_family] == :beam))
      assert anomaly.type == :language_family_anomaly
      assert anomaly.failure_rate == 0.4
      assert anomaly.total_outcomes == 5
    end

    test "case 3: unqualified rows change no confidence", ctx do
      plain = "recipe-#{ctx.tag}-plain"
      noisy = "recipe-#{ctx.tag}-noisy"
      source = "#{ctx.tag}-source"
      other = "#{ctx.tag}-other"
      target = "#{ctx.tag}-target"

      qualified = [
        {source, "success", "verified"},
        {source, "success", "verified"},
        {source, "failure", "failed"},
        {source, "success", "verified"}
      ]

      # Rows that are not evidence: no verification, scan_failed, unverified,
      # no_op, and a failure with no evidence. Spread over 2 more repos.
      unqualified =
        for repo <- [source, other],
            row <- [
              {"success", nil},
              {"success", "scan_failed"},
              {"success", "unverified"},
              {"success", "no_op"},
              {"failure", "unverified"}
            ] do
          {repo, elem(row, 0), elem(row, 1)}
        end

      add_rows(ctx, plain, qualified)
      add_rows(ctx, noisy, qualified ++ unqualified)

      assert CrossRepoLearning.repos_with_fix(noisy) == CrossRepoLearning.repos_with_fix(plain)
      assert CrossRepoLearning.repos_with_fix(plain) == [{source, 3, 4}]

      # Compare the local and the transferred paths, for the source repo
      # (local rows) and for a target repo (transferred rows only).
      for repo <- [source, target], policy <- [:moderate, :aggressive] do
        a = CrossRepoLearning.repo_confidence(plain, repo, policy)
        b = CrossRepoLearning.repo_confidence(noisy, repo, policy)

        assert Map.drop(a, [:recipe_id]) == Map.drop(b, [:recipe_id])
      end
    end

    test "case 3: unqualified rows change no anomaly", ctx do
      plain = "recipe-#{ctx.tag}-aplain"
      noisy = "recipe-#{ctx.tag}-anoisy"
      repos = for n <- 1..5, do: "#{ctx.tag}-a#{n}"
      [a1, a2, a3, a4, a5] = repos

      qualified = [
        {a1, "success", "verified"},
        {a2, "success", "verified"},
        {a3, "success", "verified"},
        {a4, "failure", "failed"},
        {a5, "failure", "failed"}
      ]

      # 20 success rows with no evidence. If they counted, the failure rate
      # would drop from 2/5 to 2/25 and the anomaly would vanish.
      unqualified =
        for repo <- repos, _ <- 1..4, do: {repo, "success", "unverified"}

      add_rows(ctx, plain, qualified)
      add_rows(ctx, noisy, qualified ++ unqualified)

      assert [a] = anomalies_for(&(&1[:recipe_id] == plain))
      assert [b] = anomalies_for(&(&1[:recipe_id] == noisy))
      assert Map.drop(a, [:recipe_id, :detected_at]) == Map.drop(b, [:recipe_id, :detected_at])
      assert b.failure_rate == 0.4
      assert b.total_outcomes == 5

      # Language path: the same, for a family.
      lang_repos = for n <- 1..5, do: "#{ctx.tag}-l#{n}"
      Enum.each(lang_repos, &add_scan(ctx, &1, "rescript"))
      [l1, l2, l3, l4, l5] = lang_repos
      lang_recipe = "recipe-#{ctx.tag}-lang"

      add_rows(ctx, lang_recipe, [
        {l1, "success", "verified"},
        {l2, "success", "verified"},
        {l3, "success", "verified"},
        {l4, "failure", "failed"},
        {l5, "failure", "failed"}
      ])

      before_noise = anomalies_for(&(&1[:language_family] == :ml_family))
      assert [%{failure_rate: 0.4, total_outcomes: 5}] = before_noise

      add_rows(ctx, lang_recipe, for(r <- lang_repos, do: {r, "success", "unverified"}))

      after_noise = anomalies_for(&(&1[:language_family] == :ml_family))
      assert [%{failure_rate: 0.4, total_outcomes: 5}] = after_noise
    end
  end
end
