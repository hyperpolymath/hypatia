# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.MatchLabelTest do
  @moduledoc """
  E0-18: each outcome row names the pattern it is about and the kind of match that
  chose its recipe (`exact`, `category` or `fuzzy`), so that a merge of 2 findings
  under 1 recipe has a label (docs/design/2026-11-14-merge-honesty.adoc, decision 1,
  option C). The tests use recipes of their own in a temporary folder and carry no
  `:verisim_data` tag, so CI runs them.
  """
  use ExUnit.Case, async: false

  alias Hypatia.{DispatchManifest, OutcomeTracker, Receipt, RecipeMatcher, TriangleRouter}

  setup do
    original = Application.fetch_env(:hypatia, :verisimdb_data_path)
    orig_annealing = Application.fetch_env(:hypatia, :annealing_state_path)

    root =
      Path.join(System.tmp_dir!(), "hypatia_match_label_#{System.unique_integer([:positive])}")

    recipes = Path.join([root, "data", "recipes"])
    File.mkdir_p!(recipes)

    write_recipe(recipes, "recipe-label-exact", %{
      "pattern_ids" => ["PA100-exact-thing"],
      "target_categories" => [],
      "languages" => ["*"]
    })

    write_recipe(recipes, "recipe-label-category", %{
      "pattern_ids" => ["PA200-unrelated"],
      "target_categories" => ["label-cat"],
      "languages" => ["*"]
    })

    write_recipe(recipes, "recipe-label-fuzzy", %{
      "pattern_ids" => ["PA300-quote-vars"],
      "target_categories" => [],
      "languages" => ["*"]
    })

    File.write!(
      Path.join(recipes, "proven-substitutions.json"),
      Jason.encode!(%{
        "substitutions" => [
          %{
            "category" => "subst-only-cat",
            "pa_rule" => "PA400",
            "proven_modules" => ["SafeThing"],
            "confidence" => 0.5
          }
        ]
      })
    )

    Application.put_env(:hypatia, :verisimdb_data_path, Path.join(root, "data"))
    Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing"))

    on_exit(fn ->
      restore(:verisimdb_data_path, original)
      restore(:annealing_state_path, orig_annealing)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  defp restore(key, {:ok, v}), do: Application.put_env(:hypatia, key, v)
  defp restore(key, :error), do: Application.delete_env(:hypatia, key)

  defp write_recipe(dir, id, fields) do
    recipe =
      Map.merge(
        %{
          "id" => id,
          "triangle_tier" => "eliminate",
          "confidence" => 0.9,
          "auto_fixable" => false,
          "action" => "none"
        },
        fields
      )

    File.write!(Path.join(dir, id <> ".json"), Jason.encode!(recipe))
  end

  defp stored_rows(root) do
    dir = Path.join([root, "data", "outcomes"])

    case File.ls(dir) do
      {:ok, files} ->
        for f <- files,
            String.ends_with?(f, ".jsonl"),
            line <- dir |> Path.join(f) |> File.read!() |> String.split("\n", trim: true),
            do: Jason.decode!(line)

      {:error, _} ->
        []
    end
  end

  defp pattern(extra) do
    Map.merge(
      %{
        "id" => "PA999-no-recipe",
        "category" => "no-such-category",
        "description" => "nothing relevant here",
        "routed_repo" => "owner/repo"
      },
      extra
    )
  end

  # ── The matcher says which kind of match it made ─────────────────────────

  describe "RecipeMatcher.best_recipe_with_kind/2" do
    test "an exact match by pattern id is :exact" do
      assert {%{"id" => "recipe-label-exact"}, :exact} =
               RecipeMatcher.best_recipe_with_kind(
                 pattern(%{"id" => "PA100-exact-thing"}),
                 "elixir"
               )
    end

    test "a match by target category is :category" do
      assert {%{"id" => "recipe-label-category"}, :category} =
               RecipeMatcher.best_recipe_with_kind(
                 pattern(%{"category" => "label-cat"}),
                 "elixir"
               )
    end

    test "a match by PA rule prefix and description keyword is :fuzzy" do
      p = pattern(%{"pa_rule" => "PA300", "description" => "please quote the variable"})

      assert {%{"id" => "recipe-label-fuzzy"}, :fuzzy} =
               RecipeMatcher.best_recipe_with_kind(p, "elixir")
    end

    test "no match is nil" do
      assert RecipeMatcher.best_recipe_with_kind(pattern(%{}), "elixir") == nil
    end

    test "best_recipe_for_pattern/2 still returns the recipe alone, as before" do
      for {extra, id} <- [
            {%{"id" => "PA100-exact-thing"}, "recipe-label-exact"},
            {%{"category" => "label-cat"}, "recipe-label-category"},
            {%{"pa_rule" => "PA300", "description" => "quote it"}, "recipe-label-fuzzy"}
          ] do
        assert %{"id" => ^id} = RecipeMatcher.best_recipe_for_pattern(pattern(extra), "elixir")
      end

      assert RecipeMatcher.best_recipe_for_pattern(pattern(%{}), "elixir") == nil
    end
  end

  # ── The router carries it, the manifest names it ─────────────────────────

  describe "routing" do
    test "the routed pattern names the kind of match, and the tuple keeps its shape" do
      assert {:eliminate, %{"id" => "recipe-label-exact"}, routed} =
               TriangleRouter.route(
                 pattern(%{"id" => "PA100-exact-thing"}),
                 "owner/repo",
                 "elixir"
               )

      assert routed["match_kind"] == "exact"
      assert routed["id"] == "PA100-exact-thing"

      assert {:eliminate, _, %{"match_kind" => "category"}} =
               TriangleRouter.route(pattern(%{"category" => "label-cat"}), "owner/repo", "elixir")
    end

    test "a category substitution that no recipe matched is labelled by the way it was chosen" do
      assert {:substitute, %{"id" => "proven-substitute-subst-only-cat"}, routed} =
               TriangleRouter.route(
                 pattern(%{"category" => "subst-only-cat"}),
                 "owner/repo",
                 "elixir"
               )

      assert routed["match_kind"] == "category"
    end

    test "a pattern that only the control tier takes has no kind of match" do
      assert {:control, routed} = TriangleRouter.route(pattern(%{}), "owner/repo", "elixir")
      refute Map.has_key?(routed, "match_kind")
    end

    test "the manifest entry of a dispatch names the pattern id and the kind of match" do
      action =
        TriangleRouter.route(pattern(%{"id" => "PA100-exact-thing"}), "owner/repo", "elixir")

      assert {:ok, [entry]} = DispatchManifest.compile([action])
      assert entry["pattern_id"] == "PA100-exact-thing"
      assert entry["match_kind"] == "exact"
      assert entry["recipe_id"] == "recipe-label-exact"
    end
  end

  # ── The row names them ───────────────────────────────────────────────────

  describe "the outcome row" do
    test "carries the pattern id and the kind of match that its dispatch had", %{root: root} do
      {:ok, record} =
        OutcomeTracker.record_outcome("recipe-label-exact", "owner/repo", "a.ex", :failure, %{
          "pattern_id" => "PA100-exact-thing",
          "match_kind" => "fuzzy"
        })

      assert record["pattern_id"] == "PA100-exact-thing"
      assert record["match_kind"] == "fuzzy"
      assert [stored] = stored_rows(root)
      assert stored["pattern_id"] == "PA100-exact-thing"
      assert stored["match_kind"] == "fuzzy"
    end

    test "record_and_verify/5 carries them from :metadata, and never takes the recipe id for a pattern",
         %{root: root} do
      {:ok, _record, _verification} =
        OutcomeTracker.record_and_verify("recipe-label-exact", "owner/repo", "a.ex", :failure,
          metadata: %{"pattern_id" => "PA100-exact-thing", "match_kind" => "category"}
        )

      # record_outcome_for_fix/5 defaults its :pattern_id option to the recipe id
      # for the rescan. That is not a pattern, and it is not written as one.
      {:ok, _record, _verification} =
        OutcomeTracker.record_outcome_for_fix(
          "recipe-label-exact",
          "owner/repo",
          "b.ex",
          :failure
        )

      assert [stored, other] = stored_rows(root)

      assert stored["pattern_id"] == "PA100-exact-thing"
      assert stored["match_kind"] == "category"
      assert other["pattern_id"] == nil
      assert other["match_kind"] == "unknown"
    end

    test "a row from a caller that gives no kind is unknown, never exact", %{root: root} do
      {:ok, record} =
        OutcomeTracker.record_outcome("recipe-label-exact", "owner/repo", "a.ex", :failure, %{})

      assert record["match_kind"] == "unknown"
      assert record["pattern_id"] == nil
      assert [%{"match_kind" => "unknown"}] = stored_rows(root)
    end

    test "a kind outside the closed set, and a pattern id that is not text, are not stored" do
      {:ok, record} =
        OutcomeTracker.record_outcome("recipe-label-exact", "owner/repo", "a.ex", :failure, %{
          "pattern_id" => 42,
          "match_kind" => "great"
        })

      assert record["match_kind"] == "unknown"
      assert record["pattern_id"] == nil

      {:ok, blank} =
        OutcomeTracker.record_outcome("recipe-label-exact", "owner/repo", "a.ex", :failure, %{
          "pattern_id" => "  "
        })

      assert blank["pattern_id"] == nil
    end

    test "an atom key and a string key for the same label are one field, and 2 are refused" do
      {:ok, record} =
        OutcomeTracker.record_outcome("recipe-label-exact", "owner/repo", "a.ex", :failure, %{
          match_kind: "exact"
        })

      assert record["match_kind"] == "exact"

      assert_raise ArgumentError, fn ->
        OutcomeTracker.record_outcome("recipe-label-exact", "owner/repo", "a.ex", :failure, %{
          :match_kind => "exact",
          "match_kind" => "fuzzy"
        })
      end
    end

    test "a label changes no credit: the row is judged by the gate alone", %{root: _root} do
      {:ok, record} =
        OutcomeTracker.record_outcome("recipe-label-exact", "owner/repo", "a.ex", :success, %{
          "pattern_id" => "PA100-exact-thing",
          "match_kind" => "exact"
        })

      counts = OutcomeTracker.qualified_outcome_counts([record])
      assert counts.successes == 0
    end
  end

  # ── The receipt names them, when it can ──────────────────────────────────

  describe "the receipt" do
    @ctx %{
      kind: :local_caller,
      log: "outcomes/2026-11.jsonl",
      line: "1",
      row_sha256: String.duplicate("a", 64)
    }

    test "carries pattern_id and match_kind and no longer names them as missing" do
      row = %{
        "event_id" => Hypatia.EventId.generate(),
        "outcome" => "failure",
        "verification" => "failed",
        "pattern_id" => "PA100-exact-thing",
        "match_kind" => "exact"
      }

      receipt = Receipt.build(row, @ctx)

      assert receipt["pattern_id"] == "PA100-exact-thing"
      assert receipt["match_kind"] == "exact"
      refute "pattern_id" in receipt["missing_evidence"]
      refute "match_kind" in receipt["missing_evidence"]
    end

    test "a row with no label, or with kind unknown, still names both as missing" do
      for extra <- [%{}, %{"match_kind" => "unknown", "pattern_id" => nil}] do
        row =
          Map.merge(
            %{
              "event_id" => Hypatia.EventId.generate(),
              "outcome" => "failure",
              "verification" => "failed"
            },
            extra
          )

        receipt = Receipt.build(row, @ctx)

        refute Map.has_key?(receipt, "match_kind")
        refute Map.has_key?(receipt, "pattern_id")
        assert "pattern_id" in receipt["missing_evidence"]
        assert "match_kind" in receipt["missing_evidence"]
      end
    end
  end
end
