# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.OutcomeTrackerTest do
  use ExUnit.Case, async: true

  # OutcomeTracker reads JSONL files which can be slow with Jason.Decoder.
  # Allow 120s per test instead of the default 60s.
  @moduletag timeout: 120_000

  alias Hypatia.OutcomeTracker

  describe "record_outcome/4" do
    test "records a successful outcome" do
      result =
        OutcomeTracker.record_outcome(
          "recipe-shell-quote-vars",
          "test-repo",
          "scripts/test.sh",
          :success
        )

      assert {:ok, record} = result
      assert record["recipe_id"] == "recipe-shell-quote-vars"
      assert record["repo"] == "test-repo"
      assert record["outcome"] == "success"
    end

    test "records a failure outcome" do
      result =
        OutcomeTracker.record_outcome(
          "recipe-todo-to-fill",
          "test-repo",
          "src/main.rs",
          :failure
        )

      assert {:ok, record} = result
      assert record["outcome"] == "failure"
    end
  end

  describe "trusted fleet evidence" do
    test "unverified or contradictory successes cannot increase recipe confidence" do
      rows = [
        %{"outcome" => "success"},
        %{"outcome" => "success", "verification" => "unverified"},
        %{"outcome" => "success", "verification" => "scan_failed"},
        %{"outcome" => "success", "verification" => "still_present"},
        %{"outcome" => "success", "verification" => "verified"},
        %{"outcome" => "failure", "verification" => "incomplete"},
        %{"outcome" => "false_positive", "verification" => "still_present"}
      ]

      assert OutcomeTracker.qualified_outcome_counts(rows) ==
               %{successes: 1, failures: 1, false_positives: 1, ignored: 4}
    end

    test "import provenance survives in the append-only destination" do
      source_id = "fleet-replay-test-" <> Integer.to_string(System.unique_integer([:positive]))
      assert {:ok, existing_ids} = OutcomeTracker.imported_fleet_ids()
      refute MapSet.member?(existing_ids, source_id)

      assert {:ok, record} =
               OutcomeTracker.record_fleet_outcome(
                 "recipe-fleet-replay-test",
                 "replay-repo",
                 ".",
                 :failure,
                 source_id,
                 "failed"
               )

      assert record["fleet_import_id"] == source_id
      assert record["origin"] == "gitbot-fleet"
      assert {:ok, imported_ids} = OutcomeTracker.imported_fleet_ids()
      assert MapSet.member?(imported_ids, source_id)
    end

    test "distinct identical fleet attempts retain distinct append-only row identities" do
      payload = ~s({"verdict":"verified","before_hits":2,"after_hits":0,"outcome":"success"})

      first = Hypatia.LearningScheduler.fleet_source_id(payload, 1)
      second = Hypatia.LearningScheduler.fleet_source_id(payload, 2)

      assert first != second
      assert first == Hypatia.LearningScheduler.fleet_source_id(payload, 1)
    end

    test "only postcondition-qualified events may advance learning" do
      refute OutcomeTracker.qualified_outcome?(%{
               "outcome" => "success",
               "verification" => "scan_failed"
             })

      refute OutcomeTracker.qualified_outcome?(%{
               "outcome" => "success",
               "verification" => "unverified"
             })

      assert OutcomeTracker.qualified_outcome?(%{
               "outcome" => "success",
               "verification" => "verified"
             })

      assert OutcomeTracker.qualified_outcome?(%{
               "outcome" => "failure",
               "verification" => "failed"
             })
    end

    test "locally observed failed fixer is qualified negative evidence" do
      recipe_id = "recipe-failed-fixer-" <> Integer.to_string(System.unique_integer([:positive]))

      assert {:ok, record, :not_verified} =
               OutcomeTracker.record_and_verify(
                 recipe_id,
                 "test-repo",
                 "src/main.rs",
                 :failure,
                 verify: false
               )

      assert record["verification"] == "failed"
      assert OutcomeTracker.qualified_outcome?(record)
    end

    test "legacy veteran maturity is invalidated by lack of qualified outcomes" do
      persisted = Hypatia.ConfidenceAnnealing.from_existing(18_640, 0, 0)
      counts = %{successes: 0, failures: 0, false_positives: 0}

      repaired = OutcomeTracker.reconcile_annealing_state(persisted, counts)
      assert repaired.outcome_count == 0
      assert repaired.stage == :nascent
      assert Hypatia.ConfidenceAnnealing.max_dispatch_tier(repaired) == :report_only

      qualified = %{successes: 5, failures: 0, false_positives: 0}
      adolescent = OutcomeTracker.reconcile_annealing_state(persisted, qualified)
      assert adolescent.outcome_count == 5
      assert adolescent.stage == :adolescent
    end

    test "batch rollback requests are negative, never evidence of success" do
      rollback = %{
        "outcome" => "false_positive",
        "verification" => "rollback_requested",
        "caused_by" => "batch_rollback",
        "batch_id" => "batch-1"
      }

      assert OutcomeTracker.qualified_outcome?(rollback)
      refute OutcomeTracker.qualified_outcome?(Map.delete(rollback, "batch_id"))
      refute OutcomeTracker.qualified_outcome?(Map.put(rollback, "outcome", "success"))
    end

    test "fleet import insists on predicate evidence and matching exit-status outcomes" do
      row = %{"repo" => "example", "pattern" => "recipe-a", "outcome" => "success"}

      # A row that is not evidence is kept as an audit row (E0-03).
      assert Hypatia.LearningScheduler.classify_fleet_record(row) ==
               {:audit, "recipe-a", "example", ".", :success, "unqualified"}

      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "verified", "before_hits" => 2, "after_hits" => 0})
             ) == {:ok, "recipe-a", "example", ".", :success, "verified"}

      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "verified", "before_hits" => 0, "after_hits" => 0})
             ) == {:audit, "recipe-a", "example", ".", :success, "unqualified"}

      assert Hypatia.LearningScheduler.classify_fleet_record(Map.put(row, "verdict", "no_op")) ==
               {:audit, "recipe-a", "example", ".", :success, "no_op"}

      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.put(row, "verdict", "unverified")
             ) == {:audit, "recipe-a", "example", ".", :success, "unverified"}

      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "failed", "exit_code" => 1, "outcome" => "failure"})
             ) == {:ok, "recipe-a", "example", ".", :failure, "failed"}

      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{
                 "verdict" => "incomplete",
                 "after_hits" => 2,
                 "outcome" => "failure"
               })
             ) == {:ok, "recipe-a", "example", ".", :failure, "incomplete"}

      # The fleet writes the legacy outcome "success" beside "incomplete".
      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{"verdict" => "incomplete", "after_hits" => 2})
             ) == {:ok, "recipe-a", "example", ".", :failure, "incomplete"}

      assert Hypatia.LearningScheduler.classify_fleet_record(
               Map.merge(row, %{
                 "verdict" => "verified",
                 "before_hits" => 1,
                 "after_hits" => 0,
                 "bot" => "hypatia"
               })
             ) == :skip
    end
  end

  describe "confidence_trend/1" do
    test "returns insufficient_data for recipe with few outcomes" do
      assert OutcomeTracker.confidence_trend("recipe-nonexistent") == :insufficient_data
    end

    # E0-03 review finding: a fleet audit row (stored, but not evidence) has
    # the outcome "success" when the fleet writes the legacy value. The trend
    # must not read it as a success, and it must not push evidence out of the
    # window of the last 10 rows.
    test "fleet audit rows do not make a recipe look like it is improving" do
      recipe = "recipe-trend-audit-#{System.system_time(:nanosecond)}"

      for n <- 1..3 do
        assert {:ok, _} =
                 OutcomeTracker.record_fleet_outcome(
                   recipe,
                   "owner/repo",
                   "a#{n}.txt",
                   :success,
                   "#{recipe}-audit-#{n}",
                   "no_op"
                 )
      end

      assert OutcomeTracker.confidence_trend(recipe) == :insufficient_data
    end

    test "fleet audit rows do not displace qualified failures from the window" do
      recipe = "recipe-trend-window-#{System.system_time(:nanosecond)}"

      for n <- 1..3 do
        assert {:ok, _} =
                 OutcomeTracker.record_fleet_outcome(
                   recipe,
                   "owner/repo",
                   "f#{n}.txt",
                   :failure,
                   "#{recipe}-failed-#{n}",
                   "failed"
                 )
      end

      for n <- 1..12 do
        assert {:ok, _} =
                 OutcomeTracker.record_fleet_outcome(
                   recipe,
                   "owner/repo",
                   "u#{n}.txt",
                   :success,
                   "#{recipe}-audit-#{n}",
                   "unverified"
                 )
      end

      assert OutcomeTracker.confidence_trend(recipe) == :declining
    end

    test "qualified rows still give the trend" do
      recipe = "recipe-trend-qualified-#{System.system_time(:nanosecond)}"

      for n <- 1..3 do
        assert {:ok, _} =
                 OutcomeTracker.record_fleet_outcome(
                   recipe,
                   "owner/repo",
                   "v#{n}.txt",
                   :success,
                   "#{recipe}-verified-#{n}",
                   "verified"
                 )
      end

      assert OutcomeTracker.confidence_trend(recipe) == :improving
    end
  end

  describe "update_recipe_confidence/1" do
    test "returns no_outcomes for recipe with no recorded outcomes" do
      assert OutcomeTracker.update_recipe_confidence("recipe-nonexistent") == :no_outcomes
    end
  end

  # ── E0-04: verification comes from a rescan, not from the caller ────

  describe "verification is set by a rescan, not by the caller" do
    # Each test writes only below a temporary data directory. A fake
    # `panic-attack` on PATH stands in for the rescan, so the result of the
    # rescan is chosen by the test.
    setup do
      keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
      original = for key <- keys, do: {key, Application.fetch_env(:hypatia, key)}
      old_path = System.get_env("PATH")
      old_mode = System.get_env("FAKE_PA_MODE")
      old_json = System.get_env("FAKE_PA_JSON")
      old_repos = System.get_env("HYPATIA_REPOS_DIR")

      root =
        System.tmp_dir!()
        |> Path.join("hypatia_e0_04_#{:erlang.unique_integer([:positive])}")

      bin = Path.join(root, "bin")
      File.mkdir_p!(bin)
      script = Path.join(bin, "panic-attack")

      File.write!(script, """
      #!/bin/sh
      case "$FAKE_PA_MODE" in
        clean) echo '{"weak_points":[]}' ;;
        present) echo '{"weak_points":[{"category":"cat-x","file":"a.txt","line":3}]}' ;;
        json) printf '%s' "$FAKE_PA_JSON" ;;
        *) exit 1 ;;
      esac
      """)

      File.chmod!(script, 0o755)

      Application.put_env(:hypatia, :verisimdb_data_path, Path.join(root, "data"))
      Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing"))
      Application.put_env(:hypatia, :fleet_path, Path.join(root, "fleet"))
      System.put_env("PATH", bin <> ":" <> (old_path || ""))
      System.put_env("HYPATIA_REPOS_DIR", root)
      System.put_env("FAKE_PA_MODE", "clean")

      on_exit(fn ->
        for {key, value} <- original do
          case value do
            {:ok, previous} -> Application.put_env(:hypatia, key, previous)
            :error -> Application.delete_env(:hypatia, key)
          end
        end

        System.put_env("PATH", old_path || "")

        for {name, old} <-
              [
                {"FAKE_PA_MODE", old_mode},
                {"FAKE_PA_JSON", old_json},
                {"HYPATIA_REPOS_DIR", old_repos}
              ] do
          if old, do: System.put_env(name, old), else: System.delete_env(name)
        end

        File.rm_rf!(root)
      end)

      %{root: root}
    end

    defp stored(root) do
      dir = Path.join([root, "data", "outcomes"])

      case File.ls(dir) do
        {:ok, files} ->
          files
          |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
          |> Enum.flat_map(fn f ->
            dir |> Path.join(f) |> File.read!() |> String.split("\n", trim: true)
          end)
          |> Enum.map(&Jason.decode!/1)

        {:error, _} ->
          []
      end
    end

    test "an unknown recipe with a clean scan is not verified", %{root: root} do
      # The recipe is not in the registry, so no category can be derived. A
      # scan cannot show that a weak point of no named category is gone.
      assert {:ok, record, result} =
               OutcomeTracker.record_outcome_for_fix(
                 "recipe-e0-04-unknown",
                 "owner/repo",
                 "a.txt",
                 :success
               )

      refute result == :verified
      assert result == :not_verified
      assert record["verification"] == "scan_skipped"

      assert [row] = stored(root)

      assert OutcomeTracker.qualified_outcome_counts([row]) ==
               %{successes: 0, failures: 0, false_positives: 0, ignored: 1}
    end

    test "an empty category is not verified, whatever the scan returns", %{root: root} do
      for mode <- ["clean", "present", "fail"], category <- [nil, ""] do
        System.put_env("FAKE_PA_MODE", mode)

        opts = if category, do: [verify: true, category: category], else: [verify: true]

        assert {:ok, record, :not_verified} =
                 OutcomeTracker.record_and_verify(
                   "recipe-e0-04-empty-#{mode}",
                   "owner/repo",
                   "a.txt",
                   :success,
                   opts
                 )

        assert record["verification"] == "scan_skipped"
      end

      assert OutcomeTracker.qualified_outcome_counts(stored(root)).successes == 0
    end

    test "a named category is still verified by a clean scan and refuted by a present one",
         %{root: root} do
      # The fix side knows the line of its finding (E0-22), so that a weak point that is still
      # there can be told from another finding of the same category in the same file.
      opts = [verify: true, category: "cat-x", repo_path: root, line: 3]

      assert {:ok, %{"verification" => "verified"}, :verified} =
               OutcomeTracker.record_and_verify(
                 "recipe-e0-04-a",
                 "owner/repo",
                 "a.txt",
                 :success,
                 opts
               )

      System.put_env("FAKE_PA_MODE", "present")

      assert {:ok, %{"verification" => "still_present"}, :false_positive} =
               OutcomeTracker.record_and_verify(
                 "recipe-e0-04-b",
                 "owner/repo",
                 "a.txt",
                 :success,
                 opts
               )
    end

    # E0-26 (owner ruling 8A): a fix is judged by its category AND its file. The
    # scanner names the file of a weak point in `file` (panic-attack's WeakPoint).
    defp rescan(root, file, weak_points, line \\ nil) do
      System.put_env("FAKE_PA_MODE", "json")
      System.put_env("FAKE_PA_JSON", Jason.encode!(%{"weak_points" => weak_points}))

      opts = [verify: true, category: "cat-x", repo_path: root]
      opts = if line, do: Keyword.put(opts, :line, line), else: opts

      OutcomeTracker.record_and_verify("recipe-e0-26", "owner/repo", file, :success, opts)
    end

    test "a clean scan verifies", %{root: root} do
      assert {:ok, %{"verification" => "verified"}, :verified} = rescan(root, "a.txt", [])
    end

    # E0-22 (owner ruling of 2026-11-18, fail closed): the fix side may give the line of its
    # finding. A weak point of the category at that line of that file is the finding that the
    # fix was meant to remove, so the fix did not hold. Another weak point of the category in the
    # same file, that the scan cannot tell from the fixed finding, gives NO verification: no
    # credit and no blame (`scan_skipped`). Lines move when a fix edits a file, so only a match
    # refutes; a mismatch never verifies.
    test "the same category at the same line of the same file refutes the fix", %{root: root} do
      assert {:ok, %{"verification" => "still_present"}, :false_positive} =
               rescan(
                 root,
                 "a.txt",
                 [%{"category" => "cat-x", "file" => "a.txt", "line" => 3}],
                 3
               )
    end

    test "2 findings of 1 category in one file, one fixed, the caller gives no line: no credit, no blame",
         %{root: root} do
      # The scanner still sees 1 weak point of the category in the file. It may be the finding
      # that was fixed (the fix did not hold) or the other one (the fix held). It cannot say.
      assert {:ok, record, :not_verified} =
               rescan(root, "a.txt", [%{"category" => "cat-x", "file" => "a.txt", "line" => 9}])

      assert record["verification"] == "scan_skipped"
      counts = OutcomeTracker.qualified_outcome_counts(stored(root))
      assert counts.successes == 0
      assert counts.failures == 0
      assert counts.false_positives == 0
    end

    test "a weak point of the category at ANOTHER line of the file is not the fixed finding, and is not proof that it is gone",
         %{root: root} do
      assert {:ok, record, :not_verified} =
               rescan(
                 root,
                 "a.txt",
                 [%{"category" => "cat-x", "file" => "a.txt", "line" => 9}],
                 3
               )

      assert record["verification"] == "scan_skipped"
      assert OutcomeTracker.qualified_outcome_counts(stored(root)).successes == 0
    end

    test "a weak point of the file with no line cannot be compared with the caller's line", %{
      root: root
    } do
      assert {:ok, record, :not_verified} =
               rescan(root, "a.txt", [%{"category" => "cat-x", "file" => "a.txt"}], 3)

      assert record["verification"] == "scan_skipped"
    end

    test "the fixed finding is gone and another finding of the category is in another file: verified",
         %{root: root} do
      assert {:ok, %{"verification" => "verified"}, :verified} =
               rescan(
                 root,
                 "a.txt",
                 [%{"category" => "cat-x", "file" => "b.txt", "line" => 3}],
                 3
               )
    end

    test "when the line matches, a second weak point in the file does not hide the failure", %{
      root: root
    } do
      assert {:ok, %{"verification" => "still_present"}, :false_positive} =
               rescan(
                 root,
                 "a.txt",
                 [
                   %{"category" => "cat-x", "file" => "a.txt", "line" => 9},
                   %{"category" => "cat-x", "file" => "a.txt", "line" => 3}
                 ],
                 3
               )
    end

    test "the line may arrive as text from a scanner, and still matches", %{root: root} do
      assert {:ok, %{"verification" => "still_present"}, :false_positive} =
               rescan(
                 root,
                 "a.txt",
                 [%{"category" => "cat-x", "file" => "a.txt", "line" => "3"}],
                 3
               )
    end

    test "the same category in ANOTHER file does not refute a fix of this file", %{root: root} do
      assert {:ok, %{"verification" => "verified"}, :verified} =
               rescan(root, "a.txt", [%{"category" => "cat-x", "file" => "b.txt"}])
    end

    test "another category in the same file does not refute it either", %{root: root} do
      assert {:ok, %{"verification" => "verified"}, :verified} =
               rescan(root, "a.txt", [%{"category" => "cat-y", "file" => "a.txt"}])
    end

    test "a weak point of the category that names no file gives no verification", %{root: root} do
      assert {:ok, record, :not_verified} = rescan(root, "a.txt", [%{"category" => "cat-x"}])
      assert record["verification"] == "scan_skipped"
      assert OutcomeTracker.qualified_outcome_counts(stored(root)).successes == 0
    end

    test "a weak point with no file in the scan does not hide one at the fixed line", %{
      root: root
    } do
      # Both are present: the one at this line of this file refutes the fix, whatever the other says.
      assert {:ok, %{"verification" => "still_present"}, :false_positive} =
               rescan(
                 root,
                 "a.txt",
                 [
                   %{"category" => "cat-x"},
                   %{"category" => "cat-x", "file" => "a.txt", "line" => 3}
                 ],
                 3
               )
    end

    test "a leading ./ does not make a different file", %{root: root} do
      assert {:ok, %{"verification" => "still_present"}, _} =
               rescan(
                 root,
                 "a.txt",
                 [%{"category" => "cat-x", "file" => "./a.txt", "line" => 3}],
                 3
               )

      assert {:ok, %{"verification" => "still_present"}, _} =
               rescan(
                 root,
                 "./a.txt",
                 [%{"category" => "cat-x", "file" => "a.txt", "line" => 3}],
                 3
               )
    end

    test "a fix at repository level keeps the old rule: the category alone", %{root: root} do
      for repo_level <- [".", ""] do
        assert {:ok, %{"verification" => "still_present"}, :false_positive} =
                 rescan(root, repo_level, [%{"category" => "cat-x", "file" => "b.txt"}])
      end
    end

    test "an absent scanner gives scan_failed and no credit, not a crash", %{root: root} do
      File.rm!(Path.join([root, "bin", "panic-attack"]))

      assert {:ok, record, :scan_unavailable} =
               OutcomeTracker.record_and_verify(
                 "recipe-e0-04-absent",
                 "owner/repo",
                 "a.txt",
                 :success,
                 verify: true,
                 category: "cat-x",
                 repo_path: root
               )

      assert record["verification"] == "scan_failed"
      assert OutcomeTracker.qualified_outcome_counts(stored(root)).successes == 0
    end

    test "a caller's verification of verified does not survive record_outcome/5", %{root: root} do
      assert {:ok, record} =
               OutcomeTracker.record_outcome(
                 "recipe-e0-04-forged",
                 "owner/repo",
                 "a.txt",
                 :success,
                 %{
                   "verification" => "verified"
                 }
               )

      refute record["verification"] == "verified"
      assert record["verification"] == "unverified"
      # The claim is kept, so the audit trail shows it.
      assert record["claimed_verification"] == "verified"

      assert [row] = stored(root)
      refute row["verification"] == "verified"
      assert OutcomeTracker.qualified_outcome_counts([row]).successes == 0
    end

    # Review finding: the guard compared one exact representation. A map with
    # an atom key, or an atom value, reached the log as "verified" anyway,
    # because the JSON encoder writes atoms as strings.
    test "the guard holds whatever the type of the key or the value", %{root: root} do
      variants = [
        %{verification: "verified"},
        %{"verification" => :verified},
        %{verification: :verified}
      ]

      for metadata <- variants do
        assert {:ok, record} =
                 OutcomeTracker.record_outcome(
                   "recipe-e0-04-types",
                   "owner/repo",
                   "a.txt",
                   :success,
                   metadata
                 )

        refute record["verification"] == "verified"
      end

      rows = stored(root)
      assert length(rows) == 3
      assert Enum.all?(rows, &(&1["verification"] == "unverified"))
      assert OutcomeTracker.qualified_outcome_counts(rows).successes == 0
    end

    test "a stored row has one verification field, not two", %{root: root} do
      assert {:ok, _} =
               OutcomeTracker.record_outcome(
                 "recipe-e0-04-dup",
                 "owner/repo",
                 "a.txt",
                 :success,
                 %{
                   verification: "unverified"
                 }
               )

      [file] = Path.wildcard(Path.join([root, "data", "outcomes", "*.jsonl"]))
      [line] = file |> File.read!() |> String.split("\n", trim: true)
      assert length(String.split(line, ~s("verification"))) == 2
    end

    test "two keys that name the same field are refused, and nothing is written", %{root: root} do
      assert_raise ArgumentError, fn ->
        OutcomeTracker.record_outcome("recipe-e0-04-clash", "owner/repo", "a.txt", :success, %{
          :verification => "verified",
          "verification" => "unverified"
        })
      end

      assert stored(root) == []
    end

    test "an audit map or a :metadata option with an atom key cannot stamp verified", %{
      root: root
    } do
      assert {:ok, audit} =
               OutcomeTracker.record_fleet_outcome(
                 "recipe-e0-04-audit",
                 "owner/repo",
                 "a.txt",
                 :success,
                 "source-id-types",
                 "unqualified",
                 %{verification: "verified", origin: "elsewhere", fleet_import_id: "forged"}
               )

      assert audit["verification"] == "unqualified"
      assert audit["origin"] == "gitbot-fleet"
      assert audit["fleet_import_id"] == "source-id-types"

      assert {:ok, via_metadata, _} =
               OutcomeTracker.record_and_verify(
                 "recipe-e0-04-meta",
                 "owner/repo",
                 "a.txt",
                 :success,
                 metadata: %{verification: "verified"}
               )

      refute via_metadata["verification"] == "verified"

      rows = stored(root)
      assert length(rows) == 2
      assert OutcomeTracker.qualified_outcome_counts(rows).successes == 0
    end

    # E0-27 (owner ruling 7A): only BatchRollback can write a rollback_requested row.
    test "a caller cannot write a rollback_requested row through the public entry", %{
      root: root
    } do
      forged = [
        %{
          "verification" => "rollback_requested",
          "caused_by" => "batch_rollback",
          "batch_id" => "b1"
        },
        %{
          verification: :rollback_requested,
          caused_by: "batch_rollback",
          batch_id: "b1"
        },
        %{verification: "rollback_requested", caused_by: "batch_rollback", batch_id: "b1"}
      ]

      for metadata <- forged do
        assert {:ok, record} =
                 OutcomeTracker.record_outcome(
                   "recipe-e0-27-forged",
                   "owner/repo",
                   "a.txt",
                   :false_positive,
                   metadata
                 )

        assert record["verification"] == "unverified"
        assert record["claimed_verification"] == "rollback_requested"
      end

      rows = stored(root)
      assert length(rows) == 3

      assert OutcomeTracker.qualified_outcome_counts(rows) ==
               %{successes: 0, failures: 0, false_positives: 0, ignored: 3}
    end

    test "the batch rollback entry writes a row that the gate counts as a false positive", %{
      root: root
    } do
      assert {:ok, record} =
               OutcomeTracker.record_batch_rollback(
                 "recipe-e0-27",
                 "owner/repo",
                 "a.txt",
                 "batch-1"
               )

      assert record["verification"] == "rollback_requested"
      assert record["caused_by"] == "batch_rollback"
      assert record["batch_id"] == "batch-1"
      assert record["outcome"] == "false_positive"

      assert [row] = stored(root)

      assert OutcomeTracker.qualified_outcome_counts([row]) ==
               %{successes: 0, failures: 0, false_positives: 1, ignored: 0}
    end

    test "the batch rollback entry refuses a batch id that is not a non-empty string", %{
      root: root
    } do
      for bad <- ["", nil, :batch, 42, "   "] do
        assert {:error, :invalid_batch_id} =
                 OutcomeTracker.record_batch_rollback("recipe-e0-27", "owner/repo", "a.txt", bad)
      end

      assert stored(root) == []
    end

    test "a caller's other fields and negative verifications are kept", %{root: root} do
      assert {:ok, record} =
               OutcomeTracker.record_outcome(
                 "recipe-e0-04-neg",
                 "owner/repo",
                 "a.txt",
                 :failure,
                 %{
                   "verification" => "failed",
                   "note" => "kept"
                 }
               )

      assert record["verification"] == "failed"
      assert record["note"] == "kept"
      refute Map.has_key?(record, "claimed_verification")
      assert OutcomeTracker.qualified_outcome_counts(stored(root)).failures == 1
    end
  end
end
