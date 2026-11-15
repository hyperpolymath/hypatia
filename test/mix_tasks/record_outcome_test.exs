# SPDX-License-Identifier: MPL-2.0
# E0-01: `mix hypatia.record_outcome --verdict`. A verdict can lower trust.
# It can never raise trust by itself (issue 984, acceptance A3).
defmodule Mix.Tasks.Hypatia.RecordOutcomeTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Hypatia.OutcomeTracker
  alias Mix.Tasks.Hypatia.RecordOutcome

  @moduletag timeout: 120_000

  # Each test writes only below a temporary data directory. A fake
  # `panic-attack` on PATH stands in for the rescan, so the result of the
  # rescan is chosen by the test and no real scanner is needed.
  setup do
    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = for key <- keys, do: {key, Application.fetch_env(:hypatia, key)}
    old_path = System.get_env("PATH")
    old_mode = System.get_env("FAKE_PA_MODE")
    old_shell = Mix.shell()

    root =
      System.tmp_dir!()
      |> Path.join("hypatia_record_outcome_test_#{:erlang.unique_integer([:positive])}")

    bin = Path.join(root, "bin")
    File.mkdir_p!(bin)

    script = Path.join(bin, "panic-attack")

    File.write!(script, """
    #!/bin/sh
    case "$FAKE_PA_MODE" in
      clean) echo '{"weak_points":[]}' ;;
      present) echo '{"weak_points":[{"category":"cat-x","file":"a.txt"}]}' ;;
      *) exit 1 ;;
    esac
    """)

    File.chmod!(script, 0o755)

    Application.put_env(:hypatia, :verisimdb_data_path, Path.join(root, "data"))
    Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing"))
    Application.put_env(:hypatia, :fleet_path, Path.join(root, "fleet"))
    System.put_env("PATH", bin <> ":" <> (old_path || ""))
    Mix.shell(Mix.Shell.Process)

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      System.put_env("PATH", old_path || "")

      case old_mode do
        nil -> System.delete_env("FAKE_PA_MODE")
        mode -> System.put_env("FAKE_PA_MODE", mode)
      end

      Mix.shell(old_shell)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  defp base_args(extra) do
    ["--recipe", "recipe-e0-01", "--repo", "owner/repo", "--file", "a.txt"] ++ extra
  end

  defp stored_rows(root) do
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

  defp run_ok(args) do
    capture_io(fn -> RecordOutcome.run(args) end)
  end

  defp assert_rejected(root, args) do
    capture_io(fn ->
      assert catch_exit(RecordOutcome.run(args)) == {:shutdown, 1}
    end)

    assert stored_rows(root) == []
  end

  # E0-18: the pattern of the dispatch and the kind of match travel on the command
  # line, through each of the 3 ways that the task writes a row.
  describe "--pattern-id and --match-kind" do
    test "a rescanned row carries both", %{root: root} do
      run_ok(
        base_args(["--outcome", "success", "--pattern-id", "PA100-x", "--match-kind", "fuzzy"])
      )

      assert [row] = stored_rows(root)
      assert row["pattern_id"] == "PA100-x"
      assert row["match_kind"] == "fuzzy"
    end

    test "a row with --no-verify carries both", %{root: root} do
      run_ok(
        base_args([
          "--outcome",
          "success",
          "--no-verify",
          "--pattern-id",
          "PA100-x",
          "--match-kind",
          "exact"
        ])
      )

      assert [row] = stored_rows(root)
      assert row["pattern_id"] == "PA100-x"
      assert row["match_kind"] == "exact"
      assert row["verification"] == "unverified"
    end

    test "a fleet verdict row carries both", %{root: root} do
      run_ok(
        base_args([
          "--outcome",
          "failure",
          "--verdict",
          "failed",
          "--pattern-id",
          "PA100-x",
          "--match-kind",
          "category"
        ])
      )

      assert [row] = stored_rows(root)
      assert row["pattern_id"] == "PA100-x"
      assert row["match_kind"] == "category"
      assert row["verification"] == "failed"
    end

    test "with neither flag the kind is unknown and there is no pattern", %{root: root} do
      run_ok(base_args(["--outcome", "failure", "--verdict", "failed"]))

      assert [row] = stored_rows(root)
      assert row["match_kind"] == "unknown"
      assert row["pattern_id"] == nil
    end

    test "a kind that the matcher does not make is refused and nothing is stored", %{root: root} do
      assert_rejected(root, base_args(["--outcome", "failure", "--match-kind", "great"]))
    end
  end

  describe "verdicts that are not positive evidence" do
    test "a no_op row leaves the qualified counts at zero", %{root: root} do
      run_ok(base_args(["--outcome", "success", "--verdict", "no_op"]))

      assert [row] = stored_rows(root)
      assert row["verdict"] == "no_op"
      assert row["verification"] == "no_op"
      counts = OutcomeTracker.qualified_outcome_counts([row])
      assert counts.successes == 0
      assert counts.failures == 0
    end

    test "an unverified row leaves the qualified counts at zero", %{root: root} do
      run_ok(base_args(["--outcome", "success", "--verdict", "unverified"]))

      assert [row] = stored_rows(root)
      assert row["verdict"] == "unverified"
      assert row["verification"] == "unverified"
      counts = OutcomeTracker.qualified_outcome_counts([row])
      assert counts.successes == 0
      assert counts.failures == 0
    end
  end

  describe "verdicts that lower trust" do
    test "a failed row counts as one failure", %{root: root} do
      run_ok(base_args(["--outcome", "failure", "--verdict", "failed"]))

      assert [row] = stored_rows(root)
      assert row["verdict"] == "failed"
      assert row["verification"] == "failed"
      counts = OutcomeTracker.qualified_outcome_counts([row])
      assert {counts.successes, counts.failures} == {0, 1}
    end

    test "an incomplete row counts as one failure", %{root: root} do
      run_ok(base_args(["--outcome", "failure", "--verdict", "incomplete"]))

      assert [row] = stored_rows(root)
      assert row["verdict"] == "incomplete"
      assert row["verification"] == "incomplete"
      counts = OutcomeTracker.qualified_outcome_counts([row])
      assert {counts.successes, counts.failures} == {0, 1}
    end
  end

  describe "rejected calls write no row" do
    test "an invalid verdict exits 1", %{root: root} do
      assert_rejected(root, base_args(["--outcome", "success", "--verdict", "great"]))
    end

    test "a pair that the table does not list exits 1", %{root: root} do
      bad_pairs = [
        {"verified", "failure"},
        {"no_op", "failure"},
        {"unverified", "failure"},
        {"incomplete", "success"},
        {"failed", "success"}
      ]

      for {verdict, outcome} <- bad_pairs do
        assert_rejected(root, base_args(["--outcome", outcome, "--verdict", verdict]))
      end
    end

    test "false_positive with any verdict exits 1", %{root: root} do
      for verdict <- ~w(verified no_op incomplete unverified failed) do
        assert_rejected(root, base_args(["--outcome", "false_positive", "--verdict", verdict]))
      end
    end

    test "an option that the task does not know exits 1", %{root: root} do
      assert_rejected(root, base_args(["--outcome", "success", "--bogus", "x"]))
      assert_rejected(root, base_args(["--outcome", "success", "--verdct", "verified"]))
    end
  end

  describe "--verdict verified keeps the rescan" do
    test "the verdict is in the row whatever the rescan returns", %{root: root} do
      args =
        base_args([
          "--outcome",
          "success",
          "--verdict",
          "verified",
          "--category",
          "cat-x",
          "--repo-path",
          root
        ])

      System.put_env("FAKE_PA_MODE", "clean")
      run_ok(args)
      assert [clean] = stored_rows(root)
      assert clean["verdict"] == "verified"
      assert clean["verification"] == "verified"

      File.rm_rf!(Path.join(root, "data"))
      System.put_env("FAKE_PA_MODE", "present")
      capture_io(fn -> assert catch_exit(RecordOutcome.run(args)) == {:shutdown, 2} end)
      rows = stored_rows(root)
      assert length(rows) == 2
      assert Enum.all?(rows, &(&1["verdict"] == "verified"))
      assert Enum.all?(rows, &(&1["verification"] == "still_present"))
      counts = OutcomeTracker.qualified_outcome_counts(rows)
      assert counts.successes == 0

      File.rm_rf!(Path.join(root, "data"))
      System.put_env("FAKE_PA_MODE", "fail")
      run_ok(args)
      assert [failed_scan] = stored_rows(root)
      assert failed_scan["verdict"] == "verified"
      assert failed_scan["verification"] == "scan_failed"
      assert OutcomeTracker.qualified_outcome_counts([failed_scan]).successes == 0
    end

    test "--no-verify with --verdict verified gives no credit", %{root: root} do
      run_ok(base_args(["--outcome", "success", "--verdict", "verified", "--no-verify"]))

      assert [row] = stored_rows(root)
      assert row["verdict"] == "verified"
      assert row["verification"] == "unverified"
      assert OutcomeTracker.qualified_outcome_counts([row]).successes == 0
    end
  end

  describe "an unknown recipe (E0-04)" do
    test "records a row that the qualified counts ignore, even with a clean scan", %{root: root} do
      System.put_env("FAKE_PA_MODE", "clean")

      # No --category and a recipe that is not in the registry: no category
      # can be named, so no rescan can show that the weak point is gone.
      run_ok(base_args(["--outcome", "success"]))

      assert [row] = stored_rows(root)
      assert row["verification"] == "scan_skipped"

      assert OutcomeTracker.qualified_outcome_counts([row]) ==
               %{successes: 0, failures: 0, false_positives: 0, ignored: 1}
    end

    test "with --verdict verified it is also ignored", %{root: root} do
      System.put_env("FAKE_PA_MODE", "clean")
      run_ok(base_args(["--outcome", "success", "--verdict", "verified"]))

      assert [row] = stored_rows(root)
      assert row["verdict"] == "verified"
      assert row["verification"] == "scan_skipped"
      assert OutcomeTracker.qualified_outcome_counts([row]).successes == 0
    end
  end

  describe "a call with no --verdict" do
    test "behaves as before and writes no verdict key", %{root: root} do
      run_ok(base_args(["--outcome", "success", "--no-verify"]))
      run_ok(base_args(["--outcome", "failure"]))

      rows = stored_rows(root)
      assert length(rows) == 2
      refute Enum.any?(rows, &Map.has_key?(&1, "verdict"))
      assert Enum.map(rows, & &1["verification"]) |> Enum.sort() == ["failed", "unverified"]
    end

    test "with a rescan it writes no verdict key", %{root: root} do
      System.put_env("FAKE_PA_MODE", "clean")

      run_ok(base_args(["--outcome", "success", "--category", "cat-x", "--repo-path", root]))

      assert [row] = stored_rows(root)
      assert row["verification"] == "verified"
      refute Map.has_key?(row, "verdict")
    end
  end

  describe "output and documentation" do
    test "--format json names the verdict" do
      out =
        run_ok(base_args(["--outcome", "failure", "--verdict", "failed", "--format", "json"]))

      payload = out |> String.trim() |> Jason.decode!()
      assert payload["verdict"] == "failed"
    end

    test "the task documentation names --verdict and the pairs" do
      doc = Mix.Task.moduledoc(RecordOutcome)
      assert doc =~ "--verdict"
      for v <- ~w(verified no_op incomplete unverified failed), do: assert(doc =~ v)
    end
  end

  describe "OutcomeTracker.record_and_verify/5 :metadata" do
    test "keys that the function sets itself win over the map", %{root: root} do
      assert {:ok, record, :not_verified} =
               OutcomeTracker.record_and_verify("recipe-e0-01", "owner/repo", "a.txt", :success,
                 metadata: %{"verification" => "verified", "verdict" => "no_op"}
               )

      assert record["verification"] == "unverified"
      assert record["verdict"] == "no_op"
      assert [row] = stored_rows(root)
      assert row["verification"] == "unverified"
      assert OutcomeTracker.qualified_outcome_counts([row]).successes == 0
    end
  end
end
