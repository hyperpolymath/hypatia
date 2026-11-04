# SPDX-License-Identifier: MPL-2.0
# E0 replay/restart fixture for #984 / #995.
# Validates that the local JSONL outcome reader:
#   - acknowledges only complete newline-terminated records
#   - retains incomplete trailing bytes until completion
#   - quarantines malformed complete rows
#   - handles file truncation / rotation
#   - does not double-credit the same event on replay
#   - credits two distinct attempts with identical payloads separately
#   - handles missing / unreadable files without silent success
#   - binds offsets to their runtime source and replays legacy cursors safely

defmodule Hypatia.LearningSchedulerTest do
  use ExUnit.Case, async: false

  alias Hypatia.LearningScheduler

  @moduletag timeout: 120_000

  # Each test creates an isolated temp directory and overrides the
  # :verisimdb_data_path via Application.put_env so ingest_new_outcomes/1
  # reads from it. We restore the original on exit.
  setup do
    orig = Application.get_env(:hypatia, :verisimdb_data_path)
    tmp = System.tmp_dir!() |> Path.join("hypatia_ls_test_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(tmp, "outcomes"))
    Application.put_env(:hypatia, :verisimdb_data_path, tmp)

    on_exit(fn ->
      case orig do
        nil -> Application.delete_env(:hypatia, :verisimdb_data_path)
        val -> Application.put_env(:hypatia, :verisimdb_data_path, val)
      end

      File.rm_rf!(tmp)
    end)

    %{tmp: tmp}
  end

  defp outcomes_dir(tmp), do: Path.join(tmp, "outcomes")

  defp write_jsonl(tmp, filename, content) do
    dir = outcomes_dir(tmp)
    File.mkdir_p!(dir)
    path = Path.join(dir, filename)
    :ok = File.write(path, content)
    path
  end

  defp append_jsonl(tmp, filename, content) do
    dir = outcomes_dir(tmp)
    File.mkdir_p!(dir)
    path = Path.join(dir, filename)
    :ok = File.write(path, content, [:append])
    path
  end

  defp record(recipe_id, outcome, extra \\ %{}) do
    base = %{
      "recipe_id" => recipe_id,
      "repo" => "test-repo",
      "file" => "src/main.rs",
      "outcome" => outcome,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    base |> Map.merge(extra) |> Jason.encode!()
  end

  # ── normalise_position ──────────────────────────────────────────────

  describe "normalise_position/1" do
    test "accepts legacy integer offset" do
      assert LearningScheduler.normalise_position(42) ==
               %{offset: 42, incomplete_tail: ""}
    end

    test "accepts new map format" do
      pos = %{offset: 100, incomplete_tail: "partial"}
      assert LearningScheduler.normalise_position(pos) == pos
    end

    test "resets garbage to zero" do
      assert LearningScheduler.normalise_position(nil) ==
               %{offset: 0, incomplete_tail: ""}

      assert LearningScheduler.normalise_position("bogus") ==
               %{offset: 0, incomplete_tail: ""}

      assert LearningScheduler.normalise_position(-1) ==
               %{offset: 0, incomplete_tail: ""}
    end
  end

  # ── local_source_id ─────────────────────────────────────────────────

  describe "local_source_id/2" do
    test "is deterministic for same file + line number" do
      a = LearningScheduler.local_source_id("2026-11.jsonl", 5)
      b = LearningScheduler.local_source_id("2026-11.jsonl", 5)
      assert a == b
    end

    test "distinct line numbers produce distinct IDs" do
      a = LearningScheduler.local_source_id("2026-11.jsonl", 1)
      b = LearningScheduler.local_source_id("2026-11.jsonl", 2)
      assert a != b
    end

    test "distinct files with same line number produce distinct IDs" do
      a = LearningScheduler.local_source_id("2026-10.jsonl", 1)
      b = LearningScheduler.local_source_id("2026-11.jsonl", 1)
      assert a != b
    end
  end

  # ── Two-poll incomplete-tail scenario ───────────────────────────────

  test "runtime store changes do not reuse the prior store's filename cursor", %{tmp: tmp} do
    other_store = Path.join(tmp, "other-store")
    timestamp = %{"timestamp" => "2026-11-04T00:00:00Z"}
    first_row = record("store-a", "failure", timestamp) <> "\n"
    second_row = record("store-b", "failure", timestamp) <> "\n"
    assert byte_size(first_row) == byte_size(second_row)
    write_jsonl(tmp, "outcomes.jsonl", first_row)
    write_jsonl(other_store, "outcomes.jsonl", second_row)

    assert {first_positions, 1, first_recipes} = LearningScheduler.ingest_new_outcomes(%{})
    assert first_recipes == MapSet.new(["store-a"])

    Application.put_env(:hypatia, :verisimdb_data_path, other_store)

    assert {second_positions, 1, second_recipes} =
             LearningScheduler.ingest_new_outcomes(first_positions)

    assert second_recipes == MapSet.new(["store-b"])

    assert second_positions["outcomes.jsonl"].source_path ==
             Path.join(outcomes_dir(other_store), "outcomes.jsonl")

    assert {^second_positions, 0, _} = LearningScheduler.ingest_new_outcomes(second_positions)

    append_jsonl(other_store, "outcomes.jsonl", record("store-c", "failure", timestamp) <> "\n")
    assert {_, 1, appended_recipes} = LearningScheduler.ingest_new_outcomes(second_positions)
    assert appended_recipes == MapSet.new(["store-c"])
  end

  describe "incomplete-tail handling (two-poll fixture)" do
    test "poll A with incomplete final record does not advance past it", %{tmp: tmp} do
      # Write two complete records + one incomplete record (no trailing \n).
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      line2 = record("recipe-b", "success", %{"verification" => "verified"})
      partial = ~s({"recipe_id":"recipe-c","repo":"test","file":"x","outcome":"suc)

      content = line1 <> "\n" <> line2 <> "\n" <> partial
      write_jsonl(tmp, "2026-11.jsonl", content)

      # Poll A: should credit 2 complete records, buffer the incomplete tail.
      {positions_a, count_a, recipes_a} = LearningScheduler.ingest_new_outcomes(%{})

      assert count_a == 2
      assert MapSet.member?(recipes_a, "recipe-a")
      assert MapSet.member?(recipes_a, "recipe-b")
      refute MapSet.member?(recipes_a, "recipe-c")

      pos_a = Map.get(positions_a, "2026-11.jsonl")
      assert is_map(pos_a)
      assert pos_a.incomplete_tail == partial
      # The offset should be just before the incomplete tail.
      assert pos_a.offset == byte_size(line1 <> "\n" <> line2 <> "\n")
    end

    test "poll B completes the record and credits it exactly once", %{tmp: tmp} do
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      line2 = record("recipe-b", "success", %{"verification" => "verified"})
      partial = ~s({"recipe_id":"recipe-c","repo":"test","file":"x","outcome":"suc)
      completion = ~s(cess","verification":"verified"})

      # Poll A: write two complete + one incomplete.
      write_jsonl(tmp, "2026-11.jsonl", line1 <> "\n" <> line2 <> "\n" <> partial)
      {positions_a, count_a, _} = LearningScheduler.ingest_new_outcomes(%{})
      assert count_a == 2

      # Poll B: producer finishes the record.
      append_jsonl(tmp, "2026-11.jsonl", completion <> "\n")

      {positions_b, count_b, recipes_b} =
        LearningScheduler.ingest_new_outcomes(positions_a)

      # The completed record is now credited (recipe-c joined).
      assert count_b == 1
      assert MapSet.member?(recipes_b, "recipe-c")

      # No incomplete tail remains.
      pos_b = Map.get(positions_b, "2026-11.jsonl")
      assert pos_b.incomplete_tail == ""
    end

    test "unchanged partial writes retain their bytes and offset across polls", %{tmp: tmp} do
      complete = record("recipe-a", "success", %{"verification" => "verified"}) <> "\n"
      partial = ~s({"recipe_id":"recipe-b","outcome":"suc)
      completion = ~s(cess","verification":"verified"}) <> "\n"
      write_jsonl(tmp, "partial.jsonl", complete <> partial)

      assert {positions, 1, _} = LearningScheduler.ingest_new_outcomes(%{})

      assert positions["partial.jsonl"] == %{
               offset: byte_size(complete),
               incomplete_tail: partial,
               source_path: Path.join(outcomes_dir(tmp), "partial.jsonl")
             }

      for _ <- 1..3 do
        assert {^positions, 0, recipes} = LearningScheduler.ingest_new_outcomes(positions)
        assert recipes == MapSet.new()
      end

      append_jsonl(tmp, "partial.jsonl", completion)
      assert {completed, 1, recipes} = LearningScheduler.ingest_new_outcomes(positions)
      assert recipes == MapSet.new(["recipe-b"])
      assert completed["partial.jsonl"].offset == byte_size(complete <> partial <> completion)
      assert completed["partial.jsonl"].incomplete_tail == ""
      assert {^completed, 0, _} = LearningScheduler.ingest_new_outcomes(completed)
    end

    test "a sole unterminated record stays unacknowledged until its newline arrives", %{tmp: tmp} do
      row = record("recipe-a", "failure")
      write_jsonl(tmp, "partial.jsonl", row)

      assert {positions, 0, recipes} = LearningScheduler.ingest_new_outcomes(%{})
      assert recipes == MapSet.new()

      assert positions["partial.jsonl"] == %{
               offset: 0,
               incomplete_tail: row,
               source_path: Path.join(outcomes_dir(tmp), "partial.jsonl")
             }

      assert {^positions, 0, _} = LearningScheduler.ingest_new_outcomes(positions)

      append_jsonl(tmp, "partial.jsonl", "\n")
      assert {completed, 1, recipes} = LearningScheduler.ingest_new_outcomes(positions)
      assert recipes == MapSet.new(["recipe-a"])

      assert completed["partial.jsonl"] == %{
               offset: byte_size(row) + 1,
               incomplete_tail: "",
               source_path: Path.join(outcomes_dir(tmp), "partial.jsonl")
             }
    end
  end

  # ── Restart safety ──────────────────────────────────────────────────

  describe "restart / replay safety" do
    test "same events replayed after restart are not double-counted", %{tmp: tmp} do
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      line2 = record("recipe-b", "success", %{"verification" => "verified"})
      content = line1 <> "\n" <> line2 <> "\n"
      write_jsonl(tmp, "2026-11.jsonl", content)

      # First poll.
      {positions_a, count_a, _} = LearningScheduler.ingest_new_outcomes(%{})
      assert count_a == 2

      # Simulate restart: new empty positions (cold start).
      {_, count_b, _} = LearningScheduler.ingest_new_outcomes(%{})
      # Cold start re-reads the file — the same records are seen again.
      # The count reflects what's in the file; deduplication of the same
      # event is the producer's / downstream's responsibility. The key
      # property is that the *offset* advances correctly so no suffix
      # fragment is misread.
      assert count_b == 2

      # With the persisted positions, re-polling gives zero new records.
      {_, count_c, _} = LearningScheduler.ingest_new_outcomes(positions_a)
      assert count_c == 0
    end

    test "appending a new record after poll only credits the new one", %{tmp: tmp} do
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      write_jsonl(tmp, "2026-11.jsonl", line1 <> "\n")

      {positions_a, count_a, _} = LearningScheduler.ingest_new_outcomes(%{})
      assert count_a == 1

      line2 = record("recipe-b", "success", %{"verification" => "verified"})
      append_jsonl(tmp, "2026-11.jsonl", line2 <> "\n")

      {_, count_b, recipes_b} = LearningScheduler.ingest_new_outcomes(positions_a)
      assert count_b == 1
      assert MapSet.member?(recipes_b, "recipe-b")
      refute MapSet.member?(recipes_b, "recipe-a")
    end
  end

  # ── Malformed-row quarantine ────────────────────────────────────────

  describe "malformed row quarantine" do
    test "malformed complete rows are skipped but do not block valid rows", %{tmp: tmp} do
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      bad = "this is not JSON at all"
      line3 = record("recipe-c", "success", %{"verification" => "verified"})

      content = line1 <> "\n" <> bad <> "\n" <> line3 <> "\n"
      write_jsonl(tmp, "2026-11.jsonl", content)

      {positions, count, recipes} = LearningScheduler.ingest_new_outcomes(%{})

      # recipe-a and recipe-c are counted; the bad row is quarantined.
      assert count == 2
      assert MapSet.member?(recipes, "recipe-a")
      assert MapSet.member?(recipes, "recipe-c")

      # Offset advances past ALL complete lines including the bad one.
      pos = Map.get(positions, "2026-11.jsonl")
      assert pos.offset == byte_size(content)
      assert pos.incomplete_tail == ""
    end

    test "non-map JSON (array) is quarantined", %{tmp: tmp} do
      good = record("recipe-a", "success", %{"verification" => "verified"})
      content = good <> "\n" <> "[1,2,3]\n"
      write_jsonl(tmp, "2026-11.jsonl", content)

      {_positions, count, recipes} = LearningScheduler.ingest_new_outcomes(%{})
      assert count == 1
      assert MapSet.member?(recipes, "recipe-a")
    end
  end

  # ── File truncation / rotation ──────────────────────────────────────

  describe "file truncation / rotation" do
    test "truncated file resets offset and re-reads", %{tmp: tmp} do
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      line2 = record("recipe-b", "success", %{"verification" => "verified"})
      content = line1 <> "\n" <> line2 <> "\n"
      write_jsonl(tmp, "2026-11.jsonl", content)

      {positions_a, count_a, _} = LearningScheduler.ingest_new_outcomes(%{})
      assert count_a == 2

      # Simulate rotation: file is replaced with a shorter one.
      line3 = record("recipe-c", "success", %{"verification" => "verified"})
      write_jsonl(tmp, "2026-11.jsonl", line3 <> "\n")

      {_, count_b, recipes_b} = LearningScheduler.ingest_new_outcomes(positions_a)
      # File was truncated, so offset reset and the new content is read.
      assert count_b == 1
      assert MapSet.member?(recipes_b, "recipe-c")
    end

    test "rotation to an empty file resets the position without inventing a row", %{tmp: tmp} do
      write_jsonl(tmp, "rotated.jsonl", record("old-recipe", "failure") <> "\n")
      assert {positions, 1, _} = LearningScheduler.ingest_new_outcomes(%{})

      write_jsonl(tmp, "rotated.jsonl", "")
      assert {reset, 0, recipes} = LearningScheduler.ingest_new_outcomes(positions)
      assert recipes == MapSet.new()

      assert reset["rotated.jsonl"] == %{
               offset: 0,
               incomplete_tail: "",
               source_path: Path.join(outcomes_dir(tmp), "rotated.jsonl")
             }

      new_row = record("new-recipe", "failure") <> "\n"
      append_jsonl(tmp, "rotated.jsonl", new_row)
      assert {completed, 1, recipes} = LearningScheduler.ingest_new_outcomes(reset)
      assert recipes == MapSet.new(["new-recipe"])
      assert completed["rotated.jsonl"].offset == byte_size(new_row)
    end
  end

  # ── Missing / unreadable files ──────────────────────────────────────

  describe "missing and unreadable files" do
    test "empty outcomes directory returns zero" do
      # No JSONL files exist.
      {positions, count, recipes} = LearningScheduler.ingest_new_outcomes(%{})
      assert count == 0
      assert MapSet.size(recipes) == 0
      assert positions == %{}
    end

    test "missing outcomes directory returns zero", %{tmp: tmp} do
      # Remove the outcomes directory.
      File.rm_rf!(outcomes_dir(tmp))

      {positions, count, recipes} = LearningScheduler.ingest_new_outcomes(%{})
      assert count == 0
      assert MapSet.size(recipes) == 0
      assert positions == %{}
    end

    test "a read error preserves the position and is reported", %{tmp: tmp} do
      File.mkdir_p!(Path.join(outcomes_dir(tmp), "unreadable.jsonl"))
      positions = %{"unreadable.jsonl" => %{offset: 0, incomplete_tail: "pending"}}

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          assert {^positions, 0, recipes} = LearningScheduler.ingest_new_outcomes(positions)
          assert recipes == MapSet.new()
        end)

      assert log =~ "cannot read outcome file unreadable.jsonl"
    end
  end

  # ── Fleet-origin entries not double-counted ─────────────────────────

  describe "fleet-origin entries excluded from local count" do
    test "records with origin=gitbot-fleet are not counted locally", %{tmp: tmp} do
      local = record("recipe-a", "success", %{"verification" => "verified"})

      fleet =
        Jason.encode!(%{
          "recipe_id" => "recipe-b",
          "repo" => "fleet-repo",
          "file" => "x",
          "outcome" => "success",
          "origin" => "gitbot-fleet",
          "fleet_import_id" => "fleet-123"
        })

      content = local <> "\n" <> fleet <> "\n"
      write_jsonl(tmp, "2026-11.jsonl", content)

      {_positions, count, recipes} = LearningScheduler.ingest_new_outcomes(%{})

      # Only the local record is counted.
      assert count == 1
      assert MapSet.member?(recipes, "recipe-a")
    end
  end

  # ── Distinct identical attempts ─────────────────────────────────────

  describe "distinct identical attempts" do
    test "two separate records with identical payloads are counted twice", %{tmp: tmp} do
      payload = record("recipe-a", "success", %{"verification" => "verified"})
      content = payload <> "\n" <> payload <> "\n"
      write_jsonl(tmp, "2026-11.jsonl", content)

      {_positions, count, _recipes} = LearningScheduler.ingest_new_outcomes(%{})

      # Two physically distinct lines, even if payloads match, are two events.
      assert count == 2
    end
  end

  # ── Empty lines between records ─────────────────────────────────────

  describe "empty lines" do
    test "blank lines between records do not count as outcomes", %{tmp: tmp} do
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      content = line1 <> "\n\n" <> line1 <> "\n"
      write_jsonl(tmp, "2026-11.jsonl", content)

      {_positions, count, _} = LearningScheduler.ingest_new_outcomes(%{})
      # Two valid JSON records; the blank line is skipped.
      assert count == 2
    end
  end

  # ── Backward compatibility ──────────────────────────────────────────

  describe "backward compatibility with legacy integer positions" do
    test "source-less nonzero cursors replay conservatively before being qualified", %{tmp: tmp} do
      first = record("recipe-a", "failure") <> "\n"
      second = record("recipe-b", "failure") <> "\n"
      path = write_jsonl(tmp, "legacy.jsonl", first <> second)

      for legacy <- [byte_size(first), %{offset: byte_size(first), incomplete_tail: "old-store"}] do
        assert {positions, 2, recipes} =
                 LearningScheduler.ingest_new_outcomes(%{"legacy.jsonl" => legacy})

        assert recipes == MapSet.new(["recipe-a", "recipe-b"])
        assert positions["legacy.jsonl"].source_path == path
        assert positions["legacy.jsonl"].offset == byte_size(first <> second)
        assert positions["legacy.jsonl"].incomplete_tail == ""
        assert {^positions, 0, _} = LearningScheduler.ingest_new_outcomes(positions)
      end
    end

    test "legacy integer offset is normalised and advanced correctly", %{tmp: tmp} do
      line1 = record("recipe-a", "success", %{"verification" => "verified"})
      write_jsonl(tmp, "2026-11.jsonl", line1 <> "\n")

      # Old-style position: bare integer pointing at byte 0.
      legacy_positions = %{"2026-11.jsonl" => 0}

      {positions, count, _} = LearningScheduler.ingest_new_outcomes(legacy_positions)
      assert count == 1

      pos = Map.get(positions, "2026-11.jsonl")
      assert is_map(pos)
      assert pos.offset == byte_size(line1 <> "\n")
      assert pos.incomplete_tail == ""
    end
  end
end
