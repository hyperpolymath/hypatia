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

    test "a detected truncation keeps its path-bound reset when the immediate read fails", %{
      tmp: tmp
    } do
      path = Path.join(outcomes_dir(tmp), "retry.jsonl")
      # A directory gives a deterministic successful stat followed by an
      # actual read error, including when the suite runs as root. The stale
      # cursor is larger than that stat, so this exercises the reset branch.
      File.mkdir!(path)
      previous_offset = File.stat!(path).size + 100

      positions = %{
        "retry.jsonl" => %{
          offset: previous_offset,
          incomplete_tail: "fragment from the previous file",
          source_path: path
        }
      }

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          assert {reset, 0, recipes} = LearningScheduler.ingest_new_outcomes(positions)
          assert recipes == MapSet.new()

          assert reset["retry.jsonl"] == %{
                   offset: 0,
                   incomplete_tail: "",
                   source_path: path
                 }

          # Before the next poll, the replacement grows past the old offset.
          # Keeping the old cursor would skip the first complete event.
          File.rmdir!(path)

          first =
            record("replacement-first", "failure", %{
              "detail" => String.duplicate("x", previous_offset + 100)
            }) <> "\n"

          second = record("replacement-second", "failure") <> "\n"
          File.write!(path, first <> second)
          assert byte_size(first <> second) > previous_offset

          assert {completed, 2, ingested} = LearningScheduler.ingest_new_outcomes(reset)
          assert ingested == MapSet.new(["replacement-first", "replacement-second"])
          assert completed["retry.jsonl"].offset == byte_size(first <> second)
          assert completed["retry.jsonl"].source_path == path
          assert {^completed, 0, _} = LearningScheduler.ingest_new_outcomes(completed)
        end)

      assert log =~ "outcome file retry.jsonl truncated"
      assert log =~ "cannot read outcome file retry.jsonl"
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

  # ── Failed destination write (#984 acceptance A4) ───────────────────

  describe "failed destination write" do
    setup %{tmp: tmp} do
      orig_fleet = Application.fetch_env(:hypatia, :fleet_path)
      fleet = Path.join(tmp, "fleet")
      Application.put_env(:hypatia, :fleet_path, fleet)

      on_exit(fn ->
        case orig_fleet do
          {:ok, val} -> Application.put_env(:hypatia, :fleet_path, val)
          :error -> Application.delete_env(:hypatia, :fleet_path)
        end
      end)

      %{fleet: fleet}
    end

    defp stored_rows(tmp) do
      case File.ls(outcomes_dir(tmp)) do
        {:ok, files} ->
          files
          |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
          |> Enum.flat_map(fn f ->
            outcomes_dir(tmp) |> Path.join(f) |> File.read!() |> String.split("\n", trim: true)
          end)

        {:error, _} ->
          []
      end
    end

    test "a failed write leaves the fleet row unacknowledged and the next poll imports it once",
         %{tmp: tmp, fleet: fleet} do
      line =
        Jason.encode!(%{
          "recipe_id" => "recipe-write-fail",
          "repo" => "synthetic",
          "file" => ".",
          "bot" => "fixture",
          "outcome" => "failure",
          "verdict" => "failed",
          "exit_code" => 1
        })

      fleet_file = Path.join(fleet, "shared-context/learning/fix-outcomes.jsonl")
      File.mkdir_p!(Path.dirname(fleet_file))
      File.write!(fleet_file, line <> "\n")

      # Only the write fails: a dangling link at the outcomes path reads as an
      # empty directory (so the id lookup succeeds), but mkdir_p cannot make it.
      File.rm_rf!(outcomes_dir(tmp))
      :ok = File.ln_s(Path.join(tmp, "does-not-exist"), outcomes_dir(tmp))

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          send(self(), {:poll, LearningScheduler.ingest_fleet_outcomes(nil)})
        end)

      assert_received {:poll, {0, signature}}

      # The warning proves that the poll reached the write.
      assert log =~ "write_failed"
      assert stored_rows(tmp) == []
      # The signature did not advance, so the same bytes are retried.
      assert signature == nil

      # Remove the obstacle. The next poll imports the row exactly once.
      :ok = File.rm(outcomes_dir(tmp))

      assert {1, signature_2} = LearningScheduler.ingest_fleet_outcomes(signature)
      assert signature_2 != nil
      assert length(stored_rows(tmp)) == 1

      # A further poll does not import it again.
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature_2)
      assert length(stored_rows(tmp)) == 1
    end
  end

  # ── E0-03: fleet rows that cannot be qualified stay as audit rows ───

  describe "fleet audit rows" do
    alias Hypatia.OutcomeTracker

    setup %{tmp: tmp} do
      orig_fleet = Application.fetch_env(:hypatia, :fleet_path)
      fleet = Path.join(tmp, "fleet")
      Application.put_env(:hypatia, :fleet_path, fleet)

      fleet_file = Path.join(fleet, "shared-context/learning/fix-outcomes.jsonl")
      File.mkdir_p!(Path.dirname(fleet_file))
      File.write!(fleet_file, "")

      on_exit(fn ->
        case orig_fleet do
          {:ok, val} -> Application.put_env(:hypatia, :fleet_path, val)
          :error -> Application.delete_env(:hypatia, :fleet_path)
        end
      end)

      %{fleet_file: fleet_file}
    end

    # The shape that the fleet writes (gitbot-fleet `legacy_outcome`): the
    # outcome is "success" beside each verdict other than "failed".
    defp fleet_row(verdict, extra \\ %{}) do
      outcome = if verdict == "failed", do: "failure", else: "success"

      %{
        "recipe_id" => "recipe-e0-03",
        "repo" => "owner/repo",
        "file" => "a.txt",
        "bot" => "robot-repo-automaton",
        "outcome" => outcome,
        "verdict" => verdict,
        "exit_code" => 0,
        "before_hits" => 2,
        "after_hits" => 0
      }
      |> Map.merge(extra)
    end

    defp write_fleet(fleet_file, rows) do
      File.write!(fleet_file, Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n")))
    end

    defp fleet_stored(tmp) do
      case File.ls(outcomes_dir(tmp)) do
        {:ok, files} ->
          files
          |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
          |> Enum.flat_map(fn f ->
            outcomes_dir(tmp)
            |> Path.join(f)
            |> File.read!()
            |> String.split("\n", trim: true)
            |> Enum.map(&Jason.decode!/1)
          end)

        {:error, _} ->
          []
      end
    end

    test "a no_op row and an unverified row are stored once and give no learning",
         %{tmp: tmp, fleet_file: fleet_file} do
      write_fleet(fleet_file, [
        fleet_row("no_op", %{"before_hits" => 0}),
        fleet_row("unverified", %{"before_hits" => nil, "after_hits" => nil})
      ])

      # An audit row is stored but is not counted as a processed outcome.
      assert {0, signature} = LearningScheduler.ingest_fleet_outcomes(nil)

      rows = fleet_stored(tmp)
      assert length(rows) == 2
      assert rows |> Enum.map(& &1["verification"]) |> Enum.sort() == ["no_op", "unverified"]
      assert Enum.all?(rows, &(&1["origin"] == "gitbot-fleet"))
      assert Enum.all?(rows, &is_binary(&1["fleet_import_id"]))

      # No positive and no negative learning: the rows fall under `ignored`.
      assert OutcomeTracker.qualified_outcome_counts(rows) ==
               %{successes: 0, failures: 0, false_positives: 0, ignored: 2}

      # A second poll, with the cache and cold, stores nothing again.
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
      assert length(fleet_stored(tmp)) == 2
    end

    test "a success row with no verdict is stored as unqualified", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      row = fleet_row("x") |> Map.drop(["verdict", "exit_code", "before_hits", "after_hits"])
      write_fleet(fleet_file, [row])

      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [stored] = fleet_stored(tmp)
      assert stored["verification"] == "unqualified"
      assert stored["outcome"] == "success"

      assert OutcomeTracker.qualified_outcome_counts([stored]) ==
               %{successes: 0, failures: 0, false_positives: 0, ignored: 1}
    end

    test "a verified verdict with no supporting numbers is stored as unqualified", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      write_fleet(fleet_file, [fleet_row("verified", %{"before_hits" => 0})])

      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [stored] = fleet_stored(tmp)
      assert stored["verification"] == "unqualified"
      assert stored["verdict"] == "verified"
      assert OutcomeTracker.qualified_outcome_counts([stored]).successes == 0
    end

    test "an incomplete verdict with the outcome success and after_hits above 0 is 1 failure",
         %{tmp: tmp, fleet_file: fleet_file} do
      # The fleet writes the legacy outcome "success" beside "incomplete".
      write_fleet(fleet_file, [fleet_row("incomplete", %{"after_hits" => 3})])

      assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [stored] = fleet_stored(tmp)
      assert stored["outcome"] == "failure"
      assert stored["verification"] == "incomplete"

      assert OutcomeTracker.qualified_outcome_counts([stored]) ==
               %{successes: 0, failures: 1, false_positives: 0, ignored: 0}
    end

    test "an incomplete verdict with the outcome failure is also 1 failure, and with no hits it is audit",
         %{tmp: tmp, fleet_file: fleet_file} do
      write_fleet(fleet_file, [
        fleet_row("incomplete", %{"outcome" => "failure", "after_hits" => 1}),
        fleet_row("incomplete", %{"after_hits" => 0, "file" => "b.txt"})
      ])

      assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      rows = fleet_stored(tmp)
      assert length(rows) == 2

      assert OutcomeTracker.qualified_outcome_counts(rows) ==
               %{successes: 0, failures: 1, false_positives: 0, ignored: 1}
    end

    test "a stored fleet row keeps the 4 audit fields of its source row", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      write_fleet(fleet_file, [
        fleet_row("verified"),
        fleet_row("failed", %{
          "exit_code" => 2,
          "file" => "c.txt",
          "before_hits" => 1,
          "after_hits" => 1
        })
      ])

      assert {2, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      rows = fleet_stored(tmp)
      verified = Enum.find(rows, &(&1["verification"] == "verified"))
      failed = Enum.find(rows, &(&1["verification"] == "failed"))

      assert Map.take(verified, ["verdict", "exit_code", "before_hits", "after_hits"]) ==
               %{"verdict" => "verified", "exit_code" => 0, "before_hits" => 2, "after_hits" => 0}

      assert Map.take(failed, ["verdict", "exit_code", "before_hits", "after_hits"]) ==
               %{"verdict" => "failed", "exit_code" => 2, "before_hits" => 1, "after_hits" => 1}
    end

    test "an audit map cannot replace fleet_import_id, origin or verification", %{tmp: tmp} do
      assert {:ok, record} =
               OutcomeTracker.record_fleet_outcome(
                 "recipe-e0-03",
                 "owner/repo",
                 "a.txt",
                 :success,
                 "source-id-1",
                 "unqualified",
                 %{
                   "verification" => "verified",
                   "origin" => "elsewhere",
                   "fleet_import_id" => "forged",
                   "outcome" => "failure",
                   "verdict" => "no_op"
                 }
               )

      assert record["verification"] == "unqualified"
      assert record["origin"] == "gitbot-fleet"
      assert record["fleet_import_id"] == "source-id-1"
      assert record["outcome"] == "success"
      assert record["verdict"] == "no_op"

      assert [stored] = fleet_stored(tmp)
      assert stored["verification"] == "unqualified"
      assert OutcomeTracker.qualified_outcome_counts([stored]).successes == 0
    end

    test "a row whose verdict and outcome disagree is audit evidence and is named in a warning",
         %{tmp: tmp, fleet_file: fleet_file} do
      # The verdict says "failed", the outcome says "success".
      write_fleet(fleet_file, [fleet_row("failed", %{"outcome" => "success", "exit_code" => 1})])

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          send(self(), {:poll, LearningScheduler.ingest_fleet_outcomes(nil)})
        end)

      assert_received {:poll, {0, _}}
      assert log =~ "verdict"
      assert log =~ "disagrees with outcome"

      assert [stored] = fleet_stored(tmp)
      assert stored["verification"] == "unqualified"
      assert stored["verdict"] == "failed"
      assert OutcomeTracker.qualified_outcome_counts([stored]).failures == 0
    end

    test "every source row goes to exactly one bucket" do
      rows = [
        fleet_row("verified"),
        fleet_row("failed", %{"exit_code" => 1}),
        fleet_row("incomplete", %{"after_hits" => 2}),
        fleet_row("no_op"),
        fleet_row("unverified"),
        fleet_row("verified", %{"before_hits" => 0}),
        fleet_row("no_op", %{"recipe_id" => nil}),
        fleet_row("no_op", %{"repo" => ""}),
        fleet_row("verified", %{"bot" => "hypatia"}),
        %{"nonsense" => true}
      ]

      buckets =
        Enum.map(rows, fn row ->
          case LearningScheduler.classify_fleet_record(row) do
            {:ok, _, _, _, _, _} -> :qualified
            {:audit, _, _, _, _, _} -> :audit
            :skip -> :skip
          end
        end)

      assert Enum.frequencies(buckets) == %{qualified: 3, audit: 3, skip: 4}
      assert length(buckets) == length(rows)
    end

    test "an audit row does not change a recipe that the scheduler recomputes", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      recipes = Path.join(tmp, "recipes")
      File.mkdir_p!(recipes)
      recipe_file = Path.join(recipes, "recipe-e0-03.json")

      File.write!(
        recipe_file,
        Jason.encode!(%{"id" => "recipe-e0-03", "confidence" => 0.5, "confidence_prior" => 0.5})
      )

      before_bytes = File.read!(recipe_file)

      write_fleet(fleet_file, [fleet_row("no_op"), fleet_row("unverified")])
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert length(fleet_stored(tmp)) == 2
      assert File.read!(recipe_file) == before_bytes
    end
  end

  # E0-11: a producer's event_id is the identity of a fleet row (issue 984, steps 6
  # and 8; docs/design/e0-receipt.adoc, sections 4 and 5.2). Ids are generated here:
  # the estate UUID gate refuses a non-v8 literal in source, so a wrong id is made
  # from a good one by changing one nibble.
  describe "producer event_id" do
    alias Hypatia.{EventId, Receipt}

    setup %{tmp: tmp} do
      orig_fleet = Application.fetch_env(:hypatia, :fleet_path)
      fleet = Path.join(tmp, "fleet")
      Application.put_env(:hypatia, :fleet_path, fleet)

      fleet_file = Path.join(fleet, "shared-context/learning/fix-outcomes.jsonl")
      File.mkdir_p!(Path.dirname(fleet_file))
      File.write!(fleet_file, "")

      on_exit(fn ->
        case orig_fleet do
          {:ok, val} -> Application.put_env(:hypatia, :fleet_path, val)
          :error -> Application.delete_env(:hypatia, :fleet_path)
        end
      end)

      %{fleet_file: fleet_file}
    end

    defp producer_row(extra \\ %{}) do
      Map.merge(
        %{
          "recipe_id" => "recipe-e0-11",
          "repo" => "owner/repo",
          "file" => "a.txt",
          "bot" => "robot-repo-automaton",
          "outcome" => "success",
          "verdict" => "verified",
          "exit_code" => 0,
          "before_hits" => 2,
          "after_hits" => 0
        },
        extra
      )
    end

    defp put_rows(fleet_file, rows) do
      File.write!(fleet_file, Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n")))
    end

    defp producer_stored(tmp) do
      case File.ls(outcomes_dir(tmp)) do
        {:ok, files} ->
          for f <- files,
              String.ends_with?(f, ".jsonl"),
              line <-
                outcomes_dir(tmp)
                |> Path.join(f)
                |> File.read!()
                |> String.split("\n", trim: true),
              do: Jason.decode!(line)

        {:error, _} ->
          []
      end
    end

    defp with_nibble(id, index, nibble) do
      binary_part(id, 0, index) <> nibble <> binary_part(id, index + 1, byte_size(id) - index - 1)
    end

    test "two rows with the same event_id on different lines are imported once", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      id = EventId.generate()
      put_rows(fleet_file, [producer_row(%{"event_id" => id}), producer_row(%{"event_id" => id})])

      assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [row] = producer_stored(tmp)
      assert row["event_id"] == id
      assert row["event_id_source"] == "producer"
      assert row["fleet_import_id"] == id
    end

    test "two rows with the same content and different event_ids are imported twice", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      a = EventId.generate()
      b = EventId.generate()
      assert a != b
      put_rows(fleet_file, [producer_row(%{"event_id" => a}), producer_row(%{"event_id" => b})])

      assert {2, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert tmp |> producer_stored() |> Enum.map(& &1["event_id"]) |> Enum.sort() ==
               Enum.sort([a, b])
    end

    test "a replay of the same log imports nothing more", %{fleet_file: fleet_file} do
      id = EventId.generate()
      put_rows(fleet_file, [producer_row(%{"event_id" => id})])

      assert {1, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
      # A cold start (no cached signature) is stopped by the stored ids.
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    end

    test "a malformed event_id is not imported and is reported with its line; the next rows are",
         %{tmp: tmp, fleet_file: fleet_file} do
      good = EventId.generate()
      version_7 = with_nibble(EventId.generate(), 14, "7")

      malformed = [
        {"not-an-id", 1},
        {version_7, 2},
        {String.upcase(good), 3},
        {nil, 4},
        {42, 5}
      ]

      rows =
        Enum.map(malformed, fn {bad, _line} -> producer_row(%{"event_id" => bad}) end) ++
          [producer_row(%{"event_id" => good})]

      put_rows(fleet_file, rows)

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          send(self(), {:poll, LearningScheduler.ingest_fleet_outcomes(nil)})
        end)

      assert_received {:poll, {1, signature}}

      assert [row] = producer_stored(tmp)
      assert row["event_id"] == good

      for {_bad, line} <- malformed do
        assert log =~ "fleet row #{line}: event_id is not a valid"
      end

      # The refused rows are not retried into the log on the next poll.
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
      assert length(producer_stored(tmp)) == 1
    end

    test "a log with no event_id gives the same ids as before this change", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      rows = [producer_row(), producer_row(%{"file" => "b.txt"})]
      put_rows(fleet_file, rows)

      assert {2, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      lines = fleet_file |> File.read!() |> String.split("\n", trim: true)

      expected =
        lines
        |> Enum.with_index(1)
        |> Enum.map(fn {l, n} -> LearningScheduler.fleet_source_id(l, n) end)

      stored = producer_stored(tmp)
      assert stored |> Enum.map(& &1["fleet_import_id"]) |> Enum.sort() == Enum.sort(expected)
      # Hypatia mints the event_id of a row that has none, and says so.
      assert Enum.all?(
               stored,
               &(&1["event_id_source"] == "hypatia" and EventId.valid?(&1["event_id"]))
             )
    end

    test "a start record is not stored, and the outcome row with its event_id is imported once",
         %{tmp: tmp, fleet_file: fleet_file} do
      id = EventId.generate()

      start = %{
        "chain_version" => "e0-receipt/1",
        "event_id" => id,
        "kind" => "start",
        "producer" => "robot-repo-automaton",
        "recipe_id" => "recipe-e0-11",
        "repo" => "owner/repo"
      }

      put_rows(fleet_file, [start, producer_row(%{"event_id" => id})])

      assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [row] = producer_stored(tmp)
      assert row["event_id"] == id
      assert row["outcome"] == "success"
      assert row["verification"] == "verified"
    end

    test "a start record alone stores nothing and counts as nothing imported", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      start = %{
        "event_id" => EventId.generate(),
        "kind" => "start",
        "recipe_id" => "recipe-e0-11",
        "repo" => "owner/repo"
      }

      put_rows(fleet_file, [start])

      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
      assert producer_stored(tmp) == []
    end

    test "the receipt of a producer row names the producer's id and says so", %{
      tmp: tmp,
      fleet_file: fleet_file
    } do
      id = EventId.generate()
      put_rows(fleet_file, [producer_row(%{"event_id" => id})])

      assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [receipt] = Receipt.all(tmp)
      assert receipt["event_id"] == id
      assert receipt["provenance"]["event_id_source"] == "producer"
      assert receipt["verdict"] == "credited_success"
    end
  end
end
