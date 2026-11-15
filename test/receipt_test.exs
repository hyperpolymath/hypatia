# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# E0-09: the receipt. Design: docs/design/e0-receipt.adoc (RULED 2026-11-15).
# Contract: schema/receipt.ncl.
defmodule Hypatia.ReceiptTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Hypatia.EventId
  alias Hypatia.FleetDispatcher
  alias Hypatia.LearningScheduler
  alias Hypatia.OutcomeTracker
  alias Hypatia.Receipt

  @moduletag timeout: 120_000

  # ── Setup: a runtime store in a temporary directory, a fake scanner, a fake
  # neural coordinator. Nothing is written outside the temporary directory.

  setup do
    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = for key <- keys, do: {key, Application.fetch_env(:hypatia, key)}
    old_path = System.get_env("PATH")
    old_mode = System.get_env("FAKE_PA_MODE")

    root = Path.join(System.tmp_dir!(), "hypatia_receipt_#{System.unique_integer([:positive])}")
    data = Path.join(root, "data")
    bin = Path.join(root, "bin")
    File.mkdir_p!(bin)

    script = Path.join(bin, "panic-attack")

    File.write!(script, """
    #!/bin/sh
    case "$FAKE_PA_MODE" in
      clean) echo '{"weak_points":[]}' ;;
      present) echo '{"weak_points":[{"category":"cat-x"}]}' ;;
      *) exit 1 ;;
    esac
    """)

    File.chmod!(script, 0o755)

    Application.put_env(:hypatia, :verisimdb_data_path, data)
    Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing"))
    Application.put_env(:hypatia, :fleet_path, Path.join(root, "fleet"))
    System.put_env("PATH", bin <> ":" <> (old_path || ""))
    System.put_env("FAKE_PA_MODE", "clean")

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      System.put_env("PATH", old_path || "")

      if old_mode,
        do: System.put_env("FAKE_PA_MODE", old_mode),
        else: System.delete_env("FAKE_PA_MODE")

      File.rm_rf!(root)
    end)

    %{root: root, data: data}
  end

  defp rows(data) do
    dir = Path.join(data, "outcomes")

    case File.ls(dir) do
      {:ok, files} ->
        for f <- files,
            String.ends_with?(f, ".jsonl"),
            line <- File.read!(Path.join(dir, f)) |> String.split("\n", trim: true),
            do: Jason.decode!(line)

      _ ->
        []
    end
  end

  defp fleet_row(recipe, verdict, extra \\ %{}) do
    Map.merge(
      %{
        "recipe_id" => recipe,
        "repo" => "owner/repo",
        "file" => "a.txt",
        "bot" => "robot-repo-automaton",
        "outcome" => if(verdict == "failed", do: "failure", else: "success"),
        "verdict" => verdict,
        "exit_code" => 0,
        "before_hits" => 2,
        "after_hits" => 0
      },
      extra
    )
  end

  defp write_fleet(root, rows) do
    file = Path.join([root, "fleet", "shared-context/learning/fix-outcomes.jsonl"])
    File.mkdir_p!(Path.dirname(file))
    File.write!(file, Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n")))
  end

  defp fake_coordinator do
    test = self()
    name = :"fake_coordinator_#{System.unique_integer([:positive])}"

    pid =
      spawn_link(fn ->
        loop = fn loop ->
          receive do
            {:source_bound, _s, {:outcome, f, o}} ->
              send(test, {:coordinator, f, o})
              loop.(loop)
          end
        end

        loop.(loop)
      end)

    Process.register(pid, name)
    name
  end

  defp neural(root, verification, success \\ true) do
    FleetDispatcher.ingest_fix_outcome(
      %{type: :fix_outcome, pattern: "recipe-neural", repo: "owner/repo", success: success},
      server: fake_coordinator(),
      learning_file: Path.join(root, "learning.jsonl"),
      trusted_verification: fn _ -> verification end
    )
  end

  # ── The canonical form ──────────────────────────────────────────────────

  @event_id "0198a7b2-6f0c-8000-8000-0123456789ab"
  @fixture "test/fixtures/receipt/e0-receipt-line.jsonl"

  defp fixture_row do
    %{
      "event_id" => @event_id,
      "event_id_source" => "hypatia",
      "source_kind" => "fleet_import",
      "source_line" => "7",
      "source_row_sha256" => String.duplicate("ab", 32),
      "source_bot" => "robot-repo-automaton",
      "origin" => "gitbot-fleet",
      "recipe_id" => "recipe-fixture",
      "repo" => "owner/repo",
      "file" => "src/a.ex",
      "outcome" => "success",
      "verification" => "verified",
      "verdict" => "verified",
      "exit_code" => 0,
      "before_hits" => 2,
      "after_hits" => 0
    }
  end

  defp fixture_ctx do
    %{
      kind: :fleet_import,
      log: "shared-context/learning/fix-outcomes.jsonl",
      line: "7",
      row_sha256: String.duplicate("ab", 32)
    }
  end

  describe "the canonical form" do
    test "the encoded line is byte-identical to the committed fixture" do
      assert {:ok, line} = Receipt.encode(Receipt.build(fixture_row(), fixture_ctx()))
      assert line <> "\n" == File.read!(@fixture)
    end

    test "the fixture is in canonical form: jq -cS prints the same bytes" do
      {out, 0} = System.cmd("jq", ["-cS", ".", @fixture])
      assert out == File.read!(@fixture)
    end

    test "keys are sorted, there is no whitespace and no number" do
      {:ok, line} = Receipt.encode(%{"b" => "1", "a" => %{"d" => ["x"], "c" => "y"}})
      assert line == ~s({"a":{"c":"y","d":["x"]},"b":"1"})
    end

    test "a number, an atom or a non-ASCII string is refused, never written" do
      assert {:error, {:not_canonical, _}} = Receipt.encode(%{"a" => 1})
      assert {:error, {:not_canonical, _}} = Receipt.encode(%{"a" => :x})
      assert {:error, {:not_canonical, _}} = Receipt.encode(%{"a" => "é"})
    end

    test "a field with text outside printable ASCII is written with %XX, and % too" do
      row = fixture_row() |> Map.put("file", "src/é 100%.ex")
      receipt = Receipt.build(row, fixture_ctx())
      assert receipt["assertion"]["file"] == "src/%C3%A9 100%25.ex"
      assert {:ok, _} = Receipt.encode(receipt)
    end

    test "receipt_sha256 covers every other field" do
      receipt = Receipt.build(fixture_row(), fixture_ctx())
      {digest, body} = Map.pop(receipt, "receipt_sha256")
      {:ok, line} = Receipt.encode(body)
      assert digest == :crypto.hash(:sha256, line) |> Base.encode16(case: :lower)

      changed = put_in(body, ["assertion", "file"], "other.ex")
      {:ok, other} = Receipt.encode(changed)
      refute :crypto.hash(:sha256, other) |> Base.encode16(case: :lower) == digest
    end
  end

  # ── The contract (needs the nickel binary; skipped, loudly, without it) ──

  describe "the Nickel contract" do
    @nickel System.get_env("NICKEL") || System.find_executable("nickel")

    defp check_contract(receipt) do
      dir = Path.join(System.tmp_dir!(), "receipt_ncl_#{System.unique_integer([:positive])}")
      File.mkdir_p!(dir)
      json = Path.join(dir, "r.json")
      ncl = Path.join(dir, "c.ncl")
      File.write!(json, Jason.encode!(receipt))
      schema = Path.expand("schema/receipt.ncl")
      File.write!(ncl, "let s = import \"#{schema}\" in (import \"#{json}\") | s.Receipt\n")
      result = System.cmd(@nickel, ["export", "--format", "json", ncl], stderr_to_stdout: true)
      File.rm_rf!(dir)
      result
    end

    defp good, do: Receipt.build(fixture_row(), fixture_ctx())

    # Re-seal a receipt after a change, so the contract refuses the CHANGE and
    # not a stale digest.
    defp reseal(receipt) do
      body = Map.delete(receipt, "receipt_sha256")
      {:ok, line} = Receipt.encode(body)
      Map.put(body, "receipt_sha256", :crypto.hash(:sha256, line) |> Base.encode16(case: :lower))
    end

    if @nickel == nil do
      @tag skip: "nickel is not installed: the contract was NOT checked"
    end

    test "a receipt that the emitter builds satisfies the contract" do
      assert {_, 0} = check_contract(good())
    end

    if @nickel == nil do
      @tag skip: "nickel is not installed: the contract was NOT checked"
    end

    test "the contract refuses a receipt that claims more than its evidence" do
      # 1. a verdict that is not the answer of the gate
      {out, code} = check_contract(good() |> Map.put("verdict", "audit_only") |> reseal())
      assert code != 0
      assert out =~ "verdict is not the answer of the gate"

      # 2. verified, with no observations
      bad = good() |> Map.put("observations", %{}) |> reseal()
      {out, code} = check_contract(bad)
      assert code != 0
      assert out =~ "verified needs observations"

      # 3. a field that is absent but not named, and one that is named but present
      bad = good() |> Map.update!("missing_evidence", &(&1 -- ["head_sha"])) |> reseal()
      {out, code} = check_contract(bad)
      assert code != 0
      assert out =~ "missing_evidence is not exact"

      # 4. a qualification outside the closed set
      {_, code} = check_contract(good() |> Map.put("qualification", "great") |> reseal())
      assert code != 0

      # 5. a malformed digest
      {_, code} = check_contract(good() |> Map.put("receipt_sha256", "xyz"))
      assert code != 0

      # 6. a number where a string belongs
      {_, code} = check_contract(good() |> put_in(["observations", "after_hits"], 0))
      assert code != 0

      # 7. a wrong chain version, and an event_id that is not version 8. The estate
      # UUID gate (`check-uuid-v8.sh --strict`) refuses a non-v8 literal in source,
      # so the wrong id is made from the version 8 one by changing its version
      # nibble (the character at index 14).
      {_, code} = check_contract(good() |> Map.put("chain_version", "e0-receipt/2"))
      assert code != 0

      version_7 = binary_part(@event_id, 0, 14) <> "7" <> binary_part(@event_id, 15, 21)
      assert String.at(version_7, 14) == "7"
      assert binary_part(version_7, 0, 14) <> "8" <> binary_part(version_7, 15, 21) == @event_id

      {out, code} = check_contract(good() |> Map.put("event_id", version_7) |> reseal())
      assert code != 0
      assert out =~ "event_id"
    end

    if @nickel == nil do
      @tag skip: "nickel is not installed: the contract was NOT checked"
    end

    test "every receipt that the integration tests below write satisfies the contract", %{
      data: data
    } do
      OutcomeTracker.record_fleet_outcome("recipe-c1", "o/r", "f", :success, "s1", "verified", %{
        "verdict" => "verified",
        "before_hits" => 2,
        "after_hits" => 0,
        "exit_code" => 0
      })

      OutcomeTracker.record_fleet_outcome("recipe-c2", "o/r", "f", :success, "s2", "unqualified")

      OutcomeTracker.record_outcome("recipe-c3", "o/r", "f", :failure, %{
        "verification" => "failed"
      })

      receipts = Receipt.all(data)
      assert length(receipts) == 3
      for r <- receipts, do: assert({_, 0} = check_contract(r))
    end
  end

  # ── The learning paths: one receipt each ────────────────────────────────

  describe "one receipt for each learning update" do
    test "a verified update on the local rescan path", %{data: data} do
      File.mkdir_p!(Path.join(data, "recipes"))

      {:ok, record, :verified} =
        OutcomeTracker.record_and_verify("recipe-r1", "o/r", "f", :success,
          verify: true,
          category: "cat-x",
          repo_path: data
        )

      assert [r] = Receipt.all(data)
      assert r["event_id"] == record["event_id"]
      assert r["verdict"] == "credited_success"
      assert r["qualification"] == "verified"
      assert r["source"]["kind"] == "local_rescan"
      assert r["observations"]["rescan"] == "verified"
      assert r["provenance"]["event_id_source"] == "hypatia"
      assert EventId.valid?(r["event_id"])
    end

    test "a verified update on the fleet import path (through the scheduler)", %{
      root: root,
      data: data
    } do
      write_fleet(root, [fleet_row("recipe-f1", "verified")])
      assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [r] = Receipt.all(data)
      assert r["verdict"] == "credited_success"
      assert r["source"]["kind"] == "fleet_import"
      assert r["source"]["line"] == "1"
      assert r["source"]["log"] == "shared-context/learning/fix-outcomes.jsonl"
      assert r["provenance"]["producer"] == "robot-repo-automaton"

      assert r["observations"] == %{
               "after_hits" => "0",
               "before_hits" => "2",
               "exit_code" => "0",
               "verdict" => "verified"
             }

      # The digest is that of the exact source line.
      [source_line] =
        Path.join([root, "fleet", "shared-context/learning/fix-outcomes.jsonl"])
        |> File.read!()
        |> String.split("\n", trim: true)

      assert r["proof_or_test_ref"]["row_sha256"] ==
               :crypto.hash(:sha256, source_line) |> Base.encode16(case: :lower)
    end

    test "a verified update on the neural path", %{root: root, data: data} do
      neural(root, "verified")
      assert_receive {:coordinator, _, "success"}, 500

      assert [r] = Receipt.all(data)
      assert r["verdict"] == "credited_success"
      assert r["source"]["kind"] == "neural_path"
    end

    test "a qualified failure on the local rescan path (a caller's own failed claim)", %{
      data: data
    } do
      {:ok, record} =
        OutcomeTracker.record_outcome("recipe-r2", "o/r", "f", :failure, %{
          "verification" => "failed"
        })

      assert [r] = Receipt.all(data)
      assert r["event_id"] == record["event_id"]
      assert r["verdict"] == "credited_failure"
      assert r["qualification"] == "failed"
      assert r["source"]["kind"] == "local_caller"
    end

    test "a qualified failure on the fleet import path", %{root: root, data: data} do
      write_fleet(root, [fleet_row("recipe-f2", "failed", %{"exit_code" => 2})])
      assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [r] = Receipt.all(data)
      assert r["verdict"] == "credited_failure"
      assert r["qualification"] == "failed"
      assert r["observations"]["exit_code"] == "2"
    end

    test "a qualified failure on the neural path", %{root: root, data: data} do
      neural(root, "failed", false)
      assert_receive {:coordinator, _, "failure"}, 500

      assert [r] = Receipt.all(data)
      assert r["verdict"] == "credited_failure"
    end

    test "an audit row (owner ruling R1) writes a receipt with verdict audit_only", %{
      root: root,
      data: data
    } do
      write_fleet(root, [fleet_row("recipe-f3", "no_op", %{"before_hits" => 0})])
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert [r] = Receipt.all(data)
      assert r["verdict"] == "audit_only"
      assert r["qualification"] == "unqualified"
    end

    test "the neural path writes no receipt when nothing is learned", %{root: root, data: data} do
      neural(root, nil)
      refute_receive {:coordinator, _, _}, 100
      assert Receipt.all(data) == []
    end
  end

  # ── Identity ────────────────────────────────────────────────────────────

  describe "the identity of an event" do
    test "a row carries the event_id that its receipt names", %{data: data} do
      {:ok, record} =
        OutcomeTracker.record_outcome("recipe-i1", "o/r", "f", :failure, %{
          "verification" => "failed"
        })

      assert [row] = rows(data)
      assert row["event_id"] == record["event_id"]
      assert row["source_kind"] == "local_caller"
      assert [%{"event_id" => id}] = Receipt.all(data)
      assert id == row["event_id"]
    end

    test "a caller cannot choose an event_id or any field of the receipt context", %{data: data} do
      chosen = EventId.generate()

      for _ <- 1..2 do
        {:ok, record} =
          OutcomeTracker.record_outcome("recipe-i2", "o/r", "f", :failure, %{
            "verification" => "failed",
            "event_id" => chosen,
            "event_id_source" => "producer",
            "source_kind" => "fleet_import",
            "source_line" => "99",
            "source_row_sha256" => String.duplicate("0", 64),
            "source_bot" => "somebody"
          })

        refute record["event_id"] == chosen
        assert record["event_id_source"] == "hypatia"
        assert record["source_kind"] == "local_caller"

        # The context a caller made up is not stored at all.
        for key <- ["source_line", "source_row_sha256", "source_bot"] do
          refute Map.has_key?(record, key)
        end
      end

      # Two events, two receipts: a chosen id cannot silence a later receipt.
      assert length(Receipt.all(data)) == 2
    end

    test "two events with identical content have different ids and two receipts", %{data: data} do
      for _ <- 1..2 do
        OutcomeTracker.record_outcome("recipe-i3", "o/r", "f", :failure, %{
          "verification" => "failed"
        })
      end

      ids = Receipt.all(data) |> Enum.map(& &1["event_id"])
      assert length(Enum.uniq(ids)) == 2
    end
  end

  # ── Replay, the store, and the failure rule ─────────────────────────────

  describe "replay" do
    test "a replay of the same event writes no second receipt", %{root: root, data: data} do
      write_fleet(root, [fleet_row("recipe-p1", "verified")])
      assert {1, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)

      assert length(rows(data)) == 1
      assert length(Receipt.all(data)) == 1
    end

    test "writing the receipt of a row twice writes one", %{data: data} do
      {:ok, _} =
        OutcomeTracker.record_outcome("recipe-p2", "o/r", "f", :failure, %{
          "verification" => "failed"
        })

      [row] = rows(data)
      assert :ok = Receipt.write(data, row)
      assert :ok = Receipt.write(data, row)
      assert length(Receipt.all(data)) == 1
    end
  end

  describe "the store of the row" do
    test "a receipt goes into the store of its row, also after the data root changes", %{
      root: root,
      data: data
    } do
      {:ok, _} =
        OutcomeTracker.record_outcome("recipe-s1", "o/r", "f", :failure, %{
          "verification" => "failed"
        })

      File.rm_rf!(Path.join(data, "receipts"))
      assert Receipt.all(data) == []

      # The configuration now points at another store.
      other = Path.join(root, "other")
      Application.put_env(:hypatia, :verisimdb_data_path, other)

      assert {1, 0} = Receipt.repair(data, "recipe-s1")
      assert length(Receipt.all(data)) == 1
      assert Receipt.all(other) == []
      refute File.exists?(Path.join(other, "receipts"))
    end
  end

  describe "the failure rule" do
    test "the row write fails: nothing else runs and no receipt is written", %{
      root: root,
      data: data
    } do
      File.mkdir_p!(data)
      :ok = File.ln_s(Path.join(root, "nowhere"), Path.join(data, "outcomes"))

      capture_log(fn ->
        assert {:error, {:outcome_log_write, _}} =
                 OutcomeTracker.record_outcome("recipe-w1", "o/r", "f", :failure, %{
                   "verification" => "failed"
                 })
      end)

      assert Receipt.all(data) == []
      refute File.exists?(Path.join(data, "receipts"))
    end

    test "the receipt write fails: the caller still gets {:ok, record}, the log says receipt_pending, and the next cycle writes it",
         %{root: root, data: data} do
      File.mkdir_p!(data)
      :ok = File.ln_s(Path.join(root, "nowhere"), Path.join(data, "receipts"))

      log =
        capture_log(fn ->
          assert {:ok, record} =
                   OutcomeTracker.record_outcome("recipe-w2", "o/r", "f", :failure, %{
                     "verification" => "failed"
                   })

          send(self(), {:record, record})
        end)

      assert_received {:record, record}
      assert log =~ "receipt_pending"
      assert log =~ record["event_id"]
      assert length(rows(data)) == 1

      # The obstacle goes. The next time the recipe is recomputed, the receipt is written.
      File.rm!(Path.join(data, "receipts"))
      assert Receipt.all(data) == []
      OutcomeTracker.update_recipe_confidence("recipe-w2")
      assert [%{"event_id" => id}] = Receipt.all(data)
      assert id == record["event_id"]

      # And once only.
      OutcomeTracker.update_recipe_confidence("recipe-w2")
      assert length(Receipt.all(data)) == 1
    end

    test "the derived update fails: the row and its receipt exist, the next cycle repeats the update, and the receipt stays one",
         %{root: root, data: data} do
      # The annealing state cannot be written: its directory is a regular file.
      blocker = Path.join(root, "blocker")
      File.write!(blocker, "x")
      Application.put_env(:hypatia, :annealing_state_path, Path.join(blocker, "annealing"))

      capture_log(fn ->
        assert {:ok, _} =
                 OutcomeTracker.record_outcome("recipe-w3", "o/r", "f", :failure, %{
                   "verification" => "failed"
                 })
      end)

      assert length(rows(data)) == 1
      assert length(Receipt.all(data)) == 1

      File.rm!(blocker)
      Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing"))
      OutcomeTracker.update_recipe_confidence("recipe-w3")
      assert length(Receipt.all(data)) == 1
    end

    test "no receipt is written before its row, and an orphan receipt is reported", %{data: data} do
      row =
        Map.put(
          %{
            "recipe_id" => "recipe-o1",
            "repo" => "o/r",
            "file" => "f",
            "outcome" => "failure",
            "verification" => "failed",
            "source_kind" => "local_caller"
          },
          "event_id",
          EventId.generate()
        )

      assert {:error, :row_not_in_log} = Receipt.write(data, row)
      assert Receipt.all(data) == []

      # A receipt whose row is not in the log (made by hand) is an orphan.
      receipt =
        Receipt.build(row, %{
          kind: :local_caller,
          log: "outcomes/2026-11.jsonl",
          line: "1",
          row_sha256: String.duplicate("a", 64)
        })

      {:ok, line} = Receipt.encode(receipt)
      File.mkdir_p!(Path.join(data, "receipts"))
      File.write!(Receipt.path(data, Date.utc_today()), line <> "\n")
      assert Receipt.orphans(data) == [row["event_id"]]
    end

    test "after the next run each row has exactly one receipt and no receipt is without its row",
         %{data: data} do
      for n <- 1..3 do
        OutcomeTracker.record_outcome("recipe-w4", "o/r", "f#{n}", :failure, %{
          "verification" => "failed"
        })
      end

      # Lose two of the three receipts, as a failed write would.
      [first | _] = Receipt.all(data)
      File.rm_rf!(Path.join(data, "receipts"))
      File.mkdir_p!(Path.join(data, "receipts"))
      {:ok, line} = Receipt.encode(first)
      File.write!(Receipt.path(data, Date.utc_today()), line <> "\n")

      assert {2, 0} = Receipt.repair(data, "recipe-w4")
      assert {0, 0} = Receipt.repair(data, "recipe-w4")

      ids = Receipt.all(data) |> Enum.map(& &1["event_id"]) |> Enum.sort()
      row_ids = rows(data) |> Enum.map(& &1["event_id"]) |> Enum.sort()
      assert ids == row_ids
      assert Receipt.orphans(data) == []
    end

    test "a row with no event_id (written before E0-09) gets no receipt: nothing is back-filled",
         %{data: data} do
      dir = Path.join(data, "outcomes")
      File.mkdir_p!(dir)

      File.write!(
        Path.join(dir, "2026-01.jsonl"),
        Jason.encode!(%{
          "recipe_id" => "recipe-l1",
          "repo" => "o/r",
          "file" => "f",
          "outcome" => "failure",
          "verification" => "failed"
        }) <> "\n"
      )

      assert {0, 0} = Receipt.repair(data, "recipe-l1")
      assert Receipt.all(data) == []
    end
  end
end
