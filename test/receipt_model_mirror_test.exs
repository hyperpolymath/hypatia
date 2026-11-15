# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ReceiptModelMirrorTest do
  @moduledoc """
  Holds the receipt emitter (`Hypatia.Receipt`) to the model proved in
  `src/Hypatia/ABI/Receipt.idr` (E0-10).

  The model covers the Idris2 types, not this run time. This test checks the
  run time against it for P1, P2 and P5 of `docs/design/e0-receipt.adoc`
  section 8. P3 (a revision keeps the history) and P4 (a fill never
  overwrites) are properties of the model only: no run-time code builds a
  revision, a fill or a withdrawal yet (E0-07), so there is nothing here to hold.

  The gate table and the list of fields are READ FROM `Receipt.idr`, not copied,
  so the model and this test cannot drift apart without a failure.

  Two differences between the model and the run time are pinned below as
  tests of their own, so that a change to either is noticed:

    * `Receipt.write/3` of an event_id that has a receipt writes nothing and
      returns `:ok`; the model's `append` answers `Left DuplicateEventId`. The
      ledger is unchanged in both, which is the property.
    * `Receipt.build/2` does not refuse a `verified` row that has no
      observations. The model cannot build one (`verifiedOk`), and the Nickel
      contract refuses it. The production entry points never make one (see the
      P2 tests); `build/2` is not a smart constructor.
  """
  use ExUnit.Case, async: true

  alias Hypatia.{EventId, LearningScheduler, Receipt}

  @idris Path.expand("../src/Hypatia/ABI/Receipt.idr", __DIR__)
  @contract Path.expand("../schema/receipt.ncl", __DIR__)

  @qualifications ~w(QVerified QFailed QIncomplete QStillPresent QRollbackRequested QUnqualified)
  @claims ~w(CSuccess CFailure CFalsePositive CUnknown)

  # ── The model, read from the Idris file ──────────────────────────────────

  # `gate q c = v` clauses, in order, then the fall-through `gate _ _ = v`.
  defp model_gate do
    lines = File.read!(@idris) |> String.split("\n")

    clauses =
      for line <- lines,
          [_, q, c, v] <- [Regex.run(~r/^gate (Q\w+) (C\w+) = (\w+)$/, line)],
          do: {q, c, v}

    [_, fallthrough] = Regex.run(~r/^gate _ _ = (\w+)$/m, File.read!(@idris))

    fn q, c ->
      case Enum.find(clauses, fn {cq, cc, _} -> cq == q and cc == c end) do
        {_, _, v} -> v
        nil -> fallthrough
      end
    end
  end

  defp snake(camel) do
    camel
    |> String.replace(~r/([a-z])([A-Z])/, "\\1_\\2")
    |> String.downcase()
  end

  defp model_fields do
    [_, list] = Regex.run(~r/allFields = \[(.*?)\]/s, File.read!(@idris))

    list
    |> String.split(",", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(fn
      "EventIdF" -> "event_id"
      "ObsVerdict" -> "verdict"
      other -> snake(other)
    end)
  end

  defp contract_fields do
    [_, list] = Regex.run(~r/let missing_names = \[(.*?)\]/s, File.read!(@contract))

    Regex.scan(~r/"([a-z_]+)"/, list) |> Enum.map(fn [_, name] -> name end)
  end

  # ── Rows and contexts ────────────────────────────────────────────────────

  @ctx %{
    kind: :local_caller,
    log: "outcomes/2026-11.jsonl",
    line: "1",
    row_sha256: String.duplicate("a", 64)
  }

  defp verification_fields("QVerified"), do: %{"verification" => "verified"}
  defp verification_fields("QFailed"), do: %{"verification" => "failed"}
  defp verification_fields("QIncomplete"), do: %{"verification" => "incomplete"}
  defp verification_fields("QStillPresent"), do: %{"verification" => "still_present"}
  defp verification_fields("QUnqualified"), do: %{"verification" => "unverified"}

  defp verification_fields("QRollbackRequested"),
    do: %{
      "verification" => "rollback_requested",
      "caused_by" => "batch_rollback",
      "batch_id" => "batch-1"
    }

  defp outcome("CSuccess"), do: "success"
  defp outcome("CFailure"), do: "failure"
  defp outcome("CFalsePositive"), do: "false_positive"
  defp outcome("CUnknown"), do: "something-else"

  defp row(q, c, extra \\ %{}) do
    %{"event_id" => EventId.generate(), "outcome" => outcome(c)}
    |> Map.merge(verification_fields(q))
    |> Map.merge(extra)
  end

  defp verdict_name("CreditedSuccess"), do: "credited_success"
  defp verdict_name("CreditedFailure"), do: "credited_failure"
  defp verdict_name("CreditedFalsePositive"), do: "credited_false_positive"
  defp verdict_name("AuditOnly"), do: "audit_only"

  # ── P2: the verdict is the answer of the gate ────────────────────────────

  describe "P2: the verdict is the gate's (Receipt.idr `gate`)" do
    test "the model's table is read from the file, and is the whole 6 x 4 table" do
      gate = model_gate()
      answers = for q <- @qualifications, c <- @claims, do: gate.(q, c)

      assert length(answers) == 24
      # Withdrawn is never an answer of the gate (`gateNeverWithdrawn`).
      refute "Withdrawn" in answers
      assert Enum.count(answers, &(&1 == "CreditedSuccess")) == 1
      assert Enum.count(answers, &(&1 == "CreditedFailure")) == 3
      assert Enum.count(answers, &(&1 == "CreditedFalsePositive")) == 3
    end

    test "the emitter gives the model's verdict for every pair that a row can produce" do
      gate = model_gate()

      results =
        for q <- @qualifications, c <- @claims do
          receipt = Receipt.build(row(q, c), @ctx)

          reachable =
            receipt["qualification"] == Macro.underscore(String.slice(q, 1..-1//1)) and
              receipt["assertion"]["claimed_outcome"] == claimed_name(c)

          {q, c, reachable, receipt}
        end

      reachable = Enum.filter(results, fn {_, _, r, _} -> r end)
      # 24 pairs; a rollback that the gate does not credit is recorded as
      # "unqualified", so (QRollbackRequested, success | failure | unknown) are not
      # pairs that a receipt can carry.
      assert length(reachable) == 21

      for {q, c, true, receipt} <- results do
        assert receipt["verdict"] == verdict_name(model_gate().(q, c)),
               "#{q} #{c}: the emitter says #{receipt["verdict"]}, the model says #{gate.(q, c)}"
      end

      for {"QRollbackRequested", c, false, receipt} <- results do
        assert c in ["CSuccess", "CFailure", "CUnknown"]
        assert receipt["qualification"] == "unqualified"
        assert receipt["verdict"] == "audit_only"
      end

      refute Enum.any?(results, fn {_, _, _, r} -> r["verdict"] == "withdrawn" end)
    end

    test "a rollback row without its batch fields is not credited" do
      receipt =
        Receipt.build(
          %{
            "event_id" => EventId.generate(),
            "outcome" => "false_positive",
            "verification" => "rollback_requested"
          },
          @ctx
        )

      assert receipt["qualification"] == "unqualified"
      assert receipt["verdict"] == "audit_only"
    end
  end

  defp claimed_name("CSuccess"), do: "success"
  defp claimed_name("CFailure"), do: "failure"
  defp claimed_name("CFalsePositive"), do: "false_positive"
  defp claimed_name("CUnknown"), do: "unknown"

  # ── P2: a verified receipt has evidence ──────────────────────────────────

  describe "P2: a verified receipt has an evidence reference and an observation" do
    defp fleet_records do
      for verdict <- ["verified", "failed", "incomplete", "unverified", "no_op", nil],
          outcome <- ["success", "failure"],
          before_hits <- [nil, 0, 2],
          after_hits <- [nil, 0, 3],
          exit_code <- [nil, 0, 1] do
        %{
          "bot" => "robot-repo-automaton",
          "recipe_id" => "recipe-x",
          "repo" => "owner/repo",
          "file" => "a.ex",
          "verdict" => verdict,
          "outcome" => outcome,
          "before_hits" => before_hits,
          "after_hits" => after_hits,
          "exit_code" => exit_code
        }
        |> Enum.reject(fn {_, v} -> is_nil(v) end)
        |> Map.new()
      end
    end

    test "a fleet row that classifies as verified always carries its numbers into the receipt" do
      verified =
        for record <- fleet_records(),
            {:ok, _recipe, _repo, _file, outcome, "verified"} <-
              [LearningScheduler.classify_fleet_record(record)] do
          stored =
            %{
              "event_id" => EventId.generate(),
              "outcome" => Atom.to_string(outcome),
              "verification" => "verified",
              "origin" => "gitbot-fleet",
              "source_kind" => "fleet_import"
            }
            |> Map.merge(Map.take(record, ["verdict", "exit_code", "before_hits", "after_hits"]))

          Receipt.build(stored, %{@ctx | kind: :fleet_import})
        end

      assert verified != [], "no case reached verified: the test would be vacuous"

      for receipt <- verified do
        assert receipt["qualification"] == "verified"
        assert map_size(receipt["observations"]) > 0
        assert receipt["proof_or_test_ref"]["row_sha256"] =~ ~r/\A[0-9a-f]{64}\z/
        # P2's model reading: a credited success is evidenced.
        assert receipt["verdict"] == "credited_success"
      end
    end

    test "a local row that the rescan verified carries the rescan as its observation" do
      receipt =
        Receipt.build(
          row("QVerified", "CSuccess", %{"source_kind" => "local_rescan"}),
          %{@ctx | kind: :local_rescan}
        )

      assert receipt["qualification"] == "verified"
      assert receipt["observations"]["rescan"] == "verified"
      assert receipt["proof_or_test_ref"]["row_sha256"] =~ ~r/\A[0-9a-f]{64}\z/
    end

    test "KNOWN DIFFERENCE: build/2 does not refuse a verified row with no observation" do
      receipt = Receipt.build(row("QVerified", "CSuccess"), @ctx)

      # The model cannot build this (`verifiedOk`) and the Nickel contract refuses
      # it ("qualification verified needs observations and an evidence reference").
      # No production entry point makes such a row; build/2 is not a smart
      # constructor. If build/2 or write/3 gains a guard, change this test and the
      # trust-boundary text of the P2 row in verification/proof-ledger.jsonl.
      assert receipt["qualification"] == "verified"
      assert receipt["observations"] == %{}
    end
  end

  # ── P5: missing_evidence is exact ────────────────────────────────────────

  describe "P5: missing_evidence names exactly the absent fields (Receipt.idr `missing`)" do
    test "the model's 14 field names are the closed set of the contract" do
      fields = model_fields()

      assert length(fields) == 14
      assert Enum.sort(fields) == Enum.sort(contract_fields())
      # The model lists them in the sorted order of the contract.
      assert fields == Enum.sort(fields)
    end

    # Where a name is looked for in a receipt (`gap_exact` of schema/receipt.ncl).
    defp present?(receipt, name)
         when name in ~w(event_id head_sha subject_id pattern_id match_kind),
         do: Map.has_key?(receipt, name)

    defp present?(receipt, name) when name in ~w(after_hits before_hits exit_code rescan verdict),
      do: Map.has_key?(receipt["observations"], name)

    defp present?(receipt, name) when name in ~w(scan_digest test_digest),
      do: Map.has_key?(receipt["proof_or_test_ref"], name)

    defp present?(receipt, "tool_version"),
      do: Map.has_key?(receipt["provenance"], "tool_version")

    # No stable finding identity exists yet (issue 984, E1).
    defp present?(_receipt, "subject_identity"), do: false

    test "a field is named exactly when it is absent, for every combination of observations" do
      names = model_fields()
      keys = ~w(after_hits before_hits exit_code verdict rescan)

      subsets = Enum.reduce(keys, [[]], fn k, acc -> acc ++ Enum.map(acc, &[k | &1]) end)
      assert length(subsets) == 32

      for subset <- subsets do
        extra =
          for k <- subset, k != "rescan", into: %{} do
            {k, if(k == "verdict", do: "verified", else: 1)}
          end

        {kind, extra} =
          if "rescan" in subset,
            do: {:local_rescan, Map.put(extra, "verification", "verified")},
            else: {:local_caller, extra}

        receipt =
          Receipt.build(
            row("QUnqualified", "CSuccess", extra),
            %{@ctx | kind: kind}
          )

        missing = receipt["missing_evidence"]

        for name <- names do
          assert name in missing == not present?(receipt, name),
                 "#{name} with #{inspect(subset)}: named=#{name in missing}, present=#{present?(receipt, name)}"
        end

        # What the model does not prove, checked here: sorted, no duplicates, and
        # nothing outside the closed set.
        assert missing == Enum.sort(Enum.uniq(missing))
        assert Enum.all?(missing, &(&1 in names))
      end
    end

    test "at E0 the set is never empty: subject_identity cannot be filled yet" do
      full =
        row("QVerified", "CSuccess", %{
          "after_hits" => 0,
          "before_hits" => 2,
          "exit_code" => 0,
          "verdict" => "verified",
          "source_kind" => "local_rescan"
        })

      receipt = Receipt.build(full, %{@ctx | kind: :local_rescan})

      # The model's "empty exactly when all present" holds; it is not reached at E0.
      assert receipt["missing_evidence"] != []
      assert "subject_identity" in receipt["missing_evidence"]
    end
  end

  # ── P1: no two receipts share an event_id ────────────────────────────────

  describe "P1: no two receipts share an event_id (Receipt.idr `append`)" do
    setup do
      root =
        Path.join(System.tmp_dir!(), "receipt_model_mirror_#{System.unique_integer([:positive])}")

      File.mkdir_p!(root)
      on_exit(fn -> File.rm_rf!(root) end)
      {:ok, root: root}
    end

    test "a second write for an event_id adds nothing, and returns :ok (a known difference)", %{
      root: root
    } do
      row = row("QFailed", "CFailure")

      assert :ok = Receipt.write(root, row, ctx: @ctx)
      before = Receipt.all(root)
      assert length(before) == 1

      # The model answers Left DuplicateEventId; the run time returns :ok and
      # leaves the ledger as it was.
      assert :ok = Receipt.write(root, row, ctx: @ctx)
      assert Receipt.all(root) == before
    end

    test "many receipts keep distinct event_ids, and a replay of any of them adds nothing", %{
      root: root
    } do
      rows = for _ <- 1..40, do: row("QFailed", "CFailure")

      for r <- rows, do: assert(:ok = Receipt.write(root, r, ctx: @ctx))
      for r <- Enum.take_random(rows, 15), do: assert(:ok = Receipt.write(root, r, ctx: @ctx))

      ids = Receipt.all(root) |> Enum.map(& &1["event_id"])
      assert length(ids) == 40
      assert length(Enum.uniq(ids)) == 40
    end

    test "a receipt is never written for an event_id that is not an id", %{root: root} do
      assert {:error, :invalid_event_id} =
               Receipt.write(root, %{"event_id" => "not-an-id", "outcome" => "success"},
                 ctx: @ctx
               )

      assert {:error, :no_event_id} = Receipt.write(root, %{"outcome" => "success"}, ctx: @ctx)
      assert Receipt.all(root) == []
    end
  end
end
