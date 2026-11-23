# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.FleetAdmissionTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog
  alias Hypatia.{EventId, LearningScheduler, OutcomeTracker, Receipt}

  setup do
    app_keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    env_keys = ["PATH", "HYPATIA_REPOS_DIR", "FLEET_ADMISSION_CALLS", "FLEET_ADMISSION_SCAN"]
    apps = for key <- app_keys, do: {key, Application.fetch_env(:hypatia, key)}
    envs = for key <- env_keys, do: {key, System.get_env(key)}
    root = Path.join(System.tmp_dir!(), "hypatia_admission_#{System.unique_integer([:positive])}")
    data = Path.join(root, "data")
    repos = Path.join(root, "repos")
    target = Path.join(repos, "owner/repo")
    bin = Path.join(root, "bin")
    fleet = Path.join(root, "fleet")
    feed = Path.join(fleet, "shared-context/learning/fix-outcomes.jsonl")
    calls = Path.join(root, "scanner-calls")
    scan = Path.join(root, "scan.json")

    for dir <- [target, bin, Path.join(data, "recipes"), Path.dirname(feed)],
        do: File.mkdir_p!(dir)

    File.write!(Path.join(target, "a.txt"), "fixture\n")
    File.write!(scan, Jason.encode!(%{"weak_points" => []}))

    File.write!(Path.join(bin, "panic-attack"), """
    #!/bin/sh
    printf '%s\n' "$2" >> "$FLEET_ADMISSION_CALLS"
    while test -f "$FLEET_ADMISSION_SCAN.pause"; do /bin/sleep 0.01; done
    /bin/cat "$FLEET_ADMISSION_SCAN"
    """)

    File.chmod!(Path.join(bin, "panic-attack"), 0o755)
    Application.put_env(:hypatia, :verisimdb_data_path, data)
    Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing"))
    Application.put_env(:hypatia, :fleet_path, fleet)
    System.put_env("PATH", bin <> ":" <> (System.get_env("PATH") || ""))
    System.put_env("HYPATIA_REPOS_DIR", repos)
    System.put_env("FLEET_ADMISSION_CALLS", calls)
    System.put_env("FLEET_ADMISSION_SCAN", scan)
    recipe(data, "recipe-one.json", ["cat-x"])

    on_exit(fn ->
      for {key, value} <- apps do
        case value do
          {:ok, old} -> Application.put_env(:hypatia, key, old)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      for {key, value} <- envs do
        if value, do: System.put_env(key, value), else: System.delete_env(key)
      end

      File.rm_rf!(root)
    end)

    %{
      root: root,
      data: data,
      repos: repos,
      target: target,
      bin: bin,
      feed: feed,
      calls: calls,
      scan: scan
    }
  end

  defp recipe(data, name, categories) do
    File.write!(
      Path.join([data, "recipes", name]),
      Jason.encode!(%{
        "id" => "recipe-admission",
        "confidence" => 0.5,
        "target_categories" => categories
      })
    )
  end

  defp attempt(extra \\ %{}) do
    Map.merge(
      %{
        "attempt_contract" => "fleet-attempt/1",
        "event_id" => EventId.generate(),
        "bot" => "dispatch-runner",
        "recipe_id" => "recipe-admission",
        "repo" => "owner/repo",
        "repo_relative_path" => "owner/repo",
        "file" => "a.txt",
        "line" => 3,
        "outcome" => "success",
        "verdict" => "verified",
        "exit_code" => 0,
        "before_hits" => 2,
        "after_hits" => 0
      },
      extra
    )
  end

  defp feed(ctx, records) do
    bytes =
      Enum.map_join(records, "", fn row ->
        if(is_binary(row), do: row, else: Jason.encode!(row)) <> "\n"
      end)

    File.write!(ctx.feed, bytes)
  end

  defp rows(ctx, directory \\ "outcomes") do
    for path <- Path.wildcard(Path.join([ctx.data, directory, "*.jsonl"])),
        line <- String.split(File.read!(path), "\n", trim: true),
        do: Jason.decode!(line)
  end

  defp scans(ctx) do
    case File.read(ctx.calls) do
      {:ok, bytes} -> String.split(bytes, "\n", trim: true)
      {:error, :enoent} -> []
    end
  end

  defp sha(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)

  test "v1 clean rescan is correlated unbound audit evidence, never positive credit", ctx do
    raw = attempt()
    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    assert row["event_id"] == raw["event_id"]
    assert row["admission_version"] == "fleet-attempt/1"
    assert row["producer_assertion"]["verdict"] == "verified"
    assert row["producer_source"]["row_sha256"] == sha(Jason.encode!(raw))
    assert row["producer_source"]["path"] == ctx.feed
    assert row["producer_source"]["bot"] == "dispatch-runner"
    observation = row["rescan_observation"]
    assert observation["binding"] == "unbound"
    assert observation["result"] == "verified"
    assert observation["observer"] == "hypatia"
    assert observation["attempt_event_id"] == raw["event_id"]
    assert observation["source_row_sha256"] == sha(Jason.encode!(raw))
    assert EventId.valid?(observation["observation_id"])
    refute observation["observation_id"] == raw["event_id"]
    assert observation["target"]["repo_path"] == ctx.target
    assert scans(ctx) == [ctx.target]

    assert OutcomeTracker.qualified_outcome_counts([row]) == %{
             successes: 0,
             failures: 0,
             false_positives: 0,
             ignored: 1
           }

    assert [receipt] = Receipt.all(ctx.data)
    assert receipt["verdict"] == "audit_only"
    refute Map.has_key?(receipt["observations"], "rescan")
    assert "rescan" in receipt["missing_evidence"]
    assert receipt["parent_event_ids"] == []
  end

  test "still-present unbound rescan neither blames nor adds another outcome", ctx do
    File.write!(
      ctx.scan,
      Jason.encode!(%{
        "weak_points" => [%{"category" => "cat-x", "file" => "a.txt", "line" => 3}]
      })
    )

    feed(ctx, [attempt()])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    assert row["rescan_observation"]["result"] == "still_present"
    assert row["outcome"] == "success"
    assert row["verification"] == "unqualified"
    assert OutcomeTracker.qualified_outcome_counts([row]).false_positives == 0

    assert OutcomeTracker.qualified_outcome_counts([Map.put(row, "verification", "verified")]).successes ==
             0
  end

  test "failure evidence retains the existing exact numerical criteria and counts once", ctx do
    records = [
      attempt(%{"outcome" => "failure", "verdict" => "failed", "exit_code" => 2}),
      attempt(%{"outcome" => "failure", "verdict" => "failed", "exit_code" => 0}),
      attempt(%{"verdict" => "incomplete", "after_hits" => 1}),
      attempt(%{"verdict" => "incomplete", "after_hits" => 0}),
      attempt(%{"verdict" => "no_op"}),
      attempt(%{"verdict" => "unverified"})
    ]

    feed(ctx, records)
    assert {2, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert length(rows(ctx)) == 6

    assert OutcomeTracker.qualified_outcome_counts(rows(ctx)) == %{
             successes: 0,
             failures: 2,
             false_positives: 0,
             ignored: 4
           }

    assert scans(ctx) == []
  end

  test "explicit unsupported versions and invalid identities are rejected before legacy routing",
       ctx do
    records =
      for version <- [nil, "", "fleet-attempt/2", 1],
          do: attempt(%{"attempt_contract" => version})

    records =
      records ++
        [
          attempt(%{"event_id" => "invalid"}),
          Map.delete(attempt(), "event_id"),
          attempt(%{"bot" => "other"})
        ]

    feed(ctx, records)
    log = capture_log(fn -> assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil) end)
    assert log =~ "rejected"
    assert rows(ctx) == []
    assert scans(ctx) == []
  end

  test "untagged E0-11 rows keep their existing qualification and source receipt", ctx do
    raw = attempt() |> Map.delete("attempt_contract")
    feed(ctx, [raw])
    assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    refute Map.has_key?(row, "admission_version")
    assert OutcomeTracker.qualified_outcome?(row)
    assert [receipt] = Receipt.all(ctx.data)
    assert receipt["source"]["log"] == "shared-context/learning/fix-outcomes.jsonl"
    assert receipt["proof_or_test_ref"]["row_sha256"] == sha(Jason.encode!(raw))
    assert scans(ctx) == []
  end

  test "producer-supplied admission and observation metadata never become consumer facts", ctx do
    raw =
      attempt(%{
        "admission_version" => "forged",
        "producer_source" => %{"path" => "forged"},
        "rescan_observation" => %{
          "binding" => "bound",
          "result" => "verified",
          "observation_id" => EventId.generate()
        },
        "source_kind" => "local_rescan",
        "verification" => "verified",
        "category" => "wrong-category"
      })

    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    assert row["admission_version"] == "fleet-attempt/1"
    assert row["rescan_observation"]["binding"] == "unbound"

    refute row["rescan_observation"]["observation_id"] ==
             raw["rescan_observation"]["observation_id"]

    assert row["rescan_observation"]["target"]["category"] == "cat-x"
    refute Map.has_key?(row["producer_assertion"], "rescan_observation")
    assert row["source_kind"] == "fleet_import"
  end

  test "public outcome entry points strip admission and observation identities", ctx do
    forged = %{
      admission_version: "fleet-attempt/1",
      producer_assertion: %{"verdict" => "verified"},
      producer_source: %{"path" => "forged"},
      rescan_observation: %{"binding" => "bound"},
      rescan_not_run: "forged",
      attempt_contract: "fleet-attempt/1",
      event_id: EventId.generate()
    }

    assert {:ok, row} =
             OutcomeTracker.record_outcome("recipe-admission", "repo", "a.txt", :success, forged)

    for key <-
          ~w(admission_version producer_assertion producer_source rescan_observation rescan_not_run attempt_contract),
        do: refute(Map.has_key?(row, key))

    refute row["event_id"] == forged.event_id

    assert {:ok, scanned, :verified} =
             OutcomeTracker.record_and_verify("recipe-admission", "repo", "a.txt", :success,
               verify: true,
               category: "cat-x",
               repo_path: ctx.target,
               metadata: forged
             )

    for key <-
          ~w(admission_version producer_assertion producer_source rescan_observation rescan_not_run attempt_contract),
        do: refute(Map.has_key?(scanned, key))
  end

  test "replay preserves the committed identity and observation, distinct attempts stay distinct",
       ctx do
    first = attempt()
    second = Map.put(first, "event_id", EventId.generate())
    feed(ctx, [first, first, second])
    assert {0, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    original = rows(ctx)
    assert length(original) == 2
    assert length(scans(ctx)) == 2
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert rows(ctx) == original
    assert length(scans(ctx)) == 2
  end

  test "same ID with different bytes is a preserved non-authorising conflict", ctx do
    raw = attempt()
    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    original = rows(ctx)
    changed = Jason.encode!(Map.put(raw, "after_hits", 8))
    feed(ctx, [changed])
    log = capture_log(fn -> assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil) end)
    assert log =~ "conflict"
    assert rows(ctx) == original
    assert length(scans(ctx)) == 1
    assert [conflict] = rows(ctx, "admission-conflicts")
    assert conflict["attempt_event_id"] == raw["event_id"]
    assert conflict["raw_source_bytes"] == changed
    assert conflict["source_row_sha256"] == sha(changed)
    assert conflict["authorising"] == false
  end

  test "receipt hashes the persisted admission, and replay repairs it without a new scan", ctx do
    raw = attempt()
    feed(ctx, [raw])
    assert {0, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    assert {:ok, log, line, bytes} = Receipt.locate(ctx.data, raw["event_id"])
    assert [receipt] = Receipt.all(ctx.data)

    assert receipt["source"] == %{
             "kind" => "fleet_import",
             "log" => log,
             "line" => Integer.to_string(line)
           }

    assert receipt["proof_or_test_ref"]["row_sha256"] == sha(bytes)
    refute sha(bytes) == sha(Jason.encode!(raw))
    File.rm_rf!(Path.join(ctx.data, "receipts"))
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
    assert Receipt.all(ctx.data) == [receipt]
    assert rows(ctx) == [row]
    assert length(scans(ctx)) == 1
  end

  test "unsafe, missing and symlink targets do not scan or invent observation IDs", ctx do
    outside = Path.join(ctx.root, "outside")
    File.mkdir_p!(outside)
    File.ln_s!(outside, Path.join(ctx.repos, "escape"))

    for relative <- ["../outside", outside, "escape", "missing", "owner/../repo", nil] do
      feed(ctx, [attempt(%{"repo_relative_path" => relative})])
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    end

    assert length(rows(ctx)) == 6

    for row <- rows(ctx) do
      assert is_binary(row["rescan_not_run"])
      refute Map.has_key?(row, "rescan_observation")
    end

    assert scans(ctx) == []
  end

  test "missing configuration, scanner or unambiguous registry mapping leaves explicit gaps",
       ctx do
    System.delete_env("HYPATIA_REPOS_DIR")
    feed(ctx, [attempt()])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    System.put_env("HYPATIA_REPOS_DIR", ctx.repos)

    for categories <- [[], ["cat-x", "cat-y"]] do
      recipe(ctx.data, "recipe-one.json", categories)
      feed(ctx, [attempt()])
      assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    end

    recipe(ctx.data, "recipe-one.json", ["cat-x"])
    recipe(ctx.data, "recipe-two.json", ["cat-x"])
    feed(ctx, [attempt()])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    File.rm!(Path.join([ctx.data, "recipes", "recipe-two.json"]))
    File.rm!(Path.join(ctx.bin, "panic-attack"))
    System.put_env("PATH", ctx.bin)
    feed(ctx, [attempt()])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert length(rows(ctx)) == 5

    for row <- rows(ctx) do
      assert is_binary(row["rescan_not_run"])
      refute Map.has_key?(row, "rescan_observation")
    end

    assert scans(ctx) == []
  end

  test "concurrent public ingest requests serialize one admission", ctx do
    feed(ctx, [attempt(%{"outcome" => "failure", "verdict" => "failed", "exit_code" => 7})])
    tasks = for _ <- 1..2, do: Task.async(fn -> LearningScheduler.ingest_fleet_outcomes(nil) end)
    results = Enum.map(tasks, &Task.await(&1, 15_000))
    assert Enum.sum(Enum.map(results, &elem(&1, 0))) == 1
    assert length(rows(ctx)) == 1
  end

  test "changed configured source binding conflicts before any new scan", ctx do
    raw = attempt()
    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    original = rows(ctx)
    other_fleet = Path.join(ctx.root, "other-fleet")
    other_feed = Path.join(other_fleet, "shared-context/learning/fix-outcomes.jsonl")
    File.mkdir_p!(Path.dirname(other_feed))
    File.write!(other_feed, Jason.encode!(raw) <> "\n")
    Application.put_env(:hypatia, :fleet_path, other_fleet)

    assert capture_log(fn -> assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil) end) =~
             "conflict"

    assert rows(ctx) == original
    assert scans(ctx) == [ctx.target]
    assert [conflict] = rows(ctx, "admission-conflicts")
    assert conflict["source"]["path"] == other_feed
    assert conflict["committed_source"]["path"] == ctx.feed
  end

  test "changing or removing the version on a committed ID remains a preserved conflict", ctx do
    raw = attempt()
    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    original = rows(ctx)

    for changed <- [
          Map.delete(raw, "attempt_contract"),
          Map.put(raw, "attempt_contract", "fleet-attempt/2")
        ] do
      feed(ctx, [changed])

      assert capture_log(fn -> assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil) end) =~
               "conflict"
    end

    assert rows(ctx) == original
    assert length(rows(ctx, "admission-conflicts")) == 2
    assert scans(ctx) == [ctx.target]
  end

  test "the internal row locator cannot admit bytes absent from the actual configured feed",
       ctx do
    raw = attempt()
    feed(ctx, [raw])

    assert {:error, :source_row_changed} =
             OutcomeTracker.import_fleet_attempt(Jason.encode!(attempt()), 1, ctx.feed)

    assert {:error, :admission_store_or_source_changed} =
             OutcomeTracker.import_fleet_attempt(
               Jason.encode!(raw),
               1,
               Path.join(ctx.root, "forged.jsonl")
             )

    assert rows(ctx) == []
    assert scans(ctx) == []
  end

  test "an outcome append failure leaves source retryable and contributes nothing", ctx do
    feed(ctx, [attempt(%{"outcome" => "failure", "verdict" => "failed", "exit_code" => 3})])
    File.write!(Path.join(ctx.data, "outcomes"), "blocked")

    assert capture_log(fn -> assert {0, nil} = LearningScheduler.ingest_fleet_outcomes(nil) end) =~
             "cannot read imported IDs"

    assert rows(ctx) == []
    assert Receipt.all(ctx.data) == []
    File.rm!(Path.join(ctx.data, "outcomes"))
    assert {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert length(rows(ctx)) == 1
    assert length(Receipt.all(ctx.data)) == 1
  end

  test "receipt failure counts the committed failure once and repairs without readmission", ctx do
    raw = attempt(%{"outcome" => "failure", "verdict" => "failed", "exit_code" => 3})
    feed(ctx, [raw])
    File.write!(Path.join(ctx.data, "receipts"), "blocked")

    assert capture_log(fn -> assert {1, nil} = LearningScheduler.ingest_fleet_outcomes(nil) end) =~
             "receipt_pending"

    assert [row] = rows(ctx)
    assert row["event_id"] == raw["event_id"]
    assert Receipt.all(ctx.data) == []
    File.rm!(Path.join(ctx.data, "receipts"))
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert rows(ctx) == [row]
    assert length(Receipt.all(ctx.data)) == 1
    assert OutcomeTracker.qualified_outcome_counts(rows(ctx)).failures == 1
  end

  test "a store switch during a real scan cannot append evidence to either destination", ctx do
    pause = ctx.scan <> ".pause"
    File.write!(pause, "wait")
    feed(ctx, [attempt()])
    task = Task.async(fn -> LearningScheduler.ingest_fleet_outcomes(nil) end)
    assert await_scan(ctx, 100)
    other_data = Path.join(ctx.root, "other-data")
    Application.put_env(:hypatia, :verisimdb_data_path, other_data)
    File.rm!(pause)
    assert {0, nil} = Task.await(task, 15_000)
    assert rows(ctx) == []
    assert rows(%{ctx | data: other_data}) == []
    assert Receipt.all(ctx.data) == []
    assert Receipt.all(other_data) == []
  end

  defp await_scan(_ctx, 0), do: false

  defp await_scan(ctx, remaining) do
    if scans(ctx) != [] do
      true
    else
      Process.sleep(10)
      await_scan(ctx, remaining - 1)
    end
  end

  test "receipt repair uses persisted admission even when caller supplies forged context", ctx do
    feed(ctx, [attempt()])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    original = Receipt.all(ctx.data)
    File.rm_rf!(Path.join(ctx.data, "receipts"))
    forged = row |> Map.put("verification", "verified") |> Map.put("source_kind", "local_rescan")

    assert :ok =
             Receipt.write(ctx.data, forged,
               ctx: %{
                 kind: :local_rescan,
                 log: "forged",
                 line: "1",
                 row_sha256: String.duplicate("a", 64)
               }
             )

    assert Receipt.all(ctx.data) == original
  end

  test "malformed scanner output is a closed unbound failure, never blame", ctx do
    File.write!(ctx.scan, Jason.encode!(%{"weak_points" => "not-an-array"}))
    feed(ctx, [attempt()])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    assert row["rescan_observation"]["result"] == "scan_failed"
    assert row["rescan_observation"]["binding"] == "unbound"
    assert OutcomeTracker.qualified_outcome_counts([row]).ignored == 1
  end

  test "failure to append after a scan leaves no committed observation or receipt", ctx do
    pause = ctx.scan <> ".pause"
    File.write!(pause, "wait")
    feed(ctx, [attempt()])
    task = Task.async(fn -> LearningScheduler.ingest_fleet_outcomes(nil) end)
    assert await_scan(ctx, 100)
    File.write!(Path.join(ctx.data, "outcomes"), "blocked")
    File.rm!(pause)
    assert {0, nil} = Task.await(task, 15_000)
    assert rows(ctx) == []
    assert Receipt.all(ctx.data) == []
    File.rm!(Path.join(ctx.data, "outcomes"))
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert length(rows(ctx)) == 1
    assert length(Receipt.all(ctx.data)) == 1
    assert length(scans(ctx)) == 2
  end

  test "a complete JSON value without its line terminator is not an admitted attempt", ctx do
    raw = attempt()
    File.write!(ctx.feed, Jason.encode!(raw))
    assert {0, nil} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert rows(ctx) == []
    assert scans(ctx) == []
    File.write!(ctx.feed, "\n", [:append])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert length(rows(ctx)) == 1
  end

  test "a nested ID mention cannot replace the admission's receipt source", ctx do
    raw = attempt()

    assert {:ok, _} =
             OutcomeTracker.record_outcome("other", "other", "other", :success, %{
               "unrelated" => %{"event_id" => raw["event_id"]}
             })

    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    receipt = Enum.find(Receipt.all(ctx.data), &(&1["event_id"] == raw["event_id"]))
    assert receipt
    assert receipt["source"]["line"] == "2"
    assert receipt["verdict"] == "audit_only"
  end

  test "source symlink or hardlink aliases into outcomes or receipts are refused", ctx do
    for directory <- ["outcomes", "receipts"], link <- [:symlink, :hardlink] do
      raw = attempt()
      feed(ctx, [raw])
      File.mkdir_p!(Path.join(ctx.data, directory))
      destination = Path.join([ctx.data, directory, "alias.jsonl"])
      File.rename!(ctx.feed, destination)

      if link == :symlink,
        do: File.ln_s!(destination, ctx.feed),
        else: File.ln!(destination, ctx.feed)

      assert capture_log(fn -> assert {0, nil} = LearningScheduler.ingest_fleet_outcomes(nil) end) =~
               "source_outcome_alias"

      assert File.read!(destination) == Jason.encode!(raw) <> "\n"
      assert scans(ctx) == []
      File.rm!(ctx.feed)
      File.rm!(destination)
    end
  end

  test "tagged raw rows with forged consumer fields in the ledger cannot count or receive a receipt",
       ctx do
    raw =
      attempt(%{
        "admission_version" => "fleet-attempt/1",
        "verification" => "verified",
        "source_kind" => "local_rescan"
      })

    File.mkdir_p!(Path.join(ctx.data, "outcomes"))
    File.write!(Path.join([ctx.data, "outcomes", "raw.jsonl"]), Jason.encode!(raw) <> "\n")
    feed(ctx, [raw])

    assert capture_log(fn -> assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil) end) =~
             "conflict"

    assert rows(ctx) == [raw]
    assert OutcomeTracker.qualified_outcome_counts([raw]).ignored == 1
    assert {:error, :invalid_admission_row} = Receipt.write(ctx.data, raw)

    assert {:error, :invalid_admission_row} =
             Receipt.write(ctx.data, Map.delete(raw, "admission_version"))

    assert Receipt.all(ctx.data) == []
    assert scans(ctx) == []
  end

  test "ambiguous persisted admission identity cannot fall back through stripped caller metadata",
       ctx do
    raw = attempt()
    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    File.write!(Path.join([ctx.data, "outcomes", "other.jsonl"]), Jason.encode!(raw) <> "\n")
    File.rm_rf!(Path.join(ctx.data, "receipts"))

    assert {:error, :ambiguous_admission_identity} =
             Receipt.write(ctx.data, Map.delete(row, "admission_version"))

    assert Receipt.all(ctx.data) == []
  end

  test "a later incomplete tail does not erase the count of a committed v1 failure", ctx do
    raw = attempt(%{"outcome" => "failure", "verdict" => "failed", "exit_code" => 9})
    File.write!(ctx.feed, Jason.encode!(raw) <> "\n{\"attempt_contract\":")
    assert {1, nil} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert [row] = rows(ctx)
    assert row["event_id"] == raw["event_id"]
    feed(ctx, [raw])
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert rows(ctx) == [row]
  end
end
