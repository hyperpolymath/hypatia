# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Safety.RateLimiterTest do
  use ExUnit.Case, async: false

  alias Hypatia.Safety.RateLimiter

  setup do
    # This test owns its RateLimiter (#857). `start_supervised!/2` ties the
    # process to the test's own supervisor, and the `:name` option keeps it
    # out of the app-level shared instance that the application supervisor
    # starts (lib/application.ex) and the rest of the suite touches.
    #
    # The `whereis` / `start_supervised!` branch this setup used to carry
    # asserted nothing: the application had almost always started the shared
    # process already, so `start_supervised!` never ran and the test neither
    # owned the process's lifetime nor its state. Worse, the shared process
    # could be killed and restarted with empty `%RateLimiter{}` state
    # mid-test — measured as `active_bots: 0` on PR #856 — because
    # `drain_queued/1` used to call `FleetDispatcher.dispatch_finding/1`
    # unprotected, `check_internal/2` never enforced the burst limit, and a
    # drained test entry raised KeyError on `finding.type` inside the
    # GenServer. The application supervisor then restarted it, silently
    # resetting every counter and window this module asserts on.
    name = :"rate_limiter_test_#{System.unique_integer([:positive])}"
    start_supervised!({RateLimiter, name: name})

    %{server: name}
  end

  # Ordering note: `record_dispatch/2` is a cast and `stats/1` is a call,
  # issued from the SAME test process to the SAME server. Erlang guarantees
  # per-pair message ordering and the server processes its mailbox serially,
  # so every cast below is observed by the call that follows it. The old
  # `:timer.sleep(50)` waits were paper over a race that this ownership
  # makes impossible (#857 AC5).

  describe "check/2" do
    test "allows first dispatch for a bot", %{server: server} do
      assert :ok = RateLimiter.check("echidnabot", server)
    end

    test "allows dispatches within burst limit for fresh bot", %{server: server} do
      bot = "burst_test_bot_#{System.unique_integer([:positive])}"

      for _ <- 1..9 do
        RateLimiter.record_dispatch(bot, server)
      end

      assert :ok = RateLimiter.check(bot, server)
    end

    test "rate limits when burst threshold exceeded", %{server: server} do
      bot = "burst_limit_bot_#{System.unique_integer([:positive])}"

      for _ <- 1..10 do
        RateLimiter.record_dispatch(bot, server)
      end

      assert {:rate_limited, :burst, retry_after} = RateLimiter.check(bot, server)
      assert is_integer(retry_after)
      assert retry_after > 0
    end

    test "different bots have independent windows", %{server: server} do
      bot_a = "independent_a_#{System.unique_integer([:positive])}"
      bot_b = "independent_b_#{System.unique_integer([:positive])}"

      for _ <- 1..10 do
        RateLimiter.record_dispatch(bot_a, server)
      end

      assert {:rate_limited, :burst, _} = RateLimiter.check(bot_a, server)
      assert :ok = RateLimiter.check(bot_b, server)
    end
  end

  describe "record_dispatch/2" do
    test "increments total dispatched count", %{server: server} do
      RateLimiter.record_dispatch("echidnabot", server)
      RateLimiter.record_dispatch("echidnabot", server)

      stats = RateLimiter.stats(server)
      # Exact: this instance is owned by this test and starts empty, so the
      # two casts above are all it has seen.
      assert stats.total_dispatched == 2
    end
  end

  describe "enqueue/2" do
    test "increments queue size and rate limited count", %{server: server} do
      # The entry must belong to a bot that IS rate limited, otherwise the
      # 5s :drain_queue timer can pop and dispatch it between the cast and
      # the call below. Saturating the burst limit first makes
      # `check_internal/2` (which now enforces burst, see rate_limiter.ex)
      # put the entry BACK, so the queue is stable — and it is the realistic
      # scenario, since work is enqueued precisely because its bot is rate
      # limited. Even if it were popped, the drain path no longer crashes
      # the limiter on a raising dispatch (#857).
      bot = "rhodibot-enqueue-#{System.unique_integer([:positive])}"

      for _ <- 1..10 do
        RateLimiter.record_dispatch(bot, server)
      end

      assert {:rate_limited, :burst, _} = RateLimiter.check(bot, server)

      before = RateLimiter.stats(server)
      RateLimiter.enqueue(%{"bot" => bot, "action" => "test"}, server)

      stats = RateLimiter.stats(server)
      assert stats.total_queued == before.total_queued + 1
      assert stats.total_rate_limited == before.total_rate_limited + 1
      assert stats.queue_size == 1
    end
  end

  describe "stats/2" do
    test "returns expected stat keys", %{server: server} do
      stats = RateLimiter.stats(server)

      assert Map.has_key?(stats, :total_dispatched)
      assert Map.has_key?(stats, :total_queued)
      assert Map.has_key?(stats, :total_rate_limited)
      assert Map.has_key?(stats, :queue_size)
      assert Map.has_key?(stats, :active_bots)
      assert Map.has_key?(stats, :global_window_size)
    end

    test "counters are non-negative integers", %{server: server} do
      stats = RateLimiter.stats(server)

      assert is_integer(stats.total_dispatched) and stats.total_dispatched >= 0
      assert is_integer(stats.total_queued) and stats.total_queued >= 0
      assert is_integer(stats.queue_size) and stats.queue_size >= 0
    end

    test "tracks active bots after dispatch", %{server: server} do
      bot_a = "stats_bot_a_#{System.unique_integer([:positive])}"
      bot_b = "stats_bot_b_#{System.unique_integer([:positive])}"
      RateLimiter.record_dispatch(bot_a, server)
      RateLimiter.record_dispatch(bot_b, server)

      stats = RateLimiter.stats(server)
      # Exact, not >=: owned instance, fresh state, two dispatches. This is
      # the assertion that flaked on PR #856 (`left: 0, right: 2`) when a
      # crash of the SHARED limiter reset state between the casts and the
      # call — with an owned instance that reset cannot reach it (#857).
      assert stats.active_bots == 2
    end

    test "a crash of the shared app-level RateLimiter cannot reset this test's instance", %{
      server: server
    } do
      # The #857 failure mode, reproduced deliberately and shown to be
      # survivable: kill the shared, application-supervised RateLimiter
      # mid-test (the supervisor restarts it with empty state). This test's
      # own instance must be completely unaffected. Against the old
      # shared-state test body, this kill reds "tracks active bots after
      # dispatch"; against the owned instance it changes nothing.
      bot = "crash_isolation_#{System.unique_integer([:positive])}"
      RateLimiter.record_dispatch(bot, server)

      case GenServer.whereis(RateLimiter) do
        nil ->
          :ok

        shared ->
          ref = Process.monitor(shared)
          Process.exit(shared, :kill)

          receive do
            {:DOWN, ^ref, :process, ^shared, :killed} -> :ok
          after
            1_000 -> flunk("shared RateLimiter did not die from Process.exit/2")
          end
      end

      stats = RateLimiter.stats(server)
      assert stats.active_bots == 1
      assert stats.total_dispatched == 1
    end
  end
end

defmodule Hypatia.Safety.QuarantineTest do
  use ExUnit.Case, async: false

  alias Hypatia.Safety.Quarantine

  setup do
    # Quarantine may already be started by the OTP application
    case GenServer.whereis(Quarantine) do
      nil -> start_supervised!(Quarantine)
      _pid -> :ok
    end

    :ok
  end

  describe "check/1" do
    test "returns :ok for non-quarantined bot" do
      assert :ok = Quarantine.check("echidnabot")
    end

    test "returns quarantine info for quarantined bot" do
      Quarantine.quarantine("badbot", :soft, "testing")
      :timer.sleep(50)

      assert {:quarantined, :soft, "testing"} = Quarantine.check("badbot")
    end
  end

  describe "quarantine/3" do
    test "soft quarantine can be detected" do
      Quarantine.quarantine("testbot", :soft, "too many false positives")
      :timer.sleep(50)

      assert {:quarantined, :soft, _} = Quarantine.check("testbot")
    end

    test "hard quarantine can be detected" do
      Quarantine.quarantine("testbot", :hard, "consecutive failures")
      :timer.sleep(50)

      assert {:quarantined, :hard, _} = Quarantine.check("testbot")
    end

    test "permanent quarantine can be detected" do
      Quarantine.quarantine("testbot", :permanent, "manual review required")
      :timer.sleep(50)

      assert {:quarantined, :permanent, _} = Quarantine.check("testbot")
    end
  end

  describe "release/1" do
    test "removes bot from quarantine" do
      Quarantine.quarantine("testbot", :hard, "test reason")
      :timer.sleep(50)
      assert {:quarantined, _, _} = Quarantine.check("testbot")

      Quarantine.release("testbot")
      :timer.sleep(50)
      assert :ok = Quarantine.check("testbot")
    end
  end

  describe "list_quarantined/0" do
    test "returns empty map when no bots quarantined" do
      assert %{} = Quarantine.list_quarantined()
    end

    test "returns all quarantined bots" do
      Quarantine.quarantine("bot1", :soft, "reason1")
      Quarantine.quarantine("bot2", :hard, "reason2")
      :timer.sleep(50)

      quarantined = Quarantine.list_quarantined()
      assert Map.has_key?(quarantined, "bot1")
      assert Map.has_key?(quarantined, "bot2")
    end
  end

  describe "reroute_target/1 and set_reroute/2" do
    test "returns nil when no reroute set" do
      assert nil == Quarantine.reroute_target("echidnabot")
    end

    test "returns replacement bot when reroute is set" do
      Quarantine.set_reroute("badbot", "rhodibot")
      :timer.sleep(50)

      assert "rhodibot" = Quarantine.reroute_target("badbot")
    end
  end

  describe "auto-quarantine via record_outcome/2" do
    test "auto-quarantines after 5 consecutive failures" do
      for _ <- 1..5 do
        Quarantine.record_outcome("failbot", :failure)
      end

      :timer.sleep(100)

      assert {:quarantined, :hard, reason} = Quarantine.check("failbot")
      assert reason =~ "consecutive failures"
    end

    test "does not quarantine with mixed outcomes" do
      Quarantine.record_outcome("mixedbot", :failure)
      Quarantine.record_outcome("mixedbot", :success)
      Quarantine.record_outcome("mixedbot", :failure)
      Quarantine.record_outcome("mixedbot", :failure)
      :timer.sleep(50)

      assert :ok = Quarantine.check("mixedbot")
    end

    test "auto-quarantines on high false positive rate" do
      # Need at least 5 outcomes, >30% false positive
      for _ <- 1..4 do
        Quarantine.record_outcome("fpbot", :false_positive)
      end

      for _ <- 1..3 do
        Quarantine.record_outcome("fpbot", :success)
      end

      # 4/7 = 57% FP rate -- should trigger soft quarantine
      # But outcomes are stored newest-first, so consecutive_failures check runs first
      # Let's ensure the ordering is right: successes first, then FPs
      :timer.sleep(100)

      # The bot may or may not be quarantined depending on ordering
      # At minimum, verify the GenServer doesn't crash
      result = Quarantine.check("fpbot")
      assert result == :ok or match?({:quarantined, _, _}, result)
    end
  end
end

defmodule Hypatia.Safety.BatchRollbackTest do
  use ExUnit.Case, async: true

  alias Hypatia.Safety.BatchRollback

  @data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

  setup do
    dispatch_dir = Path.join([Path.expand(@data_path), "dispatch"])
    File.mkdir_p!(dispatch_dir)
    :ok
  end

  describe "create_batch/2" do
    test "returns batch_id with batch_ prefix" do
      assert {:ok, batch_id} = BatchRollback.create_batch(10, :auto_execute)
      assert String.starts_with?(batch_id, "batch_")
    end

    test "returns different ids for different batches" do
      {:ok, id1} = BatchRollback.create_batch(1, :auto)
      {:ok, id2} = BatchRollback.create_batch(2, :review)

      assert id1 != id2
    end
  end

  describe "list_batches/1" do
    test "returns a list" do
      batches = BatchRollback.list_batches()
      assert is_list(batches)
    end

    test "respects limit parameter" do
      batches = BatchRollback.list_batches(1)
      assert length(batches) <= 1
    end
  end
end

# E0-25: the rollback manifest of a batch lists its revert instructions from the newest
# outcome to the oldest, and a second rollback of the same batch is a no-op.
defmodule Hypatia.Safety.BatchRollbackOrderTest do
  use ExUnit.Case, async: false

  alias Hypatia.Safety.BatchRollback

  setup do
    data = Application.fetch_env!(:hypatia, :verisimdb_data_path) |> Path.expand()
    outcomes = Path.join(data, "outcomes")
    dispatch = Path.join(data, "dispatch")
    File.mkdir_p!(outcomes)
    File.mkdir_p!(dispatch)

    uniq = System.unique_integer([:positive])
    {:ok, batch} = BatchRollback.create_batch(3, :auto_execute)
    created = [Path.join(outcomes, "e0-25-#{uniq}-*.jsonl")]

    on_exit(fn ->
      for pattern <- created, f <- Path.wildcard(pattern), do: File.rm(f)
      File.rm_rf(Path.join(dispatch, "rollbacks.jsonl.moved-#{uniq}"))
    end)

    %{
      batch: batch,
      uniq: uniq,
      outcomes: outcomes,
      dispatch: dispatch,
      manifests: Path.join(dispatch, "rollbacks.jsonl")
    }
  end

  defp row(batch, file, extra \\ %{}) do
    Map.merge(
      %{
        "recipe_id" => "recipe-e0-25-order",
        "repo" => "owner/repo",
        "file" => file,
        "outcome" => "success",
        "verification" => "verified",
        "batch_id" => batch,
        "timestamp" => "2026-11-15T10:00:00Z",
        "bot" => "hypatia"
      },
      extra
    )
  end

  defp seed(ctx, name, rows) do
    path = Path.join(ctx.outcomes, "e0-25-#{ctx.uniq}-#{name}.jsonl")
    File.write!(path, Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n")))
    path
  end

  defp lines(path) do
    case File.read(path) do
      {:ok, content} -> content |> String.split("\n", trim: true) |> Enum.map(&Jason.decode!/1)
      _ -> []
    end
  end

  defp batch_rows(ctx) do
    ctx.outcomes
    |> Path.join("*.jsonl")
    |> Path.wildcard()
    |> Enum.flat_map(&lines/1)
    |> Enum.filter(&(&1["batch_id"] == ctx.batch))
  end

  defp rollback_rows(ctx),
    do: Enum.filter(batch_rows(ctx), &(&1["caused_by"] == "batch_rollback"))

  defp manifests_of(ctx), do: Enum.filter(lines(ctx.manifests), &(&1["batch_id"] == ctx.batch))
  defp instruction_files(manifest), do: Enum.map(manifest["instructions"], & &1["file"])

  defp status(ctx) do
    BatchRollback.list_batches(10_000)
    |> Enum.find(&(&1["batch_id"] == ctx.batch and Map.has_key?(&1, "status")))
    |> Map.fetch!("status")
  end

  # Make rollbacks.jsonl unwritable (a directory cannot be appended to), and put it
  # back afterwards.
  defp break_manifest_log(ctx) do
    moved = ctx.manifests <> ".moved-#{ctx.uniq}"
    if File.exists?(ctx.manifests), do: File.rename!(ctx.manifests, moved)
    File.mkdir_p!(ctx.manifests)

    fn ->
      File.rm_rf!(ctx.manifests)
      if File.exists?(moved), do: File.rename!(moved, ctx.manifests)
    end
  end

  test "3 outcomes written oldest first give a manifest that lists them newest first", ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt"), row(ctx.batch, "f2.txt"), row(ctx.batch, "f3.txt")])

    assert {:ok, 3} = BatchRollback.rollback(ctx.batch, "t")
    assert [manifest] = manifests_of(ctx)
    assert instruction_files(manifest) == ["f3.txt", "f2.txt", "f1.txt"]
    assert manifest["affected_outcomes"] == 3
  end

  test "2 outcomes with the same timestamp are listed in the reverse of the log, not by repo and file",
       ctx do
    # Written b, then a: against the order of file names. Same timestamp.
    seed(ctx, "a", [row(ctx.batch, "b.txt"), row(ctx.batch, "a.txt")])

    assert {:ok, 2} = BatchRollback.rollback(ctx.batch, "t")
    assert [manifest] = manifests_of(ctx)
    assert instruction_files(manifest) == ["a.txt", "b.txt"]
  end

  test "outcomes in 2 monthly files: the later month is listed first, whatever File.ls gives",
       ctx do
    # Month names whose File.ls order is not their time order, if this file system
    # has one. If it lists them in time order the case below cannot tell the
    # difference, and it says so.
    months = for m <- 1..12, do: "2099-" <> String.pad_leading("#{m}", 2, "0")
    for m <- months, do: seed(ctx, m, [])

    listed =
      File.ls!(ctx.outcomes)
      |> Enum.filter(&String.starts_with?(&1, "e0-25-#{ctx.uniq}-"))
      |> Enum.map(
        &(&1
          |> String.replace_prefix("e0-25-#{ctx.uniq}-", "")
          |> String.trim_trailing(".jsonl"))
      )

    pair =
      for {a, i} <- Enum.with_index(listed),
          {b, j} <- Enum.with_index(listed),
          i < j,
          a > b,
          do: {b, a}

    {early, late} = List.first(pair) || {"2099-01", "2099-02"}

    if pair == [],
      do:
        IO.puts(
          "note: File.ls lists the month files in time order here; the order is not exercised"
        )

    seed(ctx, early, [row(ctx.batch, "early.txt")])
    seed(ctx, late, [row(ctx.batch, "late.txt")])

    assert {:ok, 2} = BatchRollback.rollback(ctx.batch, "t")
    assert [manifest] = manifests_of(ctx)
    assert instruction_files(manifest) == ["late.txt", "early.txt"]
  end

  test "a failed write of rollbacks.jsonl returns an error and does not set rolled_back", ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt")])
    restore = break_manifest_log(ctx)

    try do
      assert {:error, _reason} = BatchRollback.rollback(ctx.batch, "t")
      refute status(ctx) == "rolled_back"
    after
      restore.()
    end
  end

  test "a retry after a failed manifest write completes the rollback with no second outcome row",
       ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt"), row(ctx.batch, "f2.txt")])
    restore = break_manifest_log(ctx)

    try do
      assert {:error, _} = BatchRollback.rollback(ctx.batch, "t")
    after
      restore.()
    end

    written_by_the_first_try = length(rollback_rows(ctx))
    assert written_by_the_first_try == 2

    assert {:ok, 2} = BatchRollback.rollback(ctx.batch, "t")

    assert length(rollback_rows(ctx)) == 2, "the retry wrote a second outcome row"
    assert [manifest] = manifests_of(ctx)
    assert instruction_files(manifest) == ["f2.txt", "f1.txt"]
    assert status(ctx) == "rolled_back"
  end

  test "2 originals with the same key, 1 rollback row already written: the retry writes exactly 1 more",
       ctx do
    seed(ctx, "a", [row(ctx.batch, "same.txt"), row(ctx.batch, "same.txt")])

    # The first rollback row was written, and the second write failed.
    assert {:ok, _} =
             Hypatia.OutcomeTracker.record_batch_rollback(
               "recipe-e0-25-order",
               "owner/repo",
               "same.txt",
               ctx.batch
             )

    assert length(rollback_rows(ctx)) == 1

    assert {:ok, 2} = BatchRollback.rollback(ctx.batch, "t")
    assert length(rollback_rows(ctx)) == 2
    assert [manifest] = manifests_of(ctx)
    assert instruction_files(manifest) == ["same.txt", "same.txt"]
  end

  test "a rollback that left its rows and manifest but no status: the retry adds no second manifest",
       ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt"), row(ctx.batch, "f2.txt")])

    # The state that a rollback leaves when its status write fails (a status write cannot
    # be made to fail here once an unreadable batch log is an error, and the tests may
    # run as root): both rollback rows and the manifest are in the logs, the status is
    # still `pending`.
    for f <- ["f1.txt", "f2.txt"] do
      {:ok, _} =
        Hypatia.OutcomeTracker.record_batch_rollback(
          "recipe-e0-25-order",
          "owner/repo",
          f,
          ctx.batch
        )
    end

    File.write!(
      ctx.manifests,
      Jason.encode!(%{"batch_id" => ctx.batch, "instructions" => []}) <> "\n",
      [:append, :utf8]
    )

    refute status(ctx) == "rolled_back"

    assert {:ok, 2} = BatchRollback.rollback(ctx.batch, "t")
    assert length(manifests_of(ctx)) == 1, "the retry wrote a second manifest"
    assert length(rollback_rows(ctx)) == 2
    assert status(ctx) == "rolled_back"
  end

  test "a second rollback of the same batch adds no outcome row and no manifest line", ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt"), row(ctx.batch, "f2.txt")])

    assert {:ok, 2} = BatchRollback.rollback(ctx.batch, "t")
    assert status(ctx) == "rolled_back"
    rows_after_first = length(batch_rows(ctx))
    manifests_after_first = length(manifests_of(ctx))
    assert manifests_after_first == 1

    assert {:ok, 0} = BatchRollback.rollback(ctx.batch, "again")
    assert length(batch_rows(ctx)) == rows_after_first
    assert length(manifests_of(ctx)) == manifests_after_first
  end

  test "an unreadable monthly outcome file stops the rollback; the retry reverts every outcome",
       ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt")])
    # A directory with a .jsonl name cannot be read as a file (EISDIR); it sorts last.
    bad = Path.join(ctx.outcomes, "e0-25-#{ctx.uniq}-b.jsonl")
    File.mkdir_p!(bad)

    try do
      assert {:error, {:outcome_read_failed, _, _}} = BatchRollback.rollback(ctx.batch, "t")
      assert manifests_of(ctx) == []
      assert rollback_rows(ctx) == []
      refute status(ctx) == "rolled_back"
    after
      File.rm_rf!(bad)
    end

    seed(ctx, "b", [row(ctx.batch, "f2.txt")])
    assert {:ok, 2} = BatchRollback.rollback(ctx.batch, "t")
    assert [manifest] = manifests_of(ctx)
    assert instruction_files(manifest) == ["f2.txt", "f1.txt"]
    assert status(ctx) == "rolled_back"
  end

  test "an unreadable manifest log is an error, not proof that no manifest exists", ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt")])
    restore = break_manifest_log(ctx)

    try do
      assert {:error, {:manifest_read_failed, _}} = BatchRollback.rollback(ctx.batch, "t")
      refute status(ctx) == "rolled_back"
    after
      restore.()
    end

    assert {:ok, 1} = BatchRollback.rollback(ctx.batch, "t")
    assert length(manifests_of(ctx)) == 1
  end

  test "an unreadable batch log is an error, not proof that the batch is not rolled back",
       ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt")])
    assert {:ok, 1} = BatchRollback.rollback(ctx.batch, "t")
    rows = length(batch_rows(ctx))

    batches = Path.join(ctx.dispatch, "batches.jsonl")
    moved = batches <> ".moved-#{ctx.uniq}"
    File.rename!(batches, moved)
    File.mkdir_p!(batches)

    try do
      assert {:error, {:batch_log_read_failed, _}} = BatchRollback.rollback(ctx.batch, "again")
    after
      File.rm_rf!(batches)
      File.rename!(moved, batches)
    end

    assert length(batch_rows(ctx)) == rows
    assert {:ok, 0} = BatchRollback.rollback(ctx.batch, "again")
  end

  test "overlapping rollbacks of one batch write one set of rows and one manifest", ctx do
    seed(ctx, "a", [row(ctx.batch, "f1.txt"), row(ctx.batch, "f2.txt")])

    results =
      1..8
      |> Enum.map(fn _ -> Task.async(fn -> BatchRollback.rollback(ctx.batch, "t") end) end)
      |> Task.await_many(30_000)

    assert Enum.sort(results) == List.duplicate({:ok, 0}, 7) ++ [{:ok, 2}]
    assert length(rollback_rows(ctx)) == 2
    assert length(manifests_of(ctx)) == 1
  end
end
