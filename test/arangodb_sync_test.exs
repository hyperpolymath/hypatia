# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Data.ArangoDBSyncTest do
  # A sync run counts as complete only when every upsert succeeded. After a
  # failed upsert the run is logged as failed and `last_sync` keeps the last
  # successful timestamp. A Bandit stub stands in for ArangoDB's cursor API
  # and fails the upserts into the collections a test names.
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  alias Hypatia.Data.ArangoDB

  defmodule Stub do
    def init(failing), do: failing

    def call(conn, failing) do
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      collection = get_in(Jason.decode!(body), ["bindVars", "@collection"])

      {status, reply} =
        if collection in failing,
          do: {500, %{"error" => true, "errorNum" => 4}},
          else: {201, %{"error" => false, "result" => [%{"_key" => "k"}]}}

      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(status, Jason.encode!(reply))
    end
  end

  @previous ~U[2026-01-01 00:00:00Z]

  setup do
    dir = Path.join(System.tmp_dir!(), "arangodb-sync-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join([dir, "scans", "repo-a"]))
    File.mkdir_p!(Path.join(dir, "outcomes"))
    File.mkdir_p!(Path.join(dir, "recipes"))

    File.write!(
      Path.join([dir, "scans", "repo-a", "latest.json"]),
      Jason.encode!(%{"findings" => [%{"rule" => "r1"}]})
    )

    File.write!(Path.join([dir, "outcomes", "2026-01.jsonl"]), ~s({"recipe_id":"x"}\n))
    File.write!(Path.join([dir, "recipes", "x.json"]), ~s({"id":"x"}))

    previous = Application.get_env(:hypatia, :verisimdb_data_path)
    Application.put_env(:hypatia, :verisimdb_data_path, dir)

    on_exit(fn ->
      Application.put_env(:hypatia, :verisimdb_data_path, previous)
      File.rm_rf!(dir)
    end)

    :ok
  end

  for failing <- [["repos"], ["findings"], ["outcomes"], ["recipes"]] do
    test "a failed #{hd(failing)} upsert keeps last_sync (cast and timer)" do
      state = state(unquote(failing))

      log =
        capture_log(fn ->
          assert {:noreply, after_cast} = ArangoDB.handle_cast(:sync_from_verisimdb, state)
          assert after_cast.last_sync == @previous

          assert {:noreply, after_timer} = ArangoDB.handle_info(:sync_from_verisimdb, state)
          assert after_timer.last_sync == @previous
          assert is_reference(after_timer.sync_timer)
          Process.cancel_timer(after_timer.sync_timer)
        end)

      assert log =~ "sync from verisim-data failed (1 failed upserts"
      assert log =~ inspect(hd(unquote(failing)))
      refute log =~ "sync from verisim-data complete"
    end
  end

  test "a run in which every upsert succeeds advances last_sync" do
    state = state([])

    log =
      capture_log(fn ->
        assert {:noreply, after_cast} = ArangoDB.handle_cast(:sync_from_verisimdb, state)
        assert DateTime.compare(after_cast.last_sync, @previous) == :gt
      end)

    assert log =~ "sync from verisim-data complete"
  end

  defp state(failing) do
    pid = start_supervised!({Bandit, plug: {Stub, failing}, ip: {127, 0, 0, 1}, port: 0})
    {:ok, {_, port}} = ThousandIsland.listener_info(pid)

    %ArangoDB{
      base_url: "http://127.0.0.1:#{port}",
      database: "test",
      auth_header: "Basic test-dummy",
      connected: true,
      last_sync: @previous
    }
  end
end
