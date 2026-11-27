# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Data.ArangoDBInitialisationTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  alias Hypatia.Data.ArangoDB

  defmodule Stub do
    def init(arg), do: arg

    # Every response carries `location: /redirect-target`, and that path
    # answers a terminal 200 that would read as success, so a client that
    # followed a redirect would pass a 3xx test. Any request to it is
    # reported to the test process.
    def call(%{request_path: "/redirect-target"} = conn, {parent, _respond}) do
      send(parent, {:redirect_target, conn.method})

      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(200, Jason.encode!(%{"error" => false, "result" => []}))
    end

    def call(conn, {_parent, respond}) do
      {:ok, _body, conn} = Plug.Conn.read_body(conn)
      # A route the test does not describe answers 404 database-not-found
      # (errorNum 1228), as ArangoDB does for an unknown database, so the
      # probes of the target database fall through to `_system` and to
      # creating it.
      {status, body} =
        try do
          respond.(conn.method, conn.request_path)
        rescue
          FunctionClauseError -> {404, %{"error" => true, "errorNum" => 1228}}
        end

      conn
      |> Plug.Conn.put_resp_header("location", "/redirect-target")
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(status, Jason.encode!(body))
    end
  end

  setup do
    previous = System.get_env("HYPATIA_ARANGO_URL")

    on_exit(fn ->
      if previous,
        do: System.put_env("HYPATIA_ARANGO_URL", previous),
        else: System.delete_env("HYPATIA_ARANGO_URL")
    end)

    :ok
  end

  for status <- [301, 302, 303, 307, 308] do
    test "POST rejects #{status} with valid JSON" do
      url = serve(fn _, _ -> {unquote(status), %{"result" => []}} end)

      state = %ArangoDB{
        base_url: url,
        database: "test",
        auth_header: "Basic test-dummy",
        connected: true
      }

      assert {:reply, {:error, {unquote(status), _}}, ^state} =
               ArangoDB.handle_call({:query, "RETURN 1", %{}}, nil, state)

      refute_received {:redirect_target, _}
    end
  end

  test "POST preserves decoded 4xx errors" do
    body = %{"error" => true, "errorMessage" => "test denied"}
    url = serve(fn _, _ -> {403, body} end)

    state = %ArangoDB{
      base_url: url,
      database: "test",
      auth_header: "Basic test-dummy",
      connected: true
    }

    assert {:reply, {:error, "test denied"}, ^state} =
             ArangoDB.handle_call({:query, "RETURN 1", %{}}, nil, state)
  end

  for path <- ["/_api/database", "/_api/collection", "/_api/index", "/_api/gharial"] do
    test "redirect at #{path} leaves initialisation degraded" do
      serve(fn
        "GET", "/_api/version" ->
          {200, %{"server" => "arango"}}

        "POST", path ->
          if String.ends_with?(path, unquote(path)),
            do: {307, %{"result" => true}},
            else: {201, %{"error" => false}}
      end)

      log =
        capture_log(fn ->
          assert {:ok, state} = ArangoDB.init([])
          refute state.connected
          assert state.sync_timer == nil
        end)

      assert log =~ "schema initialisation failed"
      refute log =~ "schema ensured"
      refute log =~ "ArangoDB connected:"
      refute_received {:redirect_target, _}
    end
  end

  for existing? <- [false, true] do
    test "schema succeeds with existing resources: #{existing?}" do
      # The first read of a collection finds nothing, as if another client
      # creates it concurrently; the create then answers 1207 and the re-read
      # finds it, with the right type.
      seen = :ets.new(:seen, [:public])

      serve(fn
        "GET", "/_api/version" ->
          {200, %{"server" => "arango"}}

        "GET", "/_db/hypatia/_api/collection/" <> name ->
          if :ets.insert_new(seen, {name}),
            do: {404, %{"error" => true, "errorNum" => 1203}},
            else: {200, collection_body(name)}

        "POST", path ->
          if unquote(existing?) do
            number = if String.ends_with?(path, "/gharial"), do: 1925, else: 1207
            {409, %{"error" => true, "errorNum" => number}}
          else
            {201, %{"error" => false}}
          end

        "GET", "/_db/hypatia/_api/gharial/hypatia_graph" ->
          {200, graph(required_edge_definitions())}
      end)

      capture_log(fn ->
        assert {:ok, state} = ArangoDB.init([])
        assert state.connected
        assert is_reference(state.sync_timer)
        Process.cancel_timer(state.sync_timer)
      end)
    end
  end

  test "schema API errors enter degraded mode" do
    serve(fn
      "GET", _ -> {200, %{"server" => "arango"}}
      "POST", _ -> {401, %{"error" => true, "errorNum" => 11}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      refute state.connected
      assert state.sync_timer == nil
    end)
  end

  # A gateway or proxy in front of ArangoDB can answer with a JSON 4xx that
  # has no `"error": true`. Only a 2xx, or an existing-resource 4xx (1207,
  # 1925), counts as success.
  test "schema 4xx without the error flag enters degraded mode" do
    serve(fn
      "GET", _ -> {200, %{"server" => "arango"}}
      "POST", _ -> {403, %{"message" => "forbidden"}}
    end)

    log =
      capture_log(fn ->
        assert {:ok, state} = ArangoDB.init([])
        refute state.connected
        assert state.sync_timer == nil
      end)

    refute log =~ "schema ensured"
  end

  # A user with Administrate on a pre-provisioned target database but not on
  # `_system` cannot create databases. The client must see that the target
  # exists and not ask `_system` to create it.
  test "existing target database is not recreated through _system" do
    parent = self()

    serve(fn
      "GET", "/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", "/_db/hypatia/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", path ->
        if String.ends_with?(path, "/_api/database/current"),
          do: {200, %{"error" => false, "result" => %{"name" => "hypatia"}}},
          else: {404, %{"error" => true}}

      "POST", "/_api/database" ->
        send(parent, :created_database)
        {403, %{"error" => true, "errorNum" => 11}}

      "POST", _ ->
        {201, %{"error" => false}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      assert state.connected
      Process.cancel_timer(state.sync_timer)
    end)

    refute_received :created_database
  end

  # Without a readable target database the client still creates it, and a
  # refused creation leaves it degraded.
  test "absent target database is created, and a refusal degrades" do
    parent = self()

    serve(fn
      "GET", "/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", _ ->
        {404, %{"error" => true, "errorNum" => 1228}}

      "POST", "/_api/database" ->
        send(parent, :created_database)
        {403, %{"error" => true, "errorNum" => 11}}

      "POST", _ ->
        {201, %{"error" => false}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      refute state.connected
    end)

    assert_received :created_database
  end

  # The connectivity probe asks the target database, so a user who
  # administers only that database connects although `_system` refuses it.
  test "connectivity is probed on the target database, not _system" do
    parent = self()

    serve(fn
      "GET", "/_db/hypatia/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", "/_api/version" ->
        send(parent, :probed_system)
        {403, %{"error" => true, "errorNum" => 11}}

      "GET", "/_db/hypatia/_api/database/current" ->
        {200, %{"error" => false, "result" => %{"name" => "hypatia"}}}

      "POST", _ ->
        {201, %{"error" => false}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      assert state.connected
      Process.cancel_timer(state.sync_timer)
    end)

    refute_received :probed_system
  end

  # Only a 404 database-not-found on the target falls back to `_system`; any
  # other failure leaves the client disconnected.
  test "a failing target probe other than 404 does not fall back to _system" do
    parent = self()

    serve(fn
      "GET", "/_db/hypatia/_api/version" ->
        {503, %{"error" => true, "errorNum" => 503}}

      "GET", "/_api/version" ->
        send(parent, :probed_system)
        {200, %{"server" => "arango"}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      refute state.connected
      cancel(state.setup_timer)
    end)

    refute_received :probed_system
  end

  # A failed probe of the target database is no evidence that it is absent:
  # only a 404 with errorNum 1228 leads to creating it.
  for status <- [307, 500, 503] do
    test "a #{status} from the database probe fails setup without creating it" do
      parent = self()

      serve(fn
        "GET", "/_db/hypatia/_api/version" ->
          {200, %{"server" => "arango"}}

        "GET", "/_db/hypatia/_api/database/current" ->
          {unquote(status), %{"error" => true, "errorNum" => 1228}}

        "POST", path ->
          send(parent, {:post, path})
          {201, %{"error" => false}}
      end)

      log =
        capture_log(fn ->
          assert {:ok, state} = ArangoDB.init([])
          refute state.connected
          cancel(state.setup_timer)
        end)

      assert log =~ "schema initialisation failed"
      refute_received {:post, _}
      refute_received {:redirect_target, _}
    end
  end

  # When the database step fails, none of the collection, index or graph
  # requests is sent.
  test "a failed database step stops setup before the dependent requests" do
    parent = self()

    serve(fn
      "GET", "/_api/version" ->
        {200, %{"server" => "arango"}}

      "POST", "/_api/database" ->
        {403, %{"error" => true, "errorNum" => 11}}

      "POST", path ->
        send(parent, {:post, path})
        {201, %{"error" => false}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      refute state.connected
      cancel(state.setup_timer)
    end)

    refute_received {:post, _}
  end

  # A duplicate graph (1925) is accepted only when the existing graph has
  # the required edge definitions; order within from/to and extra edge
  # definitions do not matter.
  test "an existing graph with the required edge definitions is accepted" do
    existing =
      [%{"collection" => "extra", "from" => ["repos"], "to" => ["bots"]}] ++
        Enum.map(required_edge_definitions(), fn d ->
          %{d | "from" => Enum.reverse(d["from"]), "to" => Enum.reverse(d["to"])}
        end)

    serve_existing_graph(existing)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      assert state.connected
      Process.cancel_timer(state.sync_timer)
    end)

    # Read first: an existing graph is checked and never written to.
    refute_received {:graph_write, _}
  end

  for {name, change} <- [
        {"different vertex collections",
         quote(
           do: fn d ->
             if d["collection"] == "dispatches", do: %{d | "to" => ["repos"]}, else: d
           end
         )},
        {"a missing edge collection",
         quote(do: fn d -> if d["collection"] == "trusts", do: nil, else: d end)}
      ] do
    test "an existing graph with #{name} is a schema failure, not replaced" do
      existing =
        required_edge_definitions()
        |> Enum.map(unquote(change))
        |> Enum.reject(&is_nil/1)

      serve_existing_graph(existing)

      log =
        capture_log(fn ->
          assert {:ok, state} = ArangoDB.init([])
          refute state.connected
          assert state.sync_timer == nil
          cancel(state.setup_timer)
        end)

      assert log =~ "graph_mismatch"
      refute log =~ "schema ensured"
      refute_received {:graph_write, _}
    end
  end

  # A transient failure during setup leaves the client disconnected, with a
  # retry scheduled and no sync timer; the retry accepts what the first
  # attempt created (1207) and then connects and starts syncing.
  test "a transient schema failure is retried until setup succeeds" do
    put_retry_env(10, 40)
    first = :atomics.new(1, [])

    serve(fn
      "GET", "/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", "/_db/hypatia/_api/database/current" ->
        {200, %{"error" => false, "result" => %{"name" => "hypatia"}}}

      "POST", "/_db/hypatia/_api/collection" ->
        if :atomics.exchange(first, 1, 1) == 0,
          do: {503, %{"error" => true, "errorNum" => 503}},
          else: {409, %{"error" => true, "errorNum" => 1207}}

      # Readable once the first attempt is over: the retry finds them.
      "GET", "/_db/hypatia/_api/collection/" <> name ->
        if :atomics.get(first, 1) == 1,
          do: {200, collection_body(name)},
          else: {404, %{"error" => true, "errorNum" => 1203}}

      "POST", _ ->
        {201, %{"error" => false}}
    end)

    capture_log(fn ->
      {:ok, pid} = GenServer.start(ArangoDB, [])
      on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      assert %{connected: false, sync_timer: nil, setup_timer: retry} = :sys.get_state(pid)
      assert is_reference(retry)

      assert eventually(fn -> GenServer.call(pid, :connected?) end)
      assert %{sync_timer: sync, setup_timer: nil, setup_attempts: 0} = :sys.get_state(pid)
      assert is_reference(sync)
    end)
  end

  test "an unreachable server keeps retrying with backoff and never syncs" do
    put_retry_env(5, 20)
    serve(fn "GET", _ -> {503, %{"error" => true}} end)

    capture_log(fn ->
      {:ok, pid} = GenServer.start(ArangoDB, [])
      on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      assert eventually(fn -> :sys.get_state(pid).setup_attempts >= 4 end)
      assert %{connected: false, sync_timer: nil} = :sys.get_state(pid)
    end)
  end

  # Codex on #1195: a data-only user (database Access, collection Read/Write)
  # of a pre-provisioned schema cannot create anything; every create answers
  # 403. Each resource is read first, so such a user still connects, and no
  # create is sent.
  test "a pre-provisioned schema connects a user who may not create anything" do
    parent = self()

    serve(fn
      "GET", "/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", "/_db/hypatia/_api/database/current" ->
        {200, %{"error" => false, "result" => %{"name" => "hypatia"}}}

      "GET", "/_db/hypatia/_api/collection/" <> name ->
        edges = Enum.map(required_edge_definitions(), & &1["collection"])
        type = if name in edges, do: 3, else: 2
        {200, %{"error" => false, "name" => name, "type" => type}}

      "GET", "/_db/hypatia/_api/index" ->
        # The query string is not part of request_path: every collection
        # answers with every required index, which is enough here.
        indexes =
          for fields <- [
                ["recipe_id", "timestamp"],
                ["timestamp"],
                ["recipe_id"],
                ["repo", "severity"],
                ["pattern_id"],
                ["resolved", "timestamp"],
                ["trust_score"],
                ["quarantined"]
              ],
              do: %{"type" => "persistent", "fields" => fields}

        {200, %{"error" => false, "indexes" => [%{"type" => "primary"} | indexes]}}

      "GET", "/_db/hypatia/_api/gharial/hypatia_graph" ->
        {200, graph(required_edge_definitions())}

      "POST", path ->
        send(parent, {:post, path})
        {403, %{"error" => true, "errorNum" => 11, "errorMessage" => "forbidden"}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      assert state.connected
      Process.cancel_timer(state.sync_timer)
    end)

    refute_received {:post, _}
  end

  test "a resource that is absent is still created, and a 403 then degrades" do
    serve(fn
      "GET", "/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", "/_db/hypatia/_api/database/current" ->
        {200, %{"error" => false, "result" => %{"name" => "hypatia"}}}

      "POST", _ ->
        {403, %{"error" => true, "errorNum" => 11, "errorMessage" => "forbidden"}}
    end)

    capture_log(fn ->
      assert {:ok, state} = ArangoDB.init([])
      refute state.connected
      cancel(state.setup_timer)
    end)
  end

  # Codex on #1195: a retry runs its requests in a task, so a slow ArangoDB
  # does not stop the server answering. Here the retry's first request takes
  # 2 s; a call with a 500 ms timeout must still be answered, in degraded mode.
  test "a slow retry does not block calls" do
    put_retry_env(10, 10)
    first = :atomics.new(1, [])

    serve(fn "GET", "/_api/version" ->
      if :atomics.exchange(first, 1, 1) == 1, do: Process.sleep(2_000)
      {503, %{"error" => true}}
    end)

    capture_log(fn ->
      {:ok, pid} = GenServer.start(ArangoDB, [])
      on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      assert eventually(fn -> is_reference(:sys.get_state(pid).setup_task) end)
      assert GenServer.call(pid, :connected?, 500) == false
    end)
  end

  # Codex on #1195: the next sync is scheduled after this one returns, so a
  # slow sync is not followed by another at once. Each upsert here takes
  # 300 ms; the timer must still have (almost) the whole interval left.
  test "the next sync is scheduled after the current one finishes" do
    dir = Path.join(System.tmp_dir!(), "arango_sync_#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join([dir, "scans", "r"]))
    File.write!(Path.join([dir, "scans", "r", "latest.json"]), ~s({"findings": []}))
    previous = Application.fetch_env(:hypatia, :verisimdb_data_path)
    Application.put_env(:hypatia, :verisimdb_data_path, dir)

    on_exit(fn ->
      File.rm_rf!(dir)

      case previous do
        {:ok, old} -> Application.put_env(:hypatia, :verisimdb_data_path, old)
        :error -> Application.delete_env(:hypatia, :verisimdb_data_path)
      end
    end)

    url =
      serve(fn "POST", _ ->
        Process.sleep(300)
        {201, %{"error" => false, "result" => [%{}]}}
      end)

    state = %ArangoDB{
      base_url: url,
      database: "hypatia",
      auth_header: "Basic test-dummy",
      connected: true
    }

    capture_log(fn ->
      assert {:noreply, %{sync_timer: timer}} =
               ArangoDB.handle_info(:sync_from_verisimdb, state)

      left = Process.read_timer(timer)
      Process.cancel_timer(timer)
      assert left > 10 * 60 * 1_000 - 150
    end)
  end

  # Codex on #1195: a collection of the right name but the wrong type is a
  # schema failure, whether the read finds it first or a 1207 sends us back
  # to read it; it is never replaced.
  for {label, first_read} <- [{"found by the read", :present}, {"behind a 1207", :absent_first}] do
    test "a collection of the wrong type is a schema failure (#{label})" do
      seen = :ets.new(:seen, [:public])
      parent = self()

      serve(fn
        "GET", "/_api/version" ->
          {200, %{"server" => "arango"}}

        "GET", "/_db/hypatia/_api/collection/" <> name ->
          wrong = %{collection_body(name) | "type" => 5 - collection_body(name)["type"]}

          if unquote(first_read) == :absent_first and :ets.insert_new(seen, {name}),
            do: {404, %{"error" => true, "errorNum" => 1203}},
            else: {200, wrong}

        "POST", "/_db/hypatia/_api/collection" ->
          send(parent, :collection_create)
          {409, %{"error" => true, "errorNum" => 1207}}

        "POST", _ ->
          {201, %{"error" => false}}
      end)

      log =
        capture_log(fn ->
          assert {:ok, state} = ArangoDB.init([])
          refute state.connected
          cancel(state.setup_timer)
        end)

      assert log =~ "collection_type_mismatch"

      if unquote(first_read) == :present,
        do: refute_received(:collection_create),
        else: assert_received(:collection_create)
    end
  end

  # Codex on #1195: a unique or sparse index on the same fields is not the
  # index the client needs, so the default one is still created.
  for {label, extra} <- [{"unique", %{"unique" => true}}, {"sparse", %{"sparse" => true}}] do
    test "a #{label} index on the same fields does not stand in for the default one" do
      parent = self()

      serve(fn
        "GET", "/_api/version" ->
          {200, %{"server" => "arango"}}

        "GET", "/_db/hypatia/_api/index" ->
          # Every required index is there as the default one, except that the
          # one on ["timestamp"] carries the option.
          indexes =
            for fields <- required_index_fields() do
              index = %{"type" => "persistent", "fields" => fields}

              if fields == ["timestamp"],
                do: Map.merge(index, unquote(Macro.escape(extra))),
                else: index
            end

          {200, %{"error" => false, "indexes" => indexes}}

        "POST", "/_db/hypatia/_api/index" ->
          send(parent, :index_create)
          {201, %{"error" => false}}

        "POST", _ ->
          {201, %{"error" => false}}
      end)

      capture_log(fn ->
        assert {:ok, state} = ArangoDB.init([])
        assert state.connected
        Process.cancel_timer(state.sync_timer)
      end)

      # Exactly one create: the default index on ["timestamp"] (outcomes).
      assert_received :index_create
      refute_received :index_create
    end
  end

  defp required_index_fields do
    [
      ["recipe_id", "timestamp"],
      ["timestamp"],
      ["recipe_id"],
      ["repo", "severity"],
      ["pattern_id"],
      ["resolved", "timestamp"],
      ["trust_score"],
      ["quarantined"]
    ]
  end

  defp collection_body(name) do
    edges = Enum.map(required_edge_definitions(), & &1["collection"])
    %{"error" => false, "name" => name, "type" => if(name in edges, do: 3, else: 2)}
  end

  defp serve_existing_graph(existing) do
    parent = self()

    serve(fn
      "GET", "/_api/version" ->
        {200, %{"server" => "arango"}}

      "GET", "/_db/hypatia/_api/gharial/hypatia_graph" ->
        {200, graph(existing)}

      method, "/_db/hypatia/_api/gharial" <> _ when method != "GET" ->
        send(parent, {:graph_write, method})
        {409, %{"error" => true, "errorNum" => 1925}}

      "POST", _ ->
        {201, %{"error" => false}}
    end)
  end

  defp graph(edge_definitions) do
    %{
      "error" => false,
      "code" => 200,
      "graph" => %{
        "name" => "hypatia_graph",
        "edgeDefinitions" => edge_definitions,
        "orphanCollections" => []
      }
    }
  end

  defp required_edge_definitions do
    vertices = ["repos", "bots", "recipes", "contributors"]

    [
      %{"collection" => "trusts", "from" => vertices, "to" => vertices},
      %{"collection" => "depends_on", "from" => ["repos"], "to" => ["repos"]},
      %{"collection" => "dispatches", "from" => ["findings"], "to" => ["bots"]},
      %{"collection" => "applies_to", "from" => ["recipes"], "to" => ["patterns"]},
      %{"collection" => "repo_has_finding", "from" => ["repos"], "to" => ["findings"]},
      %{"collection" => "finding_matches_pattern", "from" => ["findings"], "to" => ["patterns"]},
      %{"collection" => "bot_executes_recipe", "from" => ["bots"], "to" => ["recipes"]},
      %{"collection" => "recipe_has_outcome", "from" => ["recipes"], "to" => ["outcomes"]},
      %{"collection" => "contributor_fixes", "from" => ["contributors"], "to" => ["outcomes"]}
    ]
  end

  defp put_retry_env(base, max) do
    for {key, value} <- [arango_setup_retry_base_ms: base, arango_setup_retry_max_ms: max] do
      previous = Application.fetch_env(:hypatia, key)
      Application.put_env(:hypatia, key, value)

      on_exit(fn ->
        case previous do
          {:ok, old} -> Application.put_env(:hypatia, key, old)
          :error -> Application.delete_env(:hypatia, key)
        end
      end)
    end
  end

  defp eventually(check, tries \\ 100) do
    cond do
      check.() -> true
      tries == 0 -> false
      true -> Process.sleep(10) && eventually(check, tries - 1)
    end
  end

  # init/1 called from a test schedules its retry on the test process.
  defp cancel(timer) when is_reference(timer), do: Process.cancel_timer(timer)
  defp cancel(_), do: :ok

  defp serve(respond) do
    pid =
      start_supervised!({Bandit, plug: {Stub, {self(), respond}}, ip: {127, 0, 0, 1}, port: 0})

    {:ok, {_, port}} = ThousandIsland.listener_info(pid)
    url = "http://127.0.0.1:#{port}"
    System.put_env("HYPATIA_ARANGO_URL", url)
    url
  end
end
