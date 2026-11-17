# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Data.ArangoDBInitialisationTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  alias Hypatia.Data.ArangoDB

  defmodule Stub do
    def init(respond), do: respond

    def call(conn, respond) do
      {:ok, _body, conn} = Plug.Conn.read_body(conn)
      # A route the test does not describe answers 404, as ArangoDB does for
      # an unknown database or path, so the probe of the target database
      # falls through to creating it.
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
    end
  end

  for existing? <- [false, true] do
    test "schema succeeds with existing resources: #{existing?}" do
      serve(fn
        "GET", "/_api/version" ->
          {200, %{"server" => "arango"}}

        "POST", path ->
          if unquote(existing?) do
            number = if String.ends_with?(path, "/gharial"), do: 1925, else: 1207
            {409, %{"error" => true, "errorNum" => number}}
          else
            {201, %{"error" => false}}
          end
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

  defp serve(respond) do
    pid = start_supervised!({Bandit, plug: {Stub, respond}, ip: {127, 0, 0, 1}, port: 0})
    {:ok, {_, port}} = ThousandIsland.listener_info(pid)
    url = "http://127.0.0.1:#{port}"
    System.put_env("HYPATIA_ARANGO_URL", url)
    url
  end
end
