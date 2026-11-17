# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Data.ArangoDBInitialisationTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  alias Hypatia.Data.ArangoDB

  defmodule Stub do
    def init(respond), do: respond

    def call(conn, respond) do
      {:ok, _body, conn} = Plug.Conn.read_body(conn)
      {status, body} = respond.(conn.method, conn.request_path)

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

  defp serve(respond) do
    pid = start_supervised!({Bandit, plug: {Stub, respond}, ip: {127, 0, 0, 1}, port: 0})
    {:ok, {_, port}} = ThousandIsland.listener_info(pid)
    url = "http://127.0.0.1:#{port}"
    System.put_env("HYPATIA_ARANGO_URL", url)
    url
  end
end
