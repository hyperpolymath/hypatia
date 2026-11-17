# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.ReviewerRedirectTest do
  # Hypatia.Reviewer sends GITHUB_TOKEN, so :httpc's own redirect handling is
  # off. It follows a redirect itself only to the origin of the first
  # request (GitHub's renamed-repository 301 stays on api.github.com), at
  # most three times.
  use ExUnit.Case, async: true
  alias Hypatia.Reviewer

  @headers [{~c"authorization", ~c"token test-dummy-reviewer"}]

  defmodule Stub do
    def init(respond), do: respond

    def call(conn, {name, parent, respond}) do
      auth = Plug.Conn.get_req_header(conn, "authorization")
      send(parent, {:hit, name, conn.request_path, auth})
      {status, location, body} = respond.(conn.request_path)

      conn
      |> then(fn c ->
        if location, do: Plug.Conn.put_resp_header(c, "location", location), else: c
      end)
      |> Plug.Conn.send_resp(status, body)
    end
  end

  setup_all do
    {:ok, _} = Application.ensure_all_started(:inets)
    :ok
  end

  test "a same-origin redirect is followed with the token" do
    a =
      serve(:a, fn
        "/start" -> {301, "/repositories/1/pulls/2", ""}
        "/repositories/1/pulls/2" -> {200, nil, "diff --git a/x b/x"}
      end)

    assert {:ok, "diff --git a/x b/x"} = Reviewer.get_same_origin("#{a}/start", @headers)
    assert_received {:hit, :a, "/repositories/1/pulls/2", ["token test-dummy-reviewer"]}
  end

  test "a redirect to another origin is refused and never contacted" do
    b = serve(:b, fn _ -> {200, nil, "should not be reached"} end)
    a = serve(:a, fn "/start" -> {301, "#{b}/final", ""} end)

    assert {:error, {:http_status, 301}} = Reviewer.get_same_origin("#{a}/start", @headers)
    refute_received {:hit, :b, _, _}
  end

  test "a redirect loop stops after three hops" do
    a = serve(:a, fn "/loop" -> {302, "/loop", ""} end)

    assert {:error, {:http_status, 302}} = Reviewer.get_same_origin("#{a}/loop", @headers)

    hits = for _ <- 1..4, do: assert_received({:hit, :a, "/loop", _})
    assert length(hits) == 4
    refute_received {:hit, :a, _, _}
  end

  defp serve(name, respond) do
    pid =
      start_supervised!(
        {Bandit, plug: {Stub, {name, self(), respond}}, ip: {127, 0, 0, 1}, port: 0},
        id: name
      )

    {:ok, {_, port}} = ThousandIsland.listener_info(pid)
    "http://127.0.0.1:#{port}"
  end
end
