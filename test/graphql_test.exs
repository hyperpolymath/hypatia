# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.GraphQLTest do
  use ExUnit.Case, async: false

  import Plug.Test

  alias Hypatia.Web.GraphQL
  alias Hypatia.Web.Router

  setup do
    System.delete_env("HYPATIA_API_ALLOW_NONLOCAL")
    System.delete_env("HYPATIA_API_BEARER_TOKEN")

    on_exit(fn ->
      System.delete_env("HYPATIA_API_ALLOW_NONLOCAL")
      System.delete_env("HYPATIA_API_BEARER_TOKEN")
    end)

    case Process.whereis(Hypatia.Watcher.PubSub) do
      nil ->
        {:ok, pid} = Registry.start_link(keys: :duplicate, name: Hypatia.Watcher.PubSub)
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    case Process.whereis(Hypatia.Watcher) do
      nil ->
        {:ok, pid} = Hypatia.Watcher.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    :ok
  end

  describe "POST /graphql protection" do
    test "requires the configured bearer token" do
      System.put_env("HYPATIA_API_BEARER_TOKEN", "test-secret-abc123")

      conn = call_router({127, 0, 0, 1})

      assert conn.status == 401
      assert Jason.decode!(conn.resp_body)["error"] == "missing_token"
    end

    test "accepts a valid bearer token from a non-loopback client" do
      System.put_env("HYPATIA_API_BEARER_TOKEN", "test-secret-abc123")

      conn =
        {10, 1, 2, 3}
        |> graphql_conn()
        |> Plug.Conn.put_req_header("authorization", "Bearer test-secret-abc123")
        |> Router.call(Router.init([]))

      assert conn.status == 200
      assert Jason.decode!(conn.resp_body)["data"]["health"]["status"] == "ok"
    end

    test "rejects a non-loopback client when no token is configured" do
      conn = call_router({10, 1, 2, 3})

      assert conn.status == 403
      assert Jason.decode!(conn.resp_body)["error"] == "loopback_only"
    end

    test "allows a non-loopback client through the explicit override" do
      System.put_env("HYPATIA_API_ALLOW_NONLOCAL", "true")

      conn = call_router({10, 1, 2, 3})

      assert conn.status == 200
      assert Jason.decode!(conn.resp_body)["data"]["health"]["status"] == "ok"
    end
  end

  describe "execute/1 — single field" do
    test "{ health } returns the health payload" do
      result = GraphQL.execute("{ health }")
      assert result["data"]["health"]["status"] == "ok"
      assert result["data"]["health"]["service"] == "hypatia"
    end

    test "{ status } returns Watcher snapshot fields" do
      result = GraphQL.execute("{ status }")
      assert is_map(result["data"]["status"])
      assert Map.has_key?(result["data"]["status"], "uptime_seconds")
    end

    test "{ alerts } returns the alerts envelope" do
      result = GraphQL.execute("{ alerts }")
      assert Map.has_key?(result["data"]["alerts"], "count")
      assert Map.has_key?(result["data"]["alerts"], "rows")
    end

    test "{ recipes } returns the recipes envelope" do
      result = GraphQL.execute("{ recipes }")
      assert is_integer(result["data"]["recipes"]["count"])
      assert is_list(result["data"]["recipes"]["rows"])
    end
  end

  describe "execute/1 — arguments" do
    test "recipes accepts a status filter" do
      result = GraphQL.execute("{ recipes(status: \"healthy\") }")
      assert is_integer(result["data"]["recipes"]["count"])
    end

    test "recipe(id: ...) returns recipe_not_found for missing id" do
      result = GraphQL.execute("{ recipe(id: \"does-not-exist-recipe-xyz\") }")
      assert result["data"]["recipe"]["error"] == "recipe_not_found"
    end

    test "recipe with no id argument returns missing_argument" do
      result = GraphQL.execute("{ recipe }")
      assert result["data"]["recipe"]["error"] == "missing_argument"
    end
  end

  describe "execute/1 — multi-field" do
    test "multiple top-level fields each resolve" do
      result = GraphQL.execute("{ health status }")
      assert Map.has_key?(result["data"], "health")
      assert Map.has_key?(result["data"], "status")
    end
  end

  describe "execute/1 — errors" do
    test "empty query → error envelope" do
      result = GraphQL.execute("{}")
      assert is_list(result["errors"])

      assert Enum.any?(
               result["errors"],
               &(&1["message"] =~ "empty_query" or &1["message"] =~ "no_fields")
             )
    end

    test "unknown field → unknown_field marker on that field's data" do
      result = GraphQL.execute("{ no_such_thing }")
      assert result["data"]["no_such_thing"]["error"] == "unknown_field"
    end

    test "subfield braces are tolerated even though they don't filter" do
      # The parser must not crash on { status { uptime_seconds } } even
      # though it ignores subfield selection.
      result = GraphQL.execute("{ status { uptime_seconds } }")
      assert is_map(result["data"]["status"])
    end
  end

  defp call_router(remote_ip) do
    remote_ip
    |> graphql_conn()
    |> Router.call(Router.init([]))
  end

  defp graphql_conn(remote_ip) do
    :post
    |> conn("/graphql", Jason.encode!(%{query: "{ health }"}))
    |> Map.put(:remote_ip, remote_ip)
    |> Plug.Conn.put_req_header("content-type", "application/json")
  end
end
