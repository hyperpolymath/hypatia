# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.ApiRouterTest do
  use ExUnit.Case, async: false
  use Plug.Test

  alias Hypatia.Web.ApiRouter
  alias Hypatia.Telemetry, as: T

  setup do
    case Process.whereis(Hypatia.Watcher) do
      nil ->
        {:ok, pid} = Hypatia.Watcher.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    System.delete_env("HYPATIA_API_ALLOW_NONLOCAL")
    :ok
  end

  describe "bearer auth gate" do
    test "no token configured → no auth required (legacy behaviour)" do
      System.delete_env("HYPATIA_API_BEARER_TOKEN")
      conn = build_conn(:get, "/status", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))
      assert conn.status == 200
    end

    test "token configured + valid Bearer header → 200" do
      System.put_env("HYPATIA_API_BEARER_TOKEN", "test-secret-abc123")
      on_exit(fn -> System.delete_env("HYPATIA_API_BEARER_TOKEN") end)

      conn =
        build_conn(:get, "/status", {127, 0, 0, 1})
        |> Plug.Conn.put_req_header("authorization", "Bearer test-secret-abc123")

      conn = ApiRouter.call(conn, ApiRouter.init([]))
      assert conn.status == 200
    end

    test "token configured + wrong Bearer header → 401" do
      System.put_env("HYPATIA_API_BEARER_TOKEN", "test-secret-abc123")
      on_exit(fn -> System.delete_env("HYPATIA_API_BEARER_TOKEN") end)

      conn =
        build_conn(:get, "/status", {127, 0, 0, 1})
        |> Plug.Conn.put_req_header("authorization", "Bearer wrong-token")

      conn = ApiRouter.call(conn, ApiRouter.init([]))
      assert conn.status == 401
      body = Jason.decode!(conn.resp_body)
      assert body["error"] == "invalid_token"
    end

    test "token configured + missing header → 401 missing_token" do
      System.put_env("HYPATIA_API_BEARER_TOKEN", "test-secret-abc123")
      on_exit(fn -> System.delete_env("HYPATIA_API_BEARER_TOKEN") end)

      conn = build_conn(:get, "/status", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))
      assert conn.status == 401
      body = Jason.decode!(conn.resp_body)
      assert body["error"] == "missing_token"
    end

    test "valid token from non-loopback IP → 200 (bearer beats loopback)" do
      System.put_env("HYPATIA_API_BEARER_TOKEN", "test-secret-abc123")
      on_exit(fn -> System.delete_env("HYPATIA_API_BEARER_TOKEN") end)

      conn =
        build_conn(:get, "/status", {10, 1, 2, 3})
        |> Plug.Conn.put_req_header("authorization", "Bearer test-secret-abc123")

      conn = ApiRouter.call(conn, ApiRouter.init([]))
      assert conn.status == 200
    end

    test "lowercase 'bearer' prefix is accepted" do
      System.put_env("HYPATIA_API_BEARER_TOKEN", "test-secret-abc123")
      on_exit(fn -> System.delete_env("HYPATIA_API_BEARER_TOKEN") end)

      conn =
        build_conn(:get, "/status", {127, 0, 0, 1})
        |> Plug.Conn.put_req_header("authorization", "bearer test-secret-abc123")

      conn = ApiRouter.call(conn, ApiRouter.init([]))
      assert conn.status == 200
    end
  end

  describe "loopback gate" do
    test "127.0.0.1 caller is allowed through" do
      conn = build_conn(:get, "/status", {127, 0, 0, 1})

      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 200
    end

    test "non-loopback caller is rejected with 403" do
      conn = build_conn(:get, "/status", {10, 1, 2, 3})

      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 403
      body = Jason.decode!(conn.resp_body)
      assert body["error"] == "loopback_only"
    end

    test "HYPATIA_API_ALLOW_NONLOCAL=true bypasses the gate" do
      System.put_env("HYPATIA_API_ALLOW_NONLOCAL", "true")

      on_exit(fn -> System.delete_env("HYPATIA_API_ALLOW_NONLOCAL") end)

      conn = build_conn(:get, "/status", {10, 1, 2, 3})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 200
    end
  end

  describe "GET /status" do
    test "returns a snapshot with normalised event keys" do
      T.scan_complete(50, 3, path: "/tmp/x", severity_floor: "low")
      Process.sleep(50)

      conn = build_conn(:get, "/status", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 200
      body = Jason.decode!(conn.resp_body)

      # Event names should be dotted strings, not JSON arrays.
      assert Map.has_key?(body["counts"]["m5"], "hypatia.scan.complete")
      assert is_integer(body["counts"]["m5"]["hypatia.scan.complete"])
    end
  end

  describe "GET /counts/:window" do
    test "returns counts for a valid window" do
      T.outcome_recorded(recipe_id: "x", repo: "r", outcome: "success", verification: "verified")
      Process.sleep(50)

      conn = build_conn(:get, "/counts/5m", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 200
      body = Jason.decode!(conn.resp_body)
      assert body["window"] == "5m"
      assert is_map(body["counts"])
    end

    test "returns 400 for an unknown window" do
      conn = build_conn(:get, "/counts/banana", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 400
      body = Jason.decode!(conn.resp_body)
      assert body["error"] == "unknown_window"
    end
  end

  describe "GET /recipes" do
    test "returns the recipe-health roll-up" do
      conn = build_conn(:get, "/recipes", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 200
      body = Jason.decode!(conn.resp_body)
      assert Map.has_key?(body, "count")
      assert is_list(body["rows"])
    end

    test "?status= filter rejects unknown atoms with 400" do
      conn = build_conn(:get, "/recipes?status=not_a_status_atom_xyz", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 400
    end
  end

  describe "404" do
    test "unknown path under /api returns 404 JSON" do
      conn = build_conn(:get, "/no_such_endpoint", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 404
      body = Jason.decode!(conn.resp_body)
      assert body["error"] == "not_found"
    end
  end

  describe "GET /recipes/:id" do
    test "returns 404 for an unknown recipe id" do
      conn = build_conn(:get, "/recipes/does-not-exist-recipe-id", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 404
      body = Jason.decode!(conn.resp_body)
      assert body["error"] == "recipe_not_found"
    end
  end

  describe "GET /quarantine" do
    test "returns the recipe + bot quarantine roster" do
      conn = build_conn(:get, "/quarantine", {127, 0, 0, 1})
      conn = ApiRouter.call(conn, ApiRouter.init([]))

      assert conn.status == 200
      body = Jason.decode!(conn.resp_body)
      assert Map.has_key?(body, "recipes")
      assert Map.has_key?(body, "bots")
      assert is_list(body["recipes"]["rows"])
      assert is_map(body["bots"]["entries"])
    end
  end

  defp build_conn(method, path, remote_ip) do
    conn(method, path)
    |> Map.put(:remote_ip, remote_ip)
  end
end
