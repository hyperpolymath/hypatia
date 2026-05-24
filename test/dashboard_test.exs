# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.DashboardTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias Hypatia.Web.Dashboard

  describe "GET /" do
    test "serves the dashboard HTML" do
      conn = conn(:get, "/") |> Dashboard.call([])

      assert conn.status == 200

      ctype =
        conn
        |> get_resp_header("content-type")
        |> List.first()

      assert ctype =~ "text/html"
      assert conn.resp_body =~ "Hypatia Watcher"
      assert conn.resp_body =~ "EventSource"
      assert conn.resp_body =~ "/api/status"
      assert conn.resp_body =~ "/api/events"
    end

    test "sets no-store cache header so refreshes always get fresh dashboard" do
      conn = conn(:get, "/") |> Dashboard.call([])

      cache_control =
        conn
        |> get_resp_header("cache-control")
        |> List.first()

      assert cache_control == "no-store"
    end

    test "references every known telemetry event name in the JS event handler list" do
      conn = conn(:get, "/") |> Dashboard.call([])

      for event <- Hypatia.Telemetry.all_events() do
        dotted = Enum.join(event, ".")

        assert conn.resp_body =~ dotted,
               "dashboard JS handler list missing event '#{dotted}' — " <>
                 "if you add a new telemetry event in Hypatia.Telemetry, " <>
                 "add it to the `known` array in dashboard.ex so the " <>
                 "stream actually renders it"
      end
    end
  end
end
