# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.MetricsTest do
  use ExUnit.Case, async: false
  use Plug.Test

  alias Hypatia.Web.Metrics
  alias Hypatia.Telemetry, as: T

  setup do
    case Process.whereis(Hypatia.Watcher) do
      nil ->
        {:ok, pid} = Hypatia.Watcher.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    :ok
  end

  describe "render/0 — exposition format" do
    test "emits HELP + TYPE lines for each metric family" do
      output = Metrics.render()

      assert output =~ "# HELP hypatia_events_total "
      assert output =~ "# TYPE hypatia_events_total gauge"
      assert output =~ "# HELP hypatia_queue_depth "
      assert output =~ "# TYPE hypatia_queue_depth gauge"
      assert output =~ "# HELP hypatia_watcher_dropped_total "
      assert output =~ "# TYPE hypatia_watcher_dropped_total counter"
      assert output =~ "# HELP hypatia_watcher_uptime_seconds "
      assert output =~ "# TYPE hypatia_watcher_uptime_seconds gauge"
      assert output =~ "# HELP hypatia_recipe_verification_rate "
      assert output =~ "# HELP hypatia_recipe_quarantine_candidates "
      assert output =~ "# HELP hypatia_recipe_degraded "
    end

    test "event counter labels include window and event" do
      T.scan_complete(50, 3, path: "/tmp/x", severity_floor: "low")
      Process.sleep(50)

      output = Metrics.render()

      # Should appear in all three windows: 5m / 1h / 1d
      assert output =~
               ~r/hypatia_events_total\{event="hypatia.scan.complete",window="5m"\} \d+/

      assert output =~
               ~r/hypatia_events_total\{event="hypatia.scan.complete",window="1h"\} \d+/

      assert output =~
               ~r/hypatia_events_total\{event="hypatia.scan.complete",window="1d"\} \d+/
    end

    test "queue depth label strips Elixir. prefix" do
      # If no supervisor is running there are no queue lines, but if there
      # is one the format must be tidy. Don't require content — assert
      # negation: NO line contains "Elixir." in a label.
      output = Metrics.render()
      refute output =~ ~r/process="Elixir\./
    end

    test "label values are escaped per the Prometheus spec" do
      # Backslashes and quotes are the dangerous chars; we don't expect
      # recipe ids to contain them, but assert by direct test of the
      # output format on something the renderer emits.
      output = Metrics.render()
      lines = String.split(output, "\n")

      # Any line with a label must close all its quote pairs.
      Enum.each(lines, fn line ->
        if line =~ "{" do
          # Count unescaped quotes within braces only.
          inside =
            line
            |> String.split("{", parts: 2)
            |> List.last()
            |> String.split("}", parts: 2)
            |> List.first()

          # Even number of unescaped " in label section.
          unescaped_quotes =
            inside
            |> String.replace("\\\"", "")
            |> String.graphemes()
            |> Enum.count(&(&1 == "\""))

          assert rem(unescaped_quotes, 2) == 0,
                 "unbalanced quotes in label section: #{line}"
        end
      end)
    end
  end

  describe "GET /metrics" do
    test "responds with prometheus text format content-type" do
      conn = conn(:get, "/metrics") |> Metrics.call([])
      assert conn.status == 200

      ctype = conn |> get_resp_header("content-type") |> List.first()
      assert ctype =~ "text/plain"
      assert ctype =~ "version=0.0.4"
    end

    test "body is the same as Metrics.render/0" do
      conn = conn(:get, "/metrics") |> Metrics.call([])
      assert conn.resp_body == Metrics.render()
    end
  end
end
