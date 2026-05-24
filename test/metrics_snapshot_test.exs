# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.MetricsSnapshotTest do
  use ExUnit.Case, async: true

  alias Hypatia.Web.MetricsSnapshot

  describe "build/0" do
    test "returns the documented Ada-TUI keys" do
      snap = MetricsSnapshot.build()

      for key <- ~w(repos weak_points dispatched outcomes recipes confidence status generated_at) do
        assert Map.has_key?(snap, key), "snapshot missing key: #{key}"
      end
    end

    test "all numeric keys are non-negative" do
      snap = MetricsSnapshot.build()

      for key <- ~w(repos weak_points dispatched outcomes recipes) do
        assert is_integer(snap[key])
        assert snap[key] >= 0
      end

      assert is_float(snap["confidence"]) or is_integer(snap["confidence"])
      assert snap["confidence"] >= 0.0
    end

    test "status is 'ok' for a normally-running build, 'degraded' on failure path" do
      snap = MetricsSnapshot.build()
      assert snap["status"] in ["ok", "degraded"]
    end

    test "generated_at parses as ISO-8601" do
      snap = MetricsSnapshot.build()
      assert {:ok, _dt, _} = DateTime.from_iso8601(snap["generated_at"])
    end

    test "result is JSON-encodable (Ada TUI consumes via Jason)" do
      snap = MetricsSnapshot.build()
      assert {:ok, _} = Jason.encode(snap)
    end
  end
end
