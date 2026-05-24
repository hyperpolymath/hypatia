# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.GraphQLTest do
  use ExUnit.Case, async: false

  alias Hypatia.Web.GraphQL

  setup do
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
      assert Enum.any?(result["errors"], &(&1["message"] =~ "empty_query" or &1["message"] =~ "no_fields"))
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
end
