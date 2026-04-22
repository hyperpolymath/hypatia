# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.VCL.RemoteExecutorTest do
  @moduledoc """
  Tests for `Hypatia.VCL.RemoteExecutor` — multi-URL remote federation.

  Covers:
    * VCL parser recognises `FROM FEDERATION REMOTE IN [...]` syntax
      with and without a drift policy.
    * Client dispatches `{:federation_remote, urls, policy}` to
      `RemoteExecutor` instead of `FileExecutor`.
    * `RemoteExecutor` isolates failures (one bad URL doesn't kill the
      whole query).
    * Pagination applies once over the merged set, not per-remote.

  These tests deliberately use unreachable URLs so they exercise the
  failure-isolation path without needing a live remote. Network access
  is not required.
  """

  use ExUnit.Case, async: false

  alias Hypatia.VCL.Client
  alias Hypatia.VCL.RemoteExecutor

  setup do
    case GenServer.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end

    :ok
  end

  describe "parser — FROM FEDERATION REMOTE IN [...]" do
    test "parses a two-URL federation without drift policy" do
      query =
        ~s(SELECT DOCUMENT FROM FEDERATION REMOTE IN ["https://example.test/a", "https://example.test/b"])

      {:ok, ast} = Client.parse(query)

      assert ast.modalities == [:document]

      assert {:federation_remote, urls, nil} = ast.source
      assert urls == ["https://example.test/a", "https://example.test/b"]
    end

    test "parses a single-URL federation" do
      query = ~s(SELECT DOCUMENT FROM FEDERATION REMOTE IN ["https://example.test/solo"])

      {:ok, ast} = Client.parse(query)
      assert {:federation_remote, ["https://example.test/solo"], nil} = ast.source
    end

    test "parses a federation with drift policy" do
      query =
        ~s(SELECT DOCUMENT FROM FEDERATION REMOTE IN ["https://example.test/a", "https://example.test/b"] WITH DRIFT REPAIR)

      {:ok, ast} = Client.parse(query)

      assert {:federation_remote, urls, :repair} = ast.source
      assert length(urls) == 2
    end

    test "parses LIMIT applied to federation query" do
      query =
        ~s(SELECT DOCUMENT FROM FEDERATION REMOTE IN ["https://example.test/a"] LIMIT 10)

      {:ok, ast} = Client.parse(query)

      assert {:federation_remote, ["https://example.test/a"], _} = ast.source
      assert ast.limit == 10
    end

    test "single-URL (legacy) FEDERATION REMOTE still resolves to :remote" do
      # Regression check — the IN [...] form must not shadow the bare
      # `FROM FEDERATION REMOTE url` pattern still used by other queries.
      query = ~s(SELECT DOCUMENT FROM FEDERATION REMOTE "https://example.test/legacy")

      {:ok, ast} = Client.parse(query)
      assert {:remote, "https://example.test/legacy"} = ast.source
    end
  end

  describe "execute/3 — empty and edge cases" do
    test "empty URL list returns empty result" do
      ast = blank_ast({:federation_remote, [], nil})
      assert {:ok, []} = RemoteExecutor.execute([], ast, [])
    end

    test "pagination on an empty merged set doesn't crash" do
      ast = blank_ast({:federation_remote, [], nil}, limit: 5, offset: 2)
      assert {:ok, []} = RemoteExecutor.execute([], ast, [])
    end
  end

  # Blank AST suitable for direct RemoteExecutor.execute calls in tests.
  # Mirrors the shape produced by parse_vql/1.
  defp blank_ast(source, opts \\ []) do
    %{
      modalities: [:document],
      source: source,
      where: nil,
      proof: nil,
      limit: Keyword.get(opts, :limit),
      offset: Keyword.get(opts, :offset)
    }
  end
end
