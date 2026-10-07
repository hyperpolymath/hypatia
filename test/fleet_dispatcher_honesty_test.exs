# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.FleetDispatcherHonestyTest do
  # Mutates process-global env (HYPATIA_RHODIBOT_URL, :verisimdb_data_path),
  # so it cannot run concurrently with other tests.
  use ExUnit.Case, async: false

  alias Hypatia.FleetDispatcher

  @finding %{
    type: :fix_suggestion,
    repo: "test-repo",
    file: "scripts/deploy.sh",
    issue: "Unquoted variable",
    suggestion: "Add double quotes"
  }

  setup do
    original_path = Application.get_env(:hypatia, :verisimdb_data_path)

    on_exit(fn ->
      System.delete_env("HYPATIA_RHODIBOT_URL")
      Application.put_env(:hypatia, :verisimdb_data_path, original_path)
    end)

    :ok
  end

  # Serve exactly one HTTP request with the given status and body on an
  # ephemeral localhost port; returns the base URL.
  defp one_shot_server(status, body) do
    {:ok, listen} = :gen_tcp.listen(0, [:binary, packet: :raw, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)

    spawn_link(fn ->
      {:ok, sock} = :gen_tcp.accept(listen)
      {:ok, _request} = :gen_tcp.recv(sock, 0, 5_000)

      :gen_tcp.send(
        sock,
        "HTTP/1.1 #{status} X\r\ncontent-type: application/json\r\n" <>
          "content-length: #{byte_size(body)}\r\nconnection: close\r\n\r\n" <> body
      )

      :gen_tcp.close(sock)
      :gen_tcp.close(listen)
    end)

    "http://127.0.0.1:#{port}"
  end

  test "a 2xx GraphQL response carrying errors is a failure, not a dispatch" do
    url = one_shot_server(200, ~s({"errors":[{"message":"Unknown field \\"suggestFix\\""}]}))
    System.put_env("HYPATIA_RHODIBOT_URL", url)

    assert {:error, {:live_dispatch_failed, "rhodibot", {:graphql_errors, [_]}}} =
             FleetDispatcher.dispatch_finding(@finding)
  end

  test "a 2xx GraphQL response with data is a live dispatch" do
    url = one_shot_server(200, ~s({"data":{"suggestFix":{"success":true,"prNumber":1}}}))
    System.put_env("HYPATIA_RHODIBOT_URL", url)

    assert {:ok, :dispatched} = FleetDispatcher.dispatch_finding(@finding)
  end

  test "a non-2xx status from a configured URL is an error" do
    url = one_shot_server(500, "{}")
    System.put_env("HYPATIA_RHODIBOT_URL", url)

    assert {:error, {:live_dispatch_failed, "rhodibot", {:http_status, 500}}} =
             FleetDispatcher.dispatch_finding(@finding)
  end

  test "an unreachable configured URL is an error, not a file dispatch" do
    {:ok, listen} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(listen)
    :gen_tcp.close(listen)
    System.put_env("HYPATIA_RHODIBOT_URL", "http://127.0.0.1:#{port}")

    assert {:error, {:live_dispatch_failed, "rhodibot", _}} =
             FleetDispatcher.dispatch_finding(@finding)
  end

  test "an unwritable manifest with no URL configured is an error" do
    blocker =
      Path.join(
        System.tmp_dir!(),
        "hypatia-manifest-blocker-#{System.unique_integer([:positive])}"
      )

    File.write!(blocker, "a file, so mkdir beneath it fails")
    on_exit(fn -> File.rm(blocker) end)
    Application.put_env(:hypatia, :verisimdb_data_path, blocker)

    assert {:error, {:manifest_write_failed, _}} = FleetDispatcher.dispatch_finding(@finding)
  end
end
