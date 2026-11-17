# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Data.ArangoDBRedirectStatusTest do
  # Hypatia.Data.ArangoDB sends Basic auth and does not follow redirects, so a
  # 3xx reaches its own status handling. A 3xx with a JSON body must be an
  # error, never `{:ok, map}`: otherwise a query or upsert that did not run
  # reads as a success. A loopback server stands in for ArangoDB. It answers
  # the version probe and schema setup successfully, then query POSTs with
  # 307 and the body `{}`.
  use ExUnit.Case, async: false

  setup do
    {:ok, listen} =
      :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true, ip: {127, 0, 0, 1}])

    {:ok, port} = :inet.port(listen)
    server = spawn_link(fn -> serve(listen) end)
    previous = System.get_env("HYPATIA_ARANGO_URL")
    System.put_env("HYPATIA_ARANGO_URL", "http://127.0.0.1:#{port}")

    on_exit(fn ->
      if previous,
        do: System.put_env("HYPATIA_ARANGO_URL", previous),
        else: System.delete_env("HYPATIA_ARANGO_URL")
    end)

    {:ok, pid} = GenServer.start(Hypatia.Data.ArangoDB, [])

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      Process.unlink(server)
      Process.exit(server, :kill)
      :gen_tcp.close(listen)
    end)

    {:ok, pid: pid}
  end

  test "the fake server is reached: the client reports connected", %{pid: pid} do
    assert GenServer.call(pid, :connected?)
  end

  test "a 307 with a JSON body from a query is an error", %{pid: pid} do
    assert {:error, {307, "{}"}} = GenServer.call(pid, {:query, "RETURN 1", %{}})
  end

  defp serve(listen) do
    case :gen_tcp.accept(listen, 10_000) do
      {:ok, sock} ->
        head = read_head(sock, "")

        reply =
          cond do
            String.starts_with?(head, "GET /_api/version") ->
              respond(200, ~s({"server":"arango","version":"3.12.0"}), "")

            String.starts_with?(head, "POST ") and String.contains?(head, "/_api/cursor ") ->
              respond(307, "{}", "Location: http://127.0.0.1:1/elsewhere\r\n")

            true ->
              respond(201, ~s({"error":false}), "")
          end

        :gen_tcp.send(sock, reply)
        :gen_tcp.close(sock)
        serve(listen)

      {:error, _} ->
        :ok
    end
  end

  defp respond(status, body, extra) do
    "HTTP/1.1 #{status} X\r\nContent-Type: application/json\r\n" <>
      extra <> "Content-Length: #{byte_size(body)}\r\nConnection: close\r\n\r\n" <> body
  end

  # Reads the request head and any Content-Length body, and returns the head.
  defp read_head(sock, acc) do
    case :binary.match(acc, "\r\n\r\n") do
      {pos, _} ->
        head = binary_part(acc, 0, pos)

        want =
          case Regex.run(~r/content-length:\s*(\d+)/i, head) do
            [_, n] -> String.to_integer(n)
            nil -> 0
          end

        if byte_size(acc) - pos - 4 >= want, do: head, else: more(sock, acc)

      :nomatch ->
        more(sock, acc)
    end
  end

  defp more(sock, acc) do
    case :gen_tcp.recv(sock, 0, 5_000) do
      {:ok, data} -> read_head(sock, acc <> data)
      {:error, _} -> acc
    end
  end
end
