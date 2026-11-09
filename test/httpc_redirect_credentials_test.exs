# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.HttpcRedirectCredentialsTest do
  # Guards the Erlang/OTP pin (mise.toml and the CI test lanes), not Hypatia's
  # code. OTP releases before 27.3.4.13 make `:httpc` resend the
  # `authorization` header to whatever host a redirect names; measured on
  # 2026-11-09, 27.3.4.4 and 27.3.4.12 forwarded it to another port and to
  # another host for 301, 302, 303, 307 and 308, GET and POST, while
  # 27.3.4.13, 27.3.4.19 and 28.5.0.8 dropped it. The two echidnabot senders
  # already set `autoredirect: false`; this test fails if the runtime itself
  # regresses, so every other `:httpc` caller stays covered.
  #
  # Each case runs a redirector and a target on loopback. The redirector
  # answers with STATUS and a Location on the target; the target reports the
  # request line and any authorization header it receives.
  use ExUnit.Case, async: true

  @token "Bearer test-dummy-redirect-probe"

  setup_all do
    {:ok, _} = Application.ensure_all_started(:inets)
    :ok
  end

  for status <- [301, 302, 303, 307, 308], method <- [:get, :post] do
    test "#{method} #{status} to another origin drops the authorization header" do
      {line, auth} = follow(unquote(method), unquote(status), :other_origin)
      assert line =~ "/final"
      assert auth == []
    end
  end

  # Positive control: the target does see the header when the redirect stays
  # on the same origin, so an empty list above is the runtime dropping it, not
  # the harness failing to observe it.
  test "same-origin redirect keeps the authorization header" do
    {_line, auth} = follow(:get, 302, :same_origin)
    assert auth == ["authorization: #{@token}"]
  end

  defp follow(method, status, where) do
    {:ok, front} = listen()
    {:ok, back} = listen()
    {:ok, front_port} = :inet.port(front)
    {:ok, back_port} = :inet.port(back)
    target = if where == :same_origin, do: front, else: back
    target_port = if where == :same_origin, do: front_port, else: back_port
    location = "http://127.0.0.1:#{target_port}/final"
    parent = self()

    server =
      spawn_link(fn ->
        {:ok, s1} = :gen_tcp.accept(front, 5_000)
        _ = read_request(s1)

        :ok =
          :gen_tcp.send(
            s1,
            "HTTP/1.1 #{status} Redirect\r\nLocation: #{location}\r\n" <>
              "Content-Length: 0\r\nConnection: close\r\n\r\n"
          )

        :gen_tcp.close(s1)
        {:ok, s2} = :gen_tcp.accept(target, 5_000)
        [line | headers] = s2 |> read_request() |> String.split("\r\n")
        auth = Enum.filter(headers, &String.starts_with?(String.downcase(&1), "authorization:"))
        send(parent, {:seen, line, auth})
        :gen_tcp.send(s2, "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
        :gen_tcp.close(s2)
      end)

    url = ~c"http://127.0.0.1:#{front_port}/start"
    headers = [{~c"authorization", String.to_charlist(@token)}]

    request =
      case method do
        :get -> {url, headers}
        :post -> {url, headers, ~c"application/json", ~c"{}"}
      end

    {:ok, {{_, 200, _}, _, _}} =
      :httpc.request(method, request, [timeout: 5_000, autoredirect: true], [])

    assert_receive {:seen, line, auth}, 5_000
    Process.unlink(server)
    Enum.each([front, back], &:gen_tcp.close/1)
    {line, auth}
  end

  defp listen do
    :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true, ip: {127, 0, 0, 1}])
  end

  # Reads up to the end of the headers, then any Content-Length body.
  defp read_request(socket, acc \\ "") do
    case :binary.match(acc, "\r\n\r\n") do
      {pos, _} ->
        head = binary_part(acc, 0, pos)
        want = content_length(head)

        if byte_size(acc) - pos - 4 >= want do
          head
        else
          read_more(socket, acc)
        end

      :nomatch ->
        read_more(socket, acc)
    end
  end

  defp read_more(socket, acc) do
    case :gen_tcp.recv(socket, 0, 5_000) do
      {:ok, data} -> read_request(socket, acc <> data)
      {:error, _} -> acc
    end
  end

  defp content_length(head) do
    case Regex.run(~r/content-length:\s*(\d+)/i, head) do
      [_, n] -> String.to_integer(n)
      nil -> 0
    end
  end
end
