# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.HttpcRedirectCredentialsTest do
  # Guards the Erlang/OTP pin (mise.toml and the CI test lanes), not Hypatia's
  # code. Older OTP releases make `:httpc` resend credentials to whatever host
  # a redirect names. Measured on loopback for 301, 302, 303, 307 and 308, GET
  # and POST, to another port and to another host:
  #
  # * an explicit `authorization` header: forwarded by 27.3.4.4 and
  #   27.3.4.12 (2026-11-09), dropped from 27.3.4.13;
  # * credentials in the URL (`http://user:pass@host`, sent as Basic auth):
  #   still forwarded by 27.3.4.13, dropped by 27.3.4.19 and 28.5.0.8
  #   (2026-11-17).
  #
  # So 27.3.4.19 is the floor on the 27 line. Credentialed callers in lib/
  # also set `autoredirect: false` (checked by the last test here), so they
  # do not depend on the pin alone.
  #
  # Each case runs a redirector and a target on loopback. The redirector
  # answers with STATUS and a Location on the target; the target reports the
  # request line and any authorization header it receives.
  use ExUnit.Case, async: true

  @token "Bearer test-dummy-redirect-probe"
  @userinfo "test-dummy-user:test-dummy-pass"

  setup_all do
    {:ok, _} = Application.ensure_all_started(:inets)
    :ok
  end

  # :other_port keeps the host (127.0.0.1) and changes the port, so a runtime
  # that compared hosts only would fail it. :other_host keeps the port and
  # moves to 127.0.0.2, so a runtime that compared ports only would fail it.
  for where <- [:other_port, :other_host],
      status <- [301, 302, 303, 307, 308],
      method <- [:get, :post] do
    test "#{method} #{status} to #{where} drops the authorization header" do
      {line, auth} = follow(unquote(method), unquote(status), unquote(where))
      assert line =~ "/final"
      assert auth == []
    end

    test "#{method} #{status} to #{where} drops credentials from the URL" do
      {line, auth} = follow(unquote(method), unquote(status), unquote(where), :userinfo)
      assert line =~ "/final"
      assert auth == []
    end
  end

  # Positive controls: the target does see the credentials when the redirect
  # stays on the same origin, so an empty list above is the runtime dropping
  # them, not the harness failing to observe them.
  test "same-origin redirect keeps the authorization header" do
    {_line, auth} = follow(:get, 302, :same_origin)
    assert auth == ["authorization: #{@token}"]
  end

  test "same-origin redirect keeps credentials from the URL" do
    {_line, auth} = follow(:get, 302, :same_origin, :userinfo)
    assert auth == ["authorization: Basic " <> Base.encode64(@userinfo)]
  end

  # Every lib/ module that sends an Authorization header must turn off
  # automatic redirects on each :httpc request, so a credential never depends
  # on the runtime pin alone (for example on the OTP 28.3 CI lanes).
  test "credentialed :httpc callers in lib/ disable automatic redirects" do
    offenders =
      Path.wildcard(Path.expand("../lib/**/*.ex", __DIR__))
      |> Enum.filter(fn path ->
        src = File.read!(path)
        String.match?(src, ~r/authorization/i) and String.contains?(src, ":httpc.request")
      end)
      |> Enum.reject(fn path ->
        src = File.read!(path)
        calls = length(String.split(src, ":httpc.request")) - 1
        guarded = length(String.split(src, "{:autoredirect, false}")) - 1
        guarded >= calls
      end)
      |> Enum.map(&Path.relative_to_cwd/1)

    assert offenders == []
  end

  defp follow(method, status, where, cred \\ :header) do
    {:ok, front} = listen({127, 0, 0, 1})
    {:ok, front_port} = :inet.port(front)

    {:ok, back} =
      if where == :other_host,
        do: listen({127, 0, 0, 2}, front_port),
        else: listen({127, 0, 0, 1})

    {:ok, back_port} = :inet.port(back)
    target = if where == :same_origin, do: front, else: back
    target_port = if where == :same_origin, do: front_port, else: back_port
    target_host = if where == :other_host, do: "127.0.0.2", else: "127.0.0.1"
    location = "http://#{target_host}:#{target_port}/final"
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

    {url, headers} =
      case cred do
        :header ->
          {~c"http://127.0.0.1:#{front_port}/start",
           [{~c"authorization", String.to_charlist(@token)}]}

        :userinfo ->
          {~c"http://#{@userinfo}@127.0.0.1:#{front_port}/start", []}
      end

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

  defp listen(ip, port \\ 0) do
    :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true, ip: ip])
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
