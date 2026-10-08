# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.EchidnabotDispatchTest do
  # Both senders of echidnabot's `submitProofObligation`: FleetDispatcher and
  # LearningScheduler. Each test captures the request a sender actually puts
  # on the wire and checks the GraphQL-over-HTTP envelope, not the sender's
  # return value alone.
  #
  # Mutates process-global env (HYPATIA_ECHIDNABOT_URL), so it cannot run
  # concurrently with other tests.
  use ExUnit.Case, async: false

  alias Hypatia.FleetDispatcher
  alias Hypatia.LearningScheduler

  @accepted ~s({"data":{"submitProofObligation":{"success":true,"proofId":"p-1"}}})

  # A claim that an inline GraphQL string literal cannot carry when only `"`
  # is escaped: a raw newline, a tab, a backslash and a quote.
  @hostile_claim "line one\nline two\twith \\ backslash and \"quotes\""

  setup do
    # A fresh manifest directory per test, so a manifest assertion reads only
    # this test's line.
    original_path = Application.get_env(:hypatia, :verisimdb_data_path)

    data_path =
      Path.join(["_build", "test", "echidnabot-dispatch-#{System.unique_integer([:positive])}"])

    Application.put_env(:hypatia, :verisimdb_data_path, data_path)

    on_exit(fn ->
      System.delete_env("HYPATIA_ECHIDNABOT_URL")
      System.delete_env("HYPATIA_FLEET_URL")
      Application.put_env(:hypatia, :verisimdb_data_path, original_path)
      File.rm_rf!(data_path)
    end)

    {:ok, manifest: Path.join([data_path, "dispatch", "pending.jsonl"])}
  end

  # Serve exactly one HTTP request on an ephemeral localhost port. The request
  # line and the full body (read to its Content-Length) are sent to the test
  # process as `{:captured, request_line, body}`. Returns the base URL.
  defp capturing_server(status, reply) do
    test_pid = self()
    {:ok, listen} = :gen_tcp.listen(0, [:binary, packet: :raw, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)

    spawn_link(fn ->
      {:ok, sock} = :gen_tcp.accept(listen)
      {head, body_start} = recv_head(sock, "")
      [request_line | header_lines] = String.split(head, "\r\n")
      body = recv_body(sock, body_start, content_length(header_lines))
      send(test_pid, {:captured, request_line, body})

      :gen_tcp.send(
        sock,
        "HTTP/1.1 #{status} X\r\ncontent-type: application/json\r\n" <>
          "content-length: #{byte_size(reply)}\r\nconnection: close\r\n\r\n" <> reply
      )

      :gen_tcp.close(sock)
      :gen_tcp.close(listen)
    end)

    "http://127.0.0.1:#{port}"
  end

  # Read until the end of the HTTP header block; returns {head, body_so_far}.
  defp recv_head(sock, acc) do
    case :binary.split(acc, "\r\n\r\n") do
      [head, rest] ->
        {head, rest}

      [_] ->
        {:ok, data} = :gen_tcp.recv(sock, 0, 5_000)
        recv_head(sock, acc <> data)
    end
  end

  # Read until the body holds `length` bytes.
  defp recv_body(_sock, acc, length) when byte_size(acc) >= length, do: acc

  defp recv_body(sock, acc, length) do
    {:ok, data} = :gen_tcp.recv(sock, 0, 5_000)
    recv_body(sock, acc <> data, length)
  end

  # The Content-Length header value, or 0 when absent.
  defp content_length(header_lines) do
    Enum.find_value(header_lines, 0, fn line ->
      case String.split(line, ":", parts: 2) do
        [name, value] ->
          if String.downcase(String.trim(name)) == "content-length",
            do: value |> String.trim() |> String.to_integer()

        _ ->
          nil
      end
    end)
  end

  # Wait for the capturing server's request; returns {request_line, decoded JSON body}.
  defp captured! do
    assert_receive {:captured, request_line, body}, 5_000
    {request_line, Jason.decode!(body)}
  end

  # A :proof_obligation finding carrying the hostile claim, with `overrides` merged in.
  defp obligation(overrides) do
    Map.merge(
      %{
        type: :proof_obligation,
        repo: "hyperpolymath/hypatia",
        claim: @hostile_claim,
        context: "context\nacross lines",
        prover_hint_override: "lean"
      },
      overrides
    )
  end

  describe "FleetDispatcher -> echidnabot" do
    test "sends claim and context as GraphQL variables, byte for byte" do
      System.put_env("HYPATIA_ECHIDNABOT_URL", capturing_server(200, @accepted))

      assert {:ok, :dispatched} = FleetDispatcher.dispatch_finding(obligation(%{}))

      {request_line, envelope} = captured!()
      assert request_line =~ ~r{^POST /graphql HTTP/1\.[01]$}

      assert envelope["variables"] == %{
               "input" => %{
                 "repo" => "hyperpolymath/hypatia",
                 "claim" => @hostile_claim,
                 "context" => "context\nacross lines",
                 "prover" => "LEAN"
               }
             }

      # The document is static: no part of the claim is spliced into it.
      refute envelope["query"] =~ "line one"
      assert envelope["query"] =~ "$input: SubmitProofObligationInput!"
    end

    test "omits the prover when the hint is not a ProverKind value" do
      System.put_env("HYPATIA_ECHIDNABOT_URL", capturing_server(200, @accepted))

      assert {:ok, :dispatched} =
               FleetDispatcher.dispatch_finding(obligation(%{prover_hint_override: "lean4"}))

      {_request_line, envelope} = captured!()
      refute Map.has_key?(envelope["variables"]["input"], "prover")
    end

    test "records the variables in the manifest line next to the static query", %{
      manifest: manifest
    } do
      System.put_env("HYPATIA_ECHIDNABOT_URL", capturing_server(200, @accepted))

      assert {:ok, :dispatched} = FleetDispatcher.dispatch_finding(obligation(%{}))
      _ = captured!()

      [line] = manifest |> File.read!() |> String.split("\n", trim: true)
      record = Jason.decode!(line)
      assert record["bot"] == "echidnabot"
      assert record["query"] == Hypatia.EchidnabotObligation.mutation()
      assert record["variables"]["input"]["claim"] == @hostile_claim
    end

    test "sends the JSON envelope, variables included, on the fleet-coordinator path" do
      System.put_env("HYPATIA_FLEET_URL", capturing_server(200, "{}"))

      assert {:ok, :dispatched} = FleetDispatcher.dispatch_finding(obligation(%{}))

      {request_line, envelope} = captured!()
      assert request_line =~ ~r{^POST /dispatch/echidnabot HTTP/1\.[01]$}
      assert envelope["query"] == Hypatia.EchidnabotObligation.mutation()
      assert envelope["variables"]["input"]["claim"] == @hostile_claim
    end
  end

  describe "EchidnabotObligation" do
    alias Hypatia.EchidnabotObligation

    test "normalise_prover_hint/1 maps ProverKind names and drops everything else" do
      assert EchidnabotObligation.normalise_prover_hint("lean") == "LEAN"
      assert EchidnabotObligation.normalise_prover_hint("hol_light") == "HOL_LIGHT"
      assert EchidnabotObligation.normalise_prover_hint("hol4") == "HOL4"
      assert EchidnabotObligation.normalise_prover_hint("lean4") == nil
      assert EchidnabotObligation.normalise_prover_hint("idris2") == nil
      assert EchidnabotObligation.normalise_prover_hint("LEAN") == nil
      assert EchidnabotObligation.normalise_prover_hint(nil) == nil
    end

    test "variables/5 sets inline only when asked and never splices values" do
      assert EchidnabotObligation.variables("o/r", "c", "x", nil) ==
               %{"input" => %{"repo" => "o/r", "claim" => "c", "context" => "x"}}

      assert EchidnabotObligation.variables("o/r", "c", "x", "coq", inline: true) ==
               %{
                 "input" => %{
                   "repo" => "o/r",
                   "claim" => "c",
                   "context" => "x",
                   "prover" => "COQ",
                   "inline" => true
                 }
               }

      # The document holds no string literal for a value to be spliced into.
      refute EchidnabotObligation.mutation() =~ "\""
    end

    test "graphql_url/1 treats the env value as a base URL" do
      assert EchidnabotObligation.graphql_url("http://localhost:9001") ==
               "http://localhost:9001/graphql"
    end
  end

  describe "LearningScheduler -> echidnabot" do
    test "re-queues with variables, a normalised prover, and the base URL + /graphql" do
      System.put_env("HYPATIA_ECHIDNABOT_URL", capturing_server(200, @accepted))

      assert :ok = LearningScheduler.requeue_candidates("cls \"a\"\nb", "hol_light", ["att-1"])

      {request_line, envelope} = captured!()
      assert request_line =~ ~r{^POST /graphql HTTP/1\.[01]$}

      assert envelope["variables"] == %{
               "input" => %{
                 "repo" => "hyperpolymath/requeue",
                 "claim" => "requeue-of-att-1",
                 "context" => "strategy-shift class=cls \"a\"\nb",
                 "prover" => "HOL_LIGHT",
                 "inline" => true
               }
             }

      refute envelope["query"] =~ "requeue-of"
    end

    test "drops a prover name that is not a ProverKind value instead of upcasing it" do
      System.put_env("HYPATIA_ECHIDNABOT_URL", capturing_server(200, @accepted))

      assert :ok = LearningScheduler.requeue_candidates("cls", "lean4", ["att-2"])

      {_request_line, envelope} = captured!()
      refute Map.has_key?(envelope["variables"]["input"], "prover")
      refute inspect(envelope) =~ "LEAN4"
    end
  end
end
