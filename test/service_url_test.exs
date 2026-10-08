# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.ServiceUrlTest do
  # No built-in service URLs: with HYPATIA_VERISIM_URL / HYPATIA_ECHIDNABOT_URL
  # unset or blank, every caller turns its feature off without a network call.
  # Each "unset" assertion checks a value only the no-default path produces
  # (`{:error, :not_configured}` or the "unset" log line), and each feature has
  # a planted positive showing the env value really is the URL used.
  #
  # Mutates process-global env, so it cannot run concurrently with other tests.
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Hypatia.LearningScheduler
  alias Hypatia.Neural.ProverRecommender
  alias Hypatia.Rules.ProofObligation
  alias Hypatia.Rules.ProofStrategySelection, as: PS
  alias Hypatia.Rules.StrategyDrift
  alias Hypatia.ServiceUrl
  alias Hypatia.VCL.ProofResolver

  @vars ["HYPATIA_VERISIM_URL", "HYPATIA_ECHIDNABOT_URL"]

  setup do
    saved = Map.new(@vars, &{&1, System.get_env(&1)})
    Enum.each(@vars, &System.delete_env/1)

    on_exit(fn ->
      Enum.each(saved, fn
        {name, nil} -> System.delete_env(name)
        {name, value} -> System.put_env(name, value)
      end)
    end)

    :ok
  end

  # Serve exactly one HTTP request on an ephemeral localhost port, send the
  # test process `{:request_line, line}`, and answer 200 with `reply`.
  # Returns the base URL.
  defp one_shot_server(reply) do
    test_pid = self()
    {:ok, listen} = :gen_tcp.listen(0, [:binary, packet: :raw, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)

    spawn_link(fn ->
      {:ok, sock} = :gen_tcp.accept(listen)
      {:ok, data} = :gen_tcp.recv(sock, 0, 5_000)
      [request_line | _] = String.split(data, "\r\n")
      send(test_pid, {:request_line, request_line})

      :gen_tcp.send(
        sock,
        "HTTP/1.1 200 OK\r\ncontent-type: application/json\r\n" <>
          "content-length: #{byte_size(reply)}\r\nconnection: close\r\n\r\n" <> reply
      )

      :gen_tcp.close(sock)
      :gen_tcp.close(listen)
    end)

    "http://127.0.0.1:#{port}"
  end

  describe "ServiceUrl" do
    test "unset, empty and whitespace-only all mean not configured" do
      assert ServiceUrl.verisim() == {:error, :not_configured}
      assert ServiceUrl.echidnabot() == {:error, :not_configured}

      for blank <- ["", "   ", "\t\n"] do
        System.put_env("HYPATIA_VERISIM_URL", blank)
        System.put_env("HYPATIA_ECHIDNABOT_URL", blank)
        assert ServiceUrl.verisim() == {:error, :not_configured}
        assert ServiceUrl.echidnabot() == {:error, :not_configured}
      end
    end

    test "a set variable is returned trimmed" do
      System.put_env("HYPATIA_VERISIM_URL", "  http://verisim.test  ")
      System.put_env("HYPATIA_ECHIDNABOT_URL", "http://echidnabot.test")
      assert ServiceUrl.verisim() == {:ok, "http://verisim.test"}
      assert ServiceUrl.echidnabot() == {:ok, "http://echidnabot.test"}
    end

    test "a non-blank :base_url wins; a nil or blank one falls through to the env" do
      System.put_env("HYPATIA_VERISIM_URL", "http://from-env.test")

      assert ServiceUrl.verisim(base_url: "http://from-opts.test") ==
               {:ok, "http://from-opts.test"}

      assert ServiceUrl.verisim(base_url: nil) == {:ok, "http://from-env.test"}
      assert ServiceUrl.verisim(base_url: " ") == {:ok, "http://from-env.test"}
    end
  end

  describe "VeriSimDB callers with HYPATIA_VERISIM_URL unset" do
    test "ProofStrategySelection.recommend/2 returns :not_configured" do
      assert PS.recommend("safety") == {:error, :not_configured}
      assert PS.recommend_with_certs("safety") == {:error, :not_configured}
      assert PS.recommend_with_novelty("safety") == {:error, :not_configured}
    end

    test "StrategyDrift reports :not_configured per class and no shifts overall" do
      assert StrategyDrift.check_shift("safety") == {:error, :not_configured}
      assert StrategyDrift.check_all_shifts() == []
    end

    test "ProverRecommender.train_from_verisim/1 returns :not_configured without a warning" do
      log =
        capture_log(fn ->
          assert ProverRecommender.train_from_verisim() == {:error, :not_configured}
        end)

      refute log =~ "fetch_attempts failed"
    end

    test "ProofResolver lookups return :not_configured" do
      assert ProofResolver.resolve("PROOF SANCTIFY(class=equiv)") == {:error, :not_configured}

      assert ProofResolver.resolve("PROOF PROVEN(class=linearity, prover=coq)") ==
               {:error, :not_configured}
    end

    test "ProofObligation.to_recipe/2 leaves prover_hint nil" do
      recipe = ProofObligation.to_recipe(%{"claim" => "x is never null", "repo" => "o/r"})
      assert recipe["prover_hint"] == nil
    end
  end

  describe "VeriSimDB callers with HYPATIA_VERISIM_URL set (planted positive)" do
    test "recommend/2 sends its request to the env URL" do
      reply = ~s({"recommendations":[{"prover":"z3","success_rate":0.9}]})
      System.put_env("HYPATIA_VERISIM_URL", one_shot_server(reply))

      assert {:ok, [%{"prover" => "z3"} | _]} = PS.recommend("safety")
      assert_receive {:request_line, line}, 5_000
      assert line =~ ~r{^GET /api/v1/proof_attempts/strategy\?class=safety&limit=5 HTTP/1\.[01]$}
    end

    test "ProofObligation.to_recipe/2 takes its prover_hint from the env URL" do
      reply = ~s({"recommendations":[{"prover":"z3","success_rate":0.9}]})
      System.put_env("HYPATIA_VERISIM_URL", one_shot_server(reply))

      recipe = ProofObligation.to_recipe(%{"claim" => "x is never null", "repo" => "o/r"})
      assert_receive {:request_line, _line}, 5_000
      assert recipe["prover_hint"] == "z3"
    end
  end

  describe "LearningScheduler.requeue_candidates/3 and HYPATIA_ECHIDNABOT_URL" do
    test "unset: sends nothing, logs the dropped candidates, returns :ok" do
      log =
        capture_log(fn ->
          assert LearningScheduler.requeue_candidates("safety", "z3", ["att-1", "att-2"]) == :ok
        end)

      assert log =~ "HYPATIA_ECHIDNABOT_URL unset"
      assert log =~ "2 attempts for class=safety"
    end

    test "blank is the same as unset" do
      System.put_env("HYPATIA_ECHIDNABOT_URL", "  ")

      log =
        capture_log(fn ->
          assert LearningScheduler.requeue_candidates("safety", "z3", ["att-1"]) == :ok
        end)

      assert log =~ "HYPATIA_ECHIDNABOT_URL unset"
    end

    test "set: the request reaches the env URL + /graphql" do
      reply = ~s({"data":{"submitProofObligation":{"success":true,"proofId":"p-1"}}})
      System.put_env("HYPATIA_ECHIDNABOT_URL", one_shot_server(reply))

      assert LearningScheduler.requeue_candidates("safety", "z3", ["att-1"]) == :ok
      assert_receive {:request_line, line}, 5_000
      assert line =~ ~r{^POST /graphql HTTP/1\.[01]$}
    end
  end
end
