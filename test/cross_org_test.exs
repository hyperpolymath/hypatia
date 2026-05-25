# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.VCL.CrossOrgTest do
  # async: false because tests mutate Application env (peer policies)
  # and the curl shell-out (in pull_findings) reaches the network if
  # we're not careful. The find_policy/peers tests stay pure.
  use ExUnit.Case, async: false

  alias Hypatia.VCL.CrossOrg

  setup do
    on_exit(fn -> Application.delete_env(:hypatia, :cross_org_policies) end)
    :ok
  end

  describe "peers/0 and find_policy/1" do
    test "peers/0 returns the configured list" do
      Application.put_env(:hypatia, :cross_org_policies, [
        %{peer_id: "a", base_url: "https://a.example"},
        %{peer_id: "b", base_url: "https://b.example"}
      ])

      assert [%{peer_id: "a"}, %{peer_id: "b"}] = CrossOrg.peers()
    end

    test "peers/0 returns [] when no config is set" do
      Application.delete_env(:hypatia, :cross_org_policies)
      assert CrossOrg.peers() == []
    end

    test "find_policy/1 returns the matching peer policy" do
      Application.put_env(:hypatia, :cross_org_policies, [
        %{peer_id: "alpha", base_url: "https://a"},
        %{peer_id: "beta", base_url: "https://b"}
      ])

      assert %{peer_id: "alpha"} = CrossOrg.find_policy("alpha")
      assert %{peer_id: "beta"} = CrossOrg.find_policy("beta")
    end

    test "find_policy/1 returns nil for unknown peer" do
      Application.put_env(:hypatia, :cross_org_policies, [
        %{peer_id: "alpha", base_url: "https://a"}
      ])

      assert CrossOrg.find_policy("nonexistent") == nil
    end
  end

  describe "pull_findings/1 — unknown peer" do
    test "returns {:error, msg} for an unknown peer_id" do
      Application.delete_env(:hypatia, :cross_org_policies)
      assert {:error, msg} = CrossOrg.pull_findings(peer_id: "ghost")
      assert msg =~ "unknown_peer_id"
    end
  end

  # The classification + policy-gating logic is private inside
  # CrossOrg.apply_policy/2. We can't unit-test it directly, but the
  # observable contract is what pull_findings/1 returns. Since pull_findings
  # has a network dependency, the most we can do here without an HTTP mock
  # is exercise the public surface; deeper behaviour testing waits for
  # the cross_org_test integration suite or a Plug-based mock peer.
end
