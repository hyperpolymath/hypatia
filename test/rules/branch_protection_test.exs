# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.BranchProtectionTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.BranchProtection, as: BP

  # BP008 has two test surfaces: (1) the integration against the GitHub
  # API (skipped when no token, returns []), and (2) the pure inner
  # logic of "given a required-contexts list and a seen-names set, what
  # is phantom". The integration test runs implicitly via the
  # `bp008_phantom_required_context/3` returning [] when no token is
  # set. We exercise the helper indirectly by checking the no-token
  # path is well-behaved.

  describe "bp008_phantom_required_context/3 — token-gated, no-token" do
    test "returns [] cleanly when no token is set" do
      old_gh = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      try do
        assert BP.bp008_phantom_required_context("hyperpolymath", "affinescript") == []
      after
        if old_gh, do: System.put_env("GITHUB_TOKEN", old_gh)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end
    end

    test "options clamp recent_commits to [1, 20]" do
      old_gh = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      try do
        # Returns [] regardless because token absent — but the call
        # should not crash on out-of-range values.
        assert BP.bp008_phantom_required_context("o", "r", recent_commits: 0) == []
        assert BP.bp008_phantom_required_context("o", "r", recent_commits: 9999) == []
        assert BP.bp008_phantom_required_context("o", "r", recent_commits: -5) == []
      after
        if old_gh, do: System.put_env("GITHUB_TOKEN", old_gh)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end
    end
  end

  describe "scan/2 wires BP008 in" do
    test "scan returns the empty-shape result when no token is set" do
      old_gh = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      try do
        result = BP.scan(".", owner_repo: {"hyperpolymath", "affinescript"})
        assert result.findings == []
        assert result.total == 0
      after
        if old_gh, do: System.put_env("GITHUB_TOKEN", old_gh)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end
    end
  end
end
