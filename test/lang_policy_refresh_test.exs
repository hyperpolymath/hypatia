# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.LangPolicyRefreshTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  describe "blocked_patterns/0 — lang-policy refresh 2026-05-25" do
    test "TypeScript reason now points to AffineScript (was ReScript)" do
      patterns = CicdRules.blocked_patterns()
      ts = Enum.find(patterns, &(&1.id == :typescript_detected))
      assert ts, "expected :typescript_detected entry"
      assert ts.reason =~ "AffineScript", "TS reason should redirect to AffineScript"
      refute ts.reason =~ "ReScript", "TS reason should not still say 'use ReScript'"
    end

    test "ReScript .res files are banned" do
      patterns = CicdRules.blocked_patterns()
      res = Enum.find(patterns, &(&1.id == :rescript_detected))
      assert res, "expected :rescript_detected entry"
      assert res.glob == "*.res"
      assert res.reason =~ "AffineScript"
    end

    test "ReScript .resi interface files are banned" do
      patterns = CicdRules.blocked_patterns()
      resi = Enum.find(patterns, &(&1.id == :rescript_interface_detected))
      assert resi, "expected :rescript_interface_detected entry"
      assert resi.glob == "*.resi"
    end

    test "Python ban remains total (no SaltStack carve-out)" do
      patterns = CicdRules.blocked_patterns()
      py = Enum.find(patterns, &(&1.id == :python_detected))
      assert py
      refute py[:applies_to], "Python ban must not be scoped via applies_to"
      refute py[:exception_repos], "Python ban must not have exception_repos"
    end

    test "Jekyll workflow files are banned (use casket-ssg instead)" do
      patterns = CicdRules.blocked_patterns()

      for id <- [:jekyll_workflow_detected, :jekyll_gh_pages_workflow_detected] do
        entry = Enum.find(patterns, &(&1.id == id))
        assert entry, "expected #{id} entry"
        assert entry.reason =~ "casket-ssg"
      end
    end

    test "_config.yml (Jekyll site config) is banned" do
      patterns = CicdRules.blocked_patterns()
      entry = Enum.find(patterns, &(&1.id == :jekyll_config_detected))
      assert entry
      assert entry.glob == "_config.yml"
      assert entry.reason =~ "casket-ssg"
    end

    test "Gemfile is banned (Ruby/Jekyll surface)" do
      patterns = CicdRules.blocked_patterns()
      entry = Enum.find(patterns, &(&1.id == :gemfile_detected))
      assert entry
      assert entry.glob == "Gemfile"
    end
  end

  describe "validate_license/2 — MPL-1.0 → MPL-2.0 rewrite policy" do
    test "MPL-2.0 is accepted" do
      assert :ok = CicdRules.validate_license("MPL-2.0")
    end

    test "MPL-1.0 is flagged as wrong_license" do
      assert {:error, :wrong_license, "MPL-1.0"} =
               CicdRules.validate_license("MPL-1.0")
    end

    test "MPL-1.0-or-later is flagged as wrong_license" do
      assert {:error, :wrong_license, "MPL-1.0-or-later"} =
               CicdRules.validate_license("MPL-1.0-or-later")
    end

    test "PMPL-1.0 is flagged as wrong_license (added 2026-05-28)" do
      assert {:error, :wrong_license, "PMPL-1.0"} =
               CicdRules.validate_license("PMPL-1.0")
    end

    test "PMPL-1.0-or-later is flagged as wrong_license (added 2026-05-28)" do
      assert {:error, :wrong_license, "PMPL-1.0-or-later"} =
               CicdRules.validate_license("PMPL-1.0-or-later")
    end

    test "MIT is still flagged" do
      assert {:error, :wrong_license, "MIT"} =
               CicdRules.validate_license("MIT")
    end

    test "AGPL-3.0-or-later is accepted for exception repos" do
      assert :ok_agpl_exception =
               CicdRules.validate_license("AGPL-3.0-or-later", "game-server-admin")
    end

    test "AGPL-3.0 is flagged for non-exception repos" do
      assert {:error, :wrong_license, "AGPL-3.0"} =
               CicdRules.validate_license("AGPL-3.0", "some-other-repo")
    end
  end
end
