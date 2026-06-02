# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.WorkflowAudit.ChapelTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowAudit

  # Extends hypatia#414 (WF024) — auto-fix wiring plus 4 prior Chapel-2.8.0
  # sharp edges from panic-attack#99. Each sub-pattern must emit a finding
  # with the matching `recipe_id` so the gitbot-fleet pipeline can route it.

  describe "check_chapel_abi_runs_on_mismatch/1 — auto_fixable + recipe_id" do
    test "fires WF024 with recipe_id chapel-runs-on-pin-22-04 on ubuntu-latest + CHAPEL_DEB_URL" do
      content = """
      jobs:
        ci:
          runs-on: ubuntu-latest
          steps:
            - run: curl -L $CHAPEL_DEB_URL -o /tmp/chapel.deb
      """

      assert [finding] =
               WorkflowAudit.check_chapel_abi_runs_on_mismatch([{"chapel.yml", content}])

      assert finding.rule == "WF024"
      assert finding.auto_fixable == true
      assert finding.recipe_id == "chapel-runs-on-pin-22-04"
      assert finding.action == :pin_runs_on_ubuntu_2204
    end

    test "no finding when runs-on is ubuntu-22.04" do
      content = """
      jobs:
        ci:
          runs-on: ubuntu-22.04
          steps:
            - run: dpkg -i chapel-2.8.0.deb
      """

      assert [] =
               WorkflowAudit.check_chapel_abi_runs_on_mismatch([{"chapel.yml", content}])
    end
  end

  describe "check_chapel_sharp_edges/1 — four sub-recipe ids" do
    test "fires chapel-manpath-guard on unguarded $MANPATH" do
      content = """
      jobs:
        ci:
          steps:
            - run: |
                export MANPATH=/opt/chapel/man:$MANPATH
                chpl --version
      """

      findings = WorkflowAudit.check_chapel_sharp_edges([{"chapel.yml", content}])
      recipes = Enum.map(findings, & &1.recipe_id)
      assert "chapel-manpath-guard" in recipes
    end

    test "does NOT fire chapel-manpath-guard when guarded with ${MANPATH:-}" do
      content = """
      jobs:
        ci:
          steps:
            - run: |
                export MANPATH=/opt/chapel/man:${MANPATH:-}
                export CHPL_LLVM=bundled
                chpl --version
      """

      findings = WorkflowAudit.check_chapel_sharp_edges([{"chapel.yml", content}])
      recipes = Enum.map(findings, & &1.recipe_id)
      refute "chapel-manpath-guard" in recipes
    end

    test "fires chapel-chpl-llvm-export when CHPL_LLVM is not exported" do
      content = """
      jobs:
        ci:
          steps:
            - run: |
                wget $CHAPEL_DEB_URL
                chpl --version
      """

      findings = WorkflowAudit.check_chapel_sharp_edges([{"chapel.yml", content}])
      recipes = Enum.map(findings, & &1.recipe_id)
      assert "chapel-chpl-llvm-export" in recipes
    end

    test "does NOT fire chapel-chpl-llvm-export when CHPL_LLVM= is present" do
      content = """
      jobs:
        ci:
          env:
            CHPL_LLVM: bundled
          steps:
            - run: |
                export MANPATH=/opt/chapel/man:${MANPATH:-}
                chpl --version
      """

      findings = WorkflowAudit.check_chapel_sharp_edges([{"chapel.yml", content}])
      recipes = Enum.map(findings, & &1.recipe_id)
      refute "chapel-chpl-llvm-export" in recipes
    end

    test "fires chapel-replace-chpl-about-with-version on `chpl --about`" do
      content = """
      jobs:
        ci:
          env:
            CHPL_LLVM: bundled
          steps:
            - run: |
                export MANPATH=/opt/chapel/man:${MANPATH:-}
                chpl --about > /tmp/chapel-info.txt
      """

      findings = WorkflowAudit.check_chapel_sharp_edges([{"chapel.yml", content}])
      recipes = Enum.map(findings, & &1.recipe_id)
      assert "chapel-replace-chpl-about-with-version" in recipes
    end

    test "fires chapel-find-comm-gasnet on brittle printchplenv glob" do
      content = """
      jobs:
        ci:
          env:
            CHPL_LLVM: bundled
          steps:
            - run: |
                export MANPATH=/opt/chapel/man:${MANPATH:-}
                printchplenv --simple | grep comm-gasnet*
      """

      findings = WorkflowAudit.check_chapel_sharp_edges([{"chapel.yml", content}])
      recipes = Enum.map(findings, & &1.recipe_id)
      assert "chapel-find-comm-gasnet" in recipes
    end

    test "no Chapel markers => no findings (negative control)" do
      content = """
      jobs:
        ci:
          runs-on: ubuntu-latest
          steps:
            - run: cargo test
      """

      assert [] = WorkflowAudit.check_chapel_sharp_edges([{"rust.yml", content}])
    end

    test "all 5 recipe_ids fire together on a maximally bad workflow" do
      content = """
      jobs:
        ci:
          runs-on: ubuntu-latest
          steps:
            - run: |
                wget $CHAPEL_DEB_URL -O chapel-2.8.0.deb
                dpkg -i chapel-2.8.0.deb
                export MANPATH=/opt/chapel/man:$MANPATH
                chpl --about
                printchplenv --simple | grep comm-gasnet*
      """

      abi = WorkflowAudit.check_chapel_abi_runs_on_mismatch([{"chapel.yml", content}])
      edges = WorkflowAudit.check_chapel_sharp_edges([{"chapel.yml", content}])
      recipes = Enum.map(abi ++ edges, & &1.recipe_id)

      assert "chapel-runs-on-pin-22-04" in recipes
      assert "chapel-manpath-guard" in recipes
      assert "chapel-chpl-llvm-export" in recipes
      assert "chapel-replace-chpl-about-with-version" in recipes
      assert "chapel-find-comm-gasnet" in recipes
      # Ensures the WF024 rule label is shared across the family
      assert Enum.all?(abi ++ edges, &(&1.rule == "WF024"))
    end
  end
end
