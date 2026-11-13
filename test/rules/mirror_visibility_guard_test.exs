# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.MirrorVisibilityGuardTest do
  # WF030. Owner ruling 2026-11-13 ("add a guard"): a forge mirror must not
  # push a private source to a forge copy that is not confirmed private.
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowAudit

  @mirror """
  on:
    push:
      branches: [main]
  jobs:
    gitlab:
      if: vars.GITLAB_MIRROR_ENABLED == 'true'
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v7.0.1
          with:
            fetch-depth: 0
        - run: git push --force "git@gitlab.com:org/${{ github.event.repository.name }}.git" --all
  """

  test "an unguarded forge mirror is flagged high, referral only" do
    [f] = WorkflowAudit.check_mirror_without_visibility_guard(%{"mirror.yml" => @mirror})
    assert f.rule == "WF030"
    assert f.severity == :high
    assert f.action == :report
  end

  test "a source-visibility check is a guard" do
    guarded =
      String.replace(
        @mirror,
        "if: vars.GITLAB_MIRROR_ENABLED == 'true'",
        "if: vars.GITLAB_MIRROR_ENABLED == 'true' && !github.event.repository.private"
      )

    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"mirror.yml" => guarded}) == []
  end

  test "a per-forge *_MIRROR_PRIVATE_CONFIRMED gate is a guard (standards#1513)" do
    guarded =
      String.replace(
        @mirror,
        "if: vars.GITLAB_MIRROR_ENABLED == 'true'",
        "if: vars.GITLAB_MIRROR_ENABLED == 'true' && (!github.event.repository.private || vars.GITLAB_MIRROR_PRIVATE_CONFIRMED == 'true')"
      )

    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"mirror.yml" => guarded}) == []
  end

  test "a comment that only mentions visibility is not a guard" do
    commented = "# TODO: add a visibility check\n" <> @mirror
    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"mirror.yml" => commented})
  end

  test "a condition that SELECTS private sources is not a guard" do
    selects =
      String.replace(
        @mirror,
        "if: vars.GITLAB_MIRROR_ENABLED == 'true'",
        "if: vars.GITLAB_MIRROR_ENABLED == 'true' && github.event.repository.private"
      )

    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"mirror.yml" => selects})
  end

  test "a force flag on a continued shell line is still a force push" do
    continued =
      String.replace(
        @mirror,
        "- run: git push --force \"git@gitlab.com:org/${{ github.event.repository.name }}.git\" --all",
        "- run: |\n            git push \\\n              --force \"git@gitlab.com:org/x.git\" --all"
      )

    assert continued =~ "git push \\\n"
    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"mirror.yml" => continued})
  end

  test "a gate in one job and a force push in another is not this rule" do
    split = """
    on: push
    jobs:
      notify:
        if: vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: echo enabled
      rewrite:
        runs-on: ubuntu-latest
        steps:
          - run: git push --force origin HEAD
    """

    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"x.yml" => split}) == []
  end

  test "a guard job the mirror job needs counts as its guard" do
    with_guard = """
    on: push
    jobs:
      visibility-guard:
        runs-on: ubuntu-latest
        steps:
          - if: github.event.repository.private && vars.GITLAB_MIRROR_PRIVATE_CONFIRMED != 'true'
            run: exit 1
      gitlab:
        needs: [visibility-guard]
        if: vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: git push --force gitlab main
    """

    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => with_guard}) == []
  end

  test "a force push not gated on a mirror variable is not this rule" do
    plain = String.replace(@mirror, "vars.GITLAB_MIRROR_ENABLED == 'true'", "true")
    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"x.yml" => plain}) == []
  end

  test "a plain (non-force) push is not this rule" do
    plain = String.replace(@mirror, "git push --force", "git push")
    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"mirror.yml" => plain}) == []
  end

  test "is part of audit/3" do
    %{findings: fs} = WorkflowAudit.audit(["mirror.yml"], %{"mirror.yml" => @mirror})
    assert Enum.any?(fs, &(&1[:rule] == "WF030"))
  end
end
