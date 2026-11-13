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

  test "a needed guard job does not count when the mirror runs after it fails" do
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
        if: always() && vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: git push --force gitlab main
    """

    assert [%{job: "gitlab"}] =
             WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => with_guard})
  end

  test "repository.private == true or != false selects private sources, not a guard" do
    for cond <- [
          "github.event.repository.private == true",
          "github.event.repository.private != false"
        ] do
      wf =
        String.replace(
          @mirror,
          "if: vars.GITLAB_MIRROR_ENABLED == 'true'",
          "if: vars.GITLAB_MIRROR_ENABLED == 'true' && #{cond}"
        )

      assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf}), cond
    end
  end

  test "repository.private == false or != true is a guard" do
    for cond <- [
          "github.event.repository.private == false",
          "github.event.repository.private != true"
        ] do
      wf =
        String.replace(
          @mirror,
          "if: vars.GITLAB_MIRROR_ENABLED == 'true'",
          "if: vars.GITLAB_MIRROR_ENABLED == 'true' && #{cond}"
        )

      assert WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf}) == [], cond
    end
  end

  test "a later top-level key does not lend a guard to the last job" do
    wf = """
    on: push
    jobs:
      gitlab:
        if: vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: git push --force gitlab main
    env:
      SOURCE_VISIBILITY: ${{ github.event.repository.visibility }}
    """

    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  test "flow-style jobs on one line are judged separately" do
    wf = """
    on: push
    jobs: {guard: {runs-on: x, steps: [{run: "echo ${{ github.event.repository.visibility }}"}]}, gitlab: {if: "vars.GITLAB_MIRROR_ENABLED == 'true'", runs-on: x, steps: [{run: "git push --force gitlab main"}]}}
    """

    assert [%{job: "gitlab"}] =
             WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  test "a quoted hash in a script does not hide the force push after it" do
    wf = """
    on: push
    jobs:
      gitlab:
        if: vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: |
              printf ' # ready'; git push --force gitlab main
    """

    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  test "a shell comment in a script is still not a guard" do
    wf = """
    on: push
    jobs:
      gitlab:
        if: vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: |
              # TODO: check github.event.repository.visibility here
              git push --force gitlab main
    """

    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  test "a guard reached through a chain of needs counts" do
    wf = """
    on: push
    jobs:
      visibility-guard:
        runs-on: ubuntu-latest
        steps:
          - if: github.event.repository.private && vars.GITLAB_MIRROR_PRIVATE_CONFIRMED != 'true'
            run: exit 1
      prepare:
        needs: [visibility-guard]
        runs-on: ubuntu-latest
        steps:
          - run: echo ok
      gitlab:
        needs: [prepare]
        if: vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: git push --force gitlab main
    """

    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf}) == []

    broken =
      String.replace(
        wf,
        "  prepare:\n    needs: [visibility-guard]\n",
        "  prepare:\n    needs: [visibility-guard]\n    if: always()\n"
      )

    assert broken != wf

    assert [%{job: "gitlab"}] =
             WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => broken})
  end

  test "an aliased job body is scanned" do
    wf = """
    on: push
    x-mirror: &mirror
      if: vars.GITLAB_MIRROR_ENABLED == 'true'
      runs-on: ubuntu-latest
      steps:
        - run: git push --force gitlab main
    jobs:
      gitlab: *mirror
    """

    assert [%{job: "gitlab"}] =
             WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  test "a continued force flag in a CRLF workflow is still a force push" do
    wf =
      """
      on: push
      jobs:
        gitlab:
          if: vars.GITLAB_MIRROR_ENABLED == 'true'
          runs-on: ubuntu-latest
          steps:
            - run: |
                git push \\
                  --force gitlab main
      """
      |> String.replace("\n", "\r\n")

    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  @chain """
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

  test "always() with an explicit success check on the guard job keeps the guard" do
    wf =
      String.replace(
        @chain,
        "if: vars.GITLAB_MIRROR_ENABLED == 'true'",
        "if: always() && needs.visibility-guard.result == 'success' && vars.GITLAB_MIRROR_ENABLED == 'true'"
      )

    assert wf != @chain
    assert WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf}) == []
  end

  test "a guard step whose failure is masked by continue-on-error is not a guard" do
    wf =
      String.replace(
        @chain,
        "        run: exit 1\n",
        "        run: exit 1\n        continue-on-error: true\n"
      )

    assert wf != @chain

    assert [%{job: "gitlab"}] =
             WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  test "a job named visibility-guard in needs.<id>.result is not itself a guard" do
    wf = """
    on: push
    jobs:
      gitlab:
        if: always() && needs.visibility-guard.result == 'success' && vars.GITLAB_MIRROR_ENABLED == 'true'
        runs-on: ubuntu-latest
        steps:
          - run: git push --force gitlab main
    """

    assert [_] = WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf})
  end

  test "grouping parentheses around a blocking comparison are still a guard" do
    for cond <- [
          "github.event.repository.private == (false)",
          "(github.event.repository.private) != true"
        ] do
      wf =
        String.replace(
          @mirror,
          "if: vars.GITLAB_MIRROR_ENABLED == 'true'",
          "if: vars.GITLAB_MIRROR_ENABLED == 'true' && #{cond}"
        )

      assert WorkflowAudit.check_mirror_without_visibility_guard(%{"m.yml" => wf}) == [], cond
    end
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
