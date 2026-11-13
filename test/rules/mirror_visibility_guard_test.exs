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
