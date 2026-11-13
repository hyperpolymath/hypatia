# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.GitleaksFullHistoryTest do
  # WF031 (CASE-036, gitbot-fleet#673). Owner ruling 2026-11-13: base..head on
  # PRs, full history on main.
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowAudit

  @wf """
  on:
    pull_request:
    push:
      branches: [main]
  jobs:
    gitleaks:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v7.0.1
          with:
            fetch-depth: 0
        - run: gitleaks detect --source . --log-opts="--all" --redact
  """

  defp check(wf), do: WorkflowAudit.check_gitleaks_full_history_on_pr(%{"s.yml" => wf})

  test "a full-history gitleaks scan on pull_request is flagged, referral only" do
    assert [%{rule: "WF031", job: "gitleaks", severity: :medium, action: :report}] = check(@wf)
  end

  test "the = form of --log-opts is the same" do
    assert [_] = check(String.replace(@wf, ~s(--log-opts="--all"), "--log-opts=--all"))
  end

  test "a base..head range on PRs is the ruled shape" do
    wf =
      String.replace(
        @wf,
        ~s(--log-opts="--all"),
        ~s(--log-opts="${{ github.event.pull_request.base.sha }}..${{ github.sha }}")
      )

    assert check(wf) == []
  end

  test "a push-only workflow may scan all history" do
    wf = String.replace(@wf, "  pull_request:\n", "")
    assert check(wf) == []
  end

  test "a job gated to push events may scan all history" do
    wf =
      String.replace(
        @wf,
        "    runs-on: ubuntu-latest",
        "    if: github.event_name == 'push'\n    runs-on: ubuntu-latest"
      )

    assert wf != @wf
    assert check(wf) == []
  end

  test "a comment mentioning --all is not a scan" do
    wf =
      String.replace(
        @wf,
        ~s(gitleaks detect --source . --log-opts="--all" --redact),
        "gitleaks detect --source . --redact  # was --log-opts=--all"
      )

    assert check(wf) == []
  end

  test "is part of audit/2" do
    %{findings: fs} = WorkflowAudit.audit(["s.yml"], %{"s.yml" => @wf})
    assert Enum.any?(fs, &(&1[:rule] == "WF031"))
  end
end
