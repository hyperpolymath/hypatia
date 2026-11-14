# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.FullHistoryScanRemainsTest do
  # WF032 (cicd-squabbler SQ-GEN-05). Owner ruling 2026-11-14: PRs scan
  # base..head; a full-history scan stays on push, schedule or a manual run.
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowAudit

  @pr_range """
  on:
    pull_request:
  jobs:
    gitleaks:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v7.0.1
          with:
            fetch-depth: 0
        - run: gitleaks git --log-opts="${{ github.event.pull_request.base.sha }}..HEAD" --redact
  """

  @scheduled_full """
  on:
    schedule:
      - cron: '0 3 * * 1'
  jobs:
    full:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v7.0.1
          with:
            fetch-depth: 0
        - run: gitleaks git --log-opts="--all" --redact
  """

  defp check(files), do: WorkflowAudit.check_full_history_scan_remains(files)

  test "a PR-range scan with no full-history scan elsewhere is referred" do
    assert [%{rule: "WF032", job: "gitleaks", severity: :medium, action: :report}] =
             check(%{"pr.yml" => @pr_range})
  end

  test "a scheduled full-history scan in another workflow satisfies it" do
    assert check(%{"pr.yml" => @pr_range, "full.yml" => @scheduled_full}) == []
  end

  test "a full scan on push or a manual run satisfies it" do
    for trigger <- ["push:\n    branches: [main]", "workflow_dispatch:"] do
      wf = String.replace(@scheduled_full, "schedule:\n    - cron: '0 3 * * 1'", trigger)
      assert wf != @scheduled_full
      assert check(%{"pr.yml" => @pr_range, "full.yml" => wf}) == [], trigger
    end
  end

  test "a full scan in the same workflow, gated to push, satisfies it" do
    wf =
      @pr_range
      |> String.replace("  pull_request:\n", "  pull_request:\n  push:\n    branches: [main]\n")
      |> Kernel.<>("""
        full:
          if: github.event_name == 'push'
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@v7.0.1
              with:
                fetch-depth: 0
            - run: gitleaks git --redact
      """)

    assert check(%{"ci.yml" => wf}) == []
  end

  test "a full scan gated to pull_request does not count" do
    wf =
      String.replace(
        @scheduled_full,
        "on:\n  schedule:\n    - cron: '0 3 * * 1'",
        "on:\n  schedule:\n    - cron: '0 3 * * 1'\n  pull_request:"
      )
      |> String.replace(
        "    runs-on: ubuntu-latest",
        "    if: github.event_name == 'pull_request'\n    runs-on: ubuntu-latest"
      )

    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf})
  end

  test "a scheduled scan that is ranged or shallow is not full history" do
    ranged =
      String.replace(@scheduled_full, ~s(--log-opts="--all"), ~s(--log-opts="HEAD~5..HEAD"))

    shallow =
      String.replace(@scheduled_full, ~s( --log-opts="--all"), "")
      |> String.replace("fetch-depth: 0", "fetch-depth: 1")

    no_git = String.replace(@scheduled_full, ~s(git --log-opts="--all"), "detect --no-git")

    for wf <- [ranged, shallow, no_git] do
      assert wf != @scheduled_full
      assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf})
    end
  end

  test "the estate rail on a non-PR trigger counts as full history" do
    rail = """
    on:
      push:
        branches: [main]
    jobs:
      secrets:
        uses: hyperpolymath/standards/.github/workflows/secret-scanner-reusable.yml@0123456789abcdef0123456789abcdef01234567
    """

    assert check(%{"pr.yml" => @pr_range, "rail.yml" => rail}) == []
  end

  test "the estate rail called only on pull_request does not count" do
    rail = """
    on:
      pull_request:
    jobs:
      secrets:
        uses: hyperpolymath/standards/.github/workflows/secret-scanner-reusable.yml@0123456789abcdef0123456789abcdef01234567
    """

    assert [%{rule: "WF032", file: "rail.yml"}] = check(%{"rail.yml" => rail})
  end

  test "gitleaks-action counts on a schedule but not on push alone" do
    action = fn trigger ->
      """
      on:
        #{trigger}
      jobs:
        scan:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@v7.0.1
              with:
                fetch-depth: 0
            - uses: gitleaks/gitleaks-action@ff98106e4c7b2bc287b24eaf42907196329070c7
      """
    end

    assert check(%{
             "pr.yml" => @pr_range,
             "a.yml" => action.("schedule:\n    - cron: '0 3 * * 1'")
           }) == []

    assert [%{rule: "WF032"}] =
             check(%{"pr.yml" => @pr_range, "a.yml" => action.("push:\n    branches: [main]")})
  end

  test "no PR scan means nothing to check" do
    assert check(%{"full.yml" => @scheduled_full}) == []
    assert check(%{}) == []
  end

  test "is part of audit/2" do
    %{findings: fs} = WorkflowAudit.audit(["pr.yml"], %{"pr.yml" => @pr_range})
    assert Enum.any?(fs, &(&1[:rule] == "WF032"))
  end
end
