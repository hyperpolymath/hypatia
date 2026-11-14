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

  test "a TruffleHog PR scan is a PR scan, and a full trufflehog git run remains" do
    tpr =
      String.replace(
        @pr_range,
        ~s(gitleaks git --log-opts="${{ github.event.pull_request.base.sha }}..HEAD" --redact),
        "trufflehog git file://. --since-commit ${{ github.event.pull_request.base.sha }}"
      )

    assert tpr != @pr_range
    assert [%{rule: "WF032"}] = check(%{"pr.yml" => tpr})

    tfull =
      String.replace(
        @scheduled_full,
        ~s(gitleaks git --log-opts="--all" --redact),
        "trufflehog git file://. --only-verified"
      )

    assert tfull != @scheduled_full
    assert check(%{"pr.yml" => tpr, "full.yml" => tfull}) == []
  end

  test "--all after a shallow checkout is not full history" do
    shallow = String.replace(@scheduled_full, "        with:\n          fetch-depth: 0\n", "")
    assert shallow != @scheduled_full
    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => shallow})
  end

  test "a step's own if: decides which events the scan runs on" do
    # A push-guarded step in a mixed workflow is not a PR scan...
    mixed =
      @pr_range
      |> String.replace("  pull_request:\n", "  pull_request:\n  push:\n    branches: [main]\n")
      |> String.replace(
        ~s(      - run: gitleaks git --log-opts="${{ github.event.pull_request.base.sha }}..HEAD" --redact),
        ~s(      - if: github.event_name == 'push'\n        run: gitleaks git --log-opts="--all" --redact)
      )

    assert check(%{"ci.yml" => mixed}) == []

    # ...and a full-history step guarded to PRs does not remain.
    pr_guarded =
      String.replace(
        @scheduled_full,
        "on:\n  schedule:\n    - cron: '0 3 * * 1'",
        "on:\n  schedule:\n    - cron: '0 3 * * 1'\n  pull_request:"
      )
      |> String.replace(
        ~s(      - run: gitleaks git --log-opts="--all" --redact),
        ~s(      - if: github.event_name == 'pull_request'\n        run: gitleaks git --log-opts="--all" --redact)
      )

    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => pr_guarded})
  end

  test "the deep checkout must be this repository's and come before the scan" do
    later =
      @scheduled_full
      |> String.replace(~s(gitleaks git --log-opts="--all" --redact), "gitleaks git --redact")
      |> String.replace(
        "      - uses: actions/checkout@v7.0.1\n        with:\n          fetch-depth: 0\n      - run: gitleaks git --redact",
        "      - uses: actions/checkout@v7.0.1\n      - run: gitleaks git --redact\n      - uses: actions/checkout@v7.0.1\n        with:\n          fetch-depth: 0"
      )

    assert later =~ "gitleaks git --redact\n      - uses: actions/checkout@v7.0.1\n        with:"

    other_repo =
      String.replace(
        @scheduled_full,
        "          fetch-depth: 0\n",
        "          fetch-depth: 0\n          repository: someone/else\n"
      )

    for wf <- [later, other_repo] do
      assert wf != @scheduled_full
      assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf})
    end
  end

  test "a step that only mentions gitleaks is not a scan" do
    install_only =
      String.replace(
        @pr_range,
        ~s(      - run: gitleaks git --log-opts="${{ github.event.pull_request.base.sha }}..HEAD" --redact),
        "      - name: Install gitleaks\n        run: curl -sSfL https://example.invalid/gitleaks.tgz -o g.tgz"
      )

    assert install_only != @pr_range
    assert check(%{"pr.yml" => install_only}) == []

    setup_job =
      String.replace(
        @scheduled_full,
        ~s(      - run: gitleaks git --log-opts="--all" --redact),
        "      - name: gitleaks cache warm-up\n        run: echo ready"
      )

    assert setup_job != @scheduled_full
    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => setup_job})
  end

  test "a != guard that rules out every non-PR trigger does not remain" do
    wf =
      String.replace(
        @scheduled_full,
        "on:\n  schedule:\n    - cron: '0 3 * * 1'",
        "on:\n  push:\n    branches: [main]\n  pull_request:"
      )
      |> String.replace(
        "    runs-on: ubuntu-latest",
        "    if: github.event_name != 'push'\n    runs-on: ubuntu-latest"
      )

    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf})
  end

  test "a guard too complex to read is a PR scan that does not remain" do
    wf =
      String.replace(
        @scheduled_full,
        "    runs-on: ubuntu-latest",
        "    if: github.event_name == 'schedule' || github.actor == 'bot'\n    runs-on: ubuntu-latest"
      )
      |> String.replace("    - cron: '0 3 * * 1'\n", "    - cron: '0 3 * * 1'\n  pull_request:\n")

    assert [%{rule: "WF032", file: "full.yml"}] = check(%{"full.yml" => wf})
  end

  test "a local reusable scanner runs on its caller's events" do
    callee = """
    on:
      workflow_call:
    jobs:
      scan:
        runs-on: ubuntu-latest
        steps:
          - uses: actions/checkout@v7.0.1
            with:
              fetch-depth: 0
          - run: gitleaks git --log-opts="--all" --redact
    """

    caller = fn trigger ->
      """
      on:
        #{trigger}
      jobs:
        secrets:
          uses: ./.github/workflows/scan.yml
      """
    end

    # Called on PRs only: a PR scan and nothing remains.
    assert [%{rule: "WF032", file: "scan.yml"}] =
             check(%{"scan.yml" => callee, "pr.yml" => caller.("pull_request:")})

    # Called on push too: the same full scan remains.
    assert check(%{
             "scan.yml" => callee,
             "pr.yml" => @pr_range,
             "main.yml" => caller.("push:\n    branches: [main]")
           }) == []
  end

  test "a trufflehog URL source needs no checkout" do
    remote =
      String.replace(
        @scheduled_full,
        ~s(gitleaks git --log-opts="--all" --redact),
        "trufflehog git https://github.com/${{ github.repository }}"
      )

    remote =
      String.replace(
        remote,
        "      - uses: actions/checkout@v7.0.1\n        with:\n          fetch-depth: 0\n",
        ""
      )

    assert remote =~ "trufflehog git https://"
    refute remote =~ "actions/checkout"
    assert check(%{"pr.yml" => @pr_range, "full.yml" => remote}) == []
  end

  test "gitleaks protect is not a full-history scan" do
    protect =
      String.replace(
        @scheduled_full,
        ~s(gitleaks git --log-opts="--all" --redact),
        "gitleaks protect --staged --redact"
      )

    assert protect != @scheduled_full
    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => protect})
  end

  test "trufflehog --max-depth or --branch limits the history" do
    for flag <- ["--max-depth 50", "--branch main"] do
      wf =
        String.replace(
          @scheduled_full,
          ~s(gitleaks git --log-opts="--all" --redact),
          "trufflehog git file://. " <> flag
        )

      assert wf != @scheduled_full
      assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf}), flag
    end
  end

  test "--all with a revision range or limit is a range" do
    for opts <- ["--all A..B", "--all ^abc123", "--all --since=2026-01-01", "--all -n 50"] do
      wf = String.replace(@scheduled_full, ~s(--log-opts="--all"), ~s(--log-opts="#{opts}"))
      assert wf != @scheduled_full
      assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf}), opts
    end
  end

  test "a deep checkout into a path feeds only a scanner run there" do
    pathed =
      String.replace(
        @scheduled_full,
        "          fetch-depth: 0\n",
        "          fetch-depth: 0\n          path: source\n"
      )

    assert pathed != @scheduled_full
    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => pathed})

    there =
      String.replace(
        pathed,
        ~s(gitleaks git --log-opts="--all" --redact),
        ~s(gitleaks git --source source --log-opts="--all" --redact)
      )

    assert there != pathed
    assert check(%{"pr.yml" => @pr_range, "full.yml" => there}) == []

    wd =
      String.replace(
        pathed,
        ~s(      - run: gitleaks git),
        ~s(      - working-directory: source\n        run: gitleaks git)
      )

    assert wd != pathed
    assert check(%{"pr.yml" => @pr_range, "full.yml" => wd}) == []
  end

  test "only the estate's own rail counts as the rail" do
    rail = fn uses ->
      "on:\n  push:\n    branches: [main]\njobs:\n  secrets:\n    uses: #{uses}\n"
    end

    sha = "@0123456789abcdef0123456789abcdef01234567"

    for good <- [
          "hyperpolymath/standards/.github/workflows/secret-scanner-reusable.yml" <> sha,
          "metadatastician/standards/.github/workflows/secret-scanner-reusable.yml" <> sha
        ] do
      assert check(%{"pr.yml" => @pr_range, "rail.yml" => rail.(good)}) == [], good
    end

    for bad <- [
          "someone/else/.github/workflows/secret-scanner-reusable.yml" <> sha,
          "./.github/workflows/secret-scanner-reusable.yml"
        ] do
      assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "rail.yml" => rail.(bad)}), bad
    end
  end

  test "a constant-false guard disables the scan" do
    for guard <- ["false", "${{ false }}", "github.event_name == 'schedule' && false"] do
      wf =
        String.replace(
          @scheduled_full,
          "    runs-on: ubuntu-latest",
          "    if: #{guard}\n    runs-on: ubuntu-latest"
        )

      assert wf != @scheduled_full
      assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf}), guard
    end

    disabled_pr =
      String.replace(
        @pr_range,
        "    runs-on: ubuntu-latest",
        "    if: false\n    runs-on: ubuntu-latest"
      )

    assert check(%{"pr.yml" => disabled_pr}) == []
  end

  test "more git history limits beside --all are a range" do
    for opts <- ["--all --max-age=1700000000", "--all --first-parent", "--all -50"] do
      wf = String.replace(@scheduled_full, ~s(--log-opts="--all"), ~s(--log-opts="#{opts}"))
      assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf}), opts
    end
  end

  test "the trufflehog action with base or head inputs is a range" do
    action = fn extra ->
      "on:\n  schedule:\n    - cron: '0 3 * * 1'\njobs:\n  scan:\n    runs-on: ubuntu-latest\n    steps:\n" <>
        "      - uses: actions/checkout@v7.0.1\n        with:\n          fetch-depth: 0\n" <>
        "      - uses: trufflesecurity/trufflehog@6c05c4a00b91aa542267d8e32a8254774799d68d\n" <>
        extra
    end

    assert check(%{"pr.yml" => @pr_range, "a.yml" => action.("")}) == []

    assert [%{rule: "WF032"}] =
             check(%{
               "pr.yml" => @pr_range,
               "a.yml" => action.("        with:\n          base: main\n          head: HEAD\n")
             })
  end

  test "only actions/checkout itself is a checkout" do
    wf =
      String.replace(
        @scheduled_full,
        "      - uses: actions/checkout@v7.0.1",
        "      - uses: actions/checkout-wrapper@v1"
      )

    assert wf != @scheduled_full
    assert [%{rule: "WF032"}] = check(%{"pr.yml" => @pr_range, "full.yml" => wf})
  end

  test "is skipped when audit/3 is told the workflow set is partial" do
    %{findings: fs} = WorkflowAudit.audit(["pr.yml"], %{"pr.yml" => @pr_range}, partial: true)
    refute Enum.any?(fs, &(&1[:rule] == "WF032"))
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
