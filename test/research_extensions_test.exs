# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensionsTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ResearchExtensions

  @tmp_dir System.tmp_dir!()

  defp create_repo_with_workflow(yaml_content, filename \\ "test.yml") do
    repo = Path.join(@tmp_dir, "re_test_#{System.unique_integer([:positive])}")
    # unique_integer restarts with each VM, so clear what an earlier run left.
    File.rm_rf!(repo)
    wf = Path.join([repo, ".github", "workflows"])
    File.mkdir_p!(wf)
    File.write!(Path.join(wf, filename), yaml_content)
    repo
  end

  defp create_repo_with_composite(action_yml_content, subdir \\ "my-action") do
    repo = Path.join(@tmp_dir, "re_composite_#{System.unique_integer([:positive])}")
    dir = Path.join(repo, subdir)
    File.mkdir_p!(dir)
    File.write!(Path.join(dir, "action.yml"), action_yml_content)
    repo
  end

  # ─── RE001 ──────────────────────────────────────────────────────────

  describe "re001_missing_harden_runner/1" do
    test "flags workflow that uses secrets but has no harden-runner" do
      repo =
        create_repo_with_workflow("""
        name: Deploy
        on: [push]
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
              - run: npm ci
        """)

      findings = ResearchExtensions.re001_missing_harden_runner(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "RE001"
      assert hd(findings).severity == :warn
      assert hd(findings).line == 7
      File.rm_rf!(repo)
    end

    test "reusable-only callers delegate runner hardening to the workflow source" do
      repo =
        create_repo_with_workflow("""
        jobs:
          mirror:
            uses: owner/standards/.github/workflows/mirror.yml@main
            secrets:
              MIRROR_KEY: ${{ secrets.MIRROR_KEY }}
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "commented hardening does not hide a real secret reference or move its location" do
      repo =
        create_repo_with_workflow("""
        # A managed header added by the action-lock tool
        # uses: step-security/harden-runner@main
        # Example: ${{ secrets.EXAMPLE }}
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
              - run: npm ci
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.line == 8
      assert finding.severity == :warn
      File.rm_rf!(repo)
    end

    test "passes when harden-runner is installed" do
      repo =
        create_repo_with_workflow("""
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - uses: step-security/harden-runner@1234567890abcdef1234567890abcdef12345678
                with:
                  egress-policy: block
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
              - run: npm ci
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "passes when harden-runner uses values are quoted" do
      repo =
        create_repo_with_workflow("""
        jobs:
          double-quoted:
            runs-on: ubuntu-latest
            steps:
              - uses: "step-security/harden-runner@main"
              - run: deploy --token=${{ secrets.DOUBLE_QUOTED_KEY }}
              - run: npm ci
          single-quoted:
            runs-on: ubuntu-latest
            steps:
              - uses: 'step-security/harden-runner@main'
              - run: deploy --token=${{ secrets.SINGLE_QUOTED_KEY }}
              - run: npm ci
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "does not treat nested multiline values as runner configuration or hardening" do
      repo =
        create_repo_with_workflow("""
        jobs:
          nested-values:
            env:
              WORKFLOW_EXAMPLE: |
                runs-on: ubuntu-latest
                - uses: step-security/harden-runner@main
            steps:
              - run: deploy --token=${{ secrets.NESTED_ONLY }}
          exposed:
            runs-on: ubuntu-latest
            steps:
              - run: |
                  runs-on: ubuntu-latest
                  - uses: step-security/harden-runner@main
                  deploy --token=${{ secrets.EXPOSED }}
              - run: npm ci
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.line == 15
      File.rm_rf!(repo)
    end

    test "mixed reusable and local jobs do not share runner or hardening state" do
      repo =
        create_repo_with_workflow("""
        jobs:
          shared:
            uses: owner/standards/.github/workflows/mirror.yml@main
            secrets:
              MIRROR_KEY: ${{ secrets.MIRROR_KEY }}
          local:
            runs-on: ubuntu-latest
            steps:
              - run: echo no credentials
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []

      File.write!(Path.join([repo, ".github/workflows", "test.yml"]), """
      jobs:
        hardened:
          runs-on: ubuntu-latest
          steps:
            - uses: step-security/harden-runner@main
            - run: deploy --token=${{ secrets.ONE }}
            - run: npm ci
        exposed:
          runs-on: ubuntu-latest
          steps:
            - run: deploy --token=${{ secrets.TWO }}
            - run: npm ci
      """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.line == 11
      assert finding.severity == :warn
      File.rm_rf!(repo)
    end

    test "passes when no secrets are referenced" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: echo hi
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    # RE001 v2: a finding needs a real credential AND an exposure.

    test "the ambient GITHUB_TOKEN alone is not a credential, even next to fetched code" do
      repo =
        create_repo_with_workflow("""
        on: [pull_request_target]
        jobs:
          label:
            runs-on: ubuntu-latest
            steps:
              - run: npm ci && gh label create x
                env:
                  GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "a credential next to only pinned actions and the repo's own scripts is :warn, with no exposure" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          sweep:
            runs-on: ubuntu-latest
            steps:
              - uses: actions/checkout@v7.0.1
              - name: cargo build (label only)
                run: ./scripts/sweep.sh
                env:
                  GH_TOKEN: ${{ secrets.DISPATCH_PAT }}
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "an untrusted-input trigger in block form is an exposure" do
      repo =
        create_repo_with_workflow("""
        on:
          pull_request_target:
            types: [opened]
        jobs:
          comment:
            runs-on: ubuntu-latest
            steps:
              - run: ./scripts/comment.sh
                env:
                  TOKEN: ${{ secrets.BOT_PAT }}
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.severity == :warn
      assert finding.line == 10
      assert finding.reason =~ "untrusted-input trigger (pull_request_target)"
      File.rm_rf!(repo)
    end

    test "an untrusted-input trigger in an inline list is an exposure" do
      repo =
        create_repo_with_workflow("""
        on: [push, issue_comment]
        jobs:
          reply:
            runs-on: ubuntu-latest
            steps:
              - run: ./reply.sh ${{ secrets.BOT_PAT }}
        """)

      assert [%{severity: :warn}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "decoy: `issues:` under top-level permissions is not an issues trigger" do
      repo =
        create_repo_with_workflow("""
        on:
          push:
        permissions:
          issues: write
        jobs:
          sync:
            runs-on: ubuntu-latest
            steps:
              - run: ./sync.sh ${{ secrets.BOT_PAT }}
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "a pull-request head checkout is an exposure" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          scan:
            runs-on: ubuntu-latest
            steps:
              - uses: actions/checkout@v7.0.1
                with:
                  ref: ${{ github.event.pull_request.head.sha }}
              - run: ./upload.sh
                env:
                  PAT: ${{ secrets.SCAN_PAT }}
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.severity == :warn
      assert finding.detail.exposures == ["a pull-request or triggering-run head checkout"]
      File.rm_rf!(repo)
    end

    test "decoy: naming the head SHA in env, if or concurrency is not a checkout" do
      repo =
        create_repo_with_workflow("""
        on: [push] # issues are triaged elsewhere
        jobs:
          report:
            if: ${{ startsWith(github.head_ref, 'dependabot/') }}
            concurrency:
              group: report-${{ github.head_ref }}
            runs-on: ubuntu-latest
            steps:
              - run: ./report.sh
                env:
                  HEAD_SHA: ${{ github.event.pull_request.head.sha }}
                  PAT: ${{ secrets.REPORT_PAT }}
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "a git fetch of the head is a checkout" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          report:
            runs-on: ubuntu-latest
            steps:
              - run: git fetch origin ${{ github.head_ref }} && ./report.sh
              - run: ./upload.sh ${{ secrets.REPORT_PAT }}
        """)

      assert [%{severity: :warn, line: 7}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "secret names are case-insensitive: secrets.github_token is the ambient token" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          label:
            runs-on: ubuntu-latest
            steps:
              - run: npm ci && gh label create x
                env:
                  GH_TOKEN: ${{ secrets.github_token }}
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "decoy: a word inside an inline trigger map's filters is not an event" do
      repo =
        create_repo_with_workflow("""
        on: {push: {paths: [issues/**]}}
        jobs:
          sync:
            runs-on: ubuntu-latest
            steps:
              - run: ./sync.sh ${{ secrets.BOT_PAT }}
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "an inline trigger map's top-level keys are events" do
      repo =
        create_repo_with_workflow("""
        on: {push: {branches: [main]}, issue_comment: {types: [created]}}
        jobs:
          reply:
            runs-on: ubuntu-latest
            steps:
              - run: ./reply.sh ${{ secrets.BOT_PAT }}
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "untrusted-input trigger (issue_comment)"
      File.rm_rf!(repo)
    end

    test "decoy: install commands in env values and comments run nothing" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - run: ./deploy.sh # npm ci happens in the image
                env:
                  NOTE: "run npm ci locally first"
                  TOKEN: ${{ secrets.DEPLOY_KEY }}
              - run: |
                  # cargo build is done upstream
                  ./publish.sh
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "a folded run scalar is matched as the command it folds into" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            steps:
              - run: >-
                  npm
                  ci
                env:
                  TOKEN: ${{ secrets.NPM_TOKEN }}
        """)

      assert [%{severity: :high, line: 10}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "a credential in the job's env is in the same step as that job's build: :high" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            env:
              NPM_TOKEN: ${{ secrets.NPM_TOKEN }}
            steps:
              - run: npm ci
        """)

      assert [%{severity: :high, line: 6}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "a job gated to trusted events is not exposed by the workflow's untrusted trigger" do
      repo =
        create_repo_with_workflow("""
        on: [push, issue_comment]
        jobs:
          deploy:
            if: github.event_name == 'push'
            runs-on: ubuntu-latest
            steps:
              - run: ./deploy.sh ${{ secrets.DEPLOY_KEY }}
          reply:
            if: ${{ github.event_name == 'push' || github.event_name == 'issue_comment' }}
            runs-on: ubuntu-latest
            steps:
              - run: ./reply.sh ${{ secrets.BOT_PAT }}
          complex:
            if: github.event_name == 'push' && github.actor != 'x'
            runs-on: ubuntu-latest
            steps:
              - run: ./other.sh ${{ secrets.OTHER_PAT }}
        """)

      # `deploy` is gated to push, so it has no exposure but is still an
      # undeclared secret-bearing job (ADR-009); `reply` admits issue_comment;
      # a condition that is not a plain event_name test is not interpreted.
      assert [
               %{line: 7, severity: :warn, detail: %{exposures: []}},
               %{line: 12, detail: %{exposures: ["an untrusted-input trigger (issue_comment)"]}},
               %{line: 17, detail: %{exposures: ["an untrusted-input trigger (issue_comment)"]}}
             ] =
               repo |> ResearchExtensions.re001_missing_harden_runner() |> Enum.sort_by(& &1.line)

      File.rm_rf!(repo)
    end

    test "a workflow_run head checkout is recognised by its head_sha field" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          report:
            runs-on: ubuntu-latest
            steps:
              - uses: actions/checkout@v7.0.1
                with:
                  ref: ${{ github.event.workflow_run.head_sha }}
                  token: ${{ secrets.REPORT_PAT }}
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.severity == :high
      assert finding.detail.exposures == ["a pull-request or triggering-run head checkout"]
      File.rm_rf!(repo)
    end

    test "a step opened by a bare `-` is still a step" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            steps:
              -
                run: npm ci
                env:
                  NPM_TOKEN: ${{ secrets.NPM_TOKEN }}
        """)

      assert [%{severity: :high}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "a step opened by `-` and a comment is still a step, with its own run:" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            steps:
              -     # install dependencies
                run: npm ci
                env:
                  NPM_TOKEN: ${{ secrets.NPM_TOKEN }}
        """)

      assert [%{severity: :high}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "an indentless steps: sequence is read: its harden-runner step counts" do
      repo =
        create_repo_with_workflow("""
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
            - uses: step-security/harden-runner@1234567890abcdef1234567890abcdef12345678
              with:
                egress-policy: block
            - run: deploy --token=${{ secrets.DEPLOY_KEY }}
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "an indentless steps: sequence is read: a credential beside an install is :high" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            steps:
            - run: npm ci
              env:
                NPM_TOKEN: ${{ secrets.NPM_TOKEN }}
            timeout-minutes: 10
        """)

      assert [%{severity: :high}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "a step that overrides the job-level secret variable does not hold it" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            env:
              NPM_TOKEN: ${{ secrets.NPM_TOKEN }}
            steps:
              - run: npm ci
                env:
                  NPM_TOKEN: ""
              - run: ./publish.sh
        """)

      # The job still holds the credential next to the install, but not in
      # the install's own environment.
      assert [%{severity: :warn, line: 6}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "overriding one job-level secret leaves the step holding the others" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            env:
              NPM_TOKEN: ${{ secrets.NPM_TOKEN }}
              REGISTRY_KEY: ${{ secrets.REGISTRY_KEY }}
            steps:
              - run: npm ci
                env:
                  NPM_TOKEN: ""
        """)

      assert [%{severity: :high, line: 7}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "decoy: a head repository passed to a non-checkout action is not a checkout" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          report:
            runs-on: ubuntu-latest
            steps:
              - uses: some-org/pr-reporter@v1
                with:
                  repository: ${{ github.event.pull_request.head.repo.full_name }}
                  token: ${{ secrets.REPORT_PAT }}
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "a folded git fetch of the head is matched across its lines" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          report:
            runs-on: ubuntu-latest
            steps:
              - run: >-
                  git fetch origin
                  ${{ github.head_ref }}
              - run: ./upload.sh ${{ secrets.REPORT_PAT }}
        """)

      assert [%{severity: :warn}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "decoy: a git fetch and a head reference in different commands are not a head fetch" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          report:
            runs-on: ubuntu-latest
            steps:
              - run: git fetch origin main && echo ${{ github.head_ref }}
              - run: ./upload.sh ${{ secrets.REPORT_PAT }}
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "an anchored block of triggers is read as a block" do
      repo =
        create_repo_with_workflow("""
        on: &events
          issue_comment:
            types: [created]
        jobs:
          reply:
            runs-on: ubuntu-latest
            steps:
              - run: ./reply.sh ${{ secrets.BOT_PAT }}
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "untrusted-input trigger (issue_comment)"
      File.rm_rf!(repo)
    end

    test "a flow trigger mapping continued past the on: line is read whole" do
      repo =
        create_repo_with_workflow("""
        on: {
          push: {branches: [main]},
          issue_comment: {types: [created]}
        }
        jobs:
          reply:
            runs-on: ubuntu-latest
            steps:
              - run: ./reply.sh ${{ secrets.BOT_PAT }}
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "untrusted-input trigger (issue_comment)"
      File.rm_rf!(repo)
    end

    test "a folded piped installer is matched across its lines" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - run: >-
                  curl -fsSL
                  https://example.invalid/install.sh
                  | bash
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
        """)

      assert [%{severity: :warn}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "decoy: an env variable named run is not the step's script" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - env:
                  run: "npm ci"
                  TOKEN: ${{ secrets.DEPLOY_KEY }}
                run: ./deploy.sh
        """)

      assert [%{severity: :warn, detail: %{exposures: []}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "a flow-style checkout with: mapping naming the head is a head checkout" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          report:
            runs-on: ubuntu-latest
            steps:
              - uses: actions/checkout@v7.0.1
                with: {ref: "${{ github.head_ref }}", token: "${{ secrets.REPORT_PAT }}"}
        """)

      assert [%{severity: :high}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "a piped installer is an exposure" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - run: curl -fsSL https://example.invalid/install.sh | bash
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
        """)

      assert [%{severity: :warn, line: 7}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "a credential in the same step as a dependency build is :high, at that step's reference" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          integration:
            runs-on: ubuntu-latest
            steps:
              - run: ./setup.sh ${{ secrets.SETUP_KEY }}
              - name: Run integration tests
                run: cargo test --workspace
                env:
                  TEST_TOKEN: ${{ secrets.TEST_TOKEN }}
        """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.severity == :high
      assert finding.line == 10
      assert finding.reason =~ "in the same step"
      File.rm_rf!(repo)
    end

    test "a job naming GITHUB_TOKEN and a real credential is judged on the credential" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          release:
            runs-on: ubuntu-latest
            steps:
              - run: mix deps.get
              - run: ./publish.sh
                env:
                  GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
                  HEX_API_KEY: ${{ secrets.HEX_API_KEY }}
        """)

      assert [%{severity: :warn, line: 10}] = ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "does not count the job token, under either spelling" do
      repo =
        create_repo_with_workflow("""
        jobs:
          labels:
            runs-on: ubuntu-latest
            steps:
              - env:
                  GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
                  ALSO: ${{ github.token }}
                run: gh label list
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "points past the job token to the first real secret" do
      repo =
        create_repo_with_workflow("""
        jobs:
          mixed:
            runs-on: ubuntu-latest
            steps:
              - run: gh api --token=${{ secrets.GITHUB_TOKEN }}
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
        """)

      assert [%{line: 6, reason: reason}] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert reason =~ "job `mixed`"
      File.rm_rf!(repo)
    end
  end

  # ─── RE001 with an egress declaration (standards ADR-009) ──────────

  describe "re001_missing_harden_runner/1 with a repo deed" do
    @smtp_workflow """
    jobs:
      notify:
        runs-on: ubuntu-24.04
        steps:
          - uses: hyperpolymath/smtp-notify-action@v0.5.0
            with:
              server_address: ${{ secrets.SMTP_HOST }}
              password: ${{ secrets.SMTP_PASS }}
    """

    defp write_deed(repo, egress) do
      File.write!(Path.join(repo, "x_chora.deed"), """
      ;; SPDX-License-Identifier: CC-BY-SA-4.0
      (repo-deed :schema-version "1.0.0" :canonical-name "x"
        :repo-uuid #u5"github.com/o/x" :beholding-chora #u5"estate/chora"
        #{egress})
      """)
    end

    defp smtp_job(secrets) do
      ~s|(job :workflow ".github/workflows/notify.yml" :job "notify" | <>
        ~s|:secrets (#{secrets}) :endpoints ("secret:SMTP_HOST") :enforcement none)|
    end

    test "an exact declaration satisfies the rule" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")

      write_deed(
        repo,
        ~s|(egress :policy "standards ADR-009" #{smtp_job(~s|"SMTP_PASS" "SMTP_HOST"|)})|
      )

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "without a declaration the fix names the deed and the exact entry" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")
      write_deed(repo, "")

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.line == 7
      assert finding.detail.fix =~ "x_chora.deed"
      assert finding.detail.fix =~ ~s|:job "notify" :secrets ("SMTP_HOST" "SMTP_PASS")|
      File.rm_rf!(repo)
    end

    test "a secret the declaration misses is drift" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")
      write_deed(repo, "(egress #{smtp_job(~s|"SMTP_HOST"|)})")

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.file == ".github/workflows/notify.yml"
      assert finding.reason =~ ~s|reads undeclared "SMTP_PASS"|
      File.rm_rf!(repo)
    end

    test "a declared secret the job no longer reads is drift" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")
      write_deed(repo, "(egress #{smtp_job(~s|"SMTP_HOST" "SMTP_PASS" "SMTP_USER"|)})")

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ ~s|declares unread "SMTP_USER"|
      refute finding.reason =~ "reads undeclared"
      File.rm_rf!(repo)
    end

    test "a declaration for a job that reads no real secret is stale" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: gh api --token=${{ secrets.GITHUB_TOKEN }}
          """,
          "notify.yml"
        )

      write_deed(repo, "(egress #{smtp_job(~s|"SMTP_HOST"|)})")

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.file == "x_chora.deed"
      assert finding.reason =~ "stale"
      File.rm_rf!(repo)
    end

    test "a declaration for a reusable-workflow job is stale" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              uses: org/repo/.github/workflows/mail.yml@v1
              secrets:
                host: ${{ secrets.SMTP_HOST }}
          """,
          "notify.yml"
        )

      write_deed(repo, "(egress #{smtp_job(~s|"SMTP_HOST"|)})")

      assert [%{file: "x_chora.deed"}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "an unreadable clause declares nothing, and the finding says why" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")
      write_deed(repo, ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify"))|)

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "the repo deed declares nothing ((job …) is missing :secrets)"
      File.rm_rf!(repo)
    end

    test "harden-runner still satisfies the rule, declared or not" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - uses: step-security/harden-runner@v2
                - run: send --pass=${{ secrets.SMTP_PASS }}
          """,
          "notify.yml"
        )

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []

      # A declaration is still an attestation: listing the wrong secrets is
      # drift, at :warn because harden-runner contains the job.
      write_deed(repo, "(egress #{smtp_job(~s|"SMTP_HOST"|)})")

      assert [%{severity: :warn, reason: reason}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      assert reason =~ ~s|reads undeclared "SMTP_PASS"|
      File.rm_rf!(repo)
    end

    test "a declared harden-runner enforcement must still be installed" do
      workflow = fn hardener ->
        """
        jobs:
          notify:
            runs-on: ubuntu-24.04
            steps:
        #{hardener}      - run: send --pass=${{ secrets.SMTP_PASS }}
        """
      end

      entry =
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("SMTP_PASS") :endpoints () :enforcement harden-runner-block))|

      hardened =
        create_repo_with_workflow(
          workflow.(
            "      - uses: step-security/harden-runner@v2\n" <>
              "        with:\n          egress-policy: block\n"
          ),
          "notify.yml"
        )

      write_deed(hardened, entry)
      assert ResearchExtensions.re001_missing_harden_runner(hardened) == []

      # The hardener was removed, but the deed still claims block enforcement.
      bare = create_repo_with_workflow(workflow.(""), "notify.yml")
      write_deed(bare, entry)
      assert [finding] = ResearchExtensions.re001_missing_harden_runner(bare)
      assert finding.reason =~ ":enforcement harden-runner-block"
      assert finding.reason =~ "does not install"

      File.rm_rf!(hardened)
      File.rm_rf!(bare)
    end

    test "a declared block enforcement needs egress-policy: block, not audit or the default" do
      check = fn with_lines ->
        repo =
          create_repo_with_workflow(
            """
            jobs:
              notify:
                runs-on: ubuntu-24.04
                steps:
                  - uses: step-security/harden-runner@v2
            #{with_lines}      - run: send --pass=${{ secrets.SMTP_PASS }}
            """,
            "notify.yml"
          )

        write_deed(
          repo,
          ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
            ~s|:secrets ("SMTP_PASS") :endpoints ("a.com:443" "b.com:443") :enforcement harden-runner-block))|
        )

        findings = ResearchExtensions.re001_missing_harden_runner(repo)
        File.rm_rf!(repo)
        findings
      end

      assert check.("        with:\n          egress-policy: block\n") == []
      assert check.("        with: {egress-policy: block}\n") == []

      assert [%{reason: audit}] = check.("        with:\n          egress-policy: audit\n")
      assert audit =~ "runs `egress-policy: audit`"

      # harden-runner audits when no egress-policy is set.
      assert [%{reason: default}] = check.("")
      assert default =~ "runs `egress-policy: audit`"

      # Quoted inputs and quoted keys are the same input to YAML.
      assert check.("        \"with\":\n          'egress-policy': \"block\"\n") == []

      assert check.(
               "        with: {allowed-endpoints: \"a.com:443, b.com:443\", egress-policy: block}\n"
             ) == []

      # A YAML comment sets no input: these steps run the default, audit.
      assert [_] = check.("        # with:\n        #   egress-policy: block\n")
      assert [_] = check.("        with:\n          disable-sudo: true # egress-policy: block\n")
      assert [_] = check.("        with: {disable-sudo: true} # {egress-policy: block}\n")

      # Text inside another input's value is not the policy input.
      assert [_] = check.("        with:\n          note: \"x, egress-policy: block\"\n")
    end

    test "a comment on the uses: line does not set the policy" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - uses: step-security/harden-runner@v2 # egress-policy: block
                - run: send --pass=${{ secrets.SMTP_PASS }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("SMTP_PASS") :endpoints () :enforcement harden-runner-block))|
      )

      assert [%{reason: reason}] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert reason =~ "runs `egress-policy: audit`"
      File.rm_rf!(repo)
    end

    test "a hardened job's declaration cannot cover a computed key" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - uses: step-security/harden-runner@v2
                  with:
                    egress-policy: block
                - run: send --pass=${{ secrets.SMTP_PASS }} --x=${{ secrets[env.NAME] }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("SMTP_PASS") :endpoints () :enforcement harden-runner-block))|
      )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.severity == :warn
      assert finding.line == 8
      assert finding.reason =~ "computed at run time"
      assert finding.reason =~ "does not cover it"
      assert finding.detail.fix =~ "remove the job's entry from x_chora.deed"
      File.rm_rf!(repo)
    end

    test "quoted keys are the same keys: \"env\", 'runs-on', \"steps\", \"jobs\"" do
      repo =
        create_repo_with_workflow(
          """
          "env":
            HIDDEN: ${{ secrets.HIDDEN }}
          "jobs":
            notify:
              'runs-on': ubuntu-24.04
              "steps":
                - run: send --pass=${{ secrets.SMTP_PASS }}
          """,
          "notify.yml"
        )

      # The job inherits HIDDEN from the quoted top-level env:, so a
      # declaration of SMTP_PASS alone has drifted.
      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("SMTP_PASS") :endpoints () :enforcement none))|
      )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "HIDDEN"
      File.rm_rf!(repo)
    end

    test "a quoted job-level 'env': holds the job's credentials" do
      repo =
        create_repo_with_workflow("""
        on: pull_request_target
        jobs:
          build:
            runs-on: ubuntu-24.04
            'env':
              TOKEN: ${{ secrets.DEPLOY_TOKEN }}
            steps:
              - uses: actions/checkout@v4
                with:
                  ref: ${{ github.event.pull_request.head.sha }}
              - run: make
        """)

      assert [%{severity: :high}] = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
    end

    test "quoted step keys still expose the credential: \"uses\", 'ref', \"run\"" do
      for step <- [
            ~s|      - "uses": actions/checkout@v4\n        with:\n          'ref': ${{ github.head_ref }}\n      - run: make|,
            ~s|      - uses: actions/checkout@v4\n      - "run": npm install|
          ] do
        repo =
          create_repo_with_workflow("""
          on: pull_request_target
          jobs:
            build:
              runs-on: ubuntu-24.04
              env:
                TOKEN: ${{ secrets.DEPLOY_TOKEN }}
              steps:
          #{step}
          """)

        assert [%{severity: :high}] = ResearchExtensions.re001_missing_harden_runner(repo), step
        File.rm_rf!(repo)
      end
    end

    test "an alias brings its anchor's secrets into the job" do
      workflow = fn second_job ->
        """
        jobs:
          build:
            runs-on: ubuntu-24.04
            env: &common
              HIDDEN: ${{ secrets.HIDDEN }}
            steps:
              - run: make
        #{second_job}
        """
      end

      deploy_entry =
        ~s|(job :workflow ".github/workflows/notify.yml" :job "deploy" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none)|

      build_entry =
        ~s|(job :workflow ".github/workflows/notify.yml" :job "build" | <>
          ~s|:secrets ("HIDDEN") :endpoints () :enforcement none)|

      check = fn second_job ->
        repo = create_repo_with_workflow(workflow.(second_job), "notify.yml")
        write_deed(repo, "(egress #{build_entry} #{deploy_entry})")
        findings = ResearchExtensions.re001_missing_harden_runner(repo)
        File.rm_rf!(repo)
        findings
      end

      # env: *common
      assert [%{line: 10, reason: plain}] =
               check.("""
                 deploy:
                   runs-on: ubuntu-24.04
                   env: *common
                   steps:
                     - run: send ${{ secrets.VISIBLE }}
               """)

      assert plain =~ "`deploy`"
      assert plain =~ "HIDDEN"

      # A merge key, `<<: *common`, is outside YAML 1.2 and GitHub refuses it:
      # the workflow is reported as unreadable, at the key, not guessed at.
      assert [%{line: 11, severity: :warn, reason: merged}] =
               check.("""
                 deploy:
                   runs-on: ubuntu-24.04
                   env:
                     <<: *common
                     OTHER: x
                   steps:
                     - run: send ${{ secrets.VISIBLE }}
               """)

      assert merged =~ "UNSUPPORTED_MERGE_KEY"

      # The same declaration, without the alias, is exact.
      assert check.("""
               deploy:
                 runs-on: ubuntu-24.04
                 steps:
                   - run: send ${{ secrets.VISIBLE }}
             """) == []
    end

    test "a job that is an alias of another job is read as that job" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            build: &job
              runs-on: ubuntu-24.04
              steps:
                - run: send ${{ secrets.SMTP_PASS }}
            deploy: *job
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "build" | <>
          ~s|:secrets ("SMTP_PASS") :endpoints () :enforcement none))|
      )

      # deploy is build, read through the alias and reported at it.
      assert [%{line: 6, reason: reason, detail: %{fix: fix}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      assert reason =~ "`deploy`"
      assert fix =~ ~s|:secrets ("SMTP_PASS")|
      File.rm_rf!(repo)
    end

    test "a merge key makes the workflow unreadable: reported, never read in part" do
      # Read as YAML 1.1, deploy's own steps: would win over the merged,
      # hardened ones. GitHub reads YAML 1.2, which has no merge key, and so
      # does RE001: one finding at the key, and the workflow's declarations
      # are not reported stale, because they cannot be checked.
      repo =
        create_repo_with_workflow(
          """
          jobs:
            build: &defaults
              runs-on: ubuntu-24.04
              env:
                HIDDEN: ${{ secrets.HIDDEN }}
              steps:
                - uses: step-security/harden-runner@v2
                  with:
                    egress-policy: block
                - run: make
            deploy:
              <<: *defaults
              env:
                OTHER: x
              steps:
                - run: send ${{ secrets.VISIBLE }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "deploy" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
      )

      assert [%{line: 12, severity: :warn, reason: reason, detail: %{fix: fix}}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      assert reason =~ "UNSUPPORTED_MERGE_KEY at line 12, column 5"
      assert fix =~ "`<<` merge key"
      File.rm_rf!(repo)
    end

    test "alias-like text inside a scalar is text, not an alias" do
      for step <- [
            ~s|      - run: echo "x *common"|,
            ~s|      - run: echo x *common|,
            ~s|      - run: echo a, *common|,
            ~s|      - run: \|\n          ls *common|
          ] do
        repo =
          create_repo_with_workflow(
            """
            jobs:
              build:
                runs-on: ubuntu-24.04
                env: &common
                  HIDDEN: ${{ secrets.HIDDEN }}
                steps:
                  - run: make
              deploy:
                runs-on: ubuntu-24.04
                steps:
                  - run: send ${{ secrets.VISIBLE }}
            #{step}
            """,
            "notify.yml"
          )

        write_deed(
          repo,
          ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "build" | <>
            ~s|:secrets ("HIDDEN") :endpoints () :enforcement none) | <>
            ~s|(job :workflow ".github/workflows/notify.yml" :job "deploy" | <>
            ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
        )

        assert ResearchExtensions.re001_missing_harden_runner(repo) == [], step
        File.rm_rf!(repo)
      end
    end

    test "a block-scalar alias keeps its script, and the script its exposure" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            build:
              runs-on: ubuntu-24.04
              steps:
                - run: &script |
                    npm install
                    deploy --token=${{ secrets.TOKEN }}
            deploy:
              runs-on: ubuntu-24.04
              steps:
                - run: *script
          """,
          "notify.yml"
        )

      write_deed(repo, "")

      # npm install runs in the step that holds TOKEN, in both jobs.
      assert [%{severity: :high, line: 7}, %{severity: :high, line: 11}] =
               ResearchExtensions.re001_missing_harden_runner(repo)

      File.rm_rf!(repo)
    end

    test "nested anchors that multiply (the billion-laughs file) are refused within a budget" do
      anchors =
        for i <- 1..9 do
          prev = if i == 1, do: ~s|"${{ secrets.SMTP_PASS }}"|, else: "*a#{i - 1}"
          "  a#{i}: &a#{i} [#{Enum.map_join(1..10, ", ", fn _ -> prev end)}]"
        end

      repo =
        create_repo_with_workflow(
          """
          on: push
          x:
          #{Enum.join(anchors, "\n")}
          jobs:
            notify:
              runs-on: ubuntu-24.04
              env:
                ALL: *a9
              steps:
                - run: make
          """,
          "notify.yml"
        )

      {micros, findings} =
        :timer.tc(fn -> ResearchExtensions.re001_missing_harden_runner(repo) end)

      # Past kyq's node budget the workflow is refused, as kyq refuses it, and
      # the refusal is a finding: a job it hides is never a silent pass.
      assert [%{severity: :warn, reason: reason}] = findings
      assert reason =~ "NODE_BUDGET_EXCEEDED"
      assert micros < 10_000_000
      File.rm_rf!(repo)
    end

    test "a quoted job key matches its declaration" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            "notify":
              runs-on: ubuntu-24.04
              steps:
                - run: send --pass=${{ secrets.SMTP_PASS }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("SMTP_PASS") :endpoints () :enforcement none))|
      )

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "references match the uppercase declaration, and github_token is the job token" do
      # GitHub matches secret names without regard to case and stores them
      # uppercase, so secrets.smtp_pass is SMTP_PASS and secrets.github_token
      # is the job token.
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: send --pass=${{ secrets.smtp_pass }} --gh=${{ secrets.github_token }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("SMTP_PASS") :endpoints () :enforcement none))|
      )

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "a lowercase job-token reference alone does not count" do
      repo =
        create_repo_with_workflow("""
        jobs:
          labels:
            runs-on: ubuntu-latest
            steps:
              - run: gh label list --token=${{ secrets.github_token }}
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "an egress clause nested in another clause declares nothing and says so" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")
      write_deed(repo, "(meta (egress #{smtp_job(~s|"SMTP_HOST" "SMTP_PASS"|)}))")

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "(egress …) nested inside another clause"
      File.rm_rf!(repo)
    end

    test "the index form and a reference later in an expression are secrets too" do
      # A declaration listing only VISIBLE must not cover a job that also reads
      # secrets['HIDDEN'] or a secret after another operand.
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: send ${{ secrets.VISIBLE }} ${{ secrets['HIDDEN'] }}
                - run: other ${{ vars.URL || secrets["LATER"] }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
      )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ ~s|reads undeclared "HIDDEN" "LATER"|
      File.rm_rf!(repo)
    end

    test "text inside a string literal is not a secret reference" do
      # Only secrets.REAL is read; the literals merely contain secret-like text.
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: echo ${{ 'secrets.NOT_A_REFERENCE' }}
                - run: echo ${{ format('{0} secrets[''X''] {1}', "secrets.Y", secrets.REAL) }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("REAL") :endpoints () :enforcement none))|
      )

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "}} inside a literal does not end the expression" do
      # format() escapes literal braces as {{ }}; HIDDEN is still read.
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: echo ${{ secrets.VISIBLE }}
                - run: echo ${{ format('{{Hello {0}!}}', secrets.HIDDEN) }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
      )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ ~s|reads undeclared "HIDDEN"|
      File.rm_rf!(repo)
    end

    test "an expression folded across lines is read whole" do
      # GitHub folds the scalar and evaluates the whole expression.
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: echo ${{ secrets.VISIBLE }}
                - run: >-
                    echo ${{
                    secrets.HIDDEN
                    }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
      )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ ~s|reads undeclared "HIDDEN"|
      File.rm_rf!(repo)
    end

    test "a finding points at the line of the first secret it reads" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: echo start
                - run: >-
                    echo ${{
                    secrets.FIRST }} ${{ secrets.SECOND }}
          """,
          "notify.yml"
        )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.line == 8
      File.rm_rf!(repo)
    end

    test "drift keeps the job's :high when the credential shares a step with fetched code" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: npm ci && deploy ${{ secrets.VISIBLE }} ${{ secrets.HIDDEN }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
      )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.severity == :high
      assert finding.reason =~ ~s|reads undeclared "HIDDEN"|
      assert finding.reason =~ "in the same step"
      assert finding.detail.exposures != []
      File.rm_rf!(repo)
    end

    test "a secret in the workflow's top-level env is read by every job" do
      repo =
        create_repo_with_workflow(
          """
          on: push
          env:
            HIDDEN: ${{ secrets.HIDDEN }}

          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: deploy ${{ secrets.VISIBLE }}
            other:
              runs-on: ubuntu-24.04
              steps:
                - run: echo hi
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
      )

      assert [drift, other] =
               repo
               |> ResearchExtensions.re001_missing_harden_runner()
               |> Enum.sort_by(& &1.reason)

      assert drift.reason =~ ~s|job `notify`|
      assert drift.reason =~ ~s|reads undeclared "HIDDEN"|
      # A job with no secret of its own still inherits HIDDEN.
      assert other.reason =~ ~s|job `other`|
      assert other.line == 3
      File.rm_rf!(repo)
    end

    test "harden-runner's owner/repo matches without regard to case" do
      repo =
        create_repo_with_workflow("""
        jobs:
          deploy:
            runs-on: ubuntu-24.04
            steps:
              - uses: Step-Security/Harden-Runner@0000000000000000000000000000000000000000
              - run: deploy ${{ secrets.KEY }}
        """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "the secrets context matches without regard to case" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: echo ${{ secrets.VISIBLE }} ${{ SECRETS.HIDDEN }} ${{ Secrets['LATER'] }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
      )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ ~s|reads undeclared "HIDDEN" "LATER"|
      File.rm_rf!(repo)
    end

    test "a key computed at run time cannot be declared" do
      for expr <- [
            "secrets[env.SECRET_NAME]",
            "toJSON(secrets)",
            "secrets.*",
            "fromJSON(SECRETS)[inputs.k]"
          ] do
        repo =
          create_repo_with_workflow(
            """
            jobs:
              notify:
                runs-on: ubuntu-24.04
                steps:
                  - run: echo ${{ secrets.VISIBLE }}
                  - run: echo ${{ #{expr} }}
            """,
            "notify.yml"
          )

        write_deed(
          repo,
          ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
            ~s|:secrets ("VISIBLE") :endpoints () :enforcement none))|
        )

        assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo), expr
        assert finding.reason =~ "computed at run time", expr
        assert finding.reason =~ "does not cover it", expr
        assert finding.line == 6, expr
        File.rm_rf!(repo)
      end
    end

    test "a job reading secrets only by a computed key is still a finding" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: echo ${{ secrets[matrix.key] }}
          """,
          "notify.yml"
        )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "computed at run time"
      refute finding.reason =~ "does not cover it"
      File.rm_rf!(repo)
    end

    test "a property named secrets is not the secrets context" do
      repo =
        create_repo_with_workflow(
          """
          jobs:
            notify:
              runs-on: ubuntu-24.04
              steps:
                - run: echo ${{ github.event.client_payload.secrets.NOT_A_SECRET }}
                - run: echo ${{ steps.my-secrets.outputs.x }} ${{ secrets.REAL }}
          """,
          "notify.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/notify.yml" :job "notify" | <>
          ~s|:secrets ("REAL") :endpoints () :enforcement none))|
      )

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    test "a malformed deed declares nothing" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")

      # No :schema-version and no separators: not a deed, whatever it says.
      File.write!(Path.join(repo, "x_chora.deed"), """
      ;; SPDX-License-Identifier: CC-BY-SA-4.0
      (repo-deed(egress(job :workflow ".github/workflows/notify.yml" :job "notify" :secrets("SMTP_HOST" "SMTP_PASS") :endpoints() :enforcement none)))
      """)

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ "the repo deed declares nothing"
      File.rm_rf!(repo)
    end

    test "the suggested (job …) entry is a valid deed clause" do
      repo = create_repo_with_workflow(@smtp_workflow, "notify.yml")
      write_deed(repo, "")

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      [entry] = Regex.run(~r/\(job .*?:enforcement none\)/, finding.detail.fix)
      write_deed(repo, "(egress #{entry})")

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end

    # ─── declared endpoints against the workflow ──────────────────────

    defp endpoint_findings(steps, endpoints) do
      repo =
        create_repo_with_workflow(
          "on: push\njobs:\n  ship:\n    runs-on: ubuntu-latest\n    steps:\n" <> steps,
          "ship.yml"
        )

      write_deed(
        repo,
        ~s|(egress (job :workflow ".github/workflows/ship.yml" :job "ship" | <>
          ~s|:secrets ("TOKEN") :endpoints (#{endpoints}) :enforcement none))|
      )

      findings = ResearchExtensions.re001_missing_harden_runner(repo)
      File.rm_rf!(repo)
      findings
    end

    test "a host a step names literally must be declared" do
      steps = """
            - run: |
                # docs: https://docs.example.org/upload
                curl -H "t: ${{ secrets.TOKEN }}" https://upload.example.com/v1#frag
      """

      assert [finding] = endpoint_findings(steps, "")
      assert finding.severity == :warn
      assert finding.line == 8
      assert finding.reason =~ ~s|names "upload.example.com:443" literally|
      refute finding.reason =~ "docs.example.org"

      assert endpoint_findings(steps, ~s|"upload.example.com"|) == []
      assert endpoint_findings(steps, ~s|"*.example.com"|) == []
      assert endpoint_findings(steps, ~s|"upload.example.com:443"|) == []
      assert [_] = endpoint_findings(steps, ~s|"upload.example.com:8443"|)
      # A wildcard covers subdomains, not the apex; a secret covers no literal host.
      assert [_] = endpoint_findings(steps, ~s|"*.upload.example.com"|)
      assert [_] = endpoint_findings(steps, ~s|"secret:TOKEN"|)
    end

    # hypatia's own push-email-notify.yml lets out
    # `${{ secrets.SMTP_HOST }}:${{ secrets.SMTP_PORT || '465' }}`: an entry
    # whose host is known only at run time. It is one unit however it is
    # spaced, and it is not a literal host to compare (it crashed the
    # comparison while it was split on its spaces).
    test "an allowed endpoint written with expressions is not a literal host" do
      run = fn allowed, endpoints ->
        workflow =
          Enum.join(
            [
              "on: push",
              "jobs:",
              "  ship:",
              "    runs-on: ubuntu-latest",
              "    steps:",
              "      - uses: step-security/harden-runner@v2",
              "        with:",
              "          egress-policy: block",
              "          allowed-endpoints: >"
            ] ++
              Enum.map(allowed, &("            " <> &1)) ++
              [~S|      - run: send --token "${{ secrets.TOKEN }}"|],
            "\n"
          ) <> "\n"

        repo = create_repo_with_workflow(workflow, "ship.yml")

        write_deed(
          repo,
          ~s|(egress (job :workflow ".github/workflows/ship.yml" :job "ship" | <>
            ~s|:secrets ("SMTP_HOST" "SMTP_PORT" "TOKEN") :endpoints (#{endpoints}) | <>
            ~s|:enforcement harden-runner-block))|
        )

        # The deed must be readable, or a hardened job passes vacuously.
        assert {:ok, _, [_]} = Hypatia.Deed.egress_declarations(repo)
        findings = ResearchExtensions.re001_missing_harden_runner(repo)
        File.rm_rf!(repo)
        findings
      end

      smtp = "${{ secrets.SMTP_HOST }}:${{ secrets.SMTP_PORT || '465' }}"

      assert run.(["github.com:443", smtp], ~s|"secret:SMTP_HOST"|) == []

      # A literal host beside the expression is still held to the deed.
      assert [finding] =
               run.(["github.com:443", "relay.example.net:587", smtp], ~s|"secret:SMTP_HOST"|)

      assert finding.reason =~ "relay.example.net:587"

      assert run.(
               ["github.com:443", "relay.example.net:587", smtp],
               ~s|"secret:SMTP_HOST" "relay.example.net:587"|
             ) == []

      # GitHub's own hosts are outside the comparison on both sides: a deed
      # may list them (hypatia_chora.deed does) and is not held to them.
      assert run.(
               ["github.com:443", smtp],
               ~s|"github.com:443" "api.github.com:443" "secret:SMTP_HOST"|
             ) ==
               []
    end

    test "an ssh remote is port 22, and GitHub's own hosts are never declared" do
      steps = """
            - run: git push git@gitlab.com:o/x.git && git push https://github.com/o/x
              env:
                KEY: ${{ secrets.TOKEN }}
            - uses: actions/checkout@v4
              with:
                repository: https://raw.githubusercontent.com/o/x
      """

      assert [finding] = endpoint_findings(steps, "")
      assert finding.reason =~ ~s|"gitlab.com:22"|
      refute finding.reason =~ "github.com"
      refute finding.reason =~ "githubusercontent"
      assert endpoint_findings(steps, ~s|"gitlab.com:22"|) == []
      assert [_] = endpoint_findings(steps, ~s|"gitlab.com:443"|)
    end

    test "harden-runner's allowed-endpoints and the declaration must agree" do
      steps = fn allowed ->
        """
              - uses: step-security/harden-runner@v2
                with:
                  egress-policy: block
                  allowed-endpoints: >
                    #{allowed}
              - run: deploy --token ${{ secrets.TOKEN }}
        """
      end

      agree = steps.("api.github.com:443 hex.pm:443 repo.hex.pm:443")
      assert endpoint_findings(agree, ~s|"hex.pm" "*.hex.pm:443"|) == []

      assert [finding] = endpoint_findings(agree, ~s|"hex.pm"|)
      assert finding.reason =~ ~s|lets out undeclared "repo.hex.pm:443"|

      assert [finding] = endpoint_findings(steps.("hex.pm:443"), ~s|"hex.pm" "cdn.example.com"|)
      assert finding.reason =~ ~s|does not allow declared "cdn.example.com"|
    end

    # A literal host with an expression for its port is still a literal host;
    # an entry with no port lets out every port, which a deed listing one
    # port does not declare.
    test "allowed-endpoints: a run-time port keeps its host; no port means every port" do
      steps = fn allowed ->
        """
              - uses: step-security/harden-runner@v2
                with:
                  egress-policy: block
                  allowed-endpoints: >
                    #{allowed}
              - run: deploy --token ${{ secrets.TOKEN }}
        """
      end

      dynamic = steps.("smtp.example.com:${{ vars.SMTP_PORT }}")
      assert [finding] = endpoint_findings(dynamic, "")
      assert finding.reason =~ ~s|lets out undeclared "smtp.example.com"|
      assert endpoint_findings(dynamic, ~s|"smtp.example.com:465"|) == []

      portless = steps.("api.example.com")
      assert [finding] = endpoint_findings(portless, ~s|"api.example.com:443"|)
      assert finding.reason =~ ~s|lets out undeclared "api.example.com"|
      assert endpoint_findings(portless, ~s|"api.example.com"|) == []
    end

    test "GitHub's own hosts are whole domains, not suffixes" do
      steps = """
            - run: curl https://notgithubusercontent.com/x https://raw.githubusercontent.com/y
              env:
                K: ${{ secrets.TOKEN }}
      """

      assert [finding] = endpoint_findings(steps, "")
      assert finding.reason =~ ~s|names "notgithubusercontent.com:443" literally|
      refute finding.reason =~ "raw.githubusercontent.com"

      allowed = """
            - uses: step-security/harden-runner@v2
              with:
                egress-policy: block
                allowed-endpoints: notgithubusercontent.com:443 githubusercontent.com:443
            - run: deploy --token ${{ secrets.TOKEN }}
      """

      assert [finding] = endpoint_findings(allowed, "")
      assert finding.reason =~ ~s|lets out undeclared "notgithubusercontent.com:443"|
      refute finding.reason =~ ~s|"githubusercontent.com:443"|
    end

    test "a URL is read with its scheme in any case, and a literal IPv4 host is a host" do
      steps = """
            - run: |
                curl HTTPS://upload.example.com/x https://10.0.0.5/upload
                curl http://127.0.0.1:8080/health http://localhost:9000 http://postgres:5432
              env:
                K: ${{ secrets.TOKEN }}
      """

      assert [finding] = endpoint_findings(steps, "")
      assert finding.reason =~ ~s|"upload.example.com:443"|
      assert finding.reason =~ ~s|"10.0.0.5:443"|
      # Loopback and single-label names (service containers) are not egress.
      refute finding.reason =~ "127.0.0.1"
      refute finding.reason =~ "localhost"
      refute finding.reason =~ "postgres"
      assert endpoint_findings(steps, ~s|"upload.example.com:443" "10.0.0.5:443"|) == []
    end

    test "a URL port written as an expression is unknown, not the scheme's default" do
      steps = """
            - run: curl https://api.example.com:${{ vars.PORT }}/upload
              env:
                K: ${{ secrets.TOKEN }}
      """

      assert endpoint_findings(steps, ~s|"api.example.com:8443"|) == []
      assert [finding] = endpoint_findings(steps, "")
      assert finding.reason =~ ~s|names "api.example.com" literally|
    end

    test "an scp remote has a path, which may start with a digit" do
      remote = fn line ->
        """
              - run: |
                  #{line}
                env:
                  K: ${{ secrets.TOKEN }}
        """
      end

      assert [finding] = endpoint_findings(remote.("git push git@gitlab.com:123team/x.git"), "")
      assert finding.reason =~ ~s|"gitlab.com:22"|

      # Prose with an address and a colon is no remote.
      assert endpoint_findings(remote.(~s|echo "owner@example.com: deployment failed"|), "") ==
               []

      # A URL's userinfo before a port is the URL's, not an scp remote.
      assert endpoint_findings(
               remote.("psql postgres://u:pass@db.example.com:5432/x"),
               ~s|"db.example.com:5432"|
             ) == []
    end

    test "a quoted # opens no shell comment" do
      steps = """
            - run: |
                git commit -m "fix #12" && git push https://gitlab.example.com/o/x.git
                echo 'a # b' https://up.example.com # https://c.example.org
              env:
                K: ${{ secrets.TOKEN }}
      """

      assert [finding] = endpoint_findings(steps, "")
      assert finding.reason =~ ~s|"gitlab.example.com:443"|
      assert finding.reason =~ ~s|"up.example.com:443"|
      refute finding.reason =~ "c.example.org"
    end
  end

  # What is read through an alias is reported at the alias (the old line
  # reader did so); the parsed model reported the anchor.
  describe "re001_missing_harden_runner/1 through an alias" do
    test "a job or a step read through an alias is reported at the alias" do
      job_alias = """
      on: push
      jobs:
        a: &j
          runs-on: ubuntu-latest
          steps:
            - run: npm ci
              env:
                K: ${{ secrets.TOKEN }}
        b: *j
      """

      step_alias = """
      on: push
      jobs:
        a:
          runs-on: ubuntu-latest
          steps:
            - &s
              run: npm ci
              env:
                K: ${{ secrets.TOKEN }}
        b:
          runs-on: ubuntu-latest
          steps:
            - *s
      """

      for {yaml, lines} <- [
            {job_alias, %{"a" => 8, "b" => 9}},
            {step_alias, %{"a" => 9, "b" => 13}}
          ] do
        repo = create_repo_with_workflow(yaml)
        findings = ResearchExtensions.re001_missing_harden_runner(repo)
        File.rm_rf!(repo)

        job_of = fn f -> hd(Regex.run(~r/job `(\w+)`/, f.reason, capture: :all_but_first)) end
        assert Map.new(findings, &{job_of.(&1), &1.line}) == lines

        assert Enum.all?(findings, &(&1.severity == :high))
      end
    end
  end

  # ─── RE002 ──────────────────────────────────────────────────────────

  # The three shapes Codex raised on #975 after it merged, each one a line
  # reader's misreading of YAML. Read from the parsed model they cannot occur.
  describe "re001_missing_harden_runner/1 on the #975 post-merge shapes" do
    @sha "1234567890abcdef1234567890abcdef12345678"

    # A hardened job whose harden-runner step carries `hardener_lines` (step
    # properties, given at the step's own column) and a block declaration.
    defp hardened_repo(hardener_lines) do
      workflow =
        Enum.join(
          [
            "jobs:",
            "  notify:",
            "    runs-on: ubuntu-24.04",
            "    steps:",
            "      - uses: step-security/harden-runner@#{@sha}"
          ] ++
            Enum.map(hardener_lines, &("        " <> &1)) ++
            [~S|      - run: curl -u "$U:${{ secrets.SMTP_PASS }}" https://mail.example.com|],
          "\n"
        ) <> "\n"

      repo = create_repo_with_workflow(workflow, "notify.yml")

      write_deed(
        repo,
        ~s|(egress :policy "standards ADR-009" (job :workflow ".github/workflows/notify.yml" | <>
          ~s|:job "notify" :secrets ("SMTP_PASS") :endpoints ("mail.example.com:443") | <>
          ~s|:enforcement harden-runner-block))|
      )

      repo
    end

    test "a `<<: [*defaults]` merge is not guessed at: the workflow is reported unreadable" do
      repo =
        create_repo_with_workflow(
          """
          x-defaults: &defaults
            runs-on: ubuntu-24.04
            env:
              TOKEN: ${{ secrets.DEPLOY_TOKEN }}
          jobs:
            deploy:
              <<: [*defaults]
              steps:
                - run: ./deploy.sh
          """,
          "deploy.yml"
        )

      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.severity == :warn
      assert finding.line == 7
      assert finding.reason =~ "UNSUPPORTED_MERGE_KEY"
      File.rm_rf!(repo)
    end

    test "text in a block scalar beside the hardener is not its input" do
      repo = hardened_repo(["name: |", "  with:", "    egress-policy: block"])

      # harden-runner runs its default, audit: the block claim is false.
      assert [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.reason =~ ":enforcement harden-runner-block"
      File.rm_rf!(repo)
    end

    test "a folded egress-policy is read as its value" do
      repo =
        hardened_repo([
          "with:",
          "  egress-policy: >-",
          "    block",
          "  allowed-endpoints: mail.example.com:443"
        ])

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end
  end

  describe "re002_harden_runner_audit_mode/1" do
    test "flags harden-runner in audit mode on push trigger" do
      repo =
        create_repo_with_workflow("""
        on:
          push:
            branches: [main]
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - uses: step-security/harden-runner@1234567890abcdef1234567890abcdef12345678
                with:
                  egress-policy: audit
              - run: build
        """)

      findings = ResearchExtensions.re002_harden_runner_audit_mode(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :high
      File.rm_rf!(repo)
    end

    test "passes when egress-policy is block" do
      repo =
        create_repo_with_workflow("""
        on:
          push:
            branches: [main]
        jobs:
          x:
            steps:
              - uses: step-security/harden-runner@1234567890abcdef1234567890abcdef12345678
                with:
                  egress-policy: block
        """)

      assert ResearchExtensions.re002_harden_runner_audit_mode(repo) == []
      File.rm_rf!(repo)
    end

    test "matches the action owner in any case, as RE001 does" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - uses: Step-Security/Harden-Runner@1234567890abcdef1234567890abcdef12345678
                with:
                  egress-policy: audit
        """)

      assert [%{rule: "RE002"}] = ResearchExtensions.re002_harden_runner_audit_mode(repo)
      File.rm_rf!(repo)
    end

    test "a hardener with no egress-policy runs harden-runner's default, audit" do
      repo =
        create_repo_with_workflow("""
        on: {release: {types: [published]}}
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - uses: "step-security/harden-runner@1234567890abcdef1234567890abcdef12345678"
        """)

      assert [%{reason: reason}] = ResearchExtensions.re002_harden_runner_audit_mode(repo)
      assert reason =~ "its default"
      File.rm_rf!(repo)
    end

    test "only a real hardener step and a real push or release trigger count" do
      repo =
        create_repo_with_workflow("""
        on:
          pull_request:
            branches: [main]
        jobs:
          push:
            runs-on: ubuntu-latest
            steps:
              - uses: step-security/harden-runner@1234567890abcdef1234567890abcdef12345678
                with:
                  egress-policy: audit
          release:
            runs-on: ubuntu-latest
            steps:
              - run: |
                  echo "uses: step-security/harden-runner@x"
                  echo "egress-policy: audit"
        """)

      assert ResearchExtensions.re002_harden_runner_audit_mode(repo) == []
      File.rm_rf!(repo)
    end

    # The fixture above is also held back by its pull_request trigger; on push
    # only the step test stands between the text and a finding.
    test "hardener text in a run block on push is not a hardener" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        jobs:
          build:
            runs-on: ubuntu-latest
            steps:
              - run: |
                  echo "uses: step-security/harden-runner@x"
                  echo "egress-policy: audit"
        """)

      assert ResearchExtensions.re002_harden_runner_audit_mode(repo) == []
      File.rm_rf!(repo)
    end

    # A job whose `if:` admits only pull_request never runs on push: its
    # hardener, defaulted or explicit, is not on a protected trigger.
    test "a job gated away from push and release is not on a protected trigger" do
      run = fn gate, policy ->
        repo =
          create_repo_with_workflow("""
          on:
            push:
            pull_request:
          jobs:
            x:
              if: github.event_name == '#{gate}'
              runs-on: ubuntu-latest
              steps:
                - uses: step-security/harden-runner@1234567890abcdef1234567890abcdef12345678
          #{policy}
          """)

        findings = ResearchExtensions.re002_harden_runner_audit_mode(repo)
        File.rm_rf!(repo)
        findings
      end

      explicit = "        with: {egress-policy: audit}"

      assert run.("pull_request", "") == []
      assert run.("pull_request", explicit) == []
      assert [%{severity: :high}] = run.("push", "")
      assert [%{severity: :high}] = run.("push", explicit)
    end

    # The line reader found `egress-policy: audit` in any file; a workflow
    # the KYAML reader refuses must not now pass silently.
    test "an unreadable workflow that names harden-runner is reported" do
      repo =
        create_repo_with_workflow("""
        on: push
        base: &b {k: 1}
        m:
          <<: *b
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - uses: step-security/harden-runner@1234567890abcdef1234567890abcdef12345678
                with:
                  egress-policy: audit
        """)

      assert [finding] = ResearchExtensions.re002_harden_runner_audit_mode(repo)
      assert finding.severity == :warn
      assert finding.line == 4
      assert finding.reason =~ "UNSUPPORTED_MERGE_KEY"
      assert finding.reason =~ "RE002 cannot check"
      # It names no secret, so RE001 has nothing to report.
      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE003 ──────────────────────────────────────────────────────────

  describe "re003_cache_key_poisoning/1" do
    test "flags cache key derived from github.head_ref" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: actions/cache@1234567890abcdef1234567890abcdef12345678
                with:
                  path: ~/.cargo
                  key: cache-${{ github.head_ref }}
        """)

      findings = ResearchExtensions.re003_cache_key_poisoning(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :high
      File.rm_rf!(repo)
    end

    test "passes when cache key uses hashFiles" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: actions/cache@1234567890abcdef1234567890abcdef12345678
                with:
                  path: ~/.cargo
                  key: cache-${{ hashFiles('**/Cargo.lock') }}
        """)

      assert ResearchExtensions.re003_cache_key_poisoning(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE004 ──────────────────────────────────────────────────────────

  describe "re004_container_tag_pin/1" do
    test "flags docker:// uses pinned by tag" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: docker://alpine:3.21
        """)

      findings = ResearchExtensions.re004_container_tag_pin(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :warn
      File.rm_rf!(repo)
    end

    test "accepts docker:// pinned by digest" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: docker://alpine:3.21@sha256:0a4eaa0eecf5f8c050e5bba433f58c052be7587ee8af3e8b3910ef9ab5fbe9f5
        """)

      assert ResearchExtensions.re004_container_tag_pin(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE005 ──────────────────────────────────────────────────────────

  describe "re005_test_swallows_exit/1" do
    test "flags test step with continue-on-error: true" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - name: run tests
                continue-on-error: true
                run: cargo test
        """)

      findings = ResearchExtensions.re005_test_swallows_exit(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :warn
      File.rm_rf!(repo)
    end

    test "flags test step with || true suffix" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - name: run-tests
                run: cargo test || true
        """)

      findings = ResearchExtensions.re005_test_swallows_exit(repo)
      assert length(findings) == 1
      File.rm_rf!(repo)
    end

    test "passes test step with no swallow" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: tests
                run: cargo test
        """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end

    test "ignores commented-out continue-on-error (YAML comment is not a swallow)" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: run tests
                # continue-on-error: true
                run: cargo test
        """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end

    test "ignores || true that only appears in a trailing comment" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: run-tests
                run: cargo test # || true was tried here and removed
        """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end

    test "real || true is not shadowed by stripping (both directions)" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: run-tests
                run: cargo test || true # reviewed swallow
        """)

      findings = ResearchExtensions.re005_test_swallows_exit(repo)
      assert length(findings) == 1
      File.rm_rf!(repo)
    end

    test "honours hypatia:ignore RE005 -- <reason> as a reviewed swallow" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              # hypatia:ignore RE005 -- flaky upstream test harness, reviewed 2026-09-26
              - name: run-tests
                run: cargo test || true
        """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end

    test "ignores || true inside a trap handler (cleanup, not the step's status)" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: Test suite under stress
                run: |
                  set -euo pipefail
                  stress-ng --cpu 4 &
                  STRESS_PID=$!
                  trap 'kill "$STRESS_PID" 2>/dev/null || true' EXIT
                  cargo test
        """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end

    test "a trap does not hide a real swallow elsewhere in the step" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: Test suite under stress
                run: |
                  trap 'kill "$PID" 2>/dev/null || true' EXIT
                  cargo test || true
        """)

      assert [%{rule: "RE005"}] = ResearchExtensions.re005_test_swallows_exit(repo)
      File.rm_rf!(repo)
    end

    test "a double-quoted trap handler with escaped quotes is still skipped" do
      repo =
        create_repo_with_workflow(~S"""
        jobs:
          x:
            steps:
              - name: run tests
                run: |
                  trap "kill \"$PID\" 2>/dev/null || true" EXIT
                  cargo test
        """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end

    test "a swallow after a trap on the same line is still reported" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: run tests
                run: |
                  trap 'kill "$PID" 2>/dev/null || true' EXIT; cargo test || true
        """)

      assert [%{rule: "RE005"}] = ResearchExtensions.re005_test_swallows_exit(repo)
      File.rm_rf!(repo)
    end

    test "ignores non-test steps with continue-on-error" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: upload metrics
                continue-on-error: true
                run: curl -X POST metrics.example.com
        """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE006 ──────────────────────────────────────────────────────────

  describe "re006_composite_unpinned_nested/1" do
    test "flags composite action calling tag-pinned third-party action" do
      repo =
        create_repo_with_composite("""
        name: my-composite
        runs:
          using: composite
          steps:
            - uses: actions/checkout@v4
            - run: echo hi
              shell: bash
        """)

      findings = ResearchExtensions.re006_composite_unpinned_nested(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "RE006"
      File.rm_rf!(repo)
    end

    test "passes composite action with SHA-pinned dependencies" do
      repo =
        create_repo_with_composite("""
        runs:
          using: composite
          steps:
            - uses: actions/checkout@1234567890abcdef1234567890abcdef12345678
        """)

      assert ResearchExtensions.re006_composite_unpinned_nested(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE007 ──────────────────────────────────────────────────────────

  describe "re007_workflow_env_secret_exposure/1" do
    test "flags top-level env: with secret reference" do
      repo =
        create_repo_with_workflow("""
        name: leaky
        on: [push]
        env:
          API_TOKEN: ${{ secrets.API_TOKEN }}
          OTHER: hello
        jobs:
          x:
            steps:
              - run: echo hi
        """)

      findings = ResearchExtensions.re007_workflow_env_secret_exposure(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :warn
      File.rm_rf!(repo)
    end

    test "passes when secrets live in a step env block only" do
      repo =
        create_repo_with_workflow("""
        on: [push]
        env:
          BUILD_PROFILE: release
        jobs:
          x:
            steps:
              - name: deploy
                env:
                  API_TOKEN: ${{ secrets.API_TOKEN }}
                run: deploy
        """)

      assert ResearchExtensions.re007_workflow_env_secret_exposure(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE008 ──────────────────────────────────────────────────────────

  describe "re008_spoofable_bot_gate/1" do
    test "flags github.actor == 'dependabot[bot]' comparison" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            if: github.actor == 'dependabot[bot]'
            steps:
              - run: echo trusted
        """)

      findings = ResearchExtensions.re008_spoofable_bot_gate(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :critical
      File.rm_rf!(repo)
    end

    test "ignores comparison against actor_id" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            if: github.actor_id == '49699333'
            steps:
              - run: echo trusted
        """)

      assert ResearchExtensions.re008_spoofable_bot_gate(repo) == []
      File.rm_rf!(repo)
    end

    test "accepts actor gate ANDed with the PR author (panoply/nextgen-typing)" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            if: github.actor == 'dependabot[bot]' && github.event.pull_request.user.login == 'dependabot[bot]'
            steps:
              - run: echo trusted
        """)

      assert ResearchExtensions.re008_spoofable_bot_gate(repo) == []
      File.rm_rf!(repo)
    end

    test "still flags when the author check is ORed or names another bot" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            if: github.actor == 'dependabot[bot]' || github.event.pull_request.user.login == 'dependabot[bot]'
          y:
            if: github.actor == 'dependabot[bot]' && github.event.pull_request.user.login == 'renovate[bot]'
            steps:
              - run: echo trusted
        """)

      assert length(ResearchExtensions.re008_spoofable_bot_gate(repo)) == 2
      File.rm_rf!(repo)
    end

    test "still flags a negated author comparison (CodeRabbit on #883)" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            if: github.actor == 'dependabot[bot]' && !(github.event.pull_request.user.login == 'dependabot[bot]')
          y:
            if: ${{ !(github.actor == 'dependabot[bot]' && github.event.pull_request.user.login == 'dependabot[bot]') }}
            steps:
              - run: echo trusted
        """)

      assert length(ResearchExtensions.re008_spoofable_bot_gate(repo)) == 2
      File.rm_rf!(repo)
    end

    test "accepts the author conjunct inside an expression wrapper and parentheses" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            if: ${{ github.actor == 'dependabot[bot]' && (github.event.pull_request.user.login == 'dependabot[bot]') }}
            steps:
              - run: echo trusted
        """)

      assert ResearchExtensions.re008_spoofable_bot_gate(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE009 ──────────────────────────────────────────────────────────

  describe "re009_fromjson_secret/1" do
    test "flags fromJSON(secrets.X) pattern" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - name: deploy
                env:
                  AWS_KEY: ${{ fromJSON(secrets.AWS_CREDS).access_key }}
                run: deploy
        """)

      findings = ResearchExtensions.re009_fromjson_secret(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :critical
      File.rm_rf!(repo)
    end

    test "passes direct secrets.X reference" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: deploy --key=${{ secrets.AWS_KEY }}
        """)

      assert ResearchExtensions.re009_fromjson_secret(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE010 ──────────────────────────────────────────────────────────

  describe "re010_workflow_run_artifact_no_provenance/1" do
    test "flags workflow_run that downloads artifact without run-id" do
      repo =
        create_repo_with_workflow("""
        on:
          workflow_run:
            workflows: [Build]
            types: [completed]
        jobs:
          consume:
            runs-on: ubuntu-latest
            steps:
              - uses: actions/download-artifact@1234567890abcdef1234567890abcdef12345678
                with:
                  name: build-output
              - run: ./consume
        """)

      findings = ResearchExtensions.re010_workflow_run_artifact_no_provenance(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :warn
      File.rm_rf!(repo)
    end

    test "passes when run-id constrained to triggering run" do
      repo =
        create_repo_with_workflow("""
        on:
          workflow_run:
            workflows: [Build]
            types: [completed]
        jobs:
          consume:
            steps:
              - uses: actions/download-artifact@1234567890abcdef1234567890abcdef12345678
                with:
                  name: build-output
                  run-id: ${{ github.event.workflow_run.id }}
                  github-token: ${{ secrets.GITHUB_TOKEN }}
        """)

      assert ResearchExtensions.re010_workflow_run_artifact_no_provenance(repo) == []
      File.rm_rf!(repo)
    end

    test "passes when provenance is verified via gh attestation verify" do
      repo =
        create_repo_with_workflow("""
        on:
          workflow_run:
            workflows: [Build]
            types: [completed]
        jobs:
          consume:
            steps:
              - uses: actions/download-artifact@1234567890abcdef1234567890abcdef12345678
                with:
                  name: build-output
              - run: gh attestation verify build-output --repo ${{ github.repository }}
        """)

      assert ResearchExtensions.re010_workflow_run_artifact_no_provenance(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── scan/1 facade ──────────────────────────────────────────────────

  describe "scan/1" do
    test "returns the standard shape on a clean workflow" do
      repo =
        create_repo_with_workflow("""
        name: clean
        on: [pull_request]
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - uses: actions/checkout@1234567890abcdef1234567890abcdef12345678
              - name: run-tests
                run: cargo test
        """)

      result = ResearchExtensions.scan(repo)
      assert is_map(result)
      assert Map.has_key?(result, :findings)
      assert Map.has_key?(result, :total)
      assert Map.has_key?(result, :by_severity)
      assert Map.has_key?(result, :dispatch)
      assert result.total == 0
      File.rm_rf!(repo)
    end

    test "returns total == 0 on a repo without .github/workflows/" do
      repo = Path.join(@tmp_dir, "re_empty_#{System.unique_integer([:positive])}")
      File.mkdir_p!(repo)
      assert ResearchExtensions.scan(repo).total == 0
      File.rm_rf!(repo)
    end

    test "dispatch_recommendations route to sustainabot at severity-calibrated confidence" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            if: github.actor == 'dependabot[bot]'
            steps:
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
        """)

      result = ResearchExtensions.scan(repo)
      assert result.total > 0

      Enum.each(result.dispatch, fn rec ->
        assert rec.bot == :sustainabot

        assert rec.confidence in [0.92, 0.85, 0.75, 0.60, 0.50],
               "confidence #{rec.confidence} not in calibrated set"
      end)

      crit = Enum.find(result.dispatch, fn rec -> rec.rule == "RE008" end)
      assert crit != nil
      assert crit.confidence == 0.92

      File.rm_rf!(repo)
    end
  end
end
