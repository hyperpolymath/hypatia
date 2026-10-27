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
          single-quoted:
            runs-on: ubuntu-latest
            steps:
              - uses: 'step-security/harden-runner@main'
              - run: deploy --token=${{ secrets.SINGLE_QUOTED_KEY }}
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
        exposed:
          runs-on: ubuntu-latest
          steps:
            - run: deploy --token=${{ secrets.TWO }}
      """)

      [finding] = ResearchExtensions.re001_missing_harden_runner(repo)
      assert finding.line == 10
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

      write_deed(repo, "(egress #{smtp_job(~s|"SMTP_HOST"|)})")

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
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
  end

  # ─── RE002 ──────────────────────────────────────────────────────────

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
