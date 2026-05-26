# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensionsTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ResearchExtensions

  @tmp_dir System.tmp_dir!()

  defp create_repo_with_workflow(yaml_content, filename \\ "test.yml") do
    repo = Path.join(@tmp_dir, "re_test_#{System.unique_integer([:positive])}")
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
      repo = create_repo_with_workflow("""
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
      File.rm_rf!(repo)
    end

    test "passes when harden-runner is installed" do
      repo = create_repo_with_workflow("""
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

    test "passes when no secrets are referenced" do
      repo = create_repo_with_workflow("""
      jobs:
        x:
          steps:
            - run: echo hi
      """)

      assert ResearchExtensions.re001_missing_harden_runner(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── RE002 ──────────────────────────────────────────────────────────

  describe "re002_harden_runner_audit_mode/1" do
    test "flags harden-runner in audit mode on push trigger" do
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
      jobs:
        x:
          steps:
            - name: tests
              run: cargo test
      """)

      assert ResearchExtensions.re005_test_swallows_exit(repo) == []
      File.rm_rf!(repo)
    end

    test "ignores non-test steps with continue-on-error" do
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_composite("""
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
      repo = create_repo_with_composite("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
      jobs:
        x:
          if: github.actor_id == '49699333'
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
      repo = create_repo_with_workflow("""
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
