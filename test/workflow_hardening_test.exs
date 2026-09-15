# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.WorkflowHardeningTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.WorkflowHardening

  @tmp_dir System.tmp_dir!()

  defp create_repo_with_workflow(yaml_content, filename \\ "test.yml") do
    repo = Path.join(@tmp_dir, "wh_test_#{System.unique_integer([:positive])}")
    wf = Path.join([repo, ".github", "workflows"])
    File.mkdir_p!(wf)
    File.write!(Path.join(wf, filename), yaml_content)
    repo
  end

  defp write_actions_lock(
         repo,
         workflow_path,
         commit \\ "3d3c42e5aac5ba805825da76410c181273ba90b1"
       ) do
    content = """
    version: 'v0.0.2'
    workflows:
        '#{workflow_path}':
            - 'actions/checkout@v7.0.1'
    dependencies:
        'actions/checkout@v7.0.1':
            ref: 'v7.0.1'
            commit: 'sha1-#{commit}'
            owner_id: 44036562
            repo_id: 197814629
    """

    File.write!(Path.join([repo, ".github", "workflows", "actions.lock"]), content)
  end

  setup context do
    on_exit(fn ->
      if context[:repo] do
        File.rm_rf!(context[:repo])
      end
    end)

    :ok
  end

  # ─── WH001 ──────────────────────────────────────────────────────────

  describe "wh001_template_injection/1" do
    test "flags github.event.pull_request.title in run:" do
      repo =
        create_repo_with_workflow("""
        name: Bad
        on: [pull_request]
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - run: echo "Title is ${{ github.event.pull_request.title }}"
        """)

      findings = WorkflowHardening.wh001_template_injection(repo)
      assert length(findings) == 1
      f = hd(findings)
      assert f.rule == "WH001"
      assert f.severity == :critical
      File.rm_rf!(repo)
    end

    test "ignores safe ${{ secrets.X }} references in run:" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: deploy --token=${{ secrets.DEPLOY_KEY }}
        """)

      assert WorkflowHardening.wh001_template_injection(repo) == []
      File.rm_rf!(repo)
    end

    # ── Boundary + membership regression (2026-09-15) ────────────────
    #
    # `@untrusted_contexts` entries are joined into one alternation with
    # `Regex.escape` and matched as PREFIXES. Before the fix the pattern
    # had no trailing boundary, so every entry also matched any LONGER
    # identifier sharing its text — and bare `github.ref` (which was also
    # wrongly a member) therefore matched `github.ref_name`.
    #
    # Measured on 523 local checkouts: 450 WH001 criticals, of which 449
    # were this alias and exactly 1 was real. Each test below pins one
    # alternative class; the first is the mutant-kill for the boundary.

    # The boundary invariant, asserted over the REAL list rather than a copy.
    # Every entry is joined into one alternation with `Regex.escape` and
    # matched as a PREFIX, so without a trailing boundary each member also
    # matches any longer identifier sharing its text. That is exactly how
    # `github.ref` came to match `github.ref_name`. This test keeps holding
    # as the list changes, which a fixture naming one pair cannot do.
    test "no untrusted context matches a LONGER identifier (boundary invariant)" do
      for ctx <- WorkflowHardening.untrusted_contexts() do
        repo =
          create_repo_with_workflow("""
          jobs:
            x:
              steps:
                - run: echo "${{ #{ctx}_name }}"
          """)

        assert WorkflowHardening.wh001_template_injection(repo) == [],
               "#{ctx} matched the longer identifier #{ctx}_name — " <>
                 "the alternation has lost its trailing boundary"

        File.rm_rf!(repo)
      end
    end

    test "does NOT flag github.ref_name — the boundary mutant-kill" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: jq --arg b "${{ github.ref_name }}" -n '$ARGS.named'
        """)

      assert WorkflowHardening.wh001_template_injection(repo) == []
      File.rm_rf!(repo)
    end

    test "does NOT flag bare github.ref — not attacker-controlled" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: echo "ref=${{ github.ref }} sha=${{ github.sha }}"
        """)

      assert WorkflowHardening.wh001_template_injection(repo) == []
      File.rm_rf!(repo)
    end

    test "still flags github.head_ref after the boundary is added" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: echo "branch ${{ github.head_ref }}"
        """)

      assert [%{rule: "WH001", severity: :critical}] =
               WorkflowHardening.wh001_template_injection(repo)

      File.rm_rf!(repo)
    end

    test "still flags a SUBPATH of a prefix entry (github.event.commits)" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: echo "${{ github.event.commits[0].message }}"
        """)

      assert [%{rule: "WH001", severity: :critical}] =
               WorkflowHardening.wh001_template_injection(repo)

      File.rm_rf!(repo)
    end

    test "flags the real estate hit: pull_request_target + head.ref in run:" do
      repo =
        create_repo_with_workflow(
          """
          on:
            pull_request_target:
              types: [opened]
          jobs:
            x:
              steps:
                - run: |
                    PR_BRANCH="${{ github.event.pull_request.head.ref }}"
                    git fetch pr-fork "$PR_BRANCH"
          """,
          "security-gate-pr-target.yml"
        )

      assert [%{rule: "WH001", severity: :critical}] =
               WorkflowHardening.wh001_template_injection(repo)

      File.rm_rf!(repo)
    end
  end

  # ─── WH002 ──────────────────────────────────────────────────────────

  describe "wh002_excessive_permissions/1" do
    test "flags workflows without a permissions block" do
      repo =
        create_repo_with_workflow("""
        name: NoPerm
        on: [push]
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - run: echo hi
        """)

      findings = WorkflowHardening.wh002_excessive_permissions(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :warn
      File.rm_rf!(repo)
    end

    test "flags permissions: write-all as high" do
      repo =
        create_repo_with_workflow("""
        permissions: write-all
        jobs:
          x:
            runs-on: ubuntu-latest
            steps:
              - run: echo hi
        """)

      findings = WorkflowHardening.wh002_excessive_permissions(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :high
      File.rm_rf!(repo)
    end

    test "passes when permissions block exists" do
      repo =
        create_repo_with_workflow("""
        permissions:
          contents: read
        jobs:
          x:
            steps:
              - run: echo hi
        """)

      assert WorkflowHardening.wh002_excessive_permissions(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── WH004 ──────────────────────────────────────────────────────────

  describe "wh004_unpinned_uses/1" do
    test "flags @v4 tag-pinned action" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: actions/checkout@v4
              - uses: foo/bar@main
        """)

      findings = WorkflowHardening.wh004_unpinned_uses(repo)
      assert length(findings) == 2
      File.rm_rf!(repo)
    end

    test "accepts only a valid lock entry associated with the current workflow" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: actions/checkout@v7.0.1
        """)

      write_actions_lock(repo, ".github/workflows/test.yml")
      assert WorkflowHardening.wh004_unpinned_uses(repo) == []

      write_actions_lock(repo, ".github/workflows/other.yml")
      assert [%{rule: "WH004"}] = WorkflowHardening.wh004_unpinned_uses(repo)
      File.rm_rf!(repo)
    end

    test "malformed lock commit fails closed" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: actions/checkout@v7.0.1
        """)

      write_actions_lock(repo, ".github/workflows/test.yml", "short")

      assert [
               %{
                 rule: "invalid_actions_lock",
                 file: ".github/workflows/actions.lock",
                 severity: :high,
                 action: :regenerate
               }
             ] = WorkflowHardening.wh004_unpinned_uses(repo)

      File.rm_rf!(repo)
    end

    test "invalid lock is reported even when no workflow has a symbolic action" do
      repo = create_repo_with_workflow("jobs: {}\n")
      write_actions_lock(repo, ".github/workflows/test.yml", "short")

      assert [%{rule: "invalid_actions_lock", severity: :high}] =
               WorkflowHardening.wh004_unpinned_uses(repo)

      File.rm_rf!(repo)
    end

    test "accepts 40-char SHA-pinned actions" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: actions/checkout@ea165f8d65b6e75b540449e92b4886f43607fa02
        """)

      assert WorkflowHardening.wh004_unpinned_uses(repo) == []
      File.rm_rf!(repo)
    end

    test "ignores local actions and docker refs" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - uses: ./local-action
              - uses: docker://alpine:3.21
        """)

      assert WorkflowHardening.wh004_unpinned_uses(repo) == []
      File.rm_rf!(repo)
    end

    test "multi-byte chars before the match do not shift the slug slice" do
      # Regression: `Regex.scan(…, return: :index)` yields BYTE offsets,
      # but the slug was extracted with String.slice/3 (grapheme-counted).
      # An em-dash in an earlier comment shifted every later slice,
      # mangling slugs ("tions/checkout@…") and — worse — breaking the
      # 40-hex pinned exemption so SHA-pinned actions were reported as
      # unpinned (observed on verisimdb#123 / hypatia#458 scan comments).
      content = """
      # security — hardening notes ✓
      jobs:
        x:
          steps:
            - uses: actions/checkout@ea165f8d65b6e75b540449e92b4886f43607fa02
            - uses: erlef/setup-beam@v1
      """

      findings = WorkflowHardening.wh004_scan_content("ci.yml", content)

      assert [finding] = findings
      assert finding.detail.uses == "erlef/setup-beam@v1"
      assert finding.detail.line == 6
    end
  end

  # ─── WH005 ──────────────────────────────────────────────────────────

  describe "wh005_hardcoded_credentials/1" do
    test "flags literal password" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            services:
              postgres:
                image: postgres:14
                credentials:
                  username: admin
                  password: super-secret-literal
        """)

      findings = WorkflowHardening.wh005_hardcoded_credentials(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :critical
      File.rm_rf!(repo)
    end

    test "accepts secrets.* references" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            services:
              postgres:
                credentials:
                  password: ${{ secrets.PG_PASSWORD }}
        """)

      assert WorkflowHardening.wh005_hardcoded_credentials(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── WH009 ──────────────────────────────────────────────────────────

  describe "wh009_overprovisioned_secrets/1" do
    test "flags toJSON(secrets) usage" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: echo "${{ toJSON(secrets) }}" >> /tmp/dump
        """)

      findings = WorkflowHardening.wh009_overprovisioned_secrets(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :critical
      File.rm_rf!(repo)
    end
  end

  # ─── WH010 ──────────────────────────────────────────────────────────

  describe "wh010_deprecated_workflow_commands/1" do
    test "flags ::set-output::" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: echo "::set-output name=foo::bar"
        """)

      findings = WorkflowHardening.wh010_deprecated_workflow_commands(repo)
      assert length(findings) == 1
      File.rm_rf!(repo)
    end
  end

  # ─── WH011 ──────────────────────────────────────────────────────────

  describe "wh011_curl_pipe_shell/1" do
    test "flags curl | sh pattern" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: curl -fsSL https://example.com/install | sh
        """)

      findings = WorkflowHardening.wh011_curl_pipe_shell(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :high
      File.rm_rf!(repo)
    end

    test "ignores curl without pipe-to-shell" do
      repo =
        create_repo_with_workflow("""
        jobs:
          x:
            steps:
              - run: curl -fsSL -o /tmp/file https://example.com/file
        """)

      assert WorkflowHardening.wh011_curl_pipe_shell(repo) == []
      File.rm_rf!(repo)
    end
  end

  # ─── scan/1 facade ──────────────────────────────────────────────────

  describe "scan/1" do
    test "returns the standard shape on a clean workflow" do
      repo =
        create_repo_with_workflow("""
        permissions:
          contents: read

        concurrency:
          group: ${{ github.workflow }}-${{ github.ref }}
          cancel-in-progress: true

        on: [pull_request]
        jobs:
          x:
            runs-on: ubuntu-latest
            timeout-minutes: 5
            steps:
              - uses: actions/checkout@ea165f8d65b6e75b540449e92b4886f43607fa02
        """)

      result = WorkflowHardening.scan(repo)
      assert is_map(result)
      assert Map.has_key?(result, :findings)
      assert Map.has_key?(result, :total)
      assert Map.has_key?(result, :by_severity)
      assert Map.has_key?(result, :dispatch)
      # Clean workflow: zero findings.
      assert result.total == 0
      File.rm_rf!(repo)
    end

    test "returns [] on a repo without .github/workflows/" do
      repo = Path.join(@tmp_dir, "wh_empty_#{System.unique_integer([:positive])}")
      File.mkdir_p!(repo)
      assert WorkflowHardening.scan(repo).total == 0
      File.rm_rf!(repo)
    end
  end

  # ── WH002 / WH013: the contents:write pair ────────────────────────────────
  #
  # These two rules are inverses and must be tested together. WH002 asks "is
  # this grant wider than the workflow needs?"; WH013 asks "is this workflow
  # starved of a grant it uses?". The failure that put 25 repos into a broken
  # state was WH002 answering the first question WITHOUT the second, so its
  # remediation removed a capability the workflow depended on.

  describe "wh002_excessive_permissions/1 — three probes, not one" do
    test "no write performed: narrowing is real hardening, stays :high" do
      repo =
        create_repo_with_workflow("""
        name: CI
        permissions:
          contents: write
        jobs:
          test:
            runs-on: ubuntu-latest
            steps:
              - run: mix test
        """)

      [f] = WorkflowHardening.wh002_excessive_permissions(repo)
      assert f.rule == "WH002"
      assert f.severity == :high
      File.rm_rf!(repo)
    end

    test "writes and NO job-level block: downgraded to :warn with a two-step recipe" do
      repo =
        create_repo_with_workflow("""
        name: Release
        permissions:
          contents: write
        jobs:
          publish:
            runs-on: ubuntu-latest
            steps:
              - run: git push origin HEAD
        """)

      [f] = WorkflowHardening.wh002_excessive_permissions(repo)
      assert f.severity == :warn
      assert f.fix_recipe == "add-job-level-contents-write-then-narrow-workflow-level"
      File.rm_rf!(repo)
    end

    test "writes WITH a job-level block: :warn, verify the writing job first" do
      repo =
        create_repo_with_workflow("""
        name: Release
        permissions:
          contents: write
        jobs:
          publish:
            runs-on: ubuntu-latest
            permissions:
              contents: write
            steps:
              - run: git push origin HEAD
        """)

      [f] = WorkflowHardening.wh002_excessive_permissions(repo)
      assert f.severity == :warn
      # This branch intentionally carries no fix_recipe: the safe action is a
      # human check of WHICH job writes, not a mechanical rewrite.
      refute Map.has_key?(f, :fix_recipe)
      assert f.reason =~ "verify the WRITING job"
      File.rm_rf!(repo)
    end

    test "performs_contents_write?/1 sees the action forms, not just git push" do
      assert WorkflowHardening.performs_contents_write?(
               "      - uses: softprops/action-gh-release@v2"
             )

      assert WorkflowHardening.performs_contents_write?("        run: gh release create v1")
      assert WorkflowHardening.performs_contents_write?("        run: gh pr merge --auto")
      refute WorkflowHardening.performs_contents_write?("        run: gh pr view 12")
    end

    test "job_level_permissions?/1 distinguishes indented from column-0" do
      refute WorkflowHardening.job_level_permissions?("permissions:\n  contents: read\n")
      assert WorkflowHardening.job_level_permissions?("jobs:\n  a:\n    permissions:\n")
    end
  end

  describe "wh013_permission_starved_write/1 — four arms" do
    test "arm 1: a correct workflow (write + grant) is silent" do
      repo =
        create_repo_with_workflow("""
        name: Release
        permissions:
          contents: read
        jobs:
          publish:
            runs-on: ubuntu-latest
            permissions:
              contents: write
            steps:
              - run: git push origin HEAD
        """)

      assert [] = WorkflowHardening.wh013_permission_starved_write(repo)
      File.rm_rf!(repo)
    end

    test "arm 2: starved AND masked — green forever, so :high" do
      repo =
        create_repo_with_workflow("""
        name: Steward
        permissions:
          contents: read
        jobs:
          record:
            runs-on: ubuntu-latest
            steps:
              - run: git push origin HEAD 2>/dev/null || echo "::warning::could not push"
        """)

      [f] = WorkflowHardening.wh013_permission_starved_write(repo)
      assert f.rule == "WH013"
      assert f.severity == :high
      assert f.fix_recipe == "grant-contents-write-to-writing-job-and-unmask"
      File.rm_rf!(repo)
    end

    test "arm 3: restoring the grant clears the finding" do
      repo =
        create_repo_with_workflow("""
        name: Steward
        permissions:
          contents: read
        jobs:
          record:
            runs-on: ubuntu-latest
            permissions:
              contents: write
            steps:
              - run: git push origin HEAD 2>/dev/null || echo "::warning::could not push"
        """)

      assert [] = WorkflowHardening.wh013_permission_starved_write(repo)
      File.rm_rf!(repo)
    end

    test "arm 4: starved but UNMASKED still fires — the job will simply fail" do
      repo =
        create_repo_with_workflow("""
        name: Steward
        permissions:
          contents: read
        jobs:
          record:
            runs-on: ubuntu-latest
            steps:
              - run: git push origin HEAD
        """)

      [f] = WorkflowHardening.wh013_permission_starved_write(repo)
      assert f.severity == :high
      assert f.fix_recipe == "grant-contents-write-to-writing-job"
      File.rm_rf!(repo)
    end

    test "a workflow that performs no write is never starved" do
      repo =
        create_repo_with_workflow("""
        name: CI
        permissions:
          contents: read
        jobs:
          test:
            runs-on: ubuntu-latest
            steps:
              - run: mix test
        """)

      assert [] = WorkflowHardening.wh013_permission_starved_write(repo)
      File.rm_rf!(repo)
    end

    test "write-all counts as a grant" do
      repo =
        create_repo_with_workflow("""
        name: Release
        permissions: write-all
        jobs:
          publish:
            steps:
              - run: git push origin HEAD
        """)

      assert [] = WorkflowHardening.wh013_permission_starved_write(repo)
      File.rm_rf!(repo)
    end

    test "masked_write?/1 recognises the three masks and nothing else" do
      assert WorkflowHardening.masked_write?("  run: git push origin HEAD 2>/dev/null\n")
      assert WorkflowHardening.masked_write?("  run: git commit -m x || true\n")
      assert WorkflowHardening.masked_write?("  run: gh pr create -t x || echo none\n")
      refute WorkflowHardening.masked_write?("  run: git push origin HEAD\n")
    end
  end

  # ─── WH014 ──────────────────────────────────────────────────────────
  #
  # Regression floor for the alert-DELETION shape measured 2026-09-14 on
  # hyperpolymath/academic-workflow-suite: a masked scanner plus an
  # unconditional SARIF upload auto-closed 84 real code-scanning alerts
  # while every run reported success. The arms below mirror the four-arm
  # planted-positive control run against the real files.

  describe "wh014_masked_scanner_upload/1" do
    test "fires on mask + upload with no findings assertion (the measured shape)" do
      repo =
        create_repo_with_workflow("""
        name: Scan
        on: [push]
        jobs:
          scan:
            runs-on: ubuntu-latest
            steps:
              - run: |
                  hypatia scan . --exit-zero > hypatia-findings.json || true
                  COUNT=$(jq '. | length' hypatia-findings.json 2>/dev/null || echo 0)
                  node write-sarif.cjs
              - uses: github/codeql-action/upload-sarif@v4.32.6
                with:
                  sarif_file: hypatia.sarif
                  category: hypatia
        """)

      assert [finding] = WorkflowHardening.wh014_masked_scanner_upload(repo)
      assert finding.rule == "WH014"
      assert finding.severity == :high
      assert finding.fix_recipe == "assert-findings-before-sarif-upload"
      File.rm_rf!(repo)
    end

    test "does not fire when the findings artefact is asserted non-empty" do
      repo =
        create_repo_with_workflow("""
        name: Scan
        on: [push]
        jobs:
          scan:
            runs-on: ubuntu-latest
            steps:
              - run: |
                  hypatia scan . --exit-zero > hypatia-findings.json || true
                  jq -e 'type == "array" and length > 0' hypatia-findings.json
              - uses: github/codeql-action/upload-sarif@v4.32.6
                with:
                  sarif_file: hypatia.sarif
        """)

      assert [] = WorkflowHardening.wh014_masked_scanner_upload(repo)
      File.rm_rf!(repo)
    end

    test "does not fire once the mask is removed" do
      repo =
        create_repo_with_workflow("""
        name: Scan
        on: [push]
        jobs:
          scan:
            runs-on: ubuntu-latest
            steps:
              - run: |
                  set -euo pipefail
                  hypatia scan . --exit-zero > hypatia-findings.json
                  node write-sarif.cjs
              - uses: github/codeql-action/upload-sarif@v4.32.6
                with:
                  sarif_file: hypatia.sarif
        """)

      assert [] = WorkflowHardening.wh014_masked_scanner_upload(repo)
      File.rm_rf!(repo)
    end

    test "does not fire on a masked scanner that never uploads SARIF" do
      # Masking without an upload cannot delete an alert: GitHub only
      # reconciles against an analysis that was actually submitted.
      repo =
        create_repo_with_workflow("""
        name: Advisory
        on: [push]
        jobs:
          scan:
            runs-on: ubuntu-latest
            steps:
              - run: hypatia scan . --exit-zero > hypatia-findings.json || true
        """)

      assert [] = WorkflowHardening.wh014_masked_scanner_upload(repo)
      File.rm_rf!(repo)
    end

    test "fires on the `|| echo 0` count mask alone" do
      repo =
        create_repo_with_workflow("""
        name: Scan
        on: [push]
        jobs:
          scan:
            runs-on: ubuntu-latest
            steps:
              - run: |
                  COUNT=$(jq '. | length' findings.json 2>/dev/null || echo 0)
              - uses: github/codeql-action/upload-sarif@v4.32.6
                with:
                  sarif_file: out.sarif
        """)

      assert [%{rule: "WH014"}] = WorkflowHardening.wh014_masked_scanner_upload(repo)
      File.rm_rf!(repo)
    end
  end
end
