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
end
