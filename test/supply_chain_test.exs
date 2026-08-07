# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.SupplyChainTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.SupplyChain

  @tmp_dir System.tmp_dir!()

  defp create_repo do
    repo = Path.join(@tmp_dir, "sc_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(repo)
    repo
  end

  defp add_workflow(repo, filename, content) do
    wf = Path.join([repo, ".github", "workflows"])
    File.mkdir_p!(wf)
    File.write!(Path.join(wf, filename), content)
  end

  setup do
    repo = create_repo()
    on_exit(fn -> File.rm_rf!(repo) end)
    {:ok, repo: repo}
  end

  # ─── SC001 ──────────────────────────────────────────────────────────

  describe "sc001_workflows_not_in_codeowners/1" do
    test "flags repo without CODEOWNERS", %{repo: repo} do
      findings = SupplyChain.sc001_workflows_not_in_codeowners(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "SC001"
    end

    test "flags CODEOWNERS without /.github/workflows/ line", %{repo: repo} do
      File.write!(Path.join(repo, "CODEOWNERS"), "/src/ @team-platform\n")
      findings = SupplyChain.sc001_workflows_not_in_codeowners(repo)
      assert length(findings) == 1
    end

    test "passes when CODEOWNERS covers workflows", %{repo: repo} do
      File.write!(
        Path.join(repo, "CODEOWNERS"),
        "/src/ @x\n/.github/workflows/ @y\n"
      )

      assert SupplyChain.sc001_workflows_not_in_codeowners(repo) == []
    end
  end

  # ─── SC002 ──────────────────────────────────────────────────────────

  describe "sc002_dependabot_missing/1" do
    test "flags missing dependabot.yml", %{repo: repo} do
      findings = SupplyChain.sc002_dependabot_missing(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :warn
    end

    test "flags dependabot.yml without github-actions ecosystem", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: npm
          directory: /
          schedule:
            interval: weekly
      """)

      findings = SupplyChain.sc002_dependabot_missing(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :info
    end

    test "passes with github-actions ecosystem present", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: "github-actions"
          directory: /
          schedule:
            interval: weekly
      """)

      assert SupplyChain.sc002_dependabot_missing(repo) == []
    end
  end

  # ─── SC005 ──────────────────────────────────────────────────────────

  describe "sc005_pull_request_target_present/1" do
    test "flags pull_request_target in `on:`", %{repo: repo} do
      add_workflow(repo, "x.yml", """
      on: [pull_request_target]
      jobs:
        x:
          runs-on: ubuntu-latest
          steps:
            - run: echo hi
      """)

      findings = SupplyChain.sc005_pull_request_target_present(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :high
    end

    test "passes plain pull_request", %{repo: repo} do
      add_workflow(repo, "x.yml", """
      on: [pull_request]
      jobs:
        x:
          steps:
            - run: echo hi
      """)

      assert SupplyChain.sc005_pull_request_target_present(repo) == []
    end
  end

  # ─── SC009 ──────────────────────────────────────────────────────────

  describe "sc009_security_md_missing/1" do
    test "flags repo without SECURITY.md", %{repo: repo} do
      findings = SupplyChain.sc009_security_md_missing(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "SC009"
    end

    test "passes when SECURITY.md exists at repo root", %{repo: repo} do
      File.write!(Path.join(repo, "SECURITY.md"), "# Security\ncontact: sec@example.com\n")
      assert SupplyChain.sc009_security_md_missing(repo) == []
    end

    test "passes when .github/SECURITY.md exists", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))
      File.write!(Path.join([repo, ".github", "SECURITY.md"]), "Contact: ...")
      assert SupplyChain.sc009_security_md_missing(repo) == []
    end
  end

  # ─── SC006 ──────────────────────────────────────────────────────────

  describe "sc006_release_without_sbom/1" do
    test "flags release workflow without SBOM action", %{repo: repo} do
      add_workflow(repo, "release.yml", """
      on:
        push:
          tags: ['v*']
      jobs:
        publish:
          steps:
            - uses: softprops/action-gh-release@v1
      """)

      findings = SupplyChain.sc006_release_without_sbom(repo)
      assert length(findings) == 1
      assert hd(findings).severity == :warn
    end

    test "passes when SBOM tool present", %{repo: repo} do
      add_workflow(repo, "release.yml", """
      on: { push: { tags: ['v*'] } }
      jobs:
        publish:
          steps:
            - uses: anchore/sbom-action@abc
            - uses: softprops/action-gh-release@v1
      """)

      assert SupplyChain.sc006_release_without_sbom(repo) == []
    end
  end

  # ─── SC012 ─────────────────────────────────────────────────────────────

  describe "sc012_dependabot_pr_fanout/1" do
    test "flags repeated ecosystem blocks and reports projected capacity", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      blocks =
        1..11
        |> Enum.map_join("\n", fn n ->
          """
            - package-ecosystem: "cargo"
              directory: "/bot-#{n}"
              schedule:
                interval: weekly
          """
        end)

      File.write!(
        Path.join([repo, ".github", "dependabot.yml"]),
        "version: 2\nupdates:\n#{blocks}"
      )

      assert [finding] = SupplyChain.sc012_dependabot_pr_fanout(repo)
      assert finding.rule == "SC012"
      assert finding.severity == :high
      assert finding.detail.update_blocks == 11
      assert finding.detail.directories == 11
      assert finding.detail.projected_open_prs == 55
    end

    test "passes grouped multi-directory configuration with one global limit", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      File.write!(Path.join([repo, ".github", "dependabot.yaml"]), """
      version: 2
      updates:
        - package-ecosystem: "cargo"
          directories:
            - "/bot-a"
            - "/bot-b"
            - "/bot-c"
          schedule:
            interval: weekly
          open-pull-requests-limit: 3
          groups:
            monorepo-dependencies:
              group-by: dependency-name
      """)

      assert SupplyChain.sc012_dependabot_pr_fanout(repo) == []
    end

    test "flags ungrouped multi-directory configuration", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: cargo
          directories:
            - /one
            - /two
          schedule:
            interval: weekly
          open-pull-requests-limit: 3
      """)

      assert [finding] = SupplyChain.sc012_dependabot_pr_fanout(repo)
      assert finding.severity == :warn
      assert finding.detail.projected_open_prs == 3
      refute finding.detail.grouped_by_dependency
    end

    test "flags one ecosystem with an excessive explicit limit", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: bundler
          directory: /
          schedule:
            interval: weekly
          open-pull-requests-limit: 99
      """)

      assert [finding] = SupplyChain.sc012_dependabot_pr_fanout(repo)
      assert finding.severity == :high
      assert finding.detail.ecosystem == "bundler"
      assert finding.detail.projected_open_prs == 99
    end

    test "flags excessive repository-wide capacity across distinct ecosystems", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      blocks =
        ~w(cargo npm pip mix)
        |> Enum.map_join("\n", fn ecosystem ->
          """
            - package-ecosystem: #{ecosystem}
              directory: /
              schedule:
                interval: weekly
              open-pull-requests-limit: 5
          """
        end)

      File.write!(
        Path.join([repo, ".github", "dependabot.yml"]),
        "version: 2\nupdates:\n#{blocks}"
      )

      assert [finding] = SupplyChain.sc012_dependabot_pr_fanout(repo)
      assert finding.severity == :warn
      assert finding.detail.ecosystem == "all"
      assert finding.detail.update_blocks == 4
      assert finding.detail.projected_open_prs == 20
    end
  end

  # ─── SC013 ─────────────────────────────────────────────────────────────

  describe "sc013_dependabot_inbox_amplifier/1" do
    test "flags explicit Dependabot reviewers and assignees", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: npm
          directory: /
          schedule:
            interval: weekly
          reviewers: [hyperpolymath]
          assignees:
            - hyperpolymath
      """)

      assert [finding] = SupplyChain.sc013_dependabot_inbox_amplifier(repo)
      assert finding.rule == "SC013"
      assert Enum.sort(finding.detail.notification_sources) == ["assignees", "reviewers"]
    end

    test "flags blanket CODEOWNERS when Dependabot is configured", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))
      File.write!(Path.join([repo, ".github", "CODEOWNERS"]), "* @hyperpolymath\n")

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: github-actions
          directory: /
          schedule: { interval: weekly }
      """)

      assert [finding] = SupplyChain.sc013_dependabot_inbox_amplifier(repo)
      assert finding.detail.notification_sources == ["blanket-codeowners"]
      assert finding.detail.codeowners_file == ".github/CODEOWNERS"
    end

    test "passes targeted security CODEOWNERS policy", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))

      File.write!(
        Path.join([repo, ".github", "CODEOWNERS"]),
        "/.github/workflows/ @hyperpolymath\n/SECURITY* @hyperpolymath\n"
      )

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: github-actions
          directory: /
          schedule: { interval: weekly }
      """)

      assert SupplyChain.sc013_dependabot_inbox_amplifier(repo) == []
    end
  end

  # ─── SC003 / SC007 / SC010 (token-gated) ────────────────────────────

  describe "API-backed rules with no token" do
    setup do
      old_t = System.get_env("GITHUB_TOKEN")
      old_p = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      on_exit(fn ->
        if old_t, do: System.put_env("GITHUB_TOKEN", old_t)
        if old_p, do: System.put_env("HYPATIA_DISPATCH_PAT", old_p)
      end)

      :ok
    end

    test "sc003 returns [] without token", %{repo: repo} do
      add_workflow(repo, "x.yml", "jobs:\n  x:\n    steps:\n      - uses: foo/bar@abc\n")
      assert SupplyChain.sc003_archived_action(repo) == []
    end

    test "sc007 returns [] without token", %{repo: repo} do
      add_workflow(repo, "x.yml", "jobs:\n  x:\n    runs-on: self-hosted\n")
      assert SupplyChain.sc007_self_hosted_on_public("o", "r", repo) == []
    end

    test "sc010 returns [] without token" do
      assert SupplyChain.sc010_webhooks_without_secret("o", "r") == []
    end
  end

  # ─── scan/2 facade ──────────────────────────────────────────────────

  describe "scan/2" do
    test "returns standard shape on minimal clean repo", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, ".github"))
      File.write!(Path.join(repo, "SECURITY.md"), "x")

      File.write!(Path.join([repo, ".github", "dependabot.yml"]), """
      version: 2
      updates:
        - package-ecosystem: "github-actions"
          directory: /
          schedule:
            interval: weekly
      """)

      File.write!(
        Path.join(repo, "CODEOWNERS"),
        "/.github/workflows/ @owner\n"
      )

      result = SupplyChain.scan(repo)
      assert is_map(result)
      assert Map.has_key?(result, :findings)
      assert Map.has_key?(result, :total)
      assert Map.has_key?(result, :by_severity)
      assert Map.has_key?(result, :dispatch)
    end

    test "surfaces SC009 + SC002 + SC001 on bare repo", %{repo: repo} do
      result = SupplyChain.scan(repo)
      rules = result.findings |> Enum.map(& &1.rule) |> Enum.uniq() |> Enum.sort()
      assert "SC001" in rules
      assert "SC002" in rules
      assert "SC009" in rules
    end
  end
end
