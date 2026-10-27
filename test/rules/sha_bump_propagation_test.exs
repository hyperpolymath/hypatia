# SPDX-License-Identifier: MPL-2.0
# Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.Rules.ShaBumpPropagationTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ShaBumpPropagation

  # hypatia#418 — detection rule shape: fires on merged PRs touching a known
  # estate reusable workflow (or composite action). Emits one finding per
  # matched workflow path so the actuator can fan out per-pin.

  @merge_sha "abcdef0123456789abcdef0123456789abcdef01"
  @old_sha "0011223344556677889900112233445566778899"

  defp event(overrides) do
    Map.merge(
      %{
        source_repo: "hyperpolymath/standards",
        files_changed: [".github/workflows/governance-reusable.yml"],
        merge_sha: @merge_sha,
        old_sha: @old_sha,
        pr_title: "ci(governance): tighten codeql pin set",
        pr_number: 999,
        estimated_consumers: 228
      },
      overrides
    )
  end

  describe "check/1 — cicd-suite reusables (cicd-suite#54)" do
    test "a change to a cicd-suite gate fires, with the gate's path" do
      [finding] =
        ShaBumpPropagation.check(
          event(%{
            source_repo: "hyperpolymath/cicd-suite",
            files_changed: [".github/workflows/rust-ci.yml", "actions/rust-ci-check/check.sh"],
            pr_title: "feat(gates): strict Rust CI"
          })
        )

      assert finding.source_repo == "hyperpolymath/cicd-suite"
      assert finding.source_workflow == ".github/workflows/rust-ci.yml"
    end

    test "every cicd-suite gate and the estate audit are registered" do
      paths =
        ShaBumpPropagation.known_reusables()
        |> Enum.filter(fn {repo, _} -> repo == "hyperpolymath/cicd-suite" end)
        |> Enum.map(fn {_, path} -> path end)

      for gate <- ~w(main-estate-audit rust-ci creusot-gate spark-gate kanren-gate) do
        assert ".github/workflows/#{gate}.yml" in paths
      end
    end
  end

  describe "check/1 — sensitivity" do
    test "fires on a known reusable workflow change" do
      [finding] = ShaBumpPropagation.check(event(%{}))

      assert finding.rule == :reusable_workflow_sha_bump_needs_propagation
      assert finding.severity == :medium
      assert finding.strategy == :review
      assert finding.source_repo == "hyperpolymath/standards"
      assert finding.source_workflow == ".github/workflows/governance-reusable.yml"
      assert finding.old_sha == @old_sha
      assert finding.new_sha == @merge_sha
      assert finding.pr_number == 999
      assert finding.estimated_consumers == 228
    end

    test "carries pr_title verbatim so the actuator can keyword-filter" do
      title = "fix: bump license header"
      [finding] = ShaBumpPropagation.check(event(%{pr_title: title}))

      # hypatia emits — actuation rejects. Title MUST reach the actuator unchanged.
      assert finding.pr_title == title
    end

    test "fires per matched workflow when a PR touches multiple reusables" do
      ev =
        event(%{
          files_changed: [
            ".github/workflows/governance-reusable.yml",
            ".github/workflows/rust-ci-reusable.yml",
            "docs/changelog.md"
          ]
        })

      findings = ShaBumpPropagation.check(ev)
      paths = findings |> Enum.map(& &1.source_workflow) |> Enum.sort()

      assert paths == [
               ".github/workflows/governance-reusable.yml",
               ".github/workflows/rust-ci-reusable.yml"
             ]
    end

    test "fires on known composite-action repos via action.yml" do
      ev =
        event(%{
          source_repo: "hyperpolymath/a2ml-validate-action",
          files_changed: ["action.yml", "README.md"]
        })

      [finding] = ShaBumpPropagation.check(ev)
      assert finding.source_repo == "hyperpolymath/a2ml-validate-action"
      assert finding.source_workflow == "action.yml"
    end
  end

  describe "check/1 — specificity" do
    test "docs-only PR on a reusable repo does NOT fire" do
      ev = event(%{files_changed: ["README.md", "docs/upgrade.adoc"]})
      assert ShaBumpPropagation.check(ev) == []
    end

    test "non-registered workflow on a reusable-repo does NOT fire" do
      ev = event(%{files_changed: [".github/workflows/internal-ci.yml"]})
      assert ShaBumpPropagation.check(ev) == []
    end

    test "non-reusable-repo PR does NOT fire even if workflow path matches" do
      ev =
        event(%{
          source_repo: "hyperpolymath/random-app",
          files_changed: [".github/workflows/governance-reusable.yml"]
        })

      assert ShaBumpPropagation.check(ev) == []
    end

    test "malformed SHA returns []" do
      ev = event(%{merge_sha: "not-a-sha"})
      assert ShaBumpPropagation.check(ev) == []
    end

    test "missing required field returns []" do
      ev = event(%{}) |> Map.delete(:merge_sha)
      assert ShaBumpPropagation.check(ev) == []
    end
  end

  describe "check/1 — input shapes" do
    test "accepts string-keyed event payloads (webhook-style)" do
      ev = %{
        "source_repo" => "hyperpolymath/standards",
        "files_changed" => [".github/workflows/governance-reusable.yml"],
        "merge_sha" => @merge_sha,
        "old_sha" => @old_sha,
        "pr_title" => "x",
        "pr_number" => 1
      }

      assert [_] = ShaBumpPropagation.check(ev)
    end
  end

  describe "check/1 — the standards repository under its current name" do
    test "fires on a merge event from metadatastician/standards" do
      [finding] = ShaBumpPropagation.check(event(%{source_repo: "metadatastician/standards"}))

      assert finding.source_repo == "metadatastician/standards"
      assert finding.source_workflow == ".github/workflows/governance-reusable.yml"
    end

    test "every standards reusable is registered under both names" do
      registered = ShaBumpPropagation.known_reusables()

      for {"hyperpolymath/standards", path} <- registered do
        assert {"metadatastician/standards", path} in registered
      end
    end
  end

  describe "known_reusables/0 — registry hygiene" do
    test "registry entries are well-formed" do
      for {repo, path} <- ShaBumpPropagation.known_reusables() do
        assert is_binary(repo)

        assert String.starts_with?(repo, "hyperpolymath/") or
                 String.starts_with?(repo, "metadatastician/")

        assert String.ends_with?(path, ".yml") or String.ends_with?(path, ".yaml")
      end
    end
  end
end
