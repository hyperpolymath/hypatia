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

  describe "known_reusables/0 — registry hygiene" do
    test "registry entries are well-formed" do
      for {repo, path} <- ShaBumpPropagation.known_reusables() do
        assert is_binary(repo)
        assert String.starts_with?(repo, "hyperpolymath/")
        assert String.ends_with?(path, ".yml") or String.ends_with?(path, ".yaml")
      end
    end
  end
end
