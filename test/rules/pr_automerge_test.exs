# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.PrAutomergeTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.PrAutomerge, as: PA

  # The two anchors of the September 2026 incident.
  @poison "1c5b675653bb5c22dbe9b12b556ec555138e09fd"
  @good "b96794f015dfd88f77b49b1c93e0fa7110f94c63"

  @policy %{
    "pin_denylist" => [
      %{
        "id" => "PIN-001",
        "action" => "github/codeql-action",
        "severity" => "critical",
        "blocked_versions" => ["4.38.1"],
        "blocked_shas" => [@poison],
        "reason" => "GitHub rejects this commit at workflow start-up."
      }
    ],
    "dependency_bots" => ["dependabot[bot]", "renovate[bot]"],
    "lockfile_names" => ["Cargo.lock", "mix.lock", "package-lock.json"]
  }

  # PR records arrive from Jason as string-keyed maps, which is the shape the
  # tests use on purpose: atom-only reads are how an archived repo silently
  # becomes an active one.
  defp pr(files, opts \\ []) do
    %{
      "author" => Keyword.get(opts, :author, "dependabot[bot]"),
      "files" => files,
      "body" => Keyword.get(opts, :body, ""),
      "version_resolution" => Keyword.get(opts, :resolution, %{}),
      "repo_archived" => Keyword.get(opts, :archived, false),
      "repo" => "hyperpolymath/example",
      "number" => 42,
      "head_sha" => "0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d",
      "base" => "main"
    }
  end

  defp file(name, patch), do: %{"filename" => name, "patch" => patch}

  defp checkout_patch(from, to) do
    """
    @@ -10,1 +10,1 @@
    -      - uses: actions/checkout@#{from}
    +      - uses: actions/checkout@#{to}
    """
  end

  defp codeql_patch(from, to) do
    """
    @@ -1,1 +1,1 @@
    -      - uses: github/codeql-action/init@#{from} # v4.38.0
    +      - uses: github/codeql-action/init@#{to} # v4.38.1
    """
  end

  describe "classify/2 — the unambiguous cases are armed" do
    test "a lockfile-only bump is an auto-merge" do
      decision =
        PA.classify(
          pr([file("mix.lock", "@@ -1,1 +1,1 @@\n-  redix 1.2.0\n+  redix 1.3.0\n")]),
          @policy
        )

      assert decision.disposition == :auto_merge
      assert decision.blocked_by == "awaiting_required_checks"
      assert decision.safety == "arm_auto"
      assert decision.change_class == "bump"
      assert decision.pool == "P2"
    end

    test "a pin-only patch bump inside a workflow stays at object level" do
      resolution = %{
        {"actions/checkout", "v4.1.7"} => "4.1.7",
        {"actions/checkout", "v4.2.0"} => "4.2.0"
      }

      decision =
        PA.classify(
          pr([file(".github/workflows/ci.yml", checkout_patch("v4.1.7", "v4.2.0"))],
            resolution: resolution
          ),
          @policy
        )

      assert decision.disposition == :auto_merge
      assert decision.change_level == "object"
      assert decision.pin_only

      assert [delta] = decision.deltas
      assert delta.status == :ok
      assert delta.source == "tags"
      refute delta.major?
    end

    test "a body claim is enough when the refs cannot be resolved" do
      body = "Bumps [actions/checkout](https://github.com/actions/checkout) from 4.1.7 to 4.2.0.\n"

      decision =
        PA.classify(
          pr([file(".github/workflows/ci.yml", checkout_patch("v4.1.7", "v4.2.0"))], body: body),
          @policy
        )

      assert decision.disposition == :auto_merge
      assert [delta] = decision.deltas
      assert delta.source == "body-claim"
    end
  end

  describe "classify/2 — PA002, majors are never armed" do
    test "a version delta that crosses a major is flagged" do
      resolution = %{
        {"actions/checkout", "v4.1.7"} => "4.1.7",
        {"actions/checkout", "v7.0.1"} => "7.0.1"
      }

      decision =
        PA.classify(
          pr([file(".github/workflows/ci.yml", checkout_patch("v4.1.7", "v7.0.1"))],
            resolution: resolution
          ),
          @policy
        )

      assert decision.disposition == :flag
      assert decision.blocked_by == "major_version_delta"
      assert decision.safety == "flag"
      assert decision.major_delta
    end

    test "an unresolvable delta is flagged rather than assumed" do
      decision =
        PA.classify(
          pr([file(".github/workflows/ci.yml", checkout_patch("v4.1.7", "v4.2.0"))]),
          @policy
        )

      assert decision.disposition == :flag
      assert decision.blocked_by == "pin_delta_unresolvable"
      assert decision.unresolved == 1
    end
  end

  describe "classify/2 — the poison dispositions" do
    test "a pin-only change onto the denylist is closed, not merged" do
      decision =
        PA.classify(
          pr([file(".github/workflows/codeql.yml", codeql_patch(@good, @poison))]),
          @policy
        )

      assert decision.disposition == :close_poison_only
      assert decision.blocked_by == "introduces_denylisted_pin"
      assert decision.safety == "flag"
      assert decision.pool == "P1"
    end

    test "poison alongside a wanted update is excised, then merged" do
      patch = """
      @@ -1,4 +1,4 @@
      -      - uses: github/codeql-action/init@#{@good} # v4.38.0
      +      - uses: github/codeql-action/init@#{@poison} # v4.38.1
      -      permissions: {}
      +      permissions:
      +        contents: read
      """

      decision = PA.classify(pr([file(".github/workflows/codeql.yml", patch)]), @policy)

      assert decision.disposition == :excise_poison_then_merge
      assert decision.blocked_by == "introduces_denylisted_pin_alongside_wanted_updates"
      refute decision.pin_only
    end

    test "poison plus a major bump closes both reasons" do
      resolution = %{
        {"github/codeql-action", @good} => "4.38.0",
        {"github/codeql-action", @poison} => "4.38.1"
      }

      decision =
        PA.classify(
          pr([file(".github/workflows/codeql.yml", codeql_patch(@good, @poison))],
            resolution: resolution
          ),
          @policy
        )

      # resolved versions do not cross a major, so this stays a poison close…
      assert decision.disposition == :close_poison_only

      # …while a genuine major alongside the poison takes the other branch.
      major_resolution = %{
        {"github/codeql-action", "aaaa1111bbbb2222cccc3333dddd4444eeee5555"} => "3.28.0",
        {"github/codeql-action", @poison} => "4.38.1"
      }

      major_patch = codeql_patch("aaaa1111bbbb2222cccc3333dddd4444eeee5555", @poison)

      major_decision =
        PA.classify(
          pr([file(".github/workflows/codeql.yml", major_patch)], resolution: major_resolution),
          @policy
        )

      assert major_decision.disposition == :close_poison_and_majors
      assert major_decision.blocked_by == "introduces_denylisted_pin_and_major_bumps"
    end
  end

  describe "classify/2 — what must never be armed" do
    test "an archived repository can never merge" do
      decision =
        PA.classify(
          pr([file("mix.lock", "@@ -1,1 +1,1 @@\n-  a 1.0.0\n+  a 1.0.1\n")], archived: true),
          @policy
        )

      assert decision.disposition == :close_archived_repo
      assert decision.blocked_by == "repository_is_archived_cannot_merge"
    end

    test "a human PR keeps its human reviewer" do
      decision =
        PA.classify(
          pr([file("mix.lock", "@@ -1,1 +1,1 @@\n-  a 1.0.0\n+  a 1.0.1\n")],
            author: "hyperpolymath"
          ),
          @policy
        )

      assert decision.disposition == :flag
      assert decision.blocked_by == "not_a_dependency_bot"
    end

    test "a licence touch routes to the owner, never the robot" do
      patch = """
      @@ -1,1 +1,2 @@
       MPL-2.0
      +SPDX-License-Identifier: MPL-2.0
      """

      decision = PA.classify(pr([file("LICENSE", patch)]), @policy)

      assert decision.disposition == :flag
      assert decision.blocked_by == "touches_licence_requires_owner_review"
      assert decision.licence_touch
    end

    test "a dependency manifest without a lockfile is flagged" do
      patch = """
      @@ -3,1 +3,1 @@
      -    "left-pad": "1.0.0",
      +    "left-pad": "1.0.1",
      """

      decision = PA.classify(pr([file("package.json", patch)]), @policy)

      assert decision.disposition == :flag
      assert decision.blocked_by == "dependency_manifest_not_lockfile"
    end
  end

  describe "poison_sites/2 and pin_lines_only?/1" do
    test "the poisoned SHA is found on the added side only" do
      patch = codeql_patch(@good, @poison)
      sites = PA.poison_sites([file(".github/workflows/codeql.yml", patch)], @policy)

      assert [site] = sites
      assert site.denylist_id == "PIN-001"
      assert site.file == ".github/workflows/codeql.yml"
      assert PA.pin_lines_only?(file(".github/workflows/codeql.yml", patch))
    end

    test "a patch that also edits a permissions block is not pin-only" do
      patch = """
      @@ -1,4 +1,4 @@
      -      - uses: github/codeql-action/init@#{@good} # v4.38.0
      +      - uses: github/codeql-action/init@#{@poison} # v4.38.1
             permissions: {}
         contents: read
      """

      refute PA.pin_lines_only?(file(".github/workflows/codeql.yml", patch))
    end

    test "a good pin is not a poison site" do
      patch = codeql_patch(@poison, @good)

      assert PA.poison_sites([file(".github/workflows/codeql.yml", patch)], @policy) == []
    end
  end

  describe "body_claims/1" do
    test "the grouped and single-claim shapes are both read" do
      body = """
      Bumps [actions/checkout](https://github.com/actions/checkout) from 4.1.7 to 4.2.0.
      Updates `github/codeql-action/init` from 4.38.0 to 4.38.1
      """

      claims = PA.body_claims(body)

      assert Enum.any?(claims, &(&1.action == "actions/checkout" and &1.from == "4.1.7" and &1.to == "4.2.0"))
      assert Enum.any?(claims, &(&1.action == "github/codeql-action/init" and &1.to == "4.38.1"))
    end

    test "a dotted version is not truncated by the sentence's full stop" do
      claims = PA.body_claims("Bumps [x](url) from 4.38.0 to 4.38.1.")

      assert [%{from: "4.38.0", to: "4.38.1"}] = claims
    end

    test "prose without a claim yields nothing" do
      assert PA.body_claims("This PR is fine, honestly.") == []
    end
  end

  describe "decision_manifest/2" do
    test "an armed decision has no vetoes" do
      decision =
        PA.classify(
          pr([file("mix.lock", "@@ -1,1 +1,1 @@\n-  a 1.0.0\n+  a 1.0.1\n")]),
          @policy
        )

      manifest = PA.decision_manifest(decision, pr([]))

      assert manifest["safety"] == "arm_auto"
      assert manifest["vetoes"] == []
      assert manifest["clamped_by"] == nil
      assert manifest["pr"]["author"] == "dependabot[bot]"
      assert manifest["pr"]["number"] == 42
      assert manifest["disposition"] == "auto_merge"
      assert manifest["blocked_by"] == "awaiting_required_checks"
    end

    test "a flagged decision carries a veto and is clamped" do
      patch = """
      @@ -1,1 +1,2 @@
       MPL-2.0
      +SPDX-License-Identifier: MPL-2.0
      """

      decision = PA.classify(pr([file("LICENSE", patch)]), @policy)
      manifest = PA.decision_manifest(decision, pr([]))

      assert manifest["safety"] == "flag"
      assert manifest["clamped_by"] == "veto"
      assert [%{"reason" => "touches_licence_requires_owner_review"} | _] = manifest["vetoes"]
    end
  end
end
