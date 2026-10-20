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
      body =
        "Bumps [actions/checkout](https://github.com/actions/checkout) from 4.1.7 to 4.2.0.\n"

      decision =
        PA.classify(
          pr([file(".github/workflows/ci.yml", checkout_patch("v4.1.7", "v4.2.0"))], body: body),
          @policy
        )

      assert decision.disposition == :auto_merge
      assert [delta] = decision.deltas
      assert delta.source == "body-claim"
    end

    test "a claim for one action is never borrowed by another of the same basename" do
      patch = """
      @@ -1,2 +1,2 @@
      -      - uses: acme/setup@v1.0.0
      -      - uses: other/setup@v3.0.0
      +      - uses: acme/setup@v1.1.0
      +      - uses: other/setup@v4.0.0
      """

      body = "Bumps [acme/setup](https://github.com/acme/setup) from 1.0.0 to 1.1.0.\n"
      f = file(".github/workflows/ci.yml", patch)
      deltas = PA.pin_deltas([f], %{}, PA.body_claims(body))

      assert %{status: :ok, source: "body-claim"} =
               Enum.find(deltas, &(&1.action == "acme/setup"))

      assert %{status: :unresolved} = Enum.find(deltas, &(&1.action == "other/setup"))

      # A claim naming only the basename belongs to neither action.
      bare = PA.body_claims("Bumps [setup](https://example.com) from 1.0.0 to 1.1.0.\n")
      assert Enum.all?(PA.pin_deltas([f], %{}, bare), &(&1.status == :unresolved))
    end

    test "a claim is checked against whichever endpoint resolved" do
      resolution = %{{"actions/checkout", "v4.1.7"} => "1.0.0"}

      body =
        "Bumps [actions/checkout](https://github.com/actions/checkout) from 2.0.0 to 2.1.0.\n"

      f = file(".github/workflows/ci.yml", checkout_patch("v4.1.7", "v4.2.0"))

      assert [%{status: :conflict}] = PA.pin_deltas([f], resolution, PA.body_claims(body))

      # Control: a claim that agrees with the resolved endpoint is taken.
      agreeing = String.replace(body, "from 2.0.0 to 2.1.0", "from 1.0.0 to 1.1.0")
      assert [%{status: :ok}] = PA.pin_deltas([f], resolution, PA.body_claims(agreeing))
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

    test "an upper-case spelling of a denylisted SHA is the same poison" do
      decision =
        Hypatia.Rules.PrAutomerge.classify(
          pr([
            file(".github/workflows/codeql.yml", codeql_patch(@good, String.upcase(@poison)))
          ]),
          @policy
        )

      assert decision.disposition == :close_poison_only
      assert decision.blocked_by == "introduces_denylisted_pin"
    end

    test "an unquoted flow-style pin ends at its delimiter: PA001 sees the poison" do
      patch = """
      @@ -1,1 +1,1 @@
      -      - { uses: github/codeql-action/init@#{@good}, with: { languages: rust } }
      +      - { uses: github/codeql-action/init@#{@poison}, with: { languages: rust } }
      """

      decision = PA.classify(pr([file(".github/workflows/codeql.yml", patch)]), @policy)

      assert [_] = decision.poison_sites
      refute decision.pin_only
      # The `with:` beside the pin makes the line more than a pin line, so
      # what an excision would leave is not known to be mergeable: flagged.
      assert decision.disposition == :flag
      assert decision.blocked_by == "introduces_denylisted_pin_alongside_unmergeable_changes"
    end

    test "a quoted uses: key and a quoted value are still a pin PA001 sees" do
      patch = """
      @@ -1,1 +1,1 @@
      -      - "uses": "github/codeql-action/init@#{@good}"
      +      - "uses": "github/codeql-action/init@#{@poison}"
      """

      decision = PA.classify(pr([file(".github/workflows/codeql.yml", patch)]), @policy)

      assert decision.disposition == :close_poison_only
      assert decision.blocked_by == "introduces_denylisted_pin"
    end

    test "poison alongside a change that would be flagged on its own is flagged, not armed" do
      # A `permissions:` edit is no pin bump (MGX-001): without the poison this
      # PR is not_unambiguously_classifiable, and finding the poison must not
      # upgrade it to an armed excise-then-merge.
      patch = """
      @@ -1,4 +1,4 @@
      -      - uses: github/codeql-action/init@#{@good} # v4.38.0
      +      - uses: github/codeql-action/init@#{@poison} # v4.38.1
      -      permissions: {}
      +      permissions:
      +        contents: read
      """

      decision = PA.classify(pr([file(".github/workflows/codeql.yml", patch)]), @policy)

      assert decision.disposition == :flag
      assert decision.safety == "flag"
      assert decision.blocked_by == "introduces_denylisted_pin_alongside_unmergeable_changes"
      assert decision.remainder_blocked_by == "not_unambiguously_classifiable"
      refute decision.pin_only
    end

    test "poison alongside a wanted lockfile update is excised, then merged" do
      # The workflow's only change is the poisoned pin: excised, it drops out,
      # and what remains is a lockfile-only bump that arms on its own.
      decision =
        PA.classify(
          pr([
            file(".github/workflows/codeql.yml", codeql_patch(@good, @poison)),
            file("mix.lock", "@@ -1,1 +1,1 @@\n-  a 1.0.0\n+  a 1.0.1\n")
          ]),
          @policy
        )

      assert decision.disposition == :excise_poison_then_merge
      assert decision.safety == "arm_auto"
      assert decision.blocked_by == "introduces_denylisted_pin_alongside_wanted_updates"
    end

    test "an unresolved bump sharing a line with a blocked pin, beside a source edit, is flagged" do
      # Codex's shape: excision leaves the unresolved ordinary bump and the
      # source file, neither of which arms on its own.
      patch = """
      @@ -1,1 +1,1 @@
      -    steps: [ { uses: actions/cache@v4.0.0 }, { uses: github/codeql-action/init@#{@good} } ]
      +    steps: [ { uses: actions/cache@v4.1.0 }, { uses: github/codeql-action/init@#{@poison} } ]
      """

      decision =
        PA.classify(
          pr([
            file(".github/workflows/ci.yml", patch),
            file("lib/runtime.ex", "@@ -1,1 +1,1 @@\n-x = 1\n+x = 2\n")
          ]),
          @policy
        )

      assert decision.safety == "flag"
      assert decision.blocked_by == "introduces_denylisted_pin_alongside_unmergeable_changes"
      assert decision.remainder_blocked_by == "pin_delta_unresolvable"
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
    test "a flow pin with neighbouring input, condition or environment edits is not pin-only" do
      for {before, after_edit} <- [
            {"with: { languages: rust }", "with: { languages: javascript }"},
            {"if: false", "if: true"},
            {"env: { MODE: test }", "env: { MODE: deploy }"}
          ] do
        patch = """
        @@ -1,1 +1,1 @@
        -      - { uses: actions/checkout@v4.1.7, #{before} }
        +      - { uses: actions/checkout@v4.2.0, #{after_edit} }
        """

        decision =
          PA.classify(
            pr([file(".github/workflows/ci.yml", patch)],
              resolution: %{
                {"actions/checkout", "v4.1.7"} => "4.1.7",
                {"actions/checkout", "v4.2.0"} => "4.2.0"
              }
            ),
            @policy
          )

        refute decision.pin_only
        assert decision.disposition == :flag
      end
    end

    test "quoted flow pins and their comments are pin-only, punctuation alone is not" do
      patch = """
      @@ -1,1 +1,1 @@
      -      - [{ "uses": "actions/checkout@v4.1.7" }, { 'uses': 'actions/cache@v4' }] # old
      +      - [{ "uses": "actions/checkout@v4.2.0" }, { 'uses': 'actions/cache@v5' }] # new
      """

      assert PA.pin_lines_only?(file(".github/workflows/ci.yml", patch))
      refute PA.pin_lines_only?(file(".github/workflows/ci.yml", "@@ -1 +1 @@\n- []\n+ {}\n"))
    end

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
      @@ -1,2 +1,3 @@
      -      - uses: github/codeql-action/init@#{@good} # v4.38.0
      +      - uses: github/codeql-action/init@#{@poison} # v4.38.1
      -      permissions: {}
      +      permissions:
      +        contents: read
      """

      refute PA.pin_lines_only?(file(".github/workflows/codeql.yml", patch))
    end

    test "a good pin is not a poison site" do
      patch = codeql_patch(@poison, @good)

      assert PA.poison_sites([file(".github/workflows/codeql.yml", patch)], @policy) == []
    end

    test "a re-pinned value under an unchanged `uses:` key is the site it continues" do
      # The key is context; only the reference on the line below it changes.
      patch = """
      @@ -1,3 +1,3 @@
             - uses:
               # pinned
      -          github/codeql-action/init@#{@good}
      +          github/codeql-action/init@#{@poison}
      """

      f = file(".github/workflows/codeql.yml", patch)
      assert [%{denylist_id: "PIN-001"}] = PA.poison_sites([f], @policy)
      assert PA.pin_lines_only?(f)

      # Control: the same value line under a key that is not `uses:` is data.
      data = String.replace(patch, "       - uses:", "       - run:")
      assert PA.poison_sites([file(".github/workflows/codeql.yml", data)], @policy) == []
    end

    test "every site of a flow-sequence line is inspected, not only the first" do
      # The first step is an ordinary resolvable bump; the second is the
      # poison. Judging the line by its first pin armed automerge.
      patch = """
      @@ -1,1 +1,1 @@
      -        { uses: actions/checkout@v4.1.6 }, { uses: github/codeql-action/init@#{@good} },
      +        { uses: actions/checkout@v4.1.7 }, { uses: github/codeql-action/init@#{@poison} },
      """

      f = file(".github/workflows/codeql.yml", patch)
      assert [site] = PA.poison_sites([f], @policy)
      assert site.denylist_id == "PIN-001"

      decision =
        PA.classify(
          pr([f],
            resolution: %{
              {"actions/checkout", "v4.1.6"} => "4.1.6",
              {"actions/checkout", "v4.1.7"} => "4.1.7"
            }
          ),
          @policy
        )

      assert decision.pin_only
      assert decision.disposition == :close_poison_only
      assert decision.blocked_by == "introduces_denylisted_pin"
    end

    test "every flow-sequence site is a delta of its own" do
      patch = """
      @@ -1,1 +1,1 @@
      -      - { uses: actions/checkout@v4.1.6 }, { uses: actions/cache@v4.0.0 }
      +      - { uses: actions/checkout@v4.1.7 }, { uses: actions/cache@v5.0.0 }
      """

      deltas = PA.pin_deltas([file(".github/workflows/ci.yml", patch)], %{}, [])
      assert Enum.map(deltas, & &1.action) == ["actions/checkout", "actions/cache"]
    end

    test "a mixed-case action spelling meets its lower-case denylist entry" do
      patch =
        codeql_patch(@good, @poison)
        |> String.replace("github/codeql-action", "GitHub/CodeQL-Action")

      assert [site] = PA.poison_sites([file(".github/workflows/codeql.yml", patch)], @policy)
      assert site.denylist_id == "PIN-001"
    end

    test "a one-line flow sequence with a major bump and a poison pin closes both reasons" do
      patch = """
      @@ -1,1 +1,1 @@
      -    steps: [ { uses: actions/checkout@v4.1.7 }, { uses: github/codeql-action/init@#{@good} } ]
      +    steps: [ { uses: actions/checkout@v7.0.1 }, { uses: github/codeql-action/init@#{@poison} } ]
      """

      resolution = %{
        {"actions/checkout", "v4.1.7"} => "4.1.7",
        {"actions/checkout", "v7.0.1"} => "7.0.1"
      }

      decision =
        PA.classify(
          pr([file(".github/workflows/ci.yml", patch)], resolution: resolution),
          @policy
        )

      assert decision.major_delta
      assert decision.disposition == :close_poison_and_majors
    end

    test "a pin written inside a run: | script is text, not a site" do
      in_hunk = """
      @@ -1,4 +1,4 @@
             - run: |
                 cat > fixture.yml <<'EOF'
      -          - uses: github/codeql-action/init@#{@good}
      +          - uses: github/codeql-action/init@#{@poison}
                 EOF
      """

      f = file(".github/workflows/ci.yml", in_hunk)
      assert PA.poison_sites([f], @policy) == []
      refute PA.pin_lines_only?(f)

      free_text = """
      @@ -9,1 +9,1 @@
      -          echo done
      +          echo { uses: github/codeql-action/init@#{@poison} }
      """

      assert PA.poison_sites([file(".github/workflows/ci.yml", free_text)], @policy) == []
    end

    test "a diff of pin lines that adds or swaps an action is not pin-only, and is not armed" do
      added = """
      @@ -3,2 +3,3 @@
             steps: [
      +        { uses: owner/action@main },
               { uses: actions/checkout@v7.0.1 },
      """

      swapped = """
      @@ -1,1 +1,1 @@
      -      - uses: actions/checkout@v7.0.1
      +      - uses: evil/checkout@v7.0.1
      """

      for patch <- [added, swapped] do
        f = file(".github/workflows/ci.yml", patch)
        refute PA.pin_lines_only?(f)
        assert PA.classify(pr([f]), @policy).safety == "flag"
      end

      # Control: the same action moved to another ref is a re-pin.
      assert PA.pin_lines_only?(
               file(".github/workflows/ci.yml", checkout_patch("v7.0.0", "v7.0.1"))
             )
    end

    test "a pin inside an input's mapping is data, not a site" do
      line = "+        with: { uses: github/codeql-action/init@#{@poison} }"
      assert PA.parse_pins(line) == []
    end

    test "a pin quoted inside another scalar or a comment is not a site" do
      line =
        ~s(+      - { name: "see uses: github/codeql-action/init@#{@poison}", uses: actions/checkout@v4.1.7 } # uses: x/y@#{@poison})

      assert [%{action: "actions/checkout", ref: "v4.1.7"}] = PA.parse_pins(line)
    end

    test "repeated sites of one action pair one-to-one, so the second site's major bump counts" do
      # The first checkout stays at v4.1.7 and the second moves to v7.0.1.
      # Pairing both removals with the first addition (the unchanged one)
      # hid the major bump and armed an excision instead of a close.
      patch = """
      @@ -1,1 +1,1 @@
      -    steps: [ { uses: actions/checkout@v4.1.7 }, { uses: actions/checkout@v4.1.7 }, { uses: github/codeql-action/init@#{@good} } ]
      +    steps: [ { uses: actions/checkout@v4.1.7 }, { uses: actions/checkout@v7.0.1 }, { uses: github/codeql-action/init@#{@poison} } ]
      """

      resolution = %{
        {"actions/checkout", "v4.1.7"} => "4.1.7",
        {"actions/checkout", "v7.0.1"} => "7.0.1"
      }

      f = file(".github/workflows/ci.yml", patch)

      assert [%{action: "actions/checkout", from: "4.1.7", to: "7.0.1", major?: true}] =
               f
               |> List.wrap()
               |> PA.pin_deltas(resolution, [])
               |> Enum.filter(&(&1.action == "actions/checkout"))

      decision = PA.classify(pr([f], resolution: resolution), @policy)
      assert decision.disposition == :close_poison_and_majors
    end

    test "a hunk that opens inside a script, header out of view, flags a poison pin rather than closing" do
      # More than three lines below `run: |`, the hunk shows no header: the
      # pin may be fixture text. Its neighbours at its own indentation are
      # shell, so the context is unknown and the PR goes to an owner.
      patch = """
      @@ -40,3 +40,3 @@
                 cat > fixture.yml <<'EOF'
      -          - uses: github/codeql-action/init@#{@good}
      +          - uses: github/codeql-action/init@#{@poison}
                 EOF
      """

      f = file(".github/workflows/ci.yml", patch)
      assert [%{certain: false}] = PA.poison_sites([f], @policy)
      refute PA.pin_lines_only?(f)

      decision = PA.classify(pr([f]), @policy)
      assert decision.disposition == :flag
      assert decision.blocked_by == "denylisted_pin_context_unknown"

      # Control: the same change among YAML siblings is a live pin.
      live = """
      @@ -12,3 +12,3 @@
             - name: Init CodeQL
      -        uses: github/codeql-action/init@#{@good}
      +        uses: github/codeql-action/init@#{@poison}
               with:
      """

      live_decision = PA.classify(pr([file(".github/workflows/ci.yml", live)]), @policy)
      assert live_decision.disposition == :close_poison_only
    end

    test "an input named uses under a step is data: no poison site, no pin-only change" do
      patch = """
      @@ -12,3 +12,3 @@
             - uses: some/wrapper@v1
               with:
      -          uses: github/codeql-action/init@#{@good}
      +          uses: github/codeql-action/init@#{@poison}
      """

      f = file(".github/workflows/ci.yml", patch)
      assert PA.poison_sites([f], @policy) == []
      refute PA.pin_lines_only?(f)
      refute PA.classify(pr([f]), @policy).disposition == :close_poison_only

      # Control: the step's own key at the same depth is the pin it was.
      live = """
      @@ -12,3 +12,3 @@
             - name: init
      -        uses: github/codeql-action/init@#{@good}
      +        uses: github/codeql-action/init@#{@poison}
      """

      assert PA.classify(pr([file(".github/workflows/ci.yml", live)]), @policy).disposition ==
               :close_poison_only
    end

    test "a re-pinned anchor an unchanged uses: *alias runs is a poison site" do
      patch = """
      @@ -1,7 +1,7 @@
       env:
      -  BAD: &bad github/codeql-action/init@#{@good}
      +  BAD: &bad github/codeql-action/init@#{@poison}
       jobs:
         a:
           steps:
             - uses: *bad
      """

      f = file(".github/workflows/ci.yml", patch)
      assert [%{certain: true}] = PA.poison_sites([f], @policy)

      decision = PA.classify(pr([f]), @policy)
      assert decision.disposition == :flag
      assert decision.blocked_by == "introduces_denylisted_pin_alongside_unmergeable_changes"
    end

    test "a step given whole as an alias makes its anchored mapping's re-pin a poison site" do
      # The mapping is anchored in a matrix entry (data); `- *step` in the
      # job's steps runs it.
      for {anchor, changed} <- [
            {"            - &step", "              uses: github/codeql-action/init@"},
            {nil, "            - step: &step { uses: github/codeql-action/init@"}
          ] do
        close = if anchor, do: "", else: " }"
        head = if anchor, do: " #{anchor}\n", else: ""

        patch =
          "@@ -1,9 +1,9 @@\n jobs:\n   a:\n     strategy:\n       matrix:\n         include:\n" <>
            head <>
            "-#{changed}#{@good}#{close}\n+#{changed}#{@poison}#{close}\n" <>
            "     steps:\n       - *step\n"

        f = file(".github/workflows/ci.yml", patch)
        assert [%{certain: true}] = PA.poison_sites([f], @policy), patch
      end
    end

    test "a pin inside a multi-line quoted run: is text; one the hunk never closes is uncertain" do
      # The quote opens on a context line and closes inside the hunk: every
      # line between is string text, never a site.
      closed = """
      @@ -1,7 +1,7 @@
             - run: "cat > fixture.yml <<EOF
                 jobs:
                   b:
                     steps:
      -                - uses: github/codeql-action/init@#{@good}
      +                - uses: github/codeql-action/init@#{@poison}
                 EOF"
      """

      f = file(".github/workflows/ci.yml", closed)
      assert PA.poison_sites([f], @policy) == []
      refute PA.pin_lines_only?(f)

      # The closing quote is past the hunk and every visible line is
      # YAML-shaped (a fixture): the line may be text, so it is not known
      # to be a pin, and the PR goes to an owner instead of closing.
      cut = """
      @@ -1,6 +1,6 @@
             - run: "cat > fixture.yml <<EOF
                 jobs:
                   b:
                     steps:
      -                - uses: github/codeql-action/init@#{@good}
      +                - uses: github/codeql-action/init@#{@poison}
      """

      g = file(".github/workflows/ci.yml", cut)
      assert [%{certain: false}] = PA.poison_sites([g], @policy)
      refute PA.pin_lines_only?(g)

      decision = PA.classify(pr([g]), @policy)
      assert decision.disposition == :flag
      assert decision.blocked_by == "denylisted_pin_context_unknown"
    end
  end

  describe "body_claims/1" do
    test "the grouped and single-claim shapes are both read" do
      body = """
      Bumps [actions/checkout](https://github.com/actions/checkout) from 4.1.7 to 4.2.0.
      Updates `github/codeql-action/init` from 4.38.0 to 4.38.1
      """

      claims = PA.body_claims(body)

      assert Enum.any?(
               claims,
               &(&1.action == "actions/checkout" and &1.from == "4.1.7" and &1.to == "4.2.0")
             )

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
