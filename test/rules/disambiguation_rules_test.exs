# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.DisambiguationRulesTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.DisambiguationRules, as: DR

  describe "scan/2 — DR001 ephapax↔AffineScript co-occurrence" do
    test "flags content that mentions both names in close proximity" do
      content = """
      The ephapax preservation theorem is proven in Coq.
      Likewise, AffineScript's borrow checker handles the analogous case.
      """

      [finding] = DR.scan(content, "src/example.rs")

      assert finding.rule == "DR001"
      assert finding.severity == :info
      assert finding.class == :disambiguation
      assert finding.proximity_line == 1
      assert finding.ephapax_line == 1
      assert finding.affinescript_line == 2
      assert finding.recommendation =~ "nextgen-languages"
    end

    test "no finding when only ephapax is mentioned" do
      content = "Ephapax is a research language for WebAssembly memory safety."
      assert DR.scan(content, "src/example.rs") == []
    end

    test "no finding when only AffineScript is mentioned" do
      content = "AffineScript is a successor to JS/TS/ReScript."
      assert DR.scan(content, "src/example.rs") == []
    end

    test "no finding when names are far apart (> 10 lines)" do
      content = """
      Ephapax line.
      #{Enum.map_join(1..15, "\n", fn _ -> "filler" end)}
      AffineScript line.
      """

      assert DR.scan(content, "src/example.rs") == []
    end

    test "exempt path: docs/disambiguation/ — no finding" do
      content = "ephapax and AffineScript are different languages."

      assert DR.scan(content, "docs/disambiguation/ephapax-vs-affinescript.md") == []
    end

    test "exempt path: README.adoc — no finding" do
      content = "ephapax and AffineScript share only typed-wasm."

      assert DR.scan(content, "/some/repo/README.adoc") == []
    end

    test "exempt path: CHANGELOG.md — no finding" do
      content = "Added support for ephapax in addition to AffineScript producers."

      assert DR.scan(content, "CHANGELOG.md") == []
    end

    test "exempt path: AGENTIC.a2ml — no finding" do
      content = "distinct-from = [\"hyperpolymath/ephapax\"]\nthis = \"AffineScript\""

      assert DR.scan(content, ".machine_readable/6a2/AGENTIC.a2ml") == []
    end

    test "exempt path: feedback_affinescript_ephapax_*.md — no finding" do
      content = "AffineScript and ephapax share only the typed-wasm target."

      assert DR.scan(content, "memory/feedback_affinescript_ephapax_siblings.md") == []
    end

    test "inline exemption marker suppresses the finding" do
      content = """
      The ephapax preservation theorem stands.
      The AffineScript borrow checker has the analogous case.
      // hypatia: allow disambiguation_rules/ephapax_affinescript_co_occurrence -- intentional comparison
      """

      assert DR.scan(content, "src/example.rs") == []
    end

    test "case-insensitive matching: Ephapax and affineScript still flagged" do
      content = """
      Ephapax preservation.
      affineScript borrow check.
      """

      assert [_finding] = DR.scan(content, "src/example.rs")
    end

    test "excerpt contains a few lines around the proximity_line" do
      content = """
      Line 1.
      Line 2 mentions ephapax.
      Line 3 mentions AffineScript.
      Line 4.
      """

      [finding] = DR.scan(content, "src/example.rs")
      assert finding.excerpt =~ "ephapax"
      assert finding.excerpt =~ "AffineScript"
      # Excerpt format: "N: <line>\n..."
      assert finding.excerpt =~ ~r/\d+: /
    end

    test "one finding per file even with multiple co-occurrences" do
      content = """
      ephapax block 1
      AffineScript block 1
      filler
      ephapax block 2
      AffineScript block 2
      """

      assert length(DR.scan(content, "src/example.rs")) == 1
    end

    test "non-string content returns empty" do
      assert DR.scan(nil, "src/example.rs") == []
      assert DR.scan("content", nil) == []
    end
  end

  describe "exempt_path?/1" do
    test "exempts canonical doc dir" do
      assert DR.exempt_path?("docs/disambiguation/foo.md")
    end

    test "exempts README files" do
      assert DR.exempt_path?("/repo/README.adoc")
      assert DR.exempt_path?("nested/README.md")
    end

    test "exempts memory entries" do
      assert DR.exempt_path?("memory/feedback_affinescript_ephapax_anything.md")
    end

    test "exempts AGENTIC.a2ml" do
      assert DR.exempt_path?(".machine_readable/6a2/AGENTIC.a2ml")
    end

    test "exempts CLAUDE.md" do
      assert DR.exempt_path?(".claude/CLAUDE.md")
      assert DR.exempt_path?("CLAUDE.md")
    end

    test "does not exempt ordinary source paths" do
      refute DR.exempt_path?("src/lib.rs")
      refute DR.exempt_path?("lib/borrow.ml")
      refute DR.exempt_path?("formal/Semantics.v")
    end
  end
end
