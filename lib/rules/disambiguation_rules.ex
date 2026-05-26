# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.DisambiguationRules do
  @moduledoc """
  Cross-reference / language-disambiguation rules.

  ## DR001 — `Ephapax` ↔ `AffineScript` co-occurrence flag

  AI agents (and humans) have repeatedly conflated `hyperpolymath/ephapax`
  with `hyperpolymath/affinescript`. The two are SEPARATE languages — they
  share only the `hyperpolymath/typed-wasm` compile target. The lexical
  overlap of the substring `affine` (Ephapax is internally dyadic with a
  sublanguage called `ephapax-affine`; AffineScript's name also derives
  from affine logic) is the entire reason this confusion class exists.

  This rule flags any content that mentions BOTH names in proximity, so
  that the author confirms they mean it. The intent is to surface
  potential confusion at PR-review time — not to block, just to ask.

  Canonical disambiguation doc:
  https://github.com/hyperpolymath/nextgen-languages/blob/main/docs/disambiguation/ephapax-vs-affinescript.md

  ### Mechanism

  - Match if a single content blob contains both
    `~r/\\bephapax\\b/i` AND `~r/\\baffinescript\\b/i` (case-insensitive,
    word-boundaried) in proximity within ~10 lines.
  - Exempt: any content whose path matches the documented disambiguation
    or cross-reference list (see `@exempt_path_patterns`).
  - Exempt: any content containing the exemption marker
    `hypatia: allow disambiguation_rules/ephapax_affinescript_co_occurrence`.

  ### Output shape

  Returns a list of findings; each finding has:

  - `rule: "DR001"`
  - `class: :disambiguation`
  - `severity: :info` (advisory, not blocking)
  - `proximity_line: <integer>` — the line where ephapax and AffineScript
    first co-occur within 10 lines.
  - `excerpt: <string>` — the surrounding ~3-line context.
  - `recommendation: <URL>` — the canonical disambiguation doc.

  ### Why not block?

  False positives are easy: cross-reference docs, ecosystem inventories,
  changelogs, and intentional comparisons all mention both languages
  legitimately. Blocking would create more friction than it removes. The
  finding asks a question; the author answers. If the author answers
  "yes, intentional", add the exemption marker.
  """

  @rule_id "DR001"
  @canonical_url "https://github.com/hyperpolymath/nextgen-languages/blob/main/docs/disambiguation/ephapax-vs-affinescript.md"
  @proximity_lines 10

  # Path patterns where ephapax+AffineScript co-occurrence is expected
  # and not a confusion signal. Substring match against the file path.
  @exempt_path_patterns [
    "docs/disambiguation/",
    "feedback_affinescript_ephapax_",
    "ECOSYSTEM.adoc",
    "ECOSYSTEM.a2ml",
    "CHANGELOG.md",
    "CHANGELOG.adoc",
    # The repos' own top-of-file admonitions:
    "/README.adoc",
    "/README.md",
    "CLAUDE.md",
    "AGENTS.md",
    # AGENTIC.a2ml @disambiguation blocks:
    "AGENTIC.a2ml",
    # nextgen-languages itself:
    "/nextgen-languages/"
  ]

  @exemption_marker "hypatia: allow disambiguation_rules/ephapax_affinescript_co_occurrence"

  # ─── Public API ───────────────────────────────────────────────────────

  @doc """
  Scan `content` (a string) for ephapax↔AffineScript co-occurrence in
  proximity. `path` is the file's path (used for exemption checks).

  Returns a list of finding maps. Empty list if no co-occurrence in
  proximity, or if the file is exempt.
  """
  def scan(content, path) when is_binary(content) and is_binary(path) do
    cond do
      exempt_path?(path) -> []
      String.contains?(content, @exemption_marker) -> []
      true -> find_co_occurrences(content)
    end
  end

  def scan(_, _), do: []

  @doc "Public helpers for the path-exemption check (testability)."
  def exempt_path?(path) when is_binary(path) do
    Enum.any?(@exempt_path_patterns, fn pat -> String.contains?(path, pat) end)
  end

  # ─── Internals ────────────────────────────────────────────────────────

  defp find_co_occurrences(content) do
    lines = String.split(content, "\n")

    ephapax_lines = lines_matching(lines, ~r/\bephapax\b/i)
    affscript_lines = lines_matching(lines, ~r/\baffinescript\b/i)

    case {ephapax_lines, affscript_lines} do
      {[], _} ->
        []

      {_, []} ->
        []

      {eph, aff} ->
        # Find any pair (e, a) where |e - a| <= @proximity_lines.
        # Stop at first to keep output small; one finding per file is
        # enough to prompt review.
        proximate_pair =
          for e <- eph,
              a <- aff,
              abs(e - a) <= @proximity_lines,
              reduce: nil do
            nil -> {e, a}
            acc -> acc
          end

        case proximate_pair do
          nil -> []
          {e, a} -> [build_finding(lines, e, a)]
        end
    end
  end

  defp lines_matching(lines, regex) do
    lines
    |> Enum.with_index(1)
    |> Enum.filter(fn {l, _} -> Regex.match?(regex, l) end)
    |> Enum.map(fn {_, idx} -> idx end)
  end

  defp build_finding(lines, eph_line, aff_line) do
    proximity_line = min(eph_line, aff_line)
    excerpt = excerpt_around(lines, proximity_line)

    %{
      rule: @rule_id,
      class: :disambiguation,
      severity: :info,
      proximity_line: proximity_line,
      ephapax_line: eph_line,
      affinescript_line: aff_line,
      excerpt: excerpt,
      recommendation: @canonical_url,
      message:
        "ephapax and AffineScript are SEPARATE languages — they share only typed-wasm as a compile target. " <>
          "If this co-occurrence is intentional cross-reference, add the line " <>
          "`#{@exemption_marker}` in a comment near the mention. See: #{@canonical_url}"
    }
  end

  defp excerpt_around(lines, idx) do
    start_idx = max(idx - 2, 1)
    end_idx = min(idx + 2, length(lines))

    lines
    |> Enum.with_index(1)
    |> Enum.filter(fn {_, i} -> i >= start_idx and i <= end_idx end)
    |> Enum.map(fn {l, i} -> "#{i}: #{l}" end)
    |> Enum.join("\n")
  end
end
