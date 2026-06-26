# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.StructuralDrift do
  @moduledoc """
  Structural drift detection rules.

  Detects repos that have drifted from the canonical RSR file layout,
  including legacy formats (.scm, .hs, .djot), misplaced state files,
  orphan gitlinks, and stale references in documentation/manifests.

  When a violation is found, triggers an INTENSIVE SCAN of the entire
  repo to check for other drift patterns -- because where there's one
  cockroach, there are usually more.

  Dispatches to:
  - rhodibot: auto-fix file moves/renames/deletions
  - echidnabot: intensive semantic analysis after initial finding
  - panicbot: security audit if secrets or unsafe patterns found

  Rule IDs: SD001-SD020
  """

  # ─── SD001: Legacy .scm state files ────────────────────────────────────

  @legacy_scm_files [
    "STATE.scm",
    "META.scm",
    "ECOSYSTEM.scm",
    "AGENTIC.scm",
    "NEUROSYM.scm",
    "PLAYBOOK.scm",
    "LANGUAGES.scm"
  ]

  @doc """
  SD001: Detect .scm state files anywhere in the repo.
  These should have been migrated to .a2ml in .machine_readable/6a2/.
  Severity: critical (blocks compliance).
  Action: convert to .a2ml, move to .machine_readable/6a2/, delete .scm.
  Triggers: intensive scan of entire repo.
  """
  def sd001_legacy_scm(repo_path) do
    findings =
      @legacy_scm_files
      |> Enum.flat_map(fn scm_file ->
        locations = [
          Path.join(repo_path, scm_file),
          Path.join([repo_path, ".machine_readable", scm_file]),
          Path.join([repo_path, ".machine_readable", "6a2", scm_file])
        ]

        locations
        |> Enum.filter(&File.exists?/1)
        |> Enum.map(fn path ->
          %{
            rule: "SD001",
            file: Path.relative_to(path, repo_path),
            severity: :critical,
            reason: "Legacy .scm state file -- must be .a2ml in .machine_readable/6a2/",
            action: :convert_and_move,
            trigger_intensive: true
          }
        end)
      end)

    findings
  end

  # ─── SD002: Trustfile.hs (legacy Haskell format) ───────────────────────

  @doc """
  SD002: Detect Trustfile.hs files anywhere in the repo.
  These should be Trustfile.a2ml in .machine_readable/contractiles/trust/.
  Severity: high.
  Action: convert to a2ml, delete .hs.
  Triggers: intensive scan.
  """
  def sd002_trustfile_hs(repo_path) do
    case find_files(repo_path, "Trustfile.hs") do
      [] ->
        []

      files ->
        Enum.map(files, fn path ->
          %{
            rule: "SD002",
            file: Path.relative_to(path, repo_path),
            severity: :high,
            reason:
              "Legacy Trustfile.hs -- must be Trustfile.a2ml in .machine_readable/contractiles/trust/",
            action: :convert_and_move,
            trigger_intensive: true
          }
        end)
    end
  end

  # ─── SD003: AI.djot (superseded by 0-AI-MANIFEST.a2ml) ────────────────

  @doc """
  SD003: Detect AI.djot files.
  Superseded by 0-AI-MANIFEST.a2ml. Content should be merged into gatekeeper.
  Severity: high.
  Action: merge into 0-AI-MANIFEST.a2ml, delete .djot.
  """
  def sd003_ai_djot(repo_path) do
    case find_files(repo_path, "AI.djot") do
      [] ->
        []

      files ->
        Enum.map(files, fn path ->
          %{
            rule: "SD003",
            file: Path.relative_to(path, repo_path),
            severity: :high,
            reason: "AI.djot superseded by 0-AI-MANIFEST.a2ml -- merge and delete",
            action: :merge_and_delete,
            trigger_intensive: true
          }
        end)
    end
  end

  # ─── SD004: Misplaced 6a2ml files ──────────────────────────────────────

  @a2ml_state_files [
    "STATE.a2ml",
    "META.a2ml",
    "ECOSYSTEM.a2ml",
    "AGENTIC.a2ml",
    "NEUROSYM.a2ml",
    "PLAYBOOK.a2ml"
  ]

  @doc """
  SD004: Detect 6a2ml files outside of .machine_readable/6a2/.
  These MUST be in .machine_readable/6a2/ and nowhere else.
  Severity: critical.
  Action: move to .machine_readable/6a2/.
  """
  def sd004_misplaced_a2ml(repo_path) do
    @a2ml_state_files
    |> Enum.flat_map(fn a2ml_file ->
      wrong_locations = [
        Path.join(repo_path, a2ml_file),
        Path.join([repo_path, ".machine_readable", a2ml_file])
      ]

      wrong_locations
      |> Enum.filter(&File.exists?/1)
      |> Enum.map(fn path ->
        %{
          rule: "SD004",
          file: Path.relative_to(path, repo_path),
          severity: :critical,
          reason: "6a2ml file outside canonical location -- must be in .machine_readable/6a2/",
          action: :move,
          target: ".machine_readable/6a2/#{a2ml_file}"
        }
      end)
    end)
  end

  # ─── SD005: Orphan gitlinks without .gitmodules ────────────────────────

  # When orphan gitlink count exceeds this threshold, actions/checkout
  # post-job cleanup (git submodule foreach) will startup_failure on
  # every gitlink it cannot resolve. Surfaced by the developer-ecosystem
  # fix (hyperpolymath/developer-ecosystem@baab1534): 69 mode-160000 stale
  # gitlinks caused scorecard workflow startup_failure on every push.
  @orphan_gitlink_checkout_failure_threshold 5

  @doc """
  SD005: Detect gitlinks (mode 160000) without corresponding .gitmodules entry.
  This is how stray repos-inside-repos (like svalinn in ats2-tui) happen.
  Bot `git add -A` catches stray clones and creates orphan submodule refs.
  Severity: critical (data corruption risk).
  Action: investigate -- move nested repo to canonical location, remove gitlink.
  Triggers: intensive scan + alert user.

  When the orphan count exceeds #{@orphan_gitlink_checkout_failure_threshold},
  emits one additional summary finding at severity :high with a reason that
  explicitly names the actions/checkout startup_failure risk.
  """
  def sd005_orphan_gitlinks(repo_path) do
    gitmodules_path = Path.join(repo_path, ".gitmodules")
    has_gitmodules = File.exists?(gitmodules_path)

    # Get all gitlinks from git index
    case System.cmd("git", ["ls-files", "--stage"], cd: repo_path) do
      {output, 0} ->
        gitlinks =
          output
          |> String.split("\n", trim: true)
          |> Enum.filter(&String.starts_with?(&1, "160000"))
          |> Enum.map(fn line ->
            # Format: "160000 <hash> <stage>\t<path>"
            line |> String.split("\t") |> List.last()
          end)
          |> Enum.filter(& &1)

        if gitlinks == [] do
          []
        else
          # Check which are in .gitmodules
          gitmodules_content = if has_gitmodules, do: File.read!(gitmodules_path), else: ""

          per_link_findings =
            Enum.flat_map(gitlinks, fn path ->
              if String.contains?(gitmodules_content, "path = #{path}") do
                # Legitimate submodule
                []
              else
                [
                  %{
                    rule: "SD005",
                    file: path,
                    severity: :critical,
                    reason:
                      "Orphan gitlink -- submodule ref without .gitmodules entry. Likely a stray clone caught by bot git-add-all.",
                    action: :investigate,
                    trigger_intensive: true,
                    alert_user: true
                  }
                ]
              end
            end)

          orphan_count = length(per_link_findings)

          # When count > threshold, emit an additional summary finding that
          # explicitly calls out the actions/checkout startup_failure risk.
          # This is the count-level signal that per-link :critical findings
          # don't surface on their own (developer-ecosystem@baab1534 had 69
          # stale gitlinks and scorecard's startup_failure was only diagnosed
          # after manual inspection of the submodule foreach error).
          summary_finding =
            if orphan_count > @orphan_gitlink_checkout_failure_threshold do
              [
                %{
                  rule: "SD005",
                  file: ".git (index)",
                  severity: :high,
                  reason:
                    "#{orphan_count} orphan gitlinks detected -- actions/checkout post-job cleanup " <>
                      "(git submodule foreach) will startup_failure with this many stale refs. " <>
                      "Remove all orphan gitlinks before next push.",
                  action: :investigate,
                  trigger_intensive: true,
                  alert_user: true,
                  count: orphan_count
                }
              ]
            else
              []
            end

          per_link_findings ++ summary_finding
        end

      _ ->
        []
    end
  end

  # ─── SD006: Trustfile example files ────────────────────────────────────

  @doc """
  SD006: Detect Trustfile example/template files that should have been
  removed after customisation.
  Severity: low.
  Action: delete.
  """
  def sd006_trustfile_examples(repo_path) do
    find_files_matching(repo_path, ~r/Trustfile.*example|example.*Trustfile/i)
    |> Enum.map(fn path ->
      %{
        rule: "SD006",
        file: Path.relative_to(path, repo_path),
        severity: :low,
        reason: "Trustfile example/template file -- should be deleted after customisation",
        action: :delete
      }
    end)
  end

  # ─── SD007: Stale references in 0-AI-MANIFEST.a2ml ────────────────────

  @stale_reference_patterns [
    {~r/STATE\.scm/, "References STATE.scm -- should be .machine_readable/6a2/STATE.a2ml"},
    {~r/META\.scm/, "References META.scm -- should be .machine_readable/6a2/META.a2ml"},
    {~r/ECOSYSTEM\.scm/,
     "References ECOSYSTEM.scm -- should be .machine_readable/6a2/ECOSYSTEM.a2ml"},
    {~r/AGENTIC\.scm/, "References AGENTIC.scm -- should be .machine_readable/6a2/AGENTIC.a2ml"},
    {~r/NEUROSYM\.scm/,
     "References NEUROSYM.scm -- should be .machine_readable/6a2/NEUROSYM.a2ml"},
    {~r/PLAYBOOK\.scm/,
     "References PLAYBOOK.scm -- should be .machine_readable/6a2/PLAYBOOK.a2ml"},
    {~r/AI\.djot/, "References AI.djot -- file has been superseded by 0-AI-MANIFEST.a2ml"},
    {~r/Trustfile\.hs/, "References Trustfile.hs -- should be Trustfile.a2ml"},
    {~r/\.machine_readable\/STATE\.a2ml/,
     "References .machine_readable/STATE.a2ml -- should be .machine_readable/6a2/STATE.a2ml"},
    {~r/\.machine_readable\/META\.a2ml/,
     "References .machine_readable/META.a2ml -- should be .machine_readable/6a2/META.a2ml"}
  ]

  @doc """
  SD007: Detect stale path references in manifest, README, and documentation files.
  These references point to files that have been moved or renamed.
  Severity: medium.
  Action: update references to canonical paths.
  """
  def sd007_stale_references(repo_path) do
    files_to_check = [
      Path.join(repo_path, "0-AI-MANIFEST.a2ml"),
      Path.join(repo_path, "README.adoc"),
      Path.join(repo_path, "README.md"),
      Path.join([repo_path, ".claude", "CLAUDE.md"])
    ]

    files_to_check
    |> Enum.filter(&File.exists?/1)
    |> Enum.flat_map(fn file_path ->
      content = File.read!(file_path)

      @stale_reference_patterns
      |> Enum.flat_map(fn {pattern, reason} ->
        if Regex.match?(pattern, content) do
          [
            %{
              rule: "SD007",
              file: Path.relative_to(file_path, repo_path),
              severity: :medium,
              reason: reason,
              action: :update_reference
            }
          ]
        else
          []
        end
      end)
    end)
  end

  # ─── SD008: Unsound formal verification patterns ───────────────────────
  #
  # Historically SD008 carried per-language patterns that were *also* picked
  # up by `Hypatia.Rules.CodeSafety.patterns_for_language/1` — every Idris,
  # Lean, Coq, Haskell, OCaml, Ada/SPARK pattern below was a duplicate of a
  # code_safety entry, so a single intentional `believe_me` would emit one
  # `code_safety/believe_me` finding AND one `structural_drift/SD008` finding
  # for the same line, inflating the count and forcing both into baselines.
  #
  # SD008 now defers to code_safety for any pattern code_safety already
  # covers. The list below holds only cross-language structural drift
  # patterns *not* otherwise reachable via the per-language pattern sets.
  # If you're adding a new per-language proof-bypass pattern, add it to
  # `lib/rules/code_safety.ex` instead.

  @unsound_patterns []

  @doc """
  SD008: Detect unsound formal verification patterns across all proof languages.
  These patterns bypass the guarantees the type/proof system provides.
  Severity: critical.
  Action: replace with proper proofs/implementations.
  Triggers: intensive scan + alert user.
  """
  def sd008_unsound_patterns(repo_path) do
    @unsound_patterns
    |> Enum.flat_map(fn {pattern, extensions, severity, reason} ->
      extensions
      |> Enum.flat_map(fn ext ->
        find_files_with_extension(repo_path, ext)
        |> Enum.flat_map(fn file_path ->
          content = File.read!(file_path)

          if Regex.match?(pattern, content) do
            # Count occurrences for severity escalation
            count = length(Regex.scan(pattern, content))

            [
              %{
                rule: "SD008",
                file: Path.relative_to(file_path, repo_path),
                severity: if(count > 3, do: :critical, else: severity),
                reason: "#{reason} (#{count} occurrence#{if count > 1, do: "s", else: ""})",
                action: :fix_proof,
                count: count,
                trigger_intensive: true,
                alert_user: true
              }
            ]
          else
            []
          end
        end)
      end)
    end)
  end

  # ─── SD009: Missing SPDX headers ──────────────────────────────────────

  @doc """
  SD009: Detect source files missing SPDX-License-Identifier header.
  Checks the first 5 lines of each source file.
  Severity: medium.
  Action: add appropriate SPDX header.
  """
  def sd009_missing_spdx(repo_path) do
    source_extensions = [
      ".idr",
      ".zig",
      ".rs",
      ".res",
      ".ex",
      ".exs",
      ".gleam",
      ".jl",
      ".ncl",
      ".hs",
      ".ads",
      ".adb",
      ".as",
      ".ml",
      ".lean"
    ]

    source_extensions
    |> Enum.flat_map(fn ext ->
      find_files_with_extension(repo_path, ext)
      # Sample -- don't scan every file
      |> Enum.take(5)
      |> Enum.flat_map(fn file_path ->
        case File.read(file_path) do
          {:ok, content} ->
            first_lines = content |> String.split("\n") |> Enum.take(5) |> Enum.join("\n")

            if String.contains?(first_lines, "SPDX-License-Identifier") do
              []
            else
              [
                %{
                  rule: "SD009",
                  file: Path.relative_to(file_path, repo_path),
                  severity: :medium,
                  reason: "Source file missing SPDX-License-Identifier header",
                  action: :add_spdx_header
                }
              ]
            end

          _ ->
            []
        end
      end)
    end)
  end

  # ─── SD010: Tracked node_modules ────────────────────────────────────────

  @doc """
  SD010: Detect node_modules/ directories that are tracked in git.
  These should ALWAYS be in .gitignore and never committed.
  Severity: high (bloats repo, security risk from vendored deps).
  Action: add to .gitignore, remove from tracking.
  """
  def sd010_tracked_node_modules(repo_path) do
    # Check if node_modules exists as a directory in the repo
    node_modules_path = Path.join(repo_path, "node_modules")

    if File.dir?(node_modules_path) do
      # Check if it's tracked by git (not in .gitignore)
      case System.cmd("git", ["ls-files", "node_modules"], cd: repo_path, stderr_to_stdout: true) do
        {output, 0} when output != "" ->
          tracked_count = output |> String.split("\n", trim: true) |> length()

          [
            %{
              rule: "SD010",
              file: "node_modules/",
              severity: :high,
              reason:
                "node_modules/ is tracked in git (#{tracked_count} files) -- must be in .gitignore",
              action: :untrack_and_gitignore,
              trigger_intensive: true
            }
          ]

        _ ->
          []
      end
    else
      # Also check for node_modules in subdirectories (monorepo packages)
      case System.cmd(
             "find",
             [
               repo_path,
               "-maxdepth",
               "3",
               "-type",
               "d",
               "-name",
               "node_modules",
               "-not",
               "-path",
               "*/.git/*"
             ],
             cd: repo_path,
             stderr_to_stdout: true
           ) do
        {output, 0} when output != "" ->
          output
          |> String.split("\n", trim: true)
          |> Enum.flat_map(fn dir ->
            case System.cmd("git", ["ls-files", Path.relative_to(dir, repo_path)],
                   cd: repo_path,
                   stderr_to_stdout: true
                 ) do
              {tracked, 0} when tracked != "" ->
                [
                  %{
                    rule: "SD010",
                    file: Path.relative_to(dir, repo_path) <> "/",
                    severity: :high,
                    reason: "Nested node_modules/ tracked in git -- must be in .gitignore",
                    action: :untrack_and_gitignore,
                    trigger_intensive: true
                  }
                ]

              _ ->
                []
            end
          end)

        _ ->
          []
      end
    end
  end

  # ─── SD011: Missing .gitignore entries ─────────────────────────────────

  @required_gitignore_patterns [
    {"node_modules", "node_modules/"},
    {"_build", "_build/"},
    {"target", "target/"},
    {"deps", "deps/"},
    {"zig-cache", "zig-cache/"},
    {"zig-out", "zig-out/"},
    {".lake", ".lake/"}
  ]

  @doc """
  SD011: Detect directories that exist but aren't in .gitignore.
  Severity: medium.
  Action: add to .gitignore.
  """
  def sd011_missing_gitignore(repo_path) do
    gitignore_path = Path.join(repo_path, ".gitignore")

    gitignore_content =
      case File.read(gitignore_path) do
        {:ok, content} -> content
        _ -> ""
      end

    @required_gitignore_patterns
    |> Enum.flat_map(fn {dir_name, pattern} ->
      dir_path = Path.join(repo_path, dir_name)

      if File.dir?(dir_path) and not String.contains?(gitignore_content, dir_name) do
        [
          %{
            rule: "SD011",
            file: ".gitignore",
            severity: :medium,
            reason: "#{dir_name}/ directory exists but is not in .gitignore -- add '#{pattern}'",
            action: :add_gitignore_entry
          }
        ]
      else
        []
      end
    end)
  end

  # ─── SD012: TOMBSTONE ────────────────────────────────────────────────
  # SD012 was never implemented; gap left between SD011 and SD013
  # during a prior renumbering. Documented as a tombstone (rather than
  # deleted from the sequence) per Hypatia audit 2026-05-28, Part 4.5:
  # reusing the SD012 slot for a fresh rule could collide with an
  # operator's mental model from logs. New rules should take SD014+.

  # ─── SD013: Path-specific gitignore patterns ───────────────────────────

  @path_specific_artifacts [
    {".zig-cache/", ~r|[^\s]*/.zig-cache/|},
    {"zig-out/", ~r|[^\s]*/zig-out/|},
    {"node_modules/", ~r|[^\s]*/node_modules/|},
    {"_build/", ~r|[^\s]*/_build/|},
    {"target/", ~r|[^\s]*/target/|}
  ]

  @doc """
  SD013: Detect path-specific gitignore patterns for build artifacts that
  should be global. For example, `src/path/.zig-cache/` should be `.zig-cache/`
  globally instead.
  Severity: low.
  Action: replace path-specific pattern with global pattern.
  """
  def sd013_path_specific_gitignore(repo_path) do
    gitignore_path = Path.join(repo_path, ".gitignore")

    case File.read(gitignore_path) do
      {:ok, content} ->
        @path_specific_artifacts
        |> Enum.flat_map(fn {global_pattern, path_regex} ->
          matches =
            content
            |> String.split("\n", trim: true)
            |> Enum.reject(&String.starts_with?(String.trim(&1), "#"))
            |> Enum.filter(&Regex.match?(path_regex, &1))

          Enum.map(matches, fn line ->
            %{
              rule: "SD013",
              file: ".gitignore",
              severity: :low,
              reason:
                "Path-specific gitignore pattern '#{String.trim(line)}' -- use global '#{global_pattern}' instead",
              action: :globalise_gitignore_pattern,
              detail: %{current: String.trim(line), recommended: global_pattern}
            }
          end)
        end)

      _ ->
        []
    end
  end

  # ─── SD014: SafeDOMExample dialect mismatch (.res lingers without .affine) ───

  @doc """
  SD014: Detect `examples/SafeDOMExample.res` lingering in repos that
  haven't picked up the canonical AffineScript version
  `examples/SafeDOMExample.affine` (lives in burble/main since the
  2026-05 ReScript→AffineScript migration).

  Three states:
    * `.res` only → `:fail` (dialect_mismatch) — repo carries the legacy
      ReScript variant with no AffineScript replacement.
    * Both `.res` and `.affine` → `:warn` (both_dialects) — delete the
      `.res` copy; the `.affine` is canonical.
    * `.affine` only or neither → no finding.

  Origin: dominant per-PR failure class on the otpiser#11 + 48-PR
  sweep was `governance / Language / package anti-pattern policy`
  firing on `examples/SafeDOMExample.res`. Template repos kept
  regenerating it; this rule catches the long tail. See
  hyperpolymath/hypatia#336.
  """
  def sd014_safedom_example_dialect(repo_path) do
    res_path = Path.join(repo_path, "examples/SafeDOMExample.res")
    affine_path = Path.join(repo_path, "examples/SafeDOMExample.affine")
    has_res = File.exists?(res_path)
    has_affine = File.exists?(affine_path)

    cond do
      has_res and not has_affine ->
        [
          %{
            rule: "SD014",
            type: :safedom_example_dialect_mismatch,
            file: "examples/SafeDOMExample.res",
            severity: :high,
            reason:
              "examples/SafeDOMExample.res lingers without the canonical " <>
                "AffineScript replacement examples/SafeDOMExample.affine. " <>
                "ReScript is banned in new code as of 2026-04-30 " <>
                "(estate policy); the canonical .affine version lives in " <>
                "burble/main. The governance/language-policy check fires " <>
                "on every push until the .res is replaced.",
            action: :replace_safedom_with_affine,
            trigger_intensive: false
          }
        ]

      has_res and has_affine ->
        [
          %{
            rule: "SD014",
            type: :safedom_example_both_dialects,
            file: "examples/SafeDOMExample.res",
            severity: :medium,
            reason:
              "Both examples/SafeDOMExample.res and " <>
                "examples/SafeDOMExample.affine are present. The .affine " <>
                "is canonical; delete the .res copy.",
            action: :delete_legacy_safedom_res,
            trigger_intensive: false
          }
        ]

      true ->
        []
    end
  end

  # ─── Comprehensive scan (triggered by any finding) ─────────────────────

  @doc """
  Run ALL structural drift checks on a repo.
  If any finding has trigger_intensive: true, escalates to echidnabot
  for deep semantic analysis of the entire repo.
  """
  def scan(repo_path) do
    findings =
      sd001_legacy_scm(repo_path) ++
        sd002_trustfile_hs(repo_path) ++
        sd003_ai_djot(repo_path) ++
        sd004_misplaced_a2ml(repo_path) ++
        sd005_orphan_gitlinks(repo_path) ++
        sd006_trustfile_examples(repo_path) ++
        sd007_stale_references(repo_path) ++
        sd008_unsound_patterns(repo_path) ++
        sd009_missing_spdx(repo_path) ++
        sd010_tracked_node_modules(repo_path) ++
        sd011_missing_gitignore(repo_path) ++
        sd013_path_specific_gitignore(repo_path) ++
        sd014_safedom_example_dialect(repo_path) ++
        sd022_stale_path_after_rename(repo_path) ++
        sd023_state_a2ml_divergence(repo_path)

    needs_intensive = Enum.any?(findings, & &1[:trigger_intensive])
    needs_alert = Enum.any?(findings, & &1[:alert_user])

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      trigger_intensive: needs_intensive,
      alert_user: needs_alert,
      dispatch: determine_dispatch(findings)
    }
  end

  # ─── Helpers ───────────────────────────────────────────────────────────

  defp group_by_severity(findings) do
    findings
    |> Enum.group_by(& &1.severity)
    |> Enum.map(fn {sev, items} -> {sev, length(items)} end)
    |> Map.new()
  end

  defp determine_dispatch(findings) do
    cond do
      Enum.any?(findings, &(&1.severity == :critical)) ->
        [:rhodibot, :echidnabot, :panicbot]

      Enum.any?(findings, &(&1.severity == :high)) ->
        [:rhodibot, :echidnabot]

      Enum.any?(findings, &(&1.severity == :medium)) ->
        [:rhodibot]

      true ->
        [:sustainabot]
    end
  end

  defp find_files(repo_path, filename) do
    case System.cmd(
           "find",
           [repo_path, "-maxdepth", "4", "-name", filename, "-not", "-path", "*/.git/*"],
           stderr_to_stdout: true
         ) do
      {output, 0} -> output |> String.split("\n", trim: true)
      _ -> []
    end
  end

  defp find_files_matching(repo_path, regex) do
    case System.cmd(
           "find",
           [
             repo_path,
             "-maxdepth",
             "4",
             "-type",
             "f",
             "-not",
             "-path",
             "*/.git/*",
             "-not",
             "-path",
             "*/node_modules/*",
             "-not",
             "-path",
             "*/target/*",
             "-not",
             "-path",
             "*/_build/*"
           ],
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        output
        |> String.split("\n", trim: true)
        |> Enum.filter(fn path -> Regex.match?(regex, Path.basename(path)) end)

      _ ->
        []
    end
  end

  defp find_files_with_extension(repo_path, ext) do
    src_dirs =
      [
        Path.join(repo_path, "src"),
        Path.join(repo_path, "lib"),
        Path.join(repo_path, "ffi")
      ]
      |> Enum.filter(&File.dir?/1)

    case src_dirs do
      [] ->
        []

      dirs ->
        dirs
        |> Enum.flat_map(fn dir ->
          case System.cmd(
                 "find",
                 [
                   dir,
                   "-maxdepth",
                   "5",
                   "-name",
                   "*#{ext}",
                   "-not",
                   "-path",
                   "*/.git/*",
                   "-not",
                   "-path",
                   "*/node_modules/*",
                   "-not",
                   "-path",
                   "*/target/*",
                   "-not",
                   "-path",
                   "*/_build/*",
                   "-not",
                   "-path",
                   "*/deps/*"
                 ],
                 stderr_to_stdout: true
               ) do
            {output, 0} -> output |> String.split("\n", trim: true)
            _ -> []
          end
        end)
    end
  end

  # ─── SD021: Workflow trigger references a non-existent branch (#363) ───
  #
  # `on.push.branches` / `on.pull_request.branches` entries that don't
  # resolve to a real branch are dead config (consolidation drift). Globs
  # and the default branch are exempt. The actual-branch list is injected by
  # the caller (git index / GitHub API). Cohort hypatia#333, pattern 4.
  @doc """
  SD021: flag workflow trigger branch refs that aren't real branches.

  `actual_branches` is the repo's live branch list (injected). Glob patterns
  and `opts[:default_branch]` (default `"main"`) are exempt. Returns one
  finding per (file, dead-branch).
  """
  def check_workflow_branch_refs(workflow_contents, actual_branches, opts \\ []) do
    default = Keyword.get(opts, :default_branch, "main")

    Enum.flat_map(workflow_contents, fn {file, content} ->
      content
      |> trigger_branches()
      |> Enum.filter(fn b ->
        b != default and not String.contains?(b, "*") and b not in actual_branches
      end)
      |> Enum.uniq()
      |> Enum.map(fn b ->
        %{
          rule: "SD021",
          file: file,
          severity: :low,
          reason:
            "workflow trigger references branch `#{b}`, which is not a real branch in this repo (dead config / consolidation drift)",
          action: :update_reference,
          branch: b
        }
      end)
    end)
  end

  defp trigger_branches(content) do
    inline =
      ~r/branches(?:-ignore)?:\s*\[([^\]]+)\]/
      |> Regex.scan(content)
      |> Enum.flat_map(fn [_, inner] -> String.split(inner, ",") end)
      |> Enum.map(&(&1 |> String.trim() |> String.trim("\"") |> String.trim("'")))

    block = content |> String.split("\n") |> branch_block_items(false, [])
    Enum.reject(inline ++ block, &(&1 == ""))
  end

  defp branch_block_items([], _in?, acc), do: Enum.reverse(acc)

  defp branch_block_items([line | rest], in?, acc) do
    item = Regex.run(~r/^\s*-\s*['"]?([A-Za-z0-9._\/*-]+)['"]?\s*$/, line)

    cond do
      Regex.match?(~r/^\s*branches(?:-ignore)?:\s*$/, line) -> branch_block_items(rest, true, acc)
      in? and Regex.match?(~r/^\s*#/, line) -> branch_block_items(rest, true, acc)
      in? and item != nil -> branch_block_items(rest, true, [Enum.at(item, 1) | acc])
      true -> branch_block_items(rest, false, acc)
    end
  end

  # ─── SD022: Stale path references after directory rename ───────────────
  #
  # When a source directory is renamed (e.g. `src/ephapax/` → `src/paint_core/`
  # in a single commit), trailing-edge documentation references frequently
  # outlive the rename. This rule scans docs for `src/<dir>/` references and
  # flags any whose `<dir>` is NOT a real directory in the current tree.
  #
  # Discovered on JoshuaJewell/paint-type 2026-06-02: PR #48 renamed
  # src/ephapax → src/paint_core in the Cargo workspace; 25 docs (49
  # occurrences) still pointed at the old path. Caught by manual sweep
  # in PR #49. This rule prevents the next recurrence.
  #
  # Exemption: CHANGELOG.md (historical references are documentation, not
  # drift) and anything under `third_party/` (vendored).

  @doc """
  SD022: Detect documentation that references a `src/<dir>/` path
  whose `<dir>` is not a real directory in the current tree.

  Severity: medium (doc-only; doesn't break the build, but misleads readers).
  Action: sed sweep `s|src/<stale-dir>|src/<new-dir>|g` once the rename
  target is identified (typically via `git log --diff-filter=R`).
  Triggers: intensive scan (where one rename-drift hits, others follow).
  """
  def sd022_stale_path_after_rename(repo_path) do
    # `real_basenames` = every <name> for which a directory `**/src/<name>`
    # (or root `src/<name>`) exists ANYWHERE in the tree. A `src/<dir>/`
    # reference that resolves to such a directory is real — it was merely
    # written unanchored (crate-relative `scripts/x/src/bin`, doc-relative
    # `ffi/zig` → `src/connectors`, or prefixed `cli/src/commands`). Only a
    # `<dir>` that exists nowhere is a drift candidate.
    {real_basenames, top_dirs} = src_dir_index(repo_path)

    if MapSet.size(real_basenames) == 0 do
      []
    else
      # Corpus / vendored / historical docs cite paths that are illustrative by
      # nature (training subjects, foreign examples), not this tree's layout.
      # Mirror the universal exemptions in lib/hypatia/scanner_suppression.ex.
      corpus_prefixes = [
        ".audittraining/",
        "test/",
        "tests/",
        "integration/fixtures/",
        "test/fixtures/",
        "tests/fixtures/",
        "scripts/fix-scripts/",
        "third_party/"
      ]

      find_files_by_ext(repo_path, [
        ".md",
        ".adoc",
        ".txt",
        ".a2ml",
        ".contractile",
        ".toml",
        ".twasm"
      ])
      |> Enum.reject(fn rel ->
        rel == "CHANGELOG.md" or Enum.any?(corpus_prefixes, &String.starts_with?(rel, &1))
      end)
      |> Enum.flat_map(fn rel ->
        path = Path.join(repo_path, rel)

        case File.read(path) do
          {:ok, content} ->
            # Negative lookbehind: only a *repo-root-relative* `src/<dir>/`
            # is rename-drift. A `src/` preceded by another path segment
            # (`vcl-ut/src/bridges/`, `cli/src/commands/`, `echidna/src/rust/`,
            # `integration/src/ci_simulation/`, `examples/nestjs/src/i18n/`)
            # belongs to that other tree/crate/repo, not this repo's `src/`.
            ~r{(?<![\w./-])src/([A-Za-z0-9_][A-Za-z0-9_-]*)/}
            |> Regex.scan(content)
            |> Enum.map(fn [_, prefix, dir] -> {prefix, dir} end)
            |> Enum.reject(fn {prefix, dir} ->
              real_path_reference?(prefix, dir, real_basenames, top_dirs)
            end)
            |> Enum.map(fn {_prefix, dir} -> dir end)
            |> Enum.uniq()
            |> Enum.reject(fn dir ->
              # Real repo-root `src/<dir>/`, OR a crate-/module-relative
              # reference that resolves against the referencing file's OWN
              # directory (e.g. a `scripts/<crate>/Cargo.toml` declaring
              # `path = "src/bin/…"`, or `ffi/zig/README.adoc` describing
              # its sibling `src/connectors/`). Only a reference that
              # resolves NOWHERE is genuine post-rename drift.
              MapSet.member?(real_subdirs, dir) or
                File.dir?(Path.join([repo_path, Path.dirname(rel), "src", dir]))
            end)
            |> Enum.map(fn stale_dir ->
              %{
                rule: "SD022",
                file: rel,
                severity: :medium,
                reason:
                  "doc references `src/#{stale_dir}/` but no such directory exists in the tree (likely surviving a directory rename)",
                action: :rename_sweep,
                stale_dir: stale_dir,
                trigger_intensive: true
              }
            end)

          _ ->
            []
        end
      end)
    end
  end

  # A `src/<dir>/` reference is REAL (not drift) when either:
  #   * <dir> exists as a `**/src/<dir>` directory anywhere in the tree
  #     (resolves regardless of how it was anchored), or
  #   * it carries a path prefix whose leading segment is NOT a real
  #     top-level directory of this repo — i.e. it cites another repo or an
  #     example project (`vcl-ut/src/core`, `examples/nestjs/src/i18n`).
  defp real_path_reference?(prefix, dir, real_basenames, top_dirs) do
    MapSet.member?(real_basenames, dir) or foreign_prefix?(prefix, top_dirs)
  end

  defp foreign_prefix?("", _top_dirs), do: false

  defp foreign_prefix?(prefix, top_dirs) do
    first = prefix |> String.trim_trailing("/") |> String.split("/") |> List.first()
    first != nil and not MapSet.member?(top_dirs, first)
  end

  # Index every `**/src/<name>` directory in the tracked tree (via the file
  # list, so it respects .gitignore and skips _build/deps), returning the set
  # of <name> basenames plus the set of real top-level directory names.
  defp src_dir_index(repo_path) do
    files =
      case System.cmd("git", ["-C", repo_path, "ls-files"], stderr_to_stdout: true) do
        {output, 0} -> String.split(output, "\n", trim: true)
        _ -> []
      end

    Enum.reduce(files, {MapSet.new(), MapSet.new()}, fn rel, {bases, tops} ->
      segs = String.split(rel, "/")

      tops =
        case segs do
          [_single] -> tops
          [top | _] -> MapSet.put(tops, top)
          _ -> tops
        end

      bases =
        segs
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.reduce(bases, fn
          ["src", name], acc -> MapSet.put(acc, name)
          _, acc -> acc
        end)

      {bases, tops}
    end)
  end

  # ─── SD023: STATE.a2ml divergence (top-level vs 6a2/) ──────────────────
  #
  # The estate v2 convention puts STATE at `.machine_readable/6a2/STATE.a2ml`.
  # Some repos retain a legacy top-level `.machine_readable/STATE.a2ml`. When
  # both exist, they MUST agree on the `last-updated` field — otherwise one
  # is stale and consumers (Hypatia, agents reading 6a2) see the wrong reality.
  #
  # Discovered on JoshuaJewell/paint-type 2026-06-02: top-level STATE.a2ml
  # was 2026-06-01 with 22% completion while 6a2/STATE.a2ml was 2026-05-11
  # with 10% completion. Caught by manual sweep; PR #49 unified them.

  @doc """
  SD023: Detect divergence between `.machine_readable/STATE.a2ml` and
  `.machine_readable/6a2/STATE.a2ml` when both exist.

  Severity: medium (one is stale; consumers may read either).
  Action: pick the freshest as truth, mirror to the other, document
  in CHANGELOG which is canonical going forward.
  """
  def sd023_state_a2ml_divergence(repo_path) do
    top = Path.join([repo_path, ".machine_readable", "STATE.a2ml"])
    six = Path.join([repo_path, ".machine_readable", "6a2", "STATE.a2ml"])

    with true <- File.exists?(top),
         true <- File.exists?(six),
         {:ok, top_content} <- File.read(top),
         {:ok, six_content} <- File.read(six) do
      top_date = extract_last_updated(top_content)
      six_date = extract_last_updated(six_content)

      cond do
        top_date == nil or six_date == nil ->
          []

        top_date == six_date ->
          []

        true ->
          [
            %{
              rule: "SD023",
              file: ".machine_readable/STATE.a2ml + .machine_readable/6a2/STATE.a2ml",
              severity: :medium,
              reason:
                "STATE.a2ml divergence: top-level last-updated=#{top_date}, 6a2/ last-updated=#{six_date}. One is stale; consumers may read either.",
              action: :unify_state,
              top_last_updated: top_date,
              six_last_updated: six_date,
              trigger_intensive: false
            }
          ]
      end
    else
      _ -> []
    end
  end

  defp extract_last_updated(content) do
    # Matches both TOML (`last-updated = "2026-06-02"`) and Scheme
    # (`(last-updated "2026-06-02")`) variants.
    case Regex.run(~r/last[-_]updated\s*[=\s]\s*"([^"]+)"/, content) do
      [_, date] -> date
      _ -> nil
    end
  end

  defp find_files_by_ext(repo_path, exts) do
    case System.cmd("git", ["-C", repo_path, "ls-files"], stderr_to_stdout: true) do
      {output, 0} ->
        output
        |> String.split("\n", trim: true)
        |> Enum.filter(fn rel -> Path.extname(rel) in exts end)

      _ ->
        []
    end
  end
end
