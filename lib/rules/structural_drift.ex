# SPDX-License-Identifier: PMPL-1.0-or-later
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
    "STATE.scm", "META.scm", "ECOSYSTEM.scm",
    "AGENTIC.scm", "NEUROSYM.scm", "PLAYBOOK.scm", "LANGUAGES.scm"
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
      [] -> []
      files ->
        Enum.map(files, fn path ->
          %{
            rule: "SD002",
            file: Path.relative_to(path, repo_path),
            severity: :high,
            reason: "Legacy Trustfile.hs -- must be Trustfile.a2ml in .machine_readable/contractiles/trust/",
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
      [] -> []
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
    "STATE.a2ml", "META.a2ml", "ECOSYSTEM.a2ml",
    "AGENTIC.a2ml", "NEUROSYM.a2ml", "PLAYBOOK.a2ml"
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

  @doc """
  SD005: Detect gitlinks (mode 160000) without corresponding .gitmodules entry.
  This is how stray repos-inside-repos (like svalinn in ats2-tui) happen.
  Bot `git add -A` catches stray clones and creates orphan submodule refs.
  Severity: critical (data corruption risk).
  Action: investigate -- move nested repo to canonical location, remove gitlink.
  Triggers: intensive scan + alert user.
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
          gitmodules_content =
            if has_gitmodules, do: File.read!(gitmodules_path), else: ""

          Enum.flat_map(gitlinks, fn path ->
            if String.contains?(gitmodules_content, "path = #{path}") do
              []  # Legitimate submodule
            else
              [%{
                rule: "SD005",
                file: path,
                severity: :critical,
                reason: "Orphan gitlink -- submodule ref without .gitmodules entry. Likely a stray clone caught by bot git-add-all.",
                action: :investigate,
                trigger_intensive: true,
                alert_user: true
              }]
            end
          end)
        end

      _ -> []
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
    {~r/ECOSYSTEM\.scm/, "References ECOSYSTEM.scm -- should be .machine_readable/6a2/ECOSYSTEM.a2ml"},
    {~r/AGENTIC\.scm/, "References AGENTIC.scm -- should be .machine_readable/6a2/AGENTIC.a2ml"},
    {~r/NEUROSYM\.scm/, "References NEUROSYM.scm -- should be .machine_readable/6a2/NEUROSYM.a2ml"},
    {~r/PLAYBOOK\.scm/, "References PLAYBOOK.scm -- should be .machine_readable/6a2/PLAYBOOK.a2ml"},
    {~r/AI\.djot/, "References AI.djot -- file has been superseded by 0-AI-MANIFEST.a2ml"},
    {~r/Trustfile\.hs/, "References Trustfile.hs -- should be Trustfile.a2ml"},
    {~r/\.machine_readable\/STATE\.a2ml/, "References .machine_readable/STATE.a2ml -- should be .machine_readable/6a2/STATE.a2ml"},
    {~r/\.machine_readable\/META\.a2ml/, "References .machine_readable/META.a2ml -- should be .machine_readable/6a2/META.a2ml"}
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
          [%{
            rule: "SD007",
            file: Path.relative_to(file_path, repo_path),
            severity: :medium,
            reason: reason,
            action: :update_reference
          }]
        else
          []
        end
      end)
    end)
  end

  # ─── SD008: Unsound formal verification patterns ───────────────────────

  @unsound_patterns [
    {~r/believe_me/, [".idr"], :critical, "Idris2 believe_me -- bypasses type checker"},
    {~r/assert_total/, [".idr"], :critical, "Idris2 assert_total -- hides non-termination"},
    {~r/\bsorry\b/, [".idr", ".lean"], :critical, "Incomplete proof (sorry) -- admits unproven goal"},
    {~r/\bAdmitted\b/, [".v"], :critical, "Coq Admitted -- incomplete proof"},
    {~r/unsafeCoerce/, [".hs"], :critical, "Haskell unsafeCoerce -- bypasses type system"},
    {~r/Obj\.magic/, [".ml"], :critical, "OCaml Obj.magic -- bypasses type system"},
    {~r/pragma\s+Suppress/, [".ads", ".adb"], :high, "Ada pragma Suppress -- disables runtime checks"}
  ]

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
          # Strip inline-directive lines for SD008 (mirrors the
          # code_safety scanner's line filter) so a comment that names
          # the unsound primitive in order to declare it accepted
          # doesn't itself trigger the rule.
          filtered = strip_inline_ignored_lines(content, "SD008")

          if Regex.match?(pattern, filtered) do
            # Count occurrences for severity escalation
            count = length(Regex.scan(pattern, filtered))

            [%{
              rule: "SD008",
              file: Path.relative_to(file_path, repo_path),
              severity: if(count > 3, do: :critical, else: severity),
              reason: "#{reason} (#{count} occurrence#{if count > 1, do: "s", else: ""})",
              action: :fix_proof,
              count: count,
              trigger_intensive: true,
              alert_user: true
            }]
          else
            []
          end
        end)
      end)
    end)
  end

  defp strip_inline_ignored_lines(content, rule_id) do
    # Permissive form: the directive lists `hypatia:ignore` followed by
    # one or more rule names, anywhere later on the same line. A line
    # is ignored for `rule_id` if its directive lists that rule (as a
    # whole word) somewhere after the marker.
    directive_re = ~r/hypatia:ignore\b.*\b#{Regex.escape(rule_id)}\b/

    content
    |> String.split(~r/\r?\n/)
    |> Enum.reject(&Regex.match?(directive_re, &1))
    |> Enum.join("\n")
  end

  # ─── SD009: Missing SPDX headers ──────────────────────────────────────

  @doc """
  SD009: Detect source files missing SPDX-License-Identifier header.
  Checks the first 5 lines of each source file.
  Severity: medium.
  Action: add appropriate SPDX header.
  """
  def sd009_missing_spdx(repo_path) do
    source_extensions = [".idr", ".zig", ".rs", ".res", ".ex", ".exs",
                         ".gleam", ".jl", ".ncl", ".hs", ".ads", ".adb",
                         ".as", ".ml", ".lean"]

    source_extensions
    |> Enum.flat_map(fn ext ->
      find_files_with_extension(repo_path, ext)
      |> Enum.take(5)  # Sample -- don't scan every file
      |> Enum.flat_map(fn file_path ->
        case File.read(file_path) do
          {:ok, content} ->
            first_lines = content |> String.split("\n") |> Enum.take(5) |> Enum.join("\n")
            if String.contains?(first_lines, "SPDX-License-Identifier") do
              []
            else
              [%{
                rule: "SD009",
                file: Path.relative_to(file_path, repo_path),
                severity: :medium,
                reason: "Source file missing SPDX-License-Identifier header",
                action: :add_spdx_header
              }]
            end

          _ -> []
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
          tracked_count =
            output |> String.split("\n", trim: true) |> length()

          [%{
            rule: "SD010",
            file: "node_modules/",
            severity: :high,
            reason: "node_modules/ is tracked in git (#{tracked_count} files) -- must be in .gitignore",
            action: :untrack_and_gitignore,
            trigger_intensive: true
          }]

        _ -> []
      end
    else
      # Also check for node_modules in subdirectories (monorepo packages)
      case System.cmd("find", [repo_path, "-maxdepth", "3", "-type", "d",
                               "-name", "node_modules",
                               "-not", "-path", "*/.git/*"],
                      cd: repo_path, stderr_to_stdout: true) do
        {output, 0} when output != "" ->
          output
          |> String.split("\n", trim: true)
          |> Enum.flat_map(fn dir ->
            case System.cmd("git", ["ls-files", Path.relative_to(dir, repo_path)],
                           cd: repo_path, stderr_to_stdout: true) do
              {tracked, 0} when tracked != "" ->
                [%{
                  rule: "SD010",
                  file: Path.relative_to(dir, repo_path) <> "/",
                  severity: :high,
                  reason: "Nested node_modules/ tracked in git -- must be in .gitignore",
                  action: :untrack_and_gitignore,
                  trigger_intensive: true
                }]

              _ -> []
            end
          end)

        _ -> []
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
        [%{
          rule: "SD011",
          file: ".gitignore",
          severity: :medium,
          reason: "#{dir_name}/ directory exists but is not in .gitignore -- add '#{pattern}'",
          action: :add_gitignore_entry
        }]
      else
        []
      end
    end)
  end

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
              reason: "Path-specific gitignore pattern '#{String.trim(line)}' -- use global '#{global_pattern}' instead",
              action: :globalise_gitignore_pattern,
              detail: %{current: String.trim(line), recommended: global_pattern}
            }
          end)
        end)

      _ -> []
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
      sd013_path_specific_gitignore(repo_path)

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
    case System.cmd("find", [repo_path, "-maxdepth", "4", "-name", filename,
                             "-not", "-path", "*/.git/*"], stderr_to_stdout: true) do
      {output, 0} -> output |> String.split("\n", trim: true)
      _ -> []
    end
  end

  defp find_files_matching(repo_path, regex) do
    case System.cmd("find", [repo_path, "-maxdepth", "4", "-type", "f",
                             "-not", "-path", "*/.git/*",
                             "-not", "-path", "*/node_modules/*",
                             "-not", "-path", "*/target/*",
                             "-not", "-path", "*/_build/*"], stderr_to_stdout: true) do
      {output, 0} ->
        output
        |> String.split("\n", trim: true)
        |> Enum.filter(fn path -> Regex.match?(regex, Path.basename(path)) end)

      _ -> []
    end
  end

  defp find_files_with_extension(repo_path, ext) do
    src_dirs = [
      Path.join(repo_path, "src"),
      Path.join(repo_path, "lib"),
      Path.join(repo_path, "ffi")
    ]
    |> Enum.filter(&File.dir?/1)

    case src_dirs do
      [] -> []
      dirs ->
        name_glob = "*" <> ext

        dirs
        |> Enum.flat_map(fn dir ->
          case System.cmd("find", [dir, "-maxdepth", "5", "-name", name_glob,
                                   "-not", "-path", "*/.git/*",
                                   "-not", "-path", "*/node_modules/*",
                                   "-not", "-path", "*/target/*",
                                   "-not", "-path", "*/_build/*",
                                   "-not", "-path", "*/deps/*"],
                          stderr_to_stdout: true) do
            {output, 0} -> output |> String.split("\n", trim: true)
            _ -> []
          end
        end)
    end
  end
end
