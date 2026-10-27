# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.RootHygiene do
  @moduledoc """
  Root directory hygiene enforcement rules.

  Scans a repository root against allowed/banned file lists to flag stale
  snapshots, executed plans, duplicate docs, wrong file names, and structural
  drift from the RSR template.

  Dispatches to:
  - rhodibot: auto-fix formatting/metadata
  - finishbot: PRs for moves/deletes
  - seambot: verify structural changes
  """

  # ─── Allowed root files (RSR template standard) ────────────────────────

  @allowed_root_files [
    "0-AI-MANIFEST.a2ml",
    "AI.a2ml",
    # Estate governance/philosophy chrome. Added 2026-08-07: these four are
    # authored, referenced from README and docs/README.adoc, and were making
    # hypatia fail its own root-hygiene rule.
    "AFFIRMATION.adoc",
    "GOVERNANCE.adoc",
    "PALIMPSEST.adoc",
    "CHANGELOG.adoc",
    "README.adoc",
    "README.md",
    "LICENSE",
    "LICENSE.txt",
    "LICENSE-MPL-2.0",
    "LICENSE-MPL-2.0.txt",
    "SECURITY.md",
    "CONTRIBUTING.md",
    "CODE_OF_CONDUCT.md",
    "CHANGELOG.md",
    "MAINTAINERS.adoc",
    "TOPOLOGY.md",
    "ROADMAP.adoc",
    "ROADMAP.md",
    "NOTICE",
    "Justfile",
    "justfile",
    "Cargo.toml",
    "Cargo.lock",
    "mix.exs",
    "mix.lock",
    "deno.json",
    "deno.lock",
    "gleam.toml",
    "rebar.config",
    "build.zig",
    "build.zig.zon",
    "Project.toml",
    "Manifest.toml",
    "boj.ipkg",
    ".editorconfig",
    ".gitignore",
    ".gitattributes",
    ".gitlab-ci.yml",
    ".nojekyll",
    ".prettierrc",
    ".prettierignore",
    "Mustfile",
    "opsm.toml",
    "contractile.just"
  ]

  # ─── Banned root files ─────────────────────────────────────────────────

  @banned_root_files [
    %{
      pattern: "AI.djot",
      reason: "Superseded by 0-AI-MANIFEST.a2ml",
      severity: :high,
      action: :delete
    },
    %{
      pattern: "STATE.scm",
      reason: "Must be in .machine_readable/ only",
      severity: :critical,
      action: :move
    },
    %{
      pattern: "META.scm",
      reason: "Must be in .machine_readable/ only",
      severity: :critical,
      action: :move
    },
    %{
      pattern: "ECOSYSTEM.scm",
      reason: "Must be in .machine_readable/ only",
      severity: :critical,
      action: :move
    },
    %{
      pattern: "AGENTIC.scm",
      reason: "Must be in .machine_readable/ only",
      severity: :critical,
      action: :move
    },
    %{
      pattern: "NEUROSYM.scm",
      reason: "Must be in .machine_readable/ only",
      severity: :critical,
      action: :move
    },
    %{
      pattern: "PLAYBOOK.scm",
      reason: "Must be in .machine_readable/ only",
      severity: :critical,
      action: :move
    },
    %{
      pattern: "LANGUAGES.scm",
      reason: "Must be in .machine_readable/ only",
      severity: :critical,
      action: :move
    },
    # Docker is permitted estate-wide; Podman/Containerfile is HIGHLY PREFERRED
    # but not mandatory. These are low-severity advisories (do not fail the
    # critical/high gate), nudging Containerfile naming unless a specific tool
    # genuinely needs the Docker-* name.
    %{
      pattern: "Dockerfile",
      reason:
        "Containerfile preferred over Dockerfile (Podman highly preferred; Docker permitted) -- rename unless a tool requires the Dockerfile name",
      severity: :low,
      action: :rename
    },
    %{
      pattern: "docker-compose.yml",
      reason: "compose.yml / podman-compose highly preferred (Docker permitted)",
      severity: :low,
      action: :rename
    },
    %{
      pattern: "docker-compose.yaml",
      reason: "compose.yml / podman-compose highly preferred (Docker permitted)",
      severity: :low,
      action: :rename
    },
    %{pattern: "Makefile", reason: "Use Justfile", severity: :medium, action: :replace},
    # JS runtime order is Bun > pnpm > npm; Deno is banned (2026-09-22).
    # Source: standards docs/JS-RUNTIME-POLICY.adoc "Hard Rules". Bun
    # lockfiles (bun.lock / bun.lockb) are permitted and expected, so they
    # are deliberately absent here: this list used to mark bun.lockb (and
    # pnpm-lock.yaml) high/delete and say "use Deno", the inverse of policy.
    %{
      pattern: "package-lock.json",
      reason: "npm lockfile must not be tracked -- use Bun (`bun install`, bun.lock)",
      severity: :high,
      action: :delete
    },
    %{pattern: "yarn.lock", reason: "Yarn banned -- use Bun", severity: :high, action: :delete},
    %{
      pattern: "pnpm-lock.yaml",
      reason:
        "pnpm is tier 2 -- permitted only where an upstream toolchain needs a " <>
          "node_modules layout; prefer Bun",
      severity: :low,
      action: :flag
    },
    %{
      pattern: ".npmrc",
      reason: "npm config must not be tracked -- use Bun",
      severity: :medium,
      action: :delete
    },
    %{
      pattern: "tsconfig.json",
      reason: "TypeScript banned -- use AffineScript",
      severity: :high,
      action: :flag
    },
    %{pattern: "go.mod", reason: "Go banned -- use Rust", severity: :high, action: :flag},
    %{pattern: "go.sum", reason: "Go banned -- use Rust", severity: :high, action: :flag},
    %{
      pattern: "AI.a2ml",
      reason: "Stray AI.a2ml in root -- use 0-AI-MANIFEST.a2ml only",
      severity: :high,
      action: :delete
    },
    %{
      pattern: "Trustfile.hs",
      reason: "Legacy Haskell Trustfile -- use Trustfile.a2ml",
      severity: :high,
      action: :convert
    },
    %{
      pattern: "AI.djot",
      reason: "Superseded by 0-AI-MANIFEST.a2ml",
      severity: :high,
      action: :delete
    }
  ]

  # ─── Stale file patterns ───────────────────────────────────────────────

  @stale_patterns [
    %{
      regex: ~r/^PLAN-.*\.md$/i,
      reason: "Executed plan left in root -- move to docs/plans/",
      severity: :low,
      action: :move
    },
    %{
      regex: ~r/^SNAPSHOT-.*\.md$/i,
      reason: "Stale snapshot in root -- move to docs/ or delete",
      severity: :low,
      action: :move
    },
    %{
      regex: ~r/^TODO-.*\.md$/i,
      reason: "TODO file in root -- move to docs/",
      severity: :low,
      action: :move
    },
    %{
      regex: ~r/^SESSION-.*\.md$/i,
      reason: "Session log in root -- move to docs/sessions/",
      severity: :low,
      action: :move
    },
    %{
      regex: ~r/^DESIGN-.*\.md$/i,
      reason: "Design doc in root -- move to docs/design/",
      severity: :low,
      action: :move
    },
    %{
      regex: ~r/^.*-STATUS-REPORT-.*\.md$/i,
      reason: "Status report in root -- move to docs/",
      severity: :low,
      action: :move
    },
    %{regex: ~r/^.*\.bak$/i, reason: "Backup file in root", severity: :medium, action: :delete},
    %{
      regex: ~r/^.*\.tmp$/i,
      reason: "Temporary file in root",
      severity: :medium,
      action: :delete
    },
    %{
      regex: ~r/^.*\.orig$/i,
      reason: "Merge artifact in root",
      severity: :medium,
      action: :delete
    },
    %{
      regex: ~r/^SONNET-TASKS\.md$/i,
      reason: "Stale AI task file -- delete or move to docs/",
      severity: :high,
      action: :delete
    },
    %{
      regex: ~r/^GEMINI\.md$/i,
      reason: "Stale AI session file -- delete",
      severity: :medium,
      action: :delete
    },
    %{
      regex: ~r/^CLAUDE-WORK.*\.md$/i,
      reason: "AI work file in repo root -- should not be committed",
      severity: :medium,
      action: :delete
    }
  ]

  def allowed_root_files, do: @allowed_root_files
  def banned_root_files, do: @banned_root_files
  def stale_patterns, do: @stale_patterns

  # ─── Scan functions ────────────────────────────────────────────────────

  @doc """
  Scan a list of root file names and return findings.
  """
  def scan(root_files) when is_list(root_files) do
    banned = scan_banned(root_files)
    stale = scan_stale(root_files)
    missing = scan_required_missing(root_files)

    %{
      findings: banned ++ stale ++ missing,
      banned_count: length(banned),
      stale_count: length(stale),
      missing_count: length(missing),
      total: length(banned) + length(stale) + length(missing)
    }
  end

  @doc """
  Scan root file names with access to their repository root.

  This form recognises a maintained `GEMINI.md` instruction pointer. The
  filename-only form above remains conservative because it cannot establish
  whether the pointer's target exists or is safe to follow.
  """
  def scan(root_files, repo_path) when is_list(root_files) and is_binary(repo_path) do
    banned = scan_banned(root_files)
    stale = scan_stale(root_files, repo_path)

    missing =
      scan_required_missing(root_files, repo_path |> Path.expand() |> Path.basename(),
        reuse_licenses: reuse_licenses?(repo_path)
      )

    %{
      findings: banned ++ stale ++ missing,
      banned_count: length(banned),
      stale_count: length(stale),
      missing_count: length(missing),
      total: length(banned) + length(stale) + length(missing)
    }
  end

  @doc """
  Check root files against the banned list.
  """
  def scan_banned(root_files) do
    Enum.flat_map(root_files, fn file ->
      case Enum.find(@banned_root_files, fn rule -> rule.pattern == file end) do
        nil ->
          []

        rule ->
          [
            %{
              file: file,
              reason: rule.reason,
              severity: rule.severity,
              action: rule.action,
              type: :banned
            }
          ]
      end
    end)
  end

  @doc """
  Check root files against stale patterns (regex-based).
  """
  def scan_stale(root_files) do
    Enum.flat_map(root_files, fn file ->
      case Enum.find(@stale_patterns, fn rule -> Regex.match?(rule.regex, file) end) do
        nil ->
          []

        rule ->
          [
            %{
              file: file,
              reason: rule.reason,
              severity: rule.severity,
              action: rule.action,
              type: :stale
            }
          ]
      end
    end)
  end

  @doc """
  Check stale root files with repository context.

  A root `GEMINI.md` is retained only when it points to an existing,
  non-circular root instruction file: `AGENTS.md`, or `CLAUDE.md` while the
  repository has not yet adopted `AGENTS.md`.
  """
  def scan_stale(root_files, repo_path) when is_list(root_files) and is_binary(repo_path) do
    root_files
    |> scan_stale()
    |> Enum.reject(fn finding ->
      finding.file == "GEMINI.md" and valid_gemini_instruction_pointer?(repo_path)
    end)
  end

  defp valid_gemini_instruction_pointer?(repo_path) do
    gemini_path = Path.join(repo_path, "GEMINI.md")

    with {:ok, content} <- File.read(gemini_path),
         {:ok, target} <- gemini_pointer_target(content),
         true <- permitted_gemini_target?(repo_path, target),
         true <- maintained_instruction_target?(repo_path, target) do
      true
    else
      _ -> false
    end
  end

  defp gemini_pointer_target(content) do
    targets = root_instruction_link_targets(content)

    cond do
      "AGENTS.md" in targets ->
        {:ok, "AGENTS.md"}

      "CLAUDE.md" in targets ->
        {:ok, "CLAUDE.md"}

      true ->
        :error
    end
  end

  defp root_instruction_link_targets(content) do
    ~r/\[[^\]]*\]\(\.\/((?:AGENTS|CLAUDE|GEMINI)\.md)(?:\s+(?:"[^"]*"|'[^']*'|\([^)]*\)))?\s*\)/
    |> Regex.scan(content, capture: :all_but_first)
    |> List.flatten()
  end

  defp permitted_gemini_target?(_repo_path, "AGENTS.md"), do: true

  defp permitted_gemini_target?(repo_path, "CLAUDE.md") do
    not File.exists?(Path.join(repo_path, "AGENTS.md"))
  end

  defp maintained_instruction_target?(repo_path, target) do
    target_path = Path.join(repo_path, target)

    case File.read(target_path) do
      {:ok, content} ->
        String.trim(content) != "" and
          "GEMINI.md" not in root_instruction_link_targets(content)

      _ ->
        false
    end
  end

  @doc """
  Check for required files that are missing.

  `repo_name` names the expected `<repo>_chora.deed` in the finding. The
  filename-only callers pass nothing and get the placeholder `repo`.

  `reuse_licenses: true` says the repo has a non-empty REUSE `LICENSES/`
  directory, which declares its licences without a root `LICENSE`. Root file
  lists carry no directories, so only `scan/2` can establish this.
  """
  def scan_required_missing(root_files, repo_name \\ "repo", opts \\ []) do
    reuse_licenses = Keyword.get(opts, :reuse_licenses, false)

    # NB: SECURITY.md is intentionally NOT listed here. It is a community-health
    # file that GitHub recognises in root, `.github/`, or `docs/`, and is owned
    # by Hypatia.Rules.CicdRules.check_repo_requirements/1 (which checks all
    # three locations). Listing it here too caused a double-report and a
    # false-positive when it lives under `.github/SECURITY.md`.
    required = [
      # The licence. The estate template ships `LICENSE.adoc` (plus REUSE
      # `LICENSES/`), so the AsciiDoc and Markdown spellings count too.
      %{
        file: "LICENSE",
        alternatives: ["LICENSE.txt", "LICENSE.md", "LICENSE.adoc"],
        reuse: true,
        severity: :critical
      },
      %{file: ".editorconfig", alternatives: [], severity: :medium},
      # The AI manifest. A2ML is retired (D312): the repo's facts now live in
      # one `<repo>_chora.deed` at the root, and the estate template ships no
      # 0-AI-MANIFEST at all. Any `*_chora.deed` satisfies this, because the
      # deed is named for the repo, not for the checkout directory. The legacy
      # spellings still satisfy it until their repos migrate; `.deed` is the
      # DEED-manifest spelling panoply ships.
      %{
        file: "#{repo_name}_chora.deed",
        alternatives: ["0-AI-MANIFEST.deed", "0-AI-MANIFEST.a2ml", "AI.a2ml"],
        pattern: ~r/\A[^\/]+_chora\.deed\z/,
        reason: "Required file missing: the repo deed `<repo>_chora.deed` (D312)",
        severity: :high
      }
    ]

    Enum.flat_map(required, fn req ->
      all_options = [req.file | req.alternatives]
      pattern = Map.get(req, :pattern)

      if Enum.any?(all_options, &(&1 in root_files)) or
           (pattern != nil and Enum.any?(root_files, &Regex.match?(pattern, &1))) or
           (reuse_licenses and Map.get(req, :reuse, false)) do
        []
      else
        [
          %{
            file: req.file,
            reason: Map.get(req, :reason, "Required file missing"),
            severity: req.severity,
            action: :create,
            type: :missing
          }
        ]
      end
    end)
  end

  @doc """
  Whether `repo_path` has a REUSE `LICENSES/` directory holding a licence
  text: a non-empty `<SPDX-ID>.<ext>` file, per the REUSE spec. The stem must
  be an identifier on the vendored SPDX License List or a `LicenseRef-`
  (`Hypatia.SPDX.licence_id?/1`), so `notes.txt`, READMEs and dotfiles such
  as `.gitkeep` do not count.

  Shared with `Hypatia.ScorecardIngestor`, so both licence checks agree.
  """
  def reuse_licenses?(repo_path) do
    dir = Path.join(repo_path, "LICENSES")

    case File.ls(dir) do
      {:ok, entries} -> Enum.any?(entries, &reuse_licence_text?(dir, &1))
      _ -> false
    end
  end

  defp reuse_licence_text?(dir, name) do
    path = Path.join(dir, name)

    Path.extname(name) != "" and
      Hypatia.SPDX.licence_id?(Path.rootname(name)) and
      File.regular?(path) and
      match?({:ok, %File.Stat{size: size}} when size > 0, File.stat(path))
  end

  @doc """
  Scan for CRLF-encoded blobs in a repo that lacks a `.gitattributes`
  declaring `eol=lf` (or `text=auto eol=lf`). Root cause of the
  "phantom diff" class observed on claude-integrations#43 2026-05-27:
  files round-trip through CRLF normalisation on every checkout,
  producing uncommittable diffs that block rebase.

  Inputs:
    * `repo_path` — absolute path to the repo root.
    * `tracked_files_with_eol` — list of `{path, :crlf | :lf | :mixed}`
      tuples (caller computes via `file -i` on each tracked file).

  Returns one finding per CRLF file when the repo's `.gitattributes`
  doesn't normalize EOL to LF. Pairs with
  `recipe-fix-gitattributes-eol` (ERR-GIT-001).
  """
  def scan_crlf_phantom_diff(repo_path, tracked_files_with_eol)
      when is_binary(repo_path) and is_list(tracked_files_with_eol) do
    gitattributes_path = Path.join(repo_path, ".gitattributes")

    normalises_lf? =
      case File.read(gitattributes_path) do
        {:ok, content} ->
          String.contains?(content, "eol=lf") or
            String.contains?(content, "text=auto") or
            String.contains?(content, "text eol=lf")

        _ ->
          false
      end

    if normalises_lf? do
      []
    else
      tracked_files_with_eol
      |> Enum.filter(fn {_path, eol} -> eol in [:crlf, :mixed] end)
      |> Enum.map(fn {path, _eol} ->
        %{
          rule: "crlf_blob_without_gitattributes",
          rule_id: "ERR-GIT-001",
          recipe_id: "recipe-fix-gitattributes-eol",
          severity: :medium,
          file: path,
          description:
            "File `#{path}` is committed with CRLF line endings but the repo has no " <>
              "`.gitattributes` normalising EOL to LF. Phantom diffs on every checkout " <>
              "(rebases fail with persistent uncommittable changes). Fix: write " <>
              "`.gitattributes` with `* text=auto eol=lf` and renormalise blobs."
        }
      end)
    end
  end

  def scan_crlf_phantom_diff(_, _), do: []

  @doc """
  Generate fleet dispatch recommendations from findings.
  """
  def dispatch_recommendations(findings) do
    Enum.map(findings, fn finding ->
      bot =
        case finding.action do
          :delete -> :rhodibot
          :rename -> :rhodibot
          :move -> :finishbot
          :replace -> :finishbot
          :create -> :finishbot
          :flag -> :seambot
        end

      confidence =
        case finding.severity do
          :critical -> 0.98
          :high -> 0.95
          :medium -> 0.85
          :low -> 0.70
          _ -> 0.50
        end

      %{
        bot: bot,
        confidence: confidence,
        file: finding.file,
        action: finding.action,
        reason: finding.reason
      }
    end)
  end
end
