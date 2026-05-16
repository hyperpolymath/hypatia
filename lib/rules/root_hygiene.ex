# SPDX-License-Identifier: PMPL-1.0-or-later
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
    %{pattern: "AI.djot", reason: "Superseded by 0-AI-MANIFEST.a2ml", severity: :high,
      action: :delete},
    %{pattern: "STATE.scm", reason: "Must be in .machine_readable/ only", severity: :critical,
      action: :move},
    %{pattern: "META.scm", reason: "Must be in .machine_readable/ only", severity: :critical,
      action: :move},
    %{pattern: "ECOSYSTEM.scm", reason: "Must be in .machine_readable/ only", severity: :critical,
      action: :move},
    %{pattern: "AGENTIC.scm", reason: "Must be in .machine_readable/ only", severity: :critical,
      action: :move},
    %{pattern: "NEUROSYM.scm", reason: "Must be in .machine_readable/ only", severity: :critical,
      action: :move},
    %{pattern: "PLAYBOOK.scm", reason: "Must be in .machine_readable/ only", severity: :critical,
      action: :move},
    %{pattern: "LANGUAGES.scm", reason: "Must be in .machine_readable/ only", severity: :critical,
      action: :move},
    # Docker is permitted estate-wide; Podman/Containerfile is HIGHLY PREFERRED
    # but not mandatory. These are low-severity advisories (do not fail the
    # critical/high gate), nudging Containerfile naming unless a specific tool
    # genuinely needs the Docker-* name.
    %{pattern: "Dockerfile", reason: "Containerfile preferred over Dockerfile (Podman highly preferred; Docker permitted) -- rename unless a tool requires the Dockerfile name", severity: :low,
      action: :rename},
    %{pattern: "docker-compose.yml", reason: "compose.yml / podman-compose highly preferred (Docker permitted)", severity: :low,
      action: :rename},
    %{pattern: "docker-compose.yaml", reason: "compose.yml / podman-compose highly preferred (Docker permitted)", severity: :low,
      action: :rename},
    %{pattern: "Makefile", reason: "Use Justfile", severity: :medium,
      action: :replace},
    %{pattern: "package-lock.json", reason: "npm banned -- use Deno", severity: :high,
      action: :delete},
    %{pattern: "yarn.lock", reason: "Yarn banned -- use Deno", severity: :high,
      action: :delete},
    %{pattern: "bun.lockb", reason: "Bun banned -- use Deno", severity: :high,
      action: :delete},
    %{pattern: "pnpm-lock.yaml", reason: "pnpm banned -- use Deno", severity: :high,
      action: :delete},
    %{pattern: ".npmrc", reason: "npm banned -- use Deno", severity: :medium,
      action: :delete},
    %{pattern: "tsconfig.json", reason: "TypeScript banned -- use ReScript", severity: :high,
      action: :flag},
    %{pattern: "go.mod", reason: "Go banned -- use Rust", severity: :high,
      action: :flag},
    %{pattern: "go.sum", reason: "Go banned -- use Rust", severity: :high,
      action: :flag},
    %{pattern: "AI.a2ml", reason: "Stray AI.a2ml in root -- use 0-AI-MANIFEST.a2ml only", severity: :high,
      action: :delete},
    %{pattern: "Trustfile.hs", reason: "Legacy Haskell Trustfile -- use Trustfile.a2ml", severity: :high,
      action: :convert},
    %{pattern: "AI.djot", reason: "Superseded by 0-AI-MANIFEST.a2ml", severity: :high,
      action: :delete}
  ]

  # ─── Stale file patterns ───────────────────────────────────────────────

  @stale_patterns [
    %{regex: ~r/^PLAN-.*\.md$/i, reason: "Executed plan left in root -- move to docs/plans/",
      severity: :low, action: :move},
    %{regex: ~r/^SNAPSHOT-.*\.md$/i, reason: "Stale snapshot in root -- move to docs/ or delete",
      severity: :low, action: :move},
    %{regex: ~r/^TODO-.*\.md$/i, reason: "TODO file in root -- move to docs/",
      severity: :low, action: :move},
    %{regex: ~r/^SESSION-.*\.md$/i, reason: "Session log in root -- move to docs/sessions/",
      severity: :low, action: :move},
    %{regex: ~r/^DESIGN-.*\.md$/i, reason: "Design doc in root -- move to docs/design/",
      severity: :low, action: :move},
    %{regex: ~r/^.*-STATUS-REPORT-.*\.md$/i, reason: "Status report in root -- move to docs/",
      severity: :low, action: :move},
    %{regex: ~r/^.*\.bak$/i, reason: "Backup file in root",
      severity: :medium, action: :delete},
    %{regex: ~r/^.*\.tmp$/i, reason: "Temporary file in root",
      severity: :medium, action: :delete},
    %{regex: ~r/^.*\.orig$/i, reason: "Merge artifact in root",
      severity: :medium, action: :delete},
    %{regex: ~r/^SONNET-TASKS\.md$/i, reason: "Stale AI task file -- delete or move to docs/",
      severity: :high, action: :delete},
    %{regex: ~r/^GEMINI\.md$/i, reason: "Stale AI session file -- delete",
      severity: :medium, action: :delete},
    %{regex: ~r/^CLAUDE-WORK.*\.md$/i, reason: "AI work file in repo root -- should not be committed",
      severity: :medium, action: :delete}
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
  Check root files against the banned list.
  """
  def scan_banned(root_files) do
    Enum.flat_map(root_files, fn file ->
      case Enum.find(@banned_root_files, fn rule -> rule.pattern == file end) do
        nil -> []
        rule -> [%{file: file, reason: rule.reason, severity: rule.severity,
                    action: rule.action, type: :banned}]
      end
    end)
  end

  @doc """
  Check root files against stale patterns (regex-based).
  """
  def scan_stale(root_files) do
    Enum.flat_map(root_files, fn file ->
      case Enum.find(@stale_patterns, fn rule -> Regex.match?(rule.regex, file) end) do
        nil -> []
        rule -> [%{file: file, reason: rule.reason, severity: rule.severity,
                    action: rule.action, type: :stale}]
      end
    end)
  end

  @doc """
  Check for required files that are missing.
  """
  def scan_required_missing(root_files) do
    required = [
      %{file: "LICENSE", alternatives: ["LICENSE.txt"], severity: :critical},
      %{file: "SECURITY.md", alternatives: [], severity: :high},
      %{file: ".editorconfig", alternatives: [], severity: :medium},
      %{file: "0-AI-MANIFEST.a2ml", alternatives: ["AI.a2ml"], severity: :high}
    ]

    Enum.flat_map(required, fn req ->
      all_options = [req.file | req.alternatives]
      if Enum.any?(all_options, &(&1 in root_files)) do
        []
      else
        [%{file: req.file, reason: "Required file missing", severity: req.severity,
            action: :create, type: :missing}]
      end
    end)
  end

  @doc """
  Generate fleet dispatch recommendations from findings.
  """
  def dispatch_recommendations(findings) do
    Enum.map(findings, fn finding ->
      bot = case finding.action do
        :delete -> :rhodibot
        :rename -> :rhodibot
        :move -> :finishbot
        :replace -> :finishbot
        :create -> :finishbot
        :flag -> :seambot
      end

      confidence = case finding.severity do
        :critical -> 0.98
        :high -> 0.95
        :medium -> 0.85
        :low -> 0.70
        _ -> 0.50
      end

      %{bot: bot, confidence: confidence, file: finding.file,
        action: finding.action, reason: finding.reason}
    end)
  end
end
