# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.CLI do
  @moduledoc """
  Escript entry point for the Hypatia neurosymbolic scanner.

  Parses command-line arguments and invokes the appropriate Elixir rule
  modules, outputting results as JSON (for CI consumption) or human-readable
  text. This is the bridge between the shell wrapper `hypatia-cli.sh` and
  the Elixir rule engine.

  ## Commands

      hypatia scan <path>       Scan a repository for security and policy issues
      hypatia report <path>     Generate a detailed report with remediation advice
      hypatia version           Print version and exit
      hypatia help              Print usage and exit

  ## Options

      --rules <list>    Comma-separated rule modules to run (default: all)
                        Available: root_hygiene,honest_completion,workflow_audit,
                                   cicd_rules,code_safety,migration_rules,scorecard,
                                   green_web,git_state,dependabot_alerts,
                                   structural_drift
      --format <fmt>    Output format: json (default), text, github
      --severity <lvl>  Minimum severity to report: critical, high, medium (default), low, info
      --path <dir>      Path to scan (alternative to positional argument)

  ## Environment Variables

      HYPATIA_FORMAT      Override --format
      HYPATIA_SEVERITY    Override --severity
  """

  @version "2.0.0"

  @all_rule_modules [
    :root_hygiene,
    :honest_completion,
    :workflow_audit,
    :cicd_rules,
    :code_safety,
    :migration_rules,
    :scorecard,
    :green_web,
    :git_state,
    :dependabot_alerts,
    :structural_drift
  ]

  @severity_order %{
    "critical" => 1,
    "high" => 2,
    "medium" => 3,
    "low" => 4,
    "info" => 5
  }

  # Skip very large blobs during generic secret scanning to keep CLI latency
  # bounded on large monorepos.
  @max_secret_scan_bytes 1_000_000

  # ─── Escript entry point ──────────────────────────────────────────────

  @doc """
  Main entry point invoked by the escript runtime.

  Parses argv, dispatches to the appropriate command, and halts with
  an appropriate exit code:

    * `0` -- success (no findings at/above the severity threshold), or
      `--exit-zero` / `HYPATIA_EXIT_ZERO` was set and the scan ran cleanly.
    * `1` -- findings exist at/above the threshold (default behaviour).
    * `2` -- error (bad arguments, scan failed, etc.).

  Findings are always written to stdout in the requested format. A
  one-line `[hypatia] scan complete: ...` summary is always written to
  stderr so CI logs are never silent on exit.
  """
  def main(argv) do
    {opts, args, _invalid} =
      OptionParser.parse(argv,
        strict: [
          rules: :string,
          format: :string,
          severity: :string,
          path: :string,
          help: :boolean,
          version: :boolean,
          exit_zero: :boolean
        ],
        aliases: [
          r: :rules,
          f: :format,
          s: :severity,
          p: :path,
          h: :help,
          v: :version
        ]
      )

    # Environment variable overrides
    format = opts[:format] || System.get_env("HYPATIA_FORMAT") || "json"
    severity = opts[:severity] || System.get_env("HYPATIA_SEVERITY") || "medium"
    exit_zero = opts[:exit_zero] || env_flag?("HYPATIA_EXIT_ZERO")

    config = %{
      format: format,
      severity: severity,
      rules: parse_rules(opts[:rules]),
      path: opts[:path],
      exit_zero: exit_zero
    }

    case args do
      ["scan" | rest] ->
        path = config.path || List.first(rest) || "."
        run_scan(path, config)

      ["report" | rest] ->
        path = config.path || List.first(rest) || "."
        run_report(path, config)

      ["version"] ->
        IO.puts("hypatia #{@version}")

      ["help"] ->
        print_usage()

      [] ->
        cond do
          opts[:version] -> IO.puts("hypatia #{@version}")
          opts[:help] -> print_usage()
          true ->
            IO.puts(:stderr, "Error: no command specified. Run 'hypatia help' for usage.")
            System.halt(2)
        end

      [unknown | _] ->
        IO.puts(:stderr, "Error: unknown command '#{unknown}'. Run 'hypatia help' for usage.")
        System.halt(2)
    end
  end

  # ─── Scan command ─────────────────────────────────────────────────────

  defp run_scan(path, config) do
    abs_path = Path.expand(path)

    unless File.dir?(abs_path) do
      IO.puts(:stderr, "Error: '#{abs_path}' is not a directory.")
      System.halt(2)
    end

    findings = collect_findings(abs_path, config.rules)

    # Filter by severity threshold
    filtered =
      findings
      |> Enum.filter(fn f ->
        sev = Map.get(f, :severity, "medium") |> to_string()
        rank = Map.get(@severity_order, sev, 5)
        threshold = Map.get(@severity_order, config.severity, 3)
        rank <= threshold
      end)

    output(filtered, config.format)
    emit_finding_summary(filtered, config)

    cond do
      length(filtered) == 0 -> :ok
      config.exit_zero -> :ok
      true -> System.halt(1)
    end
  end

  # ─── Report command ───────────────────────────────────────────────────

  defp run_report(path, config) do
    abs_path = Path.expand(path)

    unless File.dir?(abs_path) do
      IO.puts(:stderr, "Error: '#{abs_path}' is not a directory.")
      System.halt(2)
    end

    findings = collect_findings(abs_path, config.rules)

    filtered =
      findings
      |> Enum.filter(fn f ->
        sev = Map.get(f, :severity, "medium") |> to_string()
        rank = Map.get(@severity_order, sev, 5)
        threshold = Map.get(@severity_order, config.severity, 3)
        rank <= threshold
      end)

    # Report always uses text format with extra detail
    output_report(filtered, abs_path)
    emit_finding_summary(filtered, config)

    cond do
      length(filtered) == 0 -> :ok
      config.exit_zero -> :ok
      true -> System.halt(1)
    end
  end

  # ─── Diagnostic summary ──────────────────────────────────────────────
  #
  # Always emit a single-line summary on stderr after a scan/report so CI
  # logs never report a silent exit-1. Findings themselves go to stdout
  # in the requested format; this line is a separate operator-facing
  # signal that includes severity counts and the exit code about to be
  # returned. See hypatia#213.

  defp emit_finding_summary(filtered, config) do
    counts =
      Enum.reduce(filtered, %{}, fn f, acc ->
        sev = Map.get(f, :severity, "medium") |> to_string()
        Map.update(acc, sev, 1, &(&1 + 1))
      end)

    total = length(filtered)

    breakdown =
      ["critical", "high", "medium", "low", "info"]
      |> Enum.map(fn sev -> "#{sev}=#{Map.get(counts, sev, 0)}" end)
      |> Enum.join(", ")

    exit_code =
      cond do
        total == 0 -> 0
        config.exit_zero -> 0
        true -> 1
      end

    IO.puts(
      :stderr,
      "[hypatia] scan complete: #{total} findings >= #{config.severity} " <>
        "(#{breakdown}); exit #{exit_code}" <>
        if(config.exit_zero and total > 0, do: " (--exit-zero suppressed exit 1)", else: "")
    )
  end

  defp env_flag?(name) do
    case System.get_env(name) do
      nil -> false
      "" -> false
      "0" -> false
      "false" -> false
      "FALSE" -> false
      _ -> true
    end
  end

  # ─── Finding collection across rule modules ──────────────────────────

  defp collect_findings(repo_path, rules) do
    results = []

    # Root Hygiene
    results =
      if :root_hygiene in rules do
        root_files = list_root_files(repo_path)
        %{findings: findings} = Hypatia.Rules.RootHygiene.scan(root_files)

        normalized =
          Enum.map(findings, fn f ->
            %{
              rule_module: "root_hygiene",
              severity: to_string(f.severity),
              type: to_string(f.type),
              file: Map.get(f, :file, ""),
              reason: Map.get(f, :reason, ""),
              action: to_string(Map.get(f, :action, :flag))
            }
          end)

        results ++ normalized
      else
        results
      end

    # Honest Completion
    results =
      if :honest_completion in rules do
        audit = Hypatia.Rules.HonestCompletion.audit(repo_path)

        normalized =
          Enum.map(audit.findings, fn f ->
            %{
              rule_module: "honest_completion",
              severity: to_string(Map.get(f, :severity, :medium)),
              type: to_string(Map.get(f, :type, :unknown)),
              file: repo_path,
              reason: Map.get(f, :detail, ""),
              action: "flag",
              deduction: Map.get(f, :deduction, 0)
            }
          end)

        # Add completion summary as a finding if there is a significant delta
        completion_findings =
          if audit.honesty_grade in [:inflated, :dishonest] do
            [
              %{
                rule_module: "honest_completion",
                severity: "high",
                type: "inflated_completion",
                file: repo_path,
                reason:
                  "Claimed #{audit.claimed_completion}% but adjusted to #{audit.adjusted_completion}% (grade: #{audit.honesty_grade})",
                action: "flag"
              }
            ]
          else
            []
          end

        results ++ normalized ++ completion_findings
      else
        results
      end

    # Workflow Audit
    results =
      if :workflow_audit in rules do
        workflows_dir = Path.join(repo_path, ".github/workflows")

        if File.dir?(workflows_dir) do
          {:ok, wf_files} = File.ls(workflows_dir)

          yml_files =
            Enum.filter(wf_files, fn f ->
              String.ends_with?(f, ".yml") or String.ends_with?(f, ".yaml")
            end)

          wf_contents =
            yml_files
            |> Enum.map(fn f ->
              path = Path.join(workflows_dir, f)

              case File.read(path) do
                {:ok, content} -> {f, content}
                _ -> nil
              end
            end)
            |> Enum.reject(&is_nil/1)
            |> Map.new()

          %{findings: findings} =
            Hypatia.Rules.WorkflowAudit.audit(yml_files, wf_contents,
              has_codeql_supported_language: has_codeql_supported_language?(repo_path)
            )

          normalized =
            Enum.map(findings, fn f ->
              %{
                rule_module: "workflow_audit",
                severity: to_string(Map.get(f, :severity, :medium)),
                type: to_string(Map.get(f, :type, :unknown)),
                file: Map.get(f, :file, Map.get(f, :files, "") |> listify()),
                reason: Map.get(f, :detail, describe_workflow_finding(f)),
                action: to_string(Map.get(f, :action, Map.get(f, :fix, :flag)))
              }
            end)

          results ++ normalized
        else
          results ++
            [
              %{
                rule_module: "workflow_audit",
                severity: "high",
                type: "no_workflows_dir",
                file: ".github/workflows",
                reason: "No .github/workflows/ directory found",
                action: "create"
              }
            ]
        end
      else
        results
      end

    # CI/CD Rules
    results =
      if :cicd_rules in rules do
        root_files = list_root_files(repo_path)

        repo_info = %{
          visibility: "public",
          has_deps: File.exists?(Path.join(repo_path, "mix.lock")) or
                      File.exists?(Path.join(repo_path, "Cargo.lock")) or
                      File.exists?(Path.join(repo_path, "deno.lock")),
          files: root_files,
          # `repo_path` is consulted by the must-have-file check to verify
          # *nested* paths (`.github/dependabot.yml`, `.github/workflows/*`)
          # exist on disk. Without it, the rule could only see root files
          # and so reported every nested requirement as missing — that was
          # the source of three baselined FPs (dependabot.yml, scorecard.yml,
          # the literal string "permissions: read-all") in hypatia#237.
          repo_path: repo_path
        }

        missing = Hypatia.Rules.CicdRules.check_repo_requirements(repo_info)

        normalized =
          Enum.map(missing, fn m ->
            %{
              rule_module: "cicd_rules",
              severity: "high",
              type: "missing_requirement",
              file: m.missing,
              reason: "Required file missing (condition: #{m.condition})",
              action: "create"
            }
          end)

        # Check for banned file extensions in the repo. Findings are filtered
        # through ScannerSuppression so a scoped exemption in `.hypatia-ignore`
        # (or a documented org-policy exemption) can suppress specific files
        # without re-baselining every scan.
        all_files = list_all_files(repo_path)
        banned = Hypatia.Rules.CodeSafety.banned_file_extensions()

        banned_findings =
          Enum.flat_map(banned, fn rule ->
            ext = String.replace(rule.glob, "*", "")

            matches =
              Enum.filter(all_files, fn f ->
                String.ends_with?(f, ext) and not String.contains?(f, "node_modules") and
                  not String.contains?(f, ".git/") and
                  not Hypatia.ScannerSuppression.suppressed?(
                    f,
                    "cicd_rules",
                    "banned_language_file",
                    repo_path: repo_path
                  )
              end)

            Enum.map(matches, fn file ->
              %{
                rule_module: "cicd_rules",
                severity: to_string(rule.severity),
                type: "banned_language_file",
                file: file,
                reason: rule.description,
                action: "flag"
              }
            end)
          end)

        results ++ normalized ++ banned_findings
      else
        results
      end

    # Code Safety
    results =
      if :code_safety in rules do
        code_findings = scan_code_safety(repo_path)
        results ++ code_findings
      else
        results
      end

    # Migration Rules (ReScript deprecated API detection)
    results =
      if :migration_rules in rules do
        rescript_files = find_files_by_ext(repo_path, ".res")

        migration_findings =
          Enum.flat_map(rescript_files, fn file ->
            case File.read(file) do
              {:ok, content} ->
                deprecated = Hypatia.Rules.MigrationRules.scan_deprecated_usage(content)

                Enum.map(deprecated, fn d ->
                  %{
                    rule_module: "migration_rules",
                    severity: to_string(d.severity),
                    type: "deprecated_api",
                    file: file,
                    reason: "#{d.api} deprecated -- use #{d.replacement} (#{d.count} occurrences)",
                    action: to_string(d.strategy)
                  }
                end)

              _ ->
                []
            end
          end)

        results ++ migration_findings
      else
        results
      end

    # Scorecard (local scan)
    results =
      if :scorecard in rules do
        repo_name = Path.basename(repo_path)

        case Hypatia.ScorecardIngestor.local_scan(repo_path, repo_name) do
          {:ok, patterns} ->
            normalized =
              Enum.map(patterns, fn p ->
                %{
                  rule_module: "scorecard",
                  severity: String.downcase(Map.get(p, "severity", "medium")),
                  type: Map.get(p, "category", "unknown"),
                  file: repo_path,
                  reason: Map.get(p, "description", ""),
                  action:
                    if(Map.get(p, "auto_fixable", false), do: "auto_fix", else: "flag"),
                  remediation: Map.get(p, "remediation", ""),
                  scorecard_check: Map.get(p, "scorecard_check", "")
                }
              end)

            results ++ normalized

          _ ->
            results
        end
      else
        results
      end

    # Green Web Foundation checks
    results =
      if :green_web in rules do
        root_files = list_root_files(repo_path)

        # Collect file contents for workflow and deployment files
        workflows_dir = Path.join(repo_path, ".github/workflows")

        wf_contents =
          if File.dir?(workflows_dir) do
            case File.ls(workflows_dir) do
              {:ok, wf_files} ->
                wf_files
                |> Enum.filter(fn f ->
                  String.ends_with?(f, ".yml") or String.ends_with?(f, ".yaml")
                end)
                |> Enum.map(fn f ->
                  path = Path.join(workflows_dir, f)
                  case File.read(path) do
                    {:ok, content} -> {f, content}
                    _ -> nil
                  end
                end)
                |> Enum.reject(&is_nil/1)
                |> Map.new()

              _ ->
                %{}
            end
          else
            %{}
          end

        findings = Hypatia.Rules.GreenWeb.audit(repo_path, root_files, wf_contents)

        normalized =
          Enum.map(findings, fn f ->
            %{
              rule_module: "green_web",
              severity: to_string(Map.get(f, :severity, :info)),
              type: to_string(Map.get(f, :type, :unknown)),
              file: Map.get(f, :file, ""),
              reason: Map.get(f, :detail, ""),
              action: "flag"
            }
          end)

        results ++ normalized
      else
        results
      end

    # Git State
    results =
      if :git_state in rules do
        case Hypatia.Rules.GitState.scan(repo_path) do
          %{findings: findings} ->
            normalized =
              Enum.map(findings, fn f ->
                %{
                  rule_module: "git_state",
                  severity: to_string(f.severity),
                  type: f.rule,
                  file: Map.get(f, :file, "."),
                  reason: f.reason,
                  action: to_string(f.action)
                }
              end)

            results ++ normalized

          _ -> results
        end
      else
        results
      end

    # Dependabot Alerts
    results =
      if :dependabot_alerts in rules do
        case Hypatia.Rules.DependabotAlerts.scan_from_path(repo_path) do
          {:ok, %{findings: findings}} ->
            normalized =
              Enum.map(findings, fn f ->
                %{
                  rule_module: "dependabot_alerts",
                  severity: to_string(f.severity),
                  type: f.rule,
                  file: Map.get(f, :file, ""),
                  reason: f.reason,
                  action: to_string(f.action)
                }
              end)

            results ++ normalized

          {:error, reason} ->
            IO.puts(:stderr, "Warning: Dependabot alerts unavailable: #{reason}")
            results
        end
      else
        results
      end

    # Structural Drift
    results =
      if :structural_drift in rules do
        case Hypatia.Rules.StructuralDrift.scan(repo_path) do
          %{findings: findings} ->
            normalized =
              Enum.map(findings, fn f ->
                %{
                  rule_module: "structural_drift",
                  severity: to_string(f.severity),
                  type: f.rule,
                  file: Map.get(f, :file, "."),
                  reason: f.reason,
                  action: to_string(f.action)
                }
              end)

            results ++ normalized

          _ -> results
        end
      else
        results
      end

    results
  end

  # ─── Code safety scanning ────────────────────────────────────────────

  @language_extensions %{
    "rust" => [".rs"],
    "rescript" => [".res"],
    "idris2" => [".idr"],
    "haskell" => [".hs"],
    "ocaml" => [".ml", ".mli"],
    "coq" => [".v"],
    "lean" => [".lean"],
    "nickel" => [".ncl"],
    "elixir" => [".ex", ".exs"],
    "erlang" => [".erl", ".hrl"],
    "shell" => [".sh"],
    "javascript" => [".js", ".mjs"],
    "typescript" => [".ts", ".tsx"]
  }

  defp scan_code_safety(repo_path) do
    language_findings =
      Enum.flat_map(@language_extensions, fn {language, exts} ->
        files =
          Enum.flat_map(exts, fn ext ->
            find_files_by_ext(repo_path, ext)
          end)

        Enum.flat_map(files, fn file ->
          case File.read(file) do
            {:ok, content} ->
              findings = Hypatia.Rules.CodeSafety.scan_content(content, language)
              # Two-stage suppression:
              #
              #   1. Path-based — files under `lib/rules/`, `scripts/fix-scripts/`,
              #      `test/`, `.audittraining/`, etc. are training corpora /
              #      remediation scripts for the very rules being checked.
              #      Content-pattern rules firing in them is self-recursion.
              #
              #   2. File-level allow directive — first 20 lines may declare
              #      `# hypatia: allow code_safety/<rule>` for intentional usage
              #      (e.g. an Idris2 proof file that legitimately needs
              #      `believe_me`). One declaration covers every match.
              findings
              |> Enum.reject(fn f ->
                Hypatia.ScannerSuppression.suppressed?(
                  file,
                  "code_safety",
                  to_string(f.rule),
                  repo_path: repo_path
                ) or
                  Hypatia.ScannerSuppression.file_allowed?(
                    content,
                    "code_safety",
                    to_string(f.rule)
                  )
              end)
              |> Enum.map(fn f ->
                %{
                  rule_module: "code_safety",
                  severity: cli_context_severity(file, f.rule, f.severity),
                  type: to_string(f.rule),
                  file: file,
                  reason: "#{f.description} (#{f.occurrences} occurrences, #{f.cwe})",
                  action: "flag"
                }
              end)

            _ ->
              []
          end
        end)
      end)

    # Native secret scanning across generic text files (including .scm and
    # non-language-specific configs/docs) so leaks are not extension-gated.
    secret_findings =
      repo_path
      |> list_all_files()
      |> Enum.flat_map(&scan_file_for_secrets(&1, repo_path))

    language_findings ++ secret_findings
  end

  # Downgrade `unwrap*` / `panic_macro` / `expect_in_hot_path` etc. when
  # they fire inside CLI entry-point code or test code, where idiomatic
  # Rust uses `.unwrap()` to convert "bad CLI input or test fixture
  # failure" into a non-zero exit with a panic. The dangerous-by-default
  # rule still fires for library code; CLI and test contexts are
  # downgraded to `:low`.
  #
  # In-scope contexts: `cli/`, any `main.rs`, any `bin/<name>.rs`, any
  # `build.rs`, the `tools/` directory (RSR convention for one-shot
  # binaries), `fixer/`, and `tests/` directories (integration tests
  # often unwrap to surface assertion failures). Library code under
  # `adapters/src/`, `data/src/`, etc. is unaffected.
  defp cli_context_severity(file, rule, original_severity) do
    if cli_context_rule?(rule) and cli_context_file?(file) do
      to_string(:low)
    else
      to_string(original_severity)
    end
  end

  defp cli_context_rule?(rule) when rule in [
         :unwrap_without_check,
         :unwrap_dangerous_default,
         :expect_in_hot_path,
         :panic_macro,
         :todo_macro,
         :unimplemented_macro
       ],
       do: true

  defp cli_context_rule?(_), do: false

  defp cli_context_file?(file) do
    basename = Path.basename(file)

    basename in ["main.rs", "build.rs"] or
      String.contains?(file, "/cli/") or
      String.contains?(file, "/bin/") or
      String.contains?(file, "/tools/") or
      String.contains?(file, "/fixer/") or
      String.contains?(file, "/tests/") or
      String.ends_with?(file, "_test.rs")
  end

  # Path exemptions, GHA-secret-reference awareness, and inline allow
  # directives all funnel through Hypatia.ScannerSuppression. This is
  # deliberately a small predicate set — provenance-aware suppression that
  # would otherwise be re-discovered every run (training corpora, fixture
  # files, the rule definitions themselves, `${{ secrets.X }}` references
  # which point at the secret store, not leaks). Anything beyond that
  # belongs in .hypatia-baseline.json so it stays visible during review.
  defp scan_file_for_secrets(file, repo_path) do
    rule_module = "security_errors"
    rule_type = "secret_detected"

    with false <-
           Hypatia.ScannerSuppression.suppressed?(file, rule_module, rule_type,
             repo_path: repo_path
           ),
         {:ok, %File.Stat{type: :regular, size: size}} when size <= @max_secret_scan_bytes <-
           File.stat(file),
         {:ok, content} <- File.read(file),
         true <- not String.contains?(content, <<0>>),
         false <-
           Hypatia.ScannerSuppression.file_allowed?(content, rule_module, rule_type) do
      detect_secrets_with_context(content, file, rule_module, rule_type)
    else
      _ -> []
    end
  end

  defp detect_secrets_with_context(content, file, rule_module, rule_type) do
    # Per-line scan so each finding can carry line number and consult
    # inline-allow directives + GHA-secret-reference exemption. Reduces
    # the line set to a uniq `(line_number, label)` so a single line that
    # matches multiple regex patterns still produces one finding.
    lines = String.split(content, "\n")

    lines
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, idx} ->
      prev = if idx > 0, do: Enum.at(lines, idx - 1), else: nil

      cond do
        Hypatia.ScannerSuppression.context_safe_line?(rule_type, line) ->
          []

        Hypatia.ScannerSuppression.inline_allowed?(line, prev, rule_module, rule_type) ->
          []

        true ->
          Hypatia.Rules.SecurityErrors.detect_secrets(line)
          |> Enum.uniq()
          |> Enum.map(fn label ->
            %{
              rule_module: rule_module,
              severity: "critical",
              type: rule_type,
              file: file,
              line: idx + 1,
              reason: "Secret found: #{label}",
              action: "revoke_rotate_and_purge"
            }
          end)
      end
    end)
  end

  # ─── Output formatting ───────────────────────────────────────────────

  defp output(findings, "json") do
    IO.puts(Jason.encode!(findings, pretty: true))
  end

  defp output(findings, "github") do
    # GitHub Actions annotation format
    Enum.each(findings, fn f ->
      level =
        case f.severity do
          "critical" -> "error"
          "high" -> "error"
          "medium" -> "warning"
          _ -> "notice"
        end

      file = Map.get(f, :file, "")
      reason = Map.get(f, :reason, "")
      IO.puts("::#{level} file=#{file}::#{reason}")
    end)
  end

  defp output(findings, _text) do
    if findings == [] do
      IO.puts(:stderr, "No findings.")
    else
      IO.puts(:stderr, "Found #{length(findings)} issue(s):\n")

      findings
      |> Enum.group_by(& &1.severity)
      |> Enum.sort_by(fn {sev, _} -> Map.get(@severity_order, sev, 5) end)
      |> Enum.each(fn {severity, group} ->
        IO.puts(:stderr, "  [#{String.upcase(severity)}] (#{length(group)} findings)")

        Enum.each(group, fn f ->
          IO.puts(:stderr, "    #{f.file}: #{f.reason}")
        end)

        IO.puts(:stderr, "")
      end)
    end
  end

  defp output_report(findings, repo_path) do
    IO.puts("=" |> String.duplicate(72))
    IO.puts("  Hypatia Security & Policy Report")
    IO.puts("  Repository: #{repo_path}")
    IO.puts("  Date: #{DateTime.utc_now() |> DateTime.to_string()}")
    IO.puts("  Version: #{@version}")
    IO.puts("=" |> String.duplicate(72))
    IO.puts("")

    if findings == [] do
      IO.puts("  No issues found. Repository passes all checks.")
    else
      IO.puts("  Total findings: #{length(findings)}")
      IO.puts("")

      by_module = Enum.group_by(findings, & &1.rule_module)

      Enum.each(by_module, fn {mod, group} ->
        IO.puts("  --- #{format_module_name(mod)} ---")

        group
        |> Enum.sort_by(fn f -> Map.get(@severity_order, f.severity, 5) end)
        |> Enum.each(fn f ->
          IO.puts("  [#{String.upcase(f.severity)}] #{f.type}")
          IO.puts("    File:   #{f.file}")
          IO.puts("    Reason: #{f.reason}")
          IO.puts("    Action: #{f.action}")

          if Map.has_key?(f, :remediation) and f.remediation != "" do
            IO.puts("    Fix:    #{f.remediation}")
          end

          IO.puts("")
        end)
      end)
    end

    IO.puts("=" |> String.duplicate(72))
  end

  # ─── Helpers ──────────────────────────────────────────────────────────

  defp parse_rules(nil), do: @all_rule_modules

  defp parse_rules(rules_str) do
    rules_str
    |> String.split(",", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.to_existing_atom/1)
    |> Enum.filter(&(&1 in @all_rule_modules))
    |> case do
      [] -> @all_rule_modules
      rules -> rules
    end
  end

  # CodeQL supports JavaScript/TypeScript, Python, Go, Java/Kotlin, Ruby,
  # C#, C/C++, Swift. A repo whose source is purely Rust + Elixir + Idris
  # (etc.) has nothing for CodeQL to scan, so the standard "every repo gets
  # a codeql.yml" requirement is dropped to avoid a perpetual unclearable
  # finding. See hypatia#237 triage.
  @codeql_extensions ~w(.js .jsx .ts .tsx .mjs .py .go .java .kt .rb .cs
                        .cpp .cc .cxx .c .h .hpp .swift)

  defp has_codeql_supported_language?(repo_path) do
    Enum.any?(@codeql_extensions, fn ext ->
      case find_files_by_ext(repo_path, ext) do
        [_ | _] -> true
        _ -> false
      end
    end)
  end

  defp list_root_files(repo_path) do
    case File.ls(repo_path) do
      {:ok, entries} ->
        Enum.filter(entries, fn e ->
          not File.dir?(Path.join(repo_path, e))
        end)

      _ ->
        []
    end
  end

  defp list_all_files(repo_path) do
    case System.cmd("find", [repo_path, "-type", "f",
                             "-not", "-path", "*/.git/*",
                             "-not", "-path", "*/node_modules/*",
                             "-not", "-path", "*/_build/*",
                             "-not", "-path", "*/deps/*",
                             "-not", "-path", "*/target/*"],
                    stderr_to_stdout: true) do
      {output, 0} ->
        output |> String.split("\n", trim: true)

      _ ->
        []
    end
  end

  defp find_files_by_ext(repo_path, ext) do
    case System.cmd("find", [repo_path, "-type", "f", "-name", "*#{ext}",
                             "-not", "-path", "*/.git/*",
                             "-not", "-path", "*/node_modules/*",
                             "-not", "-path", "*/_build/*",
                             "-not", "-path", "*/deps/*",
                             "-not", "-path", "*/target/*"],
                    stderr_to_stdout: true) do
      {output, 0} ->
        output |> String.split("\n", trim: true)

      _ ->
        []
    end
  end

  defp describe_workflow_finding(f) do
    cond do
      Map.has_key?(f, :detail) -> f.detail
      Map.has_key?(f, :action_ref) -> "Action #{f.action_ref} needs attention"
      Map.has_key?(f, :file) -> "Issue in #{f.file}"
      true -> "Workflow issue detected"
    end
  end

  defp listify(val) when is_list(val), do: Enum.join(val, ", ")
  defp listify(val), do: to_string(val)

  defp format_module_name("root_hygiene"), do: "Root Hygiene"
  defp format_module_name("honest_completion"), do: "Honest Completion Audit"
  defp format_module_name("workflow_audit"), do: "Workflow Audit"
  defp format_module_name("cicd_rules"), do: "CI/CD Rules"
  defp format_module_name("code_safety"), do: "Code Safety"
  defp format_module_name("migration_rules"), do: "Migration Rules"
  defp format_module_name("scorecard"), do: "OpenSSF Scorecard"
  defp format_module_name("green_web"), do: "Green Web Foundation"
  defp format_module_name("git_state"), do: "Git State Sync"
  defp format_module_name("dependabot_alerts"), do: "Dependabot Alerts"
  defp format_module_name(other), do: other

  defp print_usage do
    IO.puts("""
    Hypatia #{@version} - Neurosymbolic Code Safety Scanner

    USAGE:
        hypatia <command> [options]

    COMMANDS:
        scan <path>         Scan directory for security and policy issues
        report <path>       Generate detailed report with remediation advice
        version             Show version
        help                Show this help

    OPTIONS:
        --rules, -r <list>      Comma-separated rule modules (default: all)
                                Available: root_hygiene,honest_completion,
                                workflow_audit,cicd_rules,code_safety,
                                migration_rules,scorecard,green_web,
                                git_state,dependabot_alerts
        --format, -f <fmt>      Output format: json (default), text, github
        --severity, -s <lvl>    Minimum severity: critical, high, medium (default), low
        --path, -p <dir>        Path to scan (alternative to positional arg)
        --exit-zero             Always exit 0 after a successful scan, even when
                                findings exist. Use in CI when a downstream step
                                gates on severity counts. (See HYPATIA_EXIT_ZERO.)

    EXIT CODES:
        0   No findings at or above the configured severity threshold,
            OR --exit-zero / HYPATIA_EXIT_ZERO was set and the scan ran cleanly.
        1   Findings exist at/above the threshold (default behaviour).
        2   Error (bad arguments, scan failed, etc.).

    ENVIRONMENT:
        HYPATIA_FORMAT          Override --format
        HYPATIA_SEVERITY        Override --severity
        HYPATIA_EXIT_ZERO       If set to a truthy value (1, true, anything
                                non-empty/non-zero), behaves as --exit-zero.

    EXAMPLES:
        hypatia scan .
        hypatia scan ~/repos/my-project --format text
        hypatia scan . --rules root_hygiene,code_safety --severity high
        hypatia scan . --exit-zero        # CI: emit findings, never fail step
        hypatia report . > report.txt
        HYPATIA_FORMAT=json hypatia scan .
    """)
  end
end
