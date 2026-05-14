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

          %{findings: findings} = Hypatia.Rules.WorkflowAudit.audit(yml_files, wf_contents)

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
          files: root_files
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

        # Check for banned file extensions in the repo
        all_files = list_all_files(repo_path)
        banned = Hypatia.Rules.CodeSafety.banned_file_extensions()

        banned_findings =
          Enum.flat_map(banned, fn rule ->
            ext = String.replace(rule.glob, "*", "")

            matches =
              Enum.filter(all_files, fn f ->
                String.ends_with?(f, ext) and not String.contains?(f, "node_modules") and
                  not String.contains?(f, ".git/")
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

  # Panic-shape rules whose findings are conventional / acceptable in
  # test code (panicking on bad input is the documented test failure
  # mode). Suppressed when the matching file is identifiably a test.
  @panic_shape_rules_test_exempt MapSet.new(~w(
    unwrap_without_check
    unwrap_dangerous_default
    expect_in_hot_path
    panic_macro
    todo_macro
    unimplemented_macro
  )a |> Enum.map(&Atom.to_string/1))

  defp test_file_path?(file, repo_path) do
    rel =
      file
      |> Path.relative_to(repo_path)
      |> String.replace_leading("./", "")

    String.starts_with?(rel, "test/") or
      String.starts_with?(rel, "tests/") or
      # `integration/` here is the dedicated integration-test crate, not
      # a generic integration layer — same panic-on-bad-setup conventions
      # as `tests/`.
      String.starts_with?(rel, "integration/src/") or
      String.starts_with?(rel, "integration/tests/") or
      String.contains?(rel, "/tests/") or
      String.contains?(rel, "/test/") or
      String.ends_with?(rel, "_test.rs") or
      String.ends_with?(rel, "_tests.rs") or
      String.ends_with?(rel, "_test.exs") or
      # cli/build.rs is a Cargo build script (compile-time only) — it
      # panicking just aborts the build, same as a failed compile, and
      # never runs at scan/runtime. Same semantics as test code.
      String.ends_with?(rel, "build.rs")
  end

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
              is_test = test_file_path?(file, repo_path)

              findings
              |> Enum.reject(fn f ->
                is_test and MapSet.member?(@panic_shape_rules_test_exempt, to_string(f.rule))
              end)
              |> Enum.map(fn f ->
                %{
                  rule_module: "code_safety",
                  severity: to_string(f.severity),
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
      |> Enum.reject(&secret_scan_path_exempt?(&1, repo_path))
      |> Enum.flat_map(&scan_file_for_secrets/1)

    language_findings ++ secret_findings
  end

  # Path-prefix exemptions for the secret_detected rule. These directories
  # contain example secrets / credential patterns *by design* — they exist
  # to teach the scanner or remove the very patterns they reference — and
  # are documented in `.hypatia-exemptions.md`. Test fixtures land in the
  # same bucket: a literal assertion value in a test file is not a leak.
  @secret_scan_exempt_prefixes [
    ".audittraining/",
    "scripts/fix-scripts/",
    "test/",
    "tests/",
    "integration/",
    # The scanner's own rule modules definitionally contain the
    # patterns they detect — keep them out of the self-scan.
    "lib/rules/",
    # Same for the human-readable exemptions doc, which has to
    # spell out the example secrets it grants exceptions for.
    ".hypatia-exemptions.md"
  ]
  @secret_scan_exempt_infixes [
    "/tests/",
    "/test/",
    "/.audittraining/",
    "/fixtures/"
  ]

  defp secret_scan_path_exempt?(file, repo_path) do
    rel =
      file
      |> Path.relative_to(repo_path)
      |> String.replace_leading("./", "")

    Enum.any?(@secret_scan_exempt_prefixes, &String.starts_with?(rel, &1)) or
      Enum.any?(@secret_scan_exempt_infixes, &String.contains?(rel, &1))
  end

  defp scan_file_for_secrets(file) do
    with {:ok, %File.Stat{type: :regular, size: size}} when size <= @max_secret_scan_bytes <- File.stat(file),
         {:ok, content} <- File.read(file),
         true <- not String.contains?(content, <<0>>) do
      Hypatia.Rules.SecurityErrors.detect_secrets(content)
      |> Enum.uniq()
      |> Enum.map(fn label ->
        %{
          rule_module: "security_errors",
          severity: "critical",
          type: "secret_detected",
          file: file,
          reason: "Secret found: #{label}",
          action: "revoke_rotate_and_purge"
        }
      end)
    else
      _ -> []
    end
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
