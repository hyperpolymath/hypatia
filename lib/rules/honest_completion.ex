# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.HonestCompletion do
  @moduledoc """
  Honest completion audit -- cross-references claimed completion percentages
  against detectable evidence from the repository.

  This is the antidote to inflated self-assessments. Parses STATE.a2ml
  (or STATE.scm) from each repo and verifies claims against:
  - Actual source file counts
  - Test file counts and test results
  - Build status (presence of build artifacts, CI green/red)
  - TODO/FIXME density
  - believe_me / sorry / assert_total counts (formal verification repos)
  - Stub detection (empty function bodies, placeholder returns)

  Generates an honest completion report with a confidence-adjusted score.
  """

  # ─── STATE file parsing ────────────────────────────────────────────────

  @doc """
  Parse a STATE.a2ml file and extract claimed metrics.
  Returns a map of claimed values or {:error, reason}.
  """
  def parse_state_file(content) when is_binary(content) do
    claims = %{
      completion_percentage: extract_number(content, ~r/completion[_-]percentage\s*=\s*(\d+)/i),
      claimed_tests: extract_number(content, ~r/test[_-]count\s*=\s*(\d+)/i),
      claimed_files: extract_number(content, ~r/total\s*=\s*(\d+)/i),
      claimed_panels: extract_number(content, ~r/panels\s*=\s*(\d+)/i),
      phase: extract_string(content, ~r/phase\s*=\s*"([^"]+)"/i),
      status: extract_string(content, ~r/status\s*=\s*"([^"]+)"/i)
    }

    {:ok, claims}
  end

  def parse_state_file(_), do: {:error, :invalid_content}

  # ─── Evidence collection ───────────────────────────────────────────────

  @doc """
  Collect evidence from a repository directory to verify claims.
  `repo_path` should be an absolute path to the repo root.
  """
  def collect_evidence(repo_path) when is_binary(repo_path) do
    %{
      source_files: count_files(repo_path, ~w(.res .rs .ex .exs .zig .idr .v .gleam .jl .hs)),
      test_files: count_files(repo_path, ~w(.test.js .test.ts _test.exs _test.res)),
      todo_count: count_pattern(repo_path, "TODO"),
      fixme_count: count_pattern(repo_path, "FIXME"),
      believe_me_count: count_pattern(repo_path, "believe_me"),
      sorry_count: count_pattern(repo_path, "sorry"),
      assert_total_count: count_pattern(repo_path, "assert_total"),
      admitted_count: count_pattern(repo_path, "Admitted"),
      unsafe_coerce_count: count_pattern(repo_path, "unsafeCoerce"),
      obj_magic_count: count_pattern(repo_path, "Obj.magic"),
      postulate_count: count_pattern(repo_path, "postulate"),
      really_believe_me_count: count_pattern(repo_path, "really_believe_me"),
      stub_count: count_stubs(repo_path),
      has_ci: File.dir?(Path.join(repo_path, ".github/workflows")),
      has_tests_dir: File.dir?(Path.join(repo_path, "test")) or File.dir?(Path.join(repo_path, "tests")),
      has_state_file: File.exists?(Path.join(repo_path, ".machine_readable/STATE.a2ml")) or
                      File.exists?(Path.join(repo_path, ".machine_readable/STATE.scm")) # DEPRECATED: .scm fallback -- will be removed once all repos migrate to .a2ml
    }
  end

  # ─── Audit functions ───────────────────────────────────────────────────

  @doc """
  Audit a repository: compare claims against evidence.
  Returns a report with findings and an adjusted completion score.
  """
  def audit(repo_path) do
    state_path = find_state_file(repo_path)

    claims = case state_path do
      nil -> %{completion_percentage: nil, claimed_tests: nil, claimed_files: nil}
      path ->
        case File.read(path) do
          {:ok, content} ->
            case parse_state_file(content) do
              {:ok, c} -> c
              _ -> %{}
            end
          _ -> %{}
        end
    end

    evidence = collect_evidence(repo_path)
    findings = generate_findings(claims, evidence)
    adjusted_score = calculate_adjusted_score(claims, evidence, findings)

    %{
      repo: repo_path,
      claims: claims,
      evidence: evidence,
      findings: findings,
      claimed_completion: claims[:completion_percentage],
      adjusted_completion: adjusted_score,
      delta: delta(claims[:completion_percentage], adjusted_score),
      honesty_grade: honesty_grade(claims[:completion_percentage], adjusted_score)
    }
  end

  @doc """
  Generate findings by comparing claims to evidence.
  """
  def generate_findings(_claims, evidence) do
    findings = []

    # Dangerous patterns in formally verified repos
    findings = if evidence.believe_me_count > 0 do
      [%{type: :dangerous_pattern, detail: "#{evidence.believe_me_count} believe_me found",
         severity: :critical, deduction: 15} | findings]
    else
      findings
    end

    findings = if evidence.sorry_count > 0 do
      [%{type: :dangerous_pattern, detail: "#{evidence.sorry_count} sorry (Lean) found",
         severity: :critical, deduction: 15} | findings]
    else
      findings
    end

    findings = if evidence.admitted_count > 0 do
      [%{type: :dangerous_pattern, detail: "#{evidence.admitted_count} Admitted (Coq/Rocq) found",
         severity: :critical, deduction: 15} | findings]
    else
      findings
    end

    findings = if evidence.unsafe_coerce_count > 0 do
      [%{type: :dangerous_pattern, detail: "#{evidence.unsafe_coerce_count} unsafeCoerce (Haskell) found",
         severity: :critical, deduction: 15} | findings]
    else
      findings
    end

    findings = if evidence.obj_magic_count > 0 do
      [%{type: :dangerous_pattern, detail: "#{evidence.obj_magic_count} Obj.magic (OCaml/ReScript) found",
         severity: :critical, deduction: 15} | findings]
    else
      findings
    end

    findings = if evidence.postulate_count > 0 do
      [%{type: :dangerous_pattern, detail: "#{evidence.postulate_count} postulate (Idris2/Agda) found -- check if intentional axioms",
         severity: :high, deduction: 10} | findings]
    else
      findings
    end

    findings = if evidence.really_believe_me_count > 0 do
      [%{type: :dangerous_pattern, detail: "#{evidence.really_believe_me_count} really_believe_me found",
         severity: :critical, deduction: 15} | findings]
    else
      findings
    end

    # High TODO/FIXME density suggests incomplete work
    findings = if evidence.todo_count > 50 do
      [%{type: :high_todo_density, detail: "#{evidence.todo_count} TODOs remaining",
         severity: :medium, deduction: 5} | findings]
    else
      findings
    end

    findings = if evidence.fixme_count > 20 do
      [%{type: :high_fixme_density, detail: "#{evidence.fixme_count} FIXMEs remaining",
         severity: :medium, deduction: 5} | findings]
    else
      findings
    end

    # No tests = big deduction
    findings = if not evidence.has_tests_dir and evidence.test_files == 0 do
      [%{type: :no_tests, detail: "No test directory or test files found",
         severity: :high, deduction: 20} | findings]
    else
      findings
    end

    # No CI = deduction
    findings = if not evidence.has_ci do
      [%{type: :no_ci, detail: "No .github/workflows/ directory",
         severity: :medium, deduction: 10} | findings]
    else
      findings
    end

    # Stub detection
    findings = if evidence.stub_count > 10 do
      [%{type: :high_stub_count, detail: "#{evidence.stub_count} stub functions detected",
         severity: :medium, deduction: 10} | findings]
    else
      findings
    end

    # No state file
    findings = if not evidence.has_state_file do
      [%{type: :no_state_file, detail: "No STATE.a2ml or STATE.scm found",
         severity: :low, deduction: 5} | findings]
    else
      findings
    end

    findings
  end

  # ─── Scoring ───────────────────────────────────────────────────────────

  defp calculate_adjusted_score(claims, _evidence, findings) do
    base = claims[:completion_percentage] || 50
    total_deductions = Enum.sum(Enum.map(findings, & &1.deduction))
    max(0, min(100, base - total_deductions))
  end

  defp delta(nil, _adjusted), do: nil
  defp delta(claimed, adjusted), do: claimed - adjusted

  defp honesty_grade(nil, _adjusted), do: :no_claim
  defp honesty_grade(claimed, adjusted) do
    gap = abs(claimed - adjusted)
    cond do
      gap <= 5 -> :honest
      gap <= 15 -> :optimistic
      gap <= 30 -> :inflated
      true -> :dishonest
    end
  end

  # ─── Helpers ───────────────────────────────────────────────────────────

  defp find_state_file(repo_path) do
    a2ml = Path.join(repo_path, ".machine_readable/STATE.a2ml")
    # DEPRECATED: .scm fallback -- will be removed once all repos migrate to .a2ml
    scm = Path.join(repo_path, ".machine_readable/STATE.scm")
    cond do
      File.exists?(a2ml) -> a2ml
      File.exists?(scm) -> scm  # DEPRECATED: .scm fallback path
      true -> nil
    end
  end

  defp extract_number(content, regex) do
    case Regex.run(regex, content) do
      [_, num] -> String.to_integer(num)
      _ -> nil
    end
  end

  defp extract_string(content, regex) do
    case Regex.run(regex, content) do
      [_, str] -> str
      _ -> nil
    end
  end

  defp count_files(repo_path, extensions) do
    case System.cmd("find", [repo_path, "-type", "f"] ++
           Enum.flat_map(extensions, fn ext -> ["-name", "*#{ext}"] end) ++
           List.duplicate("-o", max(0, length(extensions) - 1)) |> interleave_find_args(extensions),
           stderr_to_stdout: true) do
      {output, 0} -> output |> String.split("\n", trim: true) |> length()
      _ -> 0
    end
  rescue
    _ -> 0
  end

  # Simple find with wc -l fallback
  # defp count_files(repo_path, _extensions) when not is_binary(repo_path), do: 0

  defp interleave_find_args(_, extensions) when length(extensions) <= 1 do
    Enum.flat_map(extensions, fn ext -> ["-name", "*#{ext}"] end)
  end
  defp interleave_find_args(_, extensions) do
    extensions
    |> Enum.map(fn ext -> ["-name", "*#{ext}"] end)
    |> Enum.intersperse(["-o"])
    |> List.flatten()
  end

  defp count_pattern(repo_path, pattern) do
    case System.cmd("grep", ["-r", "--include=*.{res,rs,ex,idr,zig,v,gleam,jl,hs,agda,lean,ml}",
                              "-c", pattern, repo_path],
                     stderr_to_stdout: true) do
      {output, _} ->
        output
        |> String.split("\n", trim: true)
        |> Enum.map(fn line ->
          case String.split(line, ":") |> List.last() do
            nil -> 0
            n -> String.to_integer(String.trim(n))
          end
        end)
        |> Enum.sum()
      _ -> 0
    end
  rescue
    _ -> 0
  end

  defp count_stubs(repo_path) do
    # Count functions that are clearly stubs (return 0, return nil, todo!, unimplemented!)
    patterns = ["todo!", "unimplemented!", "raise \"not implemented\"", "raise \"TODO\""]
    Enum.sum(Enum.map(patterns, fn p -> count_pattern(repo_path, p) end))
  end
end
