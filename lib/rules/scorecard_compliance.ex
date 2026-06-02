# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ScorecardCompliance do
  @moduledoc """
  Enforces OpenSSF Scorecard best practices as automated Elixir rules.
  
  These rules specifically target the 'High Risk' items flagged by Scorecard
  to prevent quality gate failures before they reach the remote repository.
  """

  # ─── Scorecard Critical Metrics ──────────────────────────────────────
  #
  # High-risk metric categories tracked by this module:
  # :token_permissions, :pinned_dependencies, :dependency_update_tool,
  # :code_review_enforcement, :maintained_status, :fuzzing_integration

  @doc """
  Audits a repository for Scorecard compliance markers.
  """
  def audit(repo_path, file_list, file_contents \\ %{}) do
    [
      check_least_privilege(file_contents),
      check_dependency_tool(repo_path, file_list),
      check_vulnerability_scanning(file_list),
      check_fuzzing(file_list),
      check_cii_best_practices(file_list)
    ]
    |> List.flatten()
  end

  @doc """
  Check for automated vulnerability/secret scanning (Vulnerability-Scanning).
  """
  def check_vulnerability_scanning(file_list) do
    has_scanner = Enum.any?(file_list, fn f -> 
      String.contains?(f, "secret-scanner.yml") or 
      String.contains?(f, "gitleaks") or 
      String.contains?(f, "trufflehog")
    end)

    if has_scanner do
      []
    else
      [%{type: :scorecard_violation, metric: :vulnerability_scanning, file: ".github/workflows/secret-scanner.yml",
         severity: :high, detail: "Missing automated secret/vulnerability scanning. Gitleaks or TruffleHog integration is required."}]
    end
  end

  @doc """
  Check for excessive workflow permissions (Token-Permissions).
  Mandates specific permission blocks instead of 'read-all' or 'write-all'.
  """
  def check_least_privilege(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      cond do
        String.contains?(content, "permissions: write-all") ->
          [%{type: :scorecard_violation, metric: :token_permissions, file: filename,
             severity: :critical, detail: "Dangerous 'write-all' permissions detected. Use granular permissions."}]
        
        String.contains?(content, "permissions: read-all") ->
          [%{type: :scorecard_violation, metric: :token_permissions, file: filename,
             severity: :high, detail: "Broad 'read-all' permissions detected. Scorecard mandates granular blocks (e.g., contents: read)."}]
        
        true -> []
      end
    end)
  end

  @doc """
  Check for automated dependency update configuration (Dependency-Update-Tool).
  """
  def check_dependency_tool(_repo_path, file_list) do
    has_dependabot = Enum.any?(file_list, fn f -> 
      String.ends_with?(f, ".github/dependabot.yml") or String.ends_with?(f, ".github/dependabot.yaml")
    end)

    if has_dependabot do
      []
    else
      [%{type: :scorecard_violation, metric: :dependency_update_tool, file: ".github/dependabot.yml",
         severity: :high, detail: "Missing Dependabot configuration. Automated dependency updates are required."}]
    end
  end

  @doc """
  Check for Fuzzing integration.

  Three input shapes accepted:

    * `[file_path, ...]`               — legacy: file-presence check only
    * `%{files: [...], workflows: %{}}` — enriched: also classify failing
      sanitizer jobs (`PR (address)` / `PR (undefined)` / `PR (memory)`)
      by their three most common failure modes:
        1. sibling-crate missing in Docker fuzz context
        2. toolchain skew — rustc-1.91-not-supported
        3. build-script panic

  Returns the legacy list of findings, optionally enriched with one
  finding per classified sanitizer failure.
  """
  def check_fuzzing(input)

  def check_fuzzing(file_list) when is_list(file_list) do
    has_fuzzing =
      Enum.any?(file_list, fn f ->
        String.contains?(f, "fuzz") or String.contains?(f, "clusterfuzz")
      end)

    if has_fuzzing do
      []
    else
      [
        %{
          type: :scorecard_violation,
          metric: :fuzzing,
          file: "tests/fuzz/",
          severity: :medium,
          detail: "No fuzz testing detected. Consider adding ClusterFuzzLite or similar."
        }
      ]
    end
  end

  def check_fuzzing(%{files: file_list} = input) when is_list(file_list) do
    base = check_fuzzing(file_list)
    workflows = Map.get(input, :workflows, %{})
    base ++ classify_sanitizer_failures(workflows)
  end

  def check_fuzzing(_), do: []

  # Pattern set for "PR (address|undefined|memory)" job names emitted by
  # ClusterFuzzLite's PR-time sanitizer workflows. Each is a Regex that
  # matches a job name string.
  @sanitizer_job_patterns [
    {~r/^PR\s*\(\s*address\s*\)/i, :asan},
    {~r/^PR\s*\(\s*undefined\s*\)/i, :ubsan},
    {~r/^PR\s*\(\s*memory\s*\)/i, :msan}
  ]

  # Failure-mode classifiers ordered most-specific → most-generic.
  # `log_text` is the captured failure log (job step output) — any string;
  # we pattern-match for the three known recurring failure modes.
  defp classify_failure_mode(log_text) when is_binary(log_text) do
    cond do
      Regex.match?(
        ~r/(no matching package|could not find .*Cargo\.toml|sibling crate|workspace member.+not found)/i,
        log_text
      ) ->
        {:sibling_crate_missing,
         "Sibling crate referenced from fuzz/ is absent from the Docker " <>
           "build context. Add the missing crate to the fuzz Dockerfile " <>
           "COPY/workspace or vendor it before invoking cargo fuzz."}

      Regex.match?(
        ~r/(rustc 1\.9[1-9]|requires rustc.+1\.9[1-9]|not supported.+1\.9[1-9]|toolchain.+1\.9[1-9].+not (?:installed|supported))/i,
        log_text
      ) ->
        {:toolchain_skew_rustc_191,
         "Toolchain skew: a dependency declares rustc-1.91+, but the fuzz " <>
           "Docker image ships an older toolchain. Bump the rust-toolchain " <>
           "file or pin the dependency to a 1.90-compatible version."}

      Regex.match?(~r/(error: failed to run custom build command|build script .+ panicked|panicked at .*build\.rs)/i, log_text) ->
        {:build_script_panic,
         "Build-script panic: a transitive crate's `build.rs` crashed " <>
           "before any fuzz target could be linked. Inspect the panic " <>
           "message and pin the offending crate."}

      true ->
        :unknown
    end
  end

  defp classify_failure_mode(_), do: :unknown

  defp classify_sanitizer_failures(workflows) when is_map(workflows) do
    workflows
    |> Enum.flat_map(fn {job_name, payload} ->
      case classify_one_job(job_name, payload) do
        nil -> []
        finding -> [finding]
      end
    end)
  end

  defp classify_sanitizer_failures(_), do: []

  defp classify_one_job(job_name, payload) when is_binary(job_name) do
    matched =
      Enum.find_value(@sanitizer_job_patterns, nil, fn {re, kind} ->
        if Regex.match?(re, job_name), do: kind, else: nil
      end)

    log_text =
      cond do
        is_binary(payload) -> payload
        is_map(payload) -> Map.get(payload, :log, "") |> to_string()
        true -> ""
      end

    cond do
      is_nil(matched) ->
        nil

      true ->
        case classify_failure_mode(log_text) do
          :unknown ->
            %{
              type: :scorecard_violation,
              metric: :fuzzing,
              sanitizer: matched,
              job: job_name,
              severity: :low,
              classification: :unknown,
              detail:
                "Sanitizer job `#{job_name}` failed; failure mode did not " <>
                  "match any known recurring pattern."
            }

          {mode, advice} ->
            %{
              type: :scorecard_violation,
              metric: :fuzzing,
              sanitizer: matched,
              job: job_name,
              severity: :medium,
              classification: mode,
              detail: advice
            }
        end
    end
  end

  defp classify_one_job(_, _), do: nil

  @doc """
  Check for CII Best Practices badge (or entry point for the badge).
  """
  def check_cii_best_practices(file_list) do
    has_marker = Enum.any?(file_list, fn f -> 
      String.ends_with?(f, "CII-ENFORCEMENT.adoc") or String.contains?(f, "best-practices.coreinfrastructure.org")
    end)

    if has_marker do
      []
    else
      [%{type: :scorecard_violation, metric: :cii_best_practices, file: "README.md",
         severity: :low, detail: "Missing CII Best Practices marker or badge."}]
    end
  end
end
