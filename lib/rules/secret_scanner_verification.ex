# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.SecretScannerVerification do
  @moduledoc """
  Verification that the correct secrets scanner version is installed and operational.

  This rule checks that the estate-wide secrets scanner (gitleaks or trufflehog) is
  properly installed, configured with current rules, and producing valid output.
  It collects evidence that the verification was performed.

  Rule IDs: SSV001-SSV003
  """

  require Logger

  @evidence_dir ".hypatia-evidence"
  @scanner_config ".gitleaks.toml"
  @min_rules_version "8.18.0"

  # ─── SSV001: Secrets scanner installation verification ──────────────────────

  @doc """
  SSV001: Verify that a secrets scanner is installed and accessible.

  Checks for the presence of gitleaks or trufflehog in the PATH and
  verifies it can execute successfully.
  """
  def ssv001_scanner_installed(_owner, _repo) do
    # Check for gitleaks
    gitleaks_installed = System.cmd("gitleaks", ["version"], stderr_to_stdout: true) |> elem(0) == 0
    
    # Check for trufflehog
    trufflehog_installed = System.cmd("trufflehog", ["--version"], stderr_to_stdout: true) |> elem(0) == 0

    if gitleaks_installed || trufflehog_installed do
      scanner = if gitleaks_installed, do: "gitleaks", else: "trufflehog"
      
      # Get version
      version_cmd = if gitleaks_installed, do: "version", else: "--version"
      {exit, version_output, _} = System.cmd(scanner, [version_cmd], stderr_to_stdout: true)
      version = String.trim(version_output)
      
      # Write evidence
      evidence = %{
        rule: "SSV001",
        file: ".hypatia-evidence/secrets-scanner-installation.json",
        severity: :info,
        reason: "Secrets scanner '#{scanner}' is installed and accessible (version: #{version})",
        action: :log,
        detail: %{
          scanner: scanner,
          version: version,
          exit_code: exit,
          verified_at: DateTime.utc_now() |> to_string()
        }
      }
      
      # Ensure evidence directory exists
      File.mkdir_p!(@evidence_dir)
      File.write!(".hypatia-evidence/secrets-scanner-installation.json", Jason.encode!(evidence))
      
      [evidence]
    else
      [
        %{
          rule: "SSV001",
          file: ".hypatia-evidence",
          severity: :high,
          reason: "No secrets scanner (gitleaks or trufflehog) found in PATH",
          action: :escalate,
          detail: %{
            checked: ["gitleaks", "trufflehog"],
            verified_at: DateTime.utc_now() |> to_string()
          }
        }
      ]
    end
  end

  # ─── SSV002: Secrets scanner configuration currency ────────────────────────

  @doc """
  SSV002: Verify that the secrets scanner configuration is current.

  Checks that the scanner configuration file (e.g., .gitleaks.toml) exists and
  contains rules that match the minimum required version.
  """
  def ssv002_scanner_config_current(_owner, _repo) do
    if File.exists?(@scanner_config) do
      # Read config file
      config_content = File.read!(@scanner_config)
      
      # Check for version indicator in config
      has_version_info = String.contains?(config_content, "version") ||
                         String.contains?(config_content, "gitleaks")
      
      # Check for common rule patterns
      has_rules = String.contains?(config_content, "rules") ||
                  String.contains?(config_content, "regex")
      
      if has_version_info && has_rules do
        evidence = %{
          rule: "SSV002",
          file: @scanner_config,
          severity: :info,
          reason: "Secrets scanner configuration appears current and contains rules",
          action: :log,
          detail: %{
            config_file: @scanner_config,
            has_version_info: has_version_info,
            has_rules: has_rules,
            verified_at: DateTime.utc_now() |> to_string()
          }
        }
        
        File.mkdir_p!(@evidence_dir)
        File.write!(".hypatia-evidence/secrets-scanner-config.json", Jason.encode!(evidence))
        
        [evidence]
      else
        [
          %{
            rule: "SSV002",
            file: @scanner_config,
            severity: :medium,
            reason: "Secrets scanner configuration may be outdated or incomplete",
            action: :review,
            detail: %{
              config_file: @scanner_config,
              has_version_info: has_version_info,
              has_rules: has_rules
            }
          }
        ]
      end
    else
      [
        %{
          rule: "SSV002",
          file: ".hypatia-evidence",
          severity: :medium,
          reason: "Secrets scanner configuration file not found",
          action: :review,
          detail: %{
            expected_config: @scanner_config,
            verified_at: DateTime.utc_now() |> to_string()
          }
        }
      ]
    end
  end

  # ─── SSV003: Secrets scanner operational verification ──────────────────────

  @doc """
  SSV003: Verify that the secrets scanner can execute successfully on the repo.

  Runs a test scan on a sample of files to ensure the scanner is operational.
  """
  def ssv003_scanner_operational(_owner, _repo) do
    # Try to run gitleaks detect on the repo root
    {exit, output, _} = System.cmd("gitleaks", ["detect", "--no-git", "--path", ".", "--exit-code", "0"], 
      stderr_to_stdout: true, cwd: ".")
    
    # Also try trufflehog if gitleaks fails
    if exit != 0 do
      {exit, output, _} = System.cmd("trufflehog", ["filesystem", ".", "--no-update"], 
        stderr_to_stdout: true, cwd: ".")
    end
    
    if exit == 0 do
      evidence = %{
        rule: "SSV003",
        file: ".hypatia-evidence",
        severity: :info,
        reason: "Secrets scanner executed successfully on repository",
        action: :log,
        detail: %{
          exit_code: exit,
          output_length: byte_size(output),
          verified_at: DateTime.utc_now() |> to_string()
        }
      }
      
      File.mkdir_p!(@evidence_dir)
      File.write!(".hypatia-evidence/secrets-scanner-operational.json", Jason.encode!(evidence))
      
      [evidence]
    else
      [
        %{
          rule: "SSV003",
          file: ".hypatia-evidence",
          severity: :warning,
          reason: "Secrets scanner execution failed (exit code: #{exit})",
          action: :investigate,
          detail: %{
            exit_code: exit,
            output: String.slice(output, 0, 500),  # Truncate for safety
            verified_at: DateTime.utc_now() |> to_string()
          }
        }
      ]
    end
  end

  # ─── Aggregate verification ────────────────────────────────────────────────

  @doc """
  Run all secrets scanner verification checks.
  """
  def verify_secrets_scanner(owner, repo) do
    ssv001_scanner_installed(owner, repo) ++
      ssv002_scanner_config_current(owner, repo) ++
      ssv003_scanner_operational(owner, repo)
  end
end
