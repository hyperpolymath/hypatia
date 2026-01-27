# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Patterns do
  @moduledoc """
  Security pattern definitions for code scanning.
  """

  alias Hypatia.Finding

  @doc """
  Get all patterns for a given file extension.
  """
  def patterns_for_extension(ext) do
    case ext do
      ".res" -> rescript_patterns()
      ".rs" -> rust_patterns()
      ".ml" -> ocaml_patterns()
      _ -> []
    end
  end

  @doc """
  ReScript security patterns.
  """
  def rescript_patterns do
    [
      %{
        name: "getexn_on_external_data",
        regex: ~r/getExn/,
        severity: :critical,
        type: "unsafe_crash",
        cwe: "CWE-754",
        fix: "Replace getExn with switch/match or getWithDefault"
      },
      %{
        name: "obj_magic_bypass",
        regex: ~r/Obj\.magic/,
        severity: :high,
        type: "type_safety_bypass",
        cwe: "CWE-704",
        fix: "Remove Obj.magic and use proper type conversions"
      }
    ]
  end

  @doc """
  Rust security patterns.
  """
  def rust_patterns do
    [
      %{
        name: "unwrap_without_check",
        regex: ~r/\.unwrap\(\)/,
        severity: :high,
        type: "unsafe_panic",
        cwe: "CWE-754",
        fix: "Replace unwrap() with ? operator or match"
      },
      %{
        name: "expect_without_check",
        regex: ~r/\.expect\(/,
        severity: :medium,
        type: "unsafe_panic",
        cwe: "CWE-754",
        fix: "Use proper error propagation with ? operator"
      }
    ]
  end

  @doc """
  OCaml security patterns.
  """
  def ocaml_patterns do
    [
      %{
        name: "unsafe_get",
        regex: ~r/Array\.unsafe_get|String\.unsafe_get/,
        severity: :high,
        type: "bounds_check_bypass",
        cwe: "CWE-129",
        fix: "Use safe Array.get or String.get with bounds checking"
      }
    ]
  end

  @doc """
  Universal patterns (apply to all files).
  """
  def universal_patterns do
    [
      %{
        name: "wildcard_cors",
        regex: ~r/Access-Control-Allow-Origin.*"\*"/,
        severity: :critical,
        type: "cors_misconfiguration",
        cwe: "CWE-942",
        fix: "Replace '*' with environment-based origin whitelist"
      },
      %{
        name: "unverified_jwt_decode",
        regex: ~r/decodeJWT/,
        # Only match if NOT followed by verifyJWT in same context
        exclude_if: ~r/verifyJWT/,
        severity: :critical,
        type: "auth_bypass",
        cwe: "CWE-347",
        fix: "Always verify JWT signatures before trusting payload"
      }
    ]
  end

  @doc """
  Scan a line of code against patterns and return findings.
  """
  def scan_line(file, line_num, line_content, patterns) do
    Enum.flat_map(patterns, fn pattern ->
      if Regex.match?(pattern.regex, line_content) do
        # Check exclusion pattern if present
        if Map.has_key?(pattern, :exclude_if) and
             Regex.match?(pattern.exclude_if, line_content) do
          []
        else
          [
            Finding.new(%{
              severity: pattern.severity,
              type: pattern.type,
              pattern: pattern.name,
              file: file,
              line: line_num,
              code: String.trim(line_content),
              cwe: pattern.cwe,
              fix: pattern.fix
            })
          ]
        end
      else
        []
      end
    end)
  end
end
