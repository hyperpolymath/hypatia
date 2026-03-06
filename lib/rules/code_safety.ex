# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.CodeSafety do
  @moduledoc """
  Code safety rules absorbed from Logtalk code-safety-lessons.lgt and
  container-security-lessons.lgt.

  Detects dangerous patterns in Rust, ReScript, and container code.
  """

  # ---------------------------------------------------------------------------
  # Dangerous Code Patterns (per language)
  # ---------------------------------------------------------------------------

  @rust_patterns [
    %{id: :unwrap_without_check, severity: :high,
      pattern: ~r/\.unwrap\(\)/, cwe: "CWE-754",
      description: "unwrap() without prior check — DoS via panic"},
    %{id: :unwrap_dangerous_default, severity: :critical,
      pattern: ~r/\.unwrap_or\(0\)/, cwe: "CWE-754",
      description: "unwrap_or(0) with dangerous default"},
    %{id: :expect_in_hot_path, severity: :medium,
      pattern: ~r/\.expect\(/, cwe: "CWE-754",
      description: "expect() in hot path"},
    %{id: :lock_unwrap, severity: :high,
      pattern: ~r/\.lock\(\)\.unwrap\(\)/, cwe: "CWE-754",
      description: "Lock.unwrap() without poison handling"},
    %{id: :unsafe_block, severity: :medium,
      pattern: ~r/unsafe\s*\{/, cwe: "CWE-676",
      description: "unsafe block — requires SAFETY comment"}
  ]

  @rescript_patterns [
    %{id: :getexn_on_external, severity: :critical,
      pattern: ~r/getExn/, cwe: "CWE-754",
      description: "getExn on external data — use pattern matching"},
    %{id: :obj_magic, severity: :high,
      pattern: ~r/Obj\.magic/, cwe: "CWE-704",
      description: "Obj.magic bypassing type safety"},
    %{id: :json_decode_no_validation, severity: :critical,
      pattern: ~r/JSON\.parseExn/, cwe: "CWE-20",
      description: "JSON decode without validation"}
  ]

  @idris2_banned [
    %{id: :believe_me, severity: :critical,
      pattern: ~r/believe_me/, cwe: "CWE-704",
      description: "believe_me undermines formal verification"},
    %{id: :assert_total, severity: :high,
      pattern: ~r/assert_total/, cwe: "CWE-704",
      description: "assert_total bypasses totality checker"},
    %{id: :assert_smaller, severity: :high,
      pattern: ~r/assert_smaller/, cwe: "CWE-704",
      description: "assert_smaller bypasses termination checker"},
    %{id: :unsafe_perform_io, severity: :critical,
      pattern: ~r/unsafePerformIO/, cwe: "CWE-676",
      description: "unsafePerformIO breaks referential transparency"}
  ]

  @haskell_banned [
    %{id: :unsafe_coerce, severity: :critical,
      pattern: ~r/unsafeCoerce/, cwe: "CWE-704",
      description: "unsafeCoerce bypasses type system"},
    %{id: :unsafe_perform_io_hs, severity: :critical,
      pattern: ~r/unsafePerformIO/, cwe: "CWE-676",
      description: "unsafePerformIO breaks purity"},
    %{id: :undefined_error, severity: :high,
      pattern: ~r/\bundefined\b/, cwe: "CWE-754",
      description: "undefined/error causes runtime crash"}
  ]

  @ocaml_banned [
    %{id: :obj_magic_ocaml, severity: :critical,
      pattern: ~r/Obj\.magic/, cwe: "CWE-704",
      description: "Obj.magic bypasses type system"},
    %{id: :obj_repr, severity: :critical,
      pattern: ~r/Obj\.repr/, cwe: "CWE-704",
      description: "Obj.repr unsafe representation access"}
  ]

  @coq_banned [
    %{id: :admitted, severity: :critical,
      pattern: ~r/\bAdmitted\b/, cwe: "CWE-704",
      description: "Admitted leaves proof hole"}
  ]

  @lean_banned [
    %{id: :sorry, severity: :critical,
      pattern: ~r/\bsorry\b/, cwe: "CWE-704",
      description: "sorry leaves proof hole"}
  ]

  def patterns_for_language("rust"), do: @rust_patterns
  def patterns_for_language("rescript"), do: @rescript_patterns
  def patterns_for_language("idris2"), do: @idris2_banned
  def patterns_for_language("haskell"), do: @haskell_banned
  def patterns_for_language("ocaml"), do: @ocaml_banned
  def patterns_for_language("coq"), do: @coq_banned
  def patterns_for_language("lean"), do: @lean_banned
  def patterns_for_language(_), do: []

  def scan_content(content, language) do
    patterns_for_language(language)
    |> Enum.flat_map(fn rule ->
      case Regex.scan(rule.pattern, content) do
        [] -> []
        matches -> [%{rule: rule.id, severity: rule.severity, cwe: rule.cwe,
                       description: rule.description, occurrences: length(matches)}]
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Container Security Patterns
  # ---------------------------------------------------------------------------

  @container_patterns [
    %{id: :env_var_injection, severity: :critical,
      pattern: ~r/Command::new\(.*env::var/s, cwe: "CWE-78",
      description: "Environment variable in Command::new without validation"},
    %{id: :shell_unquoted, severity: :critical,
      pattern: ~r/exec\s+\$[A-Z_]+[^"]/,  cwe: "CWE-78",
      description: "Unquoted shell variable in exec"},
    %{id: :wildcard_cors, severity: :critical,
      pattern: ~r/Access-Control-Allow-Origin.*\*/, cwe: "CWE-942",
      description: "Wildcard CORS allowing any origin"},
    %{id: :default_to_root, severity: :critical,
      pattern: ~r/uid\s*[:=]\s*0\b/, cwe: "CWE-250",
      description: "Default to root UID"},
    %{id: :path_traversal_risk, severity: :high,
      pattern: ~r/user_input.*[\/\\]/, cwe: "CWE-22",
      description: "Path traversal risk with user input"},
    %{id: :permissive_bypass, severity: :high,
      pattern: ~r/[Pp]ermissive.*continue|[Aa]udit.*mode/, cwe: "CWE-863",
      description: "Permissive/audit mode continues after verification failure"}
  ]

  def container_patterns, do: @container_patterns

  def scan_container_code(content) do
    Enum.flat_map(@container_patterns, fn rule ->
      if Regex.match?(rule.pattern, content) do
        [%{rule: rule.id, severity: rule.severity, cwe: rule.cwe,
           description: rule.description}]
      else
        []
      end
    end)
  end
end
