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

  # ---------------------------------------------------------------------------
  # Nickel Patterns — config-time security and RSR policy enforcement
  # ---------------------------------------------------------------------------

  @nickel_patterns [
    %{id: :ncl_http_url, severity: :high,
      pattern: ~r/=\s*"http:\/\//, cwe: "CWE-319",
      description: "HTTP URL in Nickel config — must use HTTPS"},
    %{id: :ncl_weak_hash, severity: :high,
      pattern: ~r/(md5:|sha1:)[a-fA-F0-9]+/, cwe: "CWE-328",
      description: "Weak hash (MD5/SHA-1) in config — use SHA-256+"},
    %{id: :ncl_action_version_tag, severity: :medium,
      pattern: ~r/uses\s*=\s*"[^@]+@v\d+/, cwe: "CWE-829",
      description: "GitHub Action pinned by tag not SHA — supply chain risk"},
    %{id: :ncl_banned_language_ref, severity: :high,
      pattern: ~r/language\s*=\s*"(typescript|go|python|java|kotlin|swift|dart)"/, cwe: "CWE-1104",
      description: "Banned language referenced in Nickel build target"},
    %{id: :ncl_missing_spdx, severity: :medium,
      pattern: ~r/\A(?!.*SPDX-License-Identifier).{0,500}\z/s, cwe: "CWE-1104",
      description: "Nickel file missing SPDX-License-Identifier header"},
    %{id: :ncl_k9_missing_pedigree, severity: :high,
      pattern: ~r/\AK9!(?!.*pedigree\s*=).+\z/s, cwe: "CWE-1104",
      description: "K9 contractile missing pedigree section"},
    %{id: :ncl_hardcoded_secret, severity: :critical,
      pattern: ~r/password\s*=\s*"[^"]+"|api_key\s*=\s*"[^"]+"|secret\s*=\s*"[^"]+"/, cwe: "CWE-798",
      description: "Hardcoded credential in Nickel config — use SecretRef"},
    %{id: :ncl_docker_not_podman, severity: :medium,
      pattern: ~r/docker\s|docker\.io|dockerfile/i, cwe: "CWE-1104",
      description: "Docker reference in Nickel config — RSR requires Podman/Containerfile"}
  ]

  def patterns_for_language("rust"), do: @rust_patterns
  def patterns_for_language("rescript"), do: @rescript_patterns
  def patterns_for_language("idris2"), do: @idris2_banned
  def patterns_for_language("haskell"), do: @haskell_banned
  def patterns_for_language("ocaml"), do: @ocaml_banned
  def patterns_for_language("coq"), do: @coq_banned
  def patterns_for_language("lean"), do: @lean_banned
  def patterns_for_language("nickel"), do: @nickel_patterns
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

  # ---------------------------------------------------------------------------
  # RSR Compliance Patterns (structural, not content-based)
  # ---------------------------------------------------------------------------

  @banned_file_extensions [
    %{id: :python_file, severity: :critical, glob: "*.py",
      language: "Python", replacement: "Julia/Rust/ReScript",
      description: "Python file detected — banned language"},
    %{id: :typescript_file, severity: :critical, glob: "*.ts",
      language: "TypeScript", replacement: "ReScript",
      description: "TypeScript file detected — banned language"},
    %{id: :golang_file, severity: :critical, glob: "*.go",
      language: "Go", replacement: "Rust",
      description: "Go file detected — banned language"}
  ]

  @scm_canonical_dir ".machine_readable"
  @scm_file_names ~w(STATE.a2ml META.a2ml ECOSYSTEM.a2ml AGENTIC.a2ml NEUROSYM.a2ml PLAYBOOK.a2ml LANGUAGES.a2ml)

  def banned_file_extensions, do: @banned_file_extensions
  def scm_file_names, do: @scm_file_names

  @doc "Check if any SCM files exist outside .machine_readable/"
  def check_scm_locations(file_list) do
    Enum.flat_map(@scm_file_names, fn scm ->
      file_list
      |> Enum.filter(fn f ->
        String.ends_with?(f, "/" <> scm) and
          not String.contains?(f, @scm_canonical_dir <> "/" <> scm)
      end)
      |> Enum.map(fn f ->
        %{rule: :scm_wrong_location, severity: :critical,
          description: "#{scm} found outside #{@scm_canonical_dir}/",
          file: f, expected_dir: @scm_canonical_dir}
      end)
    end)
  end

  @doc "Check for Dockerfile instead of Containerfile"
  def check_dockerfile_naming(file_list) do
    file_list
    |> Enum.filter(fn f ->
      basename = Path.basename(f)
      basename == "Dockerfile" or String.starts_with?(basename, "Dockerfile.")
    end)
    |> Enum.map(fn f ->
      %{rule: :dockerfile_not_containerfile, severity: :high,
        description: "Dockerfile detected — must be named Containerfile",
        file: f}
    end)
  end

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
