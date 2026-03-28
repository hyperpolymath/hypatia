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
  # Elixir Patterns — runtime safety for BEAM code
  # ---------------------------------------------------------------------------

  @elixir_patterns [
    %{id: :elixir_system_cmd_interpolation, severity: :critical,
      pattern: ~r/System\.cmd\(.*#\{/, cwe: "CWE-78",
      description: "System.cmd with string interpolation — command injection risk"},
    %{id: :elixir_code_eval, severity: :critical,
      pattern: ~r/Code\.eval_string|Code\.eval_quoted|Code\.eval_file/, cwe: "CWE-94",
      description: "Code.eval_* — arbitrary code execution risk"},
    %{id: :elixir_send_unsanitised, severity: :high,
      pattern: ~r/:erlang\.binary_to_term\(/, cwe: "CWE-502",
      description: "binary_to_term without :safe option — deserialization attack"},
    %{id: :elixir_atom_from_user, severity: :high,
      pattern: ~r/String\.to_atom\(/, cwe: "CWE-400",
      description: "String.to_atom with user input exhausts atom table — use to_existing_atom"},
    %{id: :elixir_wildcard_plug_cors, severity: :high,
      pattern: ~r/origin:\s*"\*"/, cwe: "CWE-942",
      description: "Plug CORS origin wildcard — restrict to known origins"},
    %{id: :elixir_no_ssl_verify, severity: :high,
      pattern: ~r/verify:\s*:verify_none|ssl:\s*\[verify:\s*:verify_none\]/, cwe: "CWE-295",
      description: "SSL verify_none disables certificate validation — MITM risk"},
    %{id: :elixir_port_open_shell, severity: :high,
      pattern: ~r/Port\.open\(\{:spawn,/, cwe: "CWE-78",
      description: "Port.open spawn — validate command before execution"},
    %{id: :elixir_send_resp_no_escape, severity: :medium,
      pattern: ~r/send_resp\(.*#\{/, cwe: "CWE-79",
      description: "send_resp with interpolation — potential XSS if HTML content type"}
  ]

  # ---------------------------------------------------------------------------
  # JavaScript/Web Security Patterns — general JS/TS/Deno files
  # ---------------------------------------------------------------------------

  @javascript_patterns [
    %{id: :js_wildcard_cors, severity: :high,
      pattern: ~r/Access-Control-Allow-Origin["':\s]+\*/, cwe: "CWE-942",
      description: "Wildcard CORS — restrict to specific origins or use env var"},
    %{id: :js_eval, severity: :critical,
      pattern: ~r/\beval\s*\(/, cwe: "CWE-94",
      description: "eval() — arbitrary code execution"},
    %{id: :js_innerhtml, severity: :high,
      pattern: ~r/\.innerHTML\s*=/, cwe: "CWE-79",
      description: "innerHTML assignment — XSS risk, use textContent or SafeDOM"},
    %{id: :js_document_write, severity: :high,
      pattern: ~r/document\.write\s*\(/, cwe: "CWE-79",
      description: "document.write — XSS risk"},
    %{id: :js_exec_sync, severity: :high,
      pattern: ~r/execSync\s*\(|child_process/, cwe: "CWE-78",
      description: "Shell execution — validate input before passing to shell"},
    %{id: :js_deno_all_perms, severity: :high,
      pattern: ~r/deno\s+run\s+(-A|--allow-all)\b/, cwe: "CWE-250",
      description: "Deno -A grants all permissions — use specific --allow-* flags"},
    %{id: :js_http_url_in_code, severity: :medium,
      pattern: ~r/["']http:\/\/(?!localhost|127\.0\.0\.1|0\.0\.0\.0)/, cwe: "CWE-319",
      description: "HTTP URL in code — use HTTPS for non-localhost"},
    %{id: :js_hardcoded_secret, severity: :critical,
      pattern: ~r/(api_key|apiKey|secret|password|token)\s*[:=]\s*["'][a-zA-Z0-9+\/=]{8,}["']/, cwe: "CWE-798",
      description: "Possible hardcoded credential — use environment variable"}
  ]

  # ---------------------------------------------------------------------------
  # Cross-Language Patterns — stub/placeholder detection
  # ---------------------------------------------------------------------------

  @stub_crypto_patterns [
    %{id: :stub_crypto_function, severity: :critical,
      pattern: ~r/stub:(sha|blake|argon|dilithium|kyber|chacha|hkdf|shake)/, cwe: "CWE-327",
      description: "Stub cryptographic implementation — must not ship to production"},
    %{id: :stub_hash_return, severity: :critical,
      pattern: ~r/format!\("stub:/, cwe: "CWE-327",
      description: "Rust function returning stub value — placeholder not real implementation"},
    %{id: :todo_crypto, severity: :high,
      pattern: ~r/TODO.*(?:Replace with real|implement|stub).*(?:hash|crypt|sign|verify|kdf)/i, cwe: "CWE-327",
      description: "TODO marker on cryptographic function — not yet implemented"},
    %{id: :fake_signature, severity: :critical,
      pattern: ~r/fake|placeholder|dummy.*(?:signature|key|hash|cert|token)/i, cwe: "CWE-327",
      description: "Fake/placeholder cryptographic value"}
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
  def patterns_for_language("elixir"), do: @elixir_patterns
  def patterns_for_language("javascript"), do: @javascript_patterns
  def patterns_for_language("typescript"), do: @javascript_patterns
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

  @doc "Check for missing forbid(unsafe_code) in Rust entry points"
  def check_rust_safety(file_list, file_contents \\ %{}) do
    Enum.flat_map(file_list, fn f ->
      basename = Path.basename(f)
      if basename in ["lib.rs", "main.rs"] do
        content = Map.get(file_contents, f, "")
        if content != "" and not String.contains?(content, "#![forbid(unsafe_code)]") do
          [%{rule: :missing_forbid_unsafe, severity: :low,
             description: "Rust entry point missing #![forbid(unsafe_code)]",
             file: f}]
        else
          []
        end
      else
        []
      end
    end)
  end

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

  @doc "Scan for stub/placeholder cryptographic implementations"
  def scan_stub_crypto(content) do
    Enum.flat_map(@stub_crypto_patterns, fn rule ->
      case Regex.scan(rule.pattern, content) do
        [] -> []
        matches -> [%{rule: rule.id, severity: rule.severity, cwe: rule.cwe,
                       description: rule.description, occurrences: length(matches)}]
      end
    end)
  end

  def stub_crypto_patterns, do: @stub_crypto_patterns

  @doc "Scan JavaScript/TypeScript content for web security issues"
  def scan_web_security(content) do
    Enum.flat_map(@javascript_patterns, fn rule ->
      case Regex.scan(rule.pattern, content) do
        [] -> []
        matches -> [%{rule: rule.id, severity: rule.severity, cwe: rule.cwe,
                       description: rule.description, occurrences: length(matches)}]
      end
    end)
  end

  def javascript_patterns, do: @javascript_patterns
  def elixir_patterns, do: @elixir_patterns
end
