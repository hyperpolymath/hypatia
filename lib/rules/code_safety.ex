# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.CodeSafety do
  @moduledoc """
  Primary Elixir rules for code safety (migrated from legacy Logtalk engine).

  Detects dangerous patterns in Rust, ReScript, BEAM, and container code.
  """

  # ---------------------------------------------------------------------------
  # Dangerous Code Patterns (per language)
  # ---------------------------------------------------------------------------

  @rust_patterns [
    %{id: :unwrap_without_check, severity: :high,
      pattern: ~r/\.unwrap\(\)/, cwe: "CWE-754",
      description: "unwrap() without prior check -- DoS via panic"},
    %{id: :unwrap_dangerous_default, severity: :critical,
      pattern: ~r/\.unwrap_or\(0\)/, cwe: "CWE-754",
      description: "unwrap_or(0) with dangerous default"},
    %{id: :expect_in_hot_path, severity: :medium,
      pattern: ~r/\.expect\(/, cwe: "CWE-754",
      description: "expect() in hot path"},
    %{id: :lock_unwrap, severity: :high,
      pattern: ~r/\.lock\(\)\.unwrap\(\)/, cwe: "CWE-754",
      description: "Lock.unwrap() without poison handling"},
    %{id: :rwlock_unwrap, severity: :high,
      pattern: ~r/\.(read|write)\(\)\.unwrap\(\)/, cwe: "CWE-754",
      description: "RwLock read/write unwrap -- poison will cascade panic"},
    %{id: :unsafe_block, severity: :medium,
      pattern: ~r/unsafe\s*\{/, cwe: "CWE-676",
      description: "unsafe block -- requires SAFETY comment"},
    %{id: :panic_macro, severity: :high,
      pattern: ~r/panic!\s*\(/, cwe: "CWE-754",
      description: "panic! macro causes unrecoverable crash"},
    %{id: :todo_macro, severity: :high,
      pattern: ~r/todo!\s*\(/, cwe: "CWE-754",
      description: "todo! macro marks incomplete implementation that will panic"},
    %{id: :unimplemented_macro, severity: :high,
      pattern: ~r/unimplemented!\s*\(/, cwe: "CWE-754",
      description: "unimplemented! macro marks unfinished code that will panic"},
    %{id: :transmute, severity: :critical,
      pattern: ~r/transmute/, cwe: "CWE-704",
      description: "mem::transmute bypasses type safety with unchecked bit reinterpretation"},
    %{id: :mem_forget, severity: :high,
      pattern: ~r/mem::forget/, cwe: "CWE-401",
      description: "mem::forget leaks memory by skipping Drop"},
    %{id: :manually_drop, severity: :medium,
      pattern: ~r/ManuallyDrop/, cwe: "CWE-401",
      description: "ManuallyDrop requires manual resource management, risk of leaks"},
    %{id: :from_raw, severity: :high,
      pattern: ~r/from_raw/, cwe: "CWE-676",
      description: "from_raw constructs types from raw pointers without safety checks"},
    %{id: :as_ptr, severity: :medium,
      pattern: ~r/as_ptr/, cwe: "CWE-676",
      description: "as_ptr exposes raw pointer that may dangle or alias unsafely"}
  ]

  @rescript_patterns [
    %{id: :getexn_on_external, severity: :critical,
      pattern: ~r/getExn/, cwe: "CWE-754",
      description: "getExn on external data -- use pattern matching"},
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
      description: "Admitted leaves proof hole"},
    %{id: :coq_admit_tactic, severity: :critical,
      pattern: ~r/\badmit\b/, cwe: "CWE-704",
      description: "Coq admit tactic leaves goal unproven"},
    %{id: :coq_axiom, severity: :medium,
      pattern: ~r/\bAxiom\s/, cwe: "CWE-704",
      description: "User-defined Coq axiom -- not verified by kernel"}
  ]

  @lean_banned [
    %{id: :sorry, severity: :critical,
      pattern: ~r/\bsorry\b/, cwe: "CWE-704",
      description: "sorry leaves proof hole"},
    %{id: :lean_native_decide, severity: :high,
      pattern: ~r/\bnative_decide\b/, cwe: "CWE-704",
      description: "native_decide bypasses kernel checking via native code evaluation"},
    %{id: :lean_axiom, severity: :medium,
      pattern: ~r/\baxiom\s/, cwe: "CWE-704",
      description: "User-defined axiom -- not verified by Lean kernel"}
  ]

  @agda_banned [
    %{id: :agda_postulate, severity: :critical,
      pattern: ~r/\bpostulate\b/, cwe: "CWE-704",
      description: "Agda postulate assumes without proof -- potential soundness hole"},
    %{id: :agda_type_in_type, severity: :critical,
      pattern: ~r/--type-in-type/, cwe: "CWE-704",
      description: "UNSOUND: --type-in-type collapses type hierarchy (Girard's paradox)"}
  ]

  @isabelle_banned [
    %{id: :isabelle_oops, severity: :high,
      pattern: ~r/\boops\b/, cwe: "CWE-704",
      description: "Isabelle oops -- intentionally incomplete proof"}
  ]

  @hol4_banned [
    %{id: :hol4_mk_thm, severity: :critical,
      pattern: ~r/\bmk_thm\b/, cwe: "CWE-704",
      description: "UNSOUND: mk_thm bypasses HOL4 kernel entirely"}
  ]

  @zig_patterns [
    %{id: :zig_ptr_cast, severity: :high,
      pattern: ~r/@ptrCast/, cwe: "CWE-704",
      description: "Zig @ptrCast performs unchecked pointer type conversion"},
    %{id: :zig_align_cast, severity: :high,
      pattern: ~r/@alignCast/, cwe: "CWE-704",
      description: "Zig @alignCast performs unchecked alignment cast"},
    %{id: :zig_int_to_ptr, severity: :high,
      pattern: ~r/@intToPtr/, cwe: "CWE-676",
      description: "Zig @intToPtr converts integer to pointer without safety checks"},
    %{id: :zig_bit_cast, severity: :medium,
      pattern: ~r/@bitCast/, cwe: "CWE-704",
      description: "Zig @bitCast reinterprets bits without type checking"},
    %{id: :zig_ptr_to_int, severity: :medium,
      pattern: ~r/@ptrToInt/, cwe: "CWE-676",
      description: "Zig @ptrToInt exposes raw pointer address"}
  ]

  @fstar_banned [
    %{id: :fstar_admit, severity: :critical,
      pattern: ~r/\badmit\b/, cwe: "CWE-704",
      description: "F* admit accepts goal without proof"},
    %{id: :fstar_assume, severity: :critical,
      pattern: ~r/\bassume\b/, cwe: "CWE-704",
      description: "F* assume introduces unverified assumption"}
  ]

  @ada_spark_patterns [
    %{id: :ada_pragma_suppress, severity: :high,
      pattern: ~r/pragma\s+Suppress/, cwe: "CWE-704",
      description: "Ada pragma Suppress disables runtime checks"},
    %{id: :ada_unchecked_conversion, severity: :high,
      pattern: ~r/Unchecked_Conversion/, cwe: "CWE-704",
      description: "Ada Unchecked_Conversion bypasses type safety"},
    %{id: :ada_unchecked_deallocation, severity: :high,
      pattern: ~r/Unchecked_Deallocation/, cwe: "CWE-401",
      description: "Ada Unchecked_Deallocation manual memory management"},
    %{id: :ada_unchecked_access, severity: :high,
      pattern: ~r/Unchecked_Access/, cwe: "CWE-676",
      description: "Ada Unchecked_Access bypasses accessibility checks"}
  ]

  # ---------------------------------------------------------------------------
  # Elixir Patterns -- runtime safety for BEAM code
  # ---------------------------------------------------------------------------

  @elixir_patterns [
    %{id: :elixir_system_cmd_interpolation, severity: :critical,
      pattern: ~r/System\.cmd\(.*#\{/, cwe: "CWE-78",
      description: "System.cmd with string interpolation -- command injection risk"},
    %{id: :elixir_code_eval, severity: :critical,
      pattern: ~r/Code\.eval_string|Code\.eval_quoted|Code\.eval_file/, cwe: "CWE-94",
      description: "Code.eval_* -- arbitrary code execution risk"},
    %{id: :elixir_send_unsanitised, severity: :high,
      pattern: ~r/:erlang\.binary_to_term\(/, cwe: "CWE-502",
      description: "binary_to_term without :safe option -- deserialization attack"},
    %{id: :elixir_atom_from_user, severity: :high,
      pattern: ~r/String\.to_atom\(/, cwe: "CWE-400",
      description: "String.to_existing_atom with user input exhausts atom table -- use to_existing_atom"},
    %{id: :elixir_wildcard_plug_cors, severity: :high,
      pattern: ~r/origin:\s*"\*"/, cwe: "CWE-942",
      description: "Plug CORS origin wildcard -- restrict to known origins"},
    %{id: :elixir_no_ssl_verify, severity: :high,
      pattern: ~r/verify:\s*:verify_none|ssl:\s*\[verify:\s*:verify_none\]/, cwe: "CWE-295",
      description: "SSL verify_none disables certificate validation -- MITM risk"},
    %{id: :elixir_port_open_shell, severity: :high,
      pattern: ~r/Port\.open\(\{:spawn,/, cwe: "CWE-78",
      description: "Port.open spawn -- validate command before execution"},
    %{id: :elixir_send_resp_no_escape, severity: :medium,
      pattern: ~r/send_resp\(.*#\{/, cwe: "CWE-79",
      description: "send_resp with interpolation -- potential XSS if HTML content type"}
  ]

  # ---------------------------------------------------------------------------
  # Erlang Patterns -- runtime safety for BEAM code
  # ---------------------------------------------------------------------------

  @erlang_patterns [
    %{id: :erlang_binary_to_term_unsafe, severity: :critical,
      pattern: ~r/\b(?:erlang:)?binary_to_term\(/, cwe: "CWE-502",
      description: "binary_to_term without safe decoding options -- deserialization attack"},
    %{id: :erlang_atom_from_untrusted, severity: :high,
      pattern: ~r/\b(?:erlang:)?(?:binary_to_atom|list_to_atom)\(/, cwe: "CWE-400",
      description: "Atom creation from untrusted input may exhaust atom table"},
    %{id: :erlang_command_exec, severity: :high,
      pattern: ~r/\bos:cmd\(|open_port\(\s*\{spawn,|erlang:open_port\(\s*\{spawn,/, cwe: "CWE-78",
      description: "Shell command execution from Erlang runtime -- validate input and avoid shell"},
    %{id: :erlang_tls_verify_none, severity: :high,
      pattern: ~r/ssl:connect\([^)]*\{verify,\s*verify_none\}/s, cwe: "CWE-295",
      description: "TLS verification disabled in ssl:connect -- MITM risk"},
    %{id: :erlang_insecure_httpc, severity: :medium,
      pattern: ~r/httpc:request\(\s*\"http:\/\//, cwe: "CWE-319",
      description: "Insecure HTTP request in Erlang code -- use HTTPS"}
  ]

  # ---------------------------------------------------------------------------
  # JavaScript/Web Security Patterns -- general JS/TS/Deno files
  # ---------------------------------------------------------------------------

  @javascript_patterns [
    %{id: :js_wildcard_cors, severity: :high,
      pattern: ~r/Access-Control-Allow-Origin["':\s]+\*/, cwe: "CWE-942",
      description: "Wildcard CORS -- restrict to specific origins or use env var"},
    %{id: :js_eval, severity: :critical,
      pattern: ~r/\beval\s*\(/, cwe: "CWE-94",
      description: "eval() -- arbitrary code execution"},
    %{id: :js_innerhtml, severity: :high,
      pattern: ~r/\.innerHTML\s*=/, cwe: "CWE-79",
      description: "innerHTML assignment -- XSS risk, use textContent or SafeDOM"},
    %{id: :js_document_write, severity: :high,
      pattern: ~r/document\.write\s*\(/, cwe: "CWE-79",
      description: "document.write -- XSS risk"},
    %{id: :js_exec_sync, severity: :high,
      pattern: ~r/execSync\s*\(|child_process/, cwe: "CWE-78",
      description: "Shell execution -- validate input before passing to shell"},
    %{id: :js_deno_all_perms, severity: :high,
      pattern: ~r/deno\s+run\s+(-A|--allow-all)\b/, cwe: "CWE-250",
      description: "Deno -A grants all permissions -- use specific --allow-* flags"},
    %{id: :js_http_url_in_code, severity: :medium,
      pattern: ~r/["']http:\/\/(?!localhost|127\.0\.0\.1|0\.0\.0\.0)/, cwe: "CWE-319",
      description: "HTTP URL in code -- use HTTPS for non-localhost"},
    %{id: :js_hardcoded_secret, severity: :critical,
      pattern: ~r/(api_key|apiKey|secret|password|token)\s*[:=]\s*["'][a-zA-Z0-9+\/=]{8,}["']/, cwe: "CWE-798",
      description: "Possible hardcoded credential -- use environment variable"},
    %{id: :js_insecure_random_security_context, severity: :high,
      pattern: ~r/(?i)\b(?:session|token|nonce|secret|auth|csrf)\w*\b\s*[:=][^\n]*Math\.random\(/, cwe: "CWE-338",
      description: "Math.random() used for security-sensitive identifier/token generation -- use crypto.randomUUID or getRandomValues"}
  ]

  @shell_patterns [
    %{id: :shell_download_then_run, severity: :high,
      pattern: ~r/\b(?:curl|wget)\b[^\n|;]*\|\s*(?:sh|bash)\b/, cwe: "CWE-494",
      description: "Download-and-execute pattern (curl|wget pipe to shell) -- verify integrity before execution"},
    %{id: :shell_process_substitution_download_exec, severity: :high,
      pattern: ~r/\b(?:sh|bash)\s+<\(\s*(?:curl|wget)\b/, cwe: "CWE-494",
      description: "Process-substitution download-and-execute detected -- avoid executing remote scripts directly"}
  ]

  # ---------------------------------------------------------------------------
  # Cross-Language Patterns -- stub/placeholder detection
  # ---------------------------------------------------------------------------

  @stub_crypto_patterns [
    %{id: :stub_crypto_function, severity: :critical,
      pattern: ~r/stub:(sha|blake|argon|dilithium|kyber|chacha|hkdf|shake)/, cwe: "CWE-327",
      description: "Stub cryptographic implementation -- must not ship to production"},
    %{id: :stub_hash_return, severity: :critical,
      pattern: ~r/format!\("stub:/, cwe: "CWE-327",
      description: "Rust function returning stub value -- placeholder not real implementation"},
    %{id: :todo_crypto, severity: :high,
      pattern: ~r/TODO.*(?:Replace with real|implement|stub).*(?:hash|crypt|sign|verify|kdf)/i, cwe: "CWE-327",
      description: "TODO marker on cryptographic function -- not yet implemented"},
    %{id: :fake_signature, severity: :critical,
      pattern: ~r/fake|placeholder|dummy.*(?:signature|key|hash|cert|token)/i, cwe: "CWE-327",
      description: "Fake/placeholder cryptographic value"}
  ]

  # ---------------------------------------------------------------------------
  # Nickel Patterns -- config-time security and RSR policy enforcement
  # ---------------------------------------------------------------------------

  @nickel_patterns [
    # Match `= "http://…"` but NOT loopback addresses (localhost,
    # 127.0.0.1, [::1]) — those legitimately speak plaintext because
    # there's no network path to MITM. Negative-lookahead handles all
    # three forms in one alternation.
    %{id: :ncl_http_url, severity: :high,
      pattern: ~r/=\s*"http:\/\/(?!localhost|127\.0\.0\.1|\[::1\])/,
      cwe: "CWE-319",
      description: "HTTP URL in Nickel config -- must use HTTPS"},
    %{id: :ncl_weak_hash, severity: :high,
      pattern: ~r/(md5:|sha1:)[a-fA-F0-9]+/, cwe: "CWE-328",
      description: "Weak hash (MD5/SHA-1) in config -- use SHA-256+"},
    %{id: :ncl_action_version_tag, severity: :medium,
      pattern: ~r/uses\s*=\s*"[^@]+@v\d+/, cwe: "CWE-829",
      description: "GitHub Action pinned by tag not SHA -- supply chain risk"},
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
      description: "Hardcoded credential in Nickel config -- use SecretRef"},
    %{id: :ncl_docker_not_podman, severity: :medium,
      pattern: ~r/docker\s|docker\.io|dockerfile/i, cwe: "CWE-1104",
      description: "Docker reference in Nickel config -- RSR requires Podman/Containerfile"}
  ]

  def patterns_for_language("rust"), do: @rust_patterns
  def patterns_for_language("rescript"), do: @rescript_patterns
  def patterns_for_language("idris2"), do: @idris2_banned
  def patterns_for_language("haskell"), do: @haskell_banned
  def patterns_for_language("ocaml"), do: @ocaml_banned
  def patterns_for_language("coq"), do: @coq_banned
  def patterns_for_language("lean"), do: @lean_banned
  def patterns_for_language("agda"), do: @agda_banned
  def patterns_for_language("isabelle"), do: @isabelle_banned
  def patterns_for_language("hol4"), do: @hol4_banned
  def patterns_for_language("zig"), do: @zig_patterns
  def patterns_for_language("fstar"), do: @fstar_banned
  def patterns_for_language("ada"), do: @ada_spark_patterns
  def patterns_for_language("spark"), do: @ada_spark_patterns
  def patterns_for_language("nickel"), do: @nickel_patterns
  def patterns_for_language("elixir"), do: @elixir_patterns
  def patterns_for_language("erlang"), do: @erlang_patterns
  def patterns_for_language("javascript"), do: @javascript_patterns
  def patterns_for_language("typescript"), do: @javascript_patterns
  def patterns_for_language("shell"), do: @shell_patterns
  def patterns_for_language("bash"), do: @shell_patterns
  def patterns_for_language(_), do: []

  def scan_content(content, language) do
    scannable = strip_test_sections(content, language)

    patterns_for_language(language)
    |> Enum.flat_map(fn rule ->
      # Strip lines carrying an inline `hypatia:ignore` directive for
      # *this* rule before counting matches. The directive can name the
      # rule bare (`hypatia:ignore from_raw`) or namespaced
      # (`hypatia:ignore code_safety/from_raw`); either form silences a
      # single line. Lines below 2 chars after stripping the marker are
      # also dropped (catches `// hypatia:ignore` on a line of its own
      # the convention reads as "next line is exempt").
      rule_id_str = Atom.to_string(rule.id)

      filtered =
        scannable
        |> String.split(~r/\r?\n/)
        |> filter_lines_for_rule(rule_id_str)
        |> Enum.join("\n")

      case Regex.scan(rule.pattern, filtered) do
        [] -> []
        matches -> [%{rule: rule.id, severity: rule.severity, cwe: rule.cwe,
                       description: rule.description, occurrences: length(matches)}]
      end
    end)
  end

  defp filter_lines_for_rule(lines, rule_id) do
    # A line is "ignored for this rule" if it contains an
    # `hypatia:ignore` marker followed (anywhere later on the same
    # line) by the rule name as a whole word. This is permissive on
    # purpose: a single directive can list multiple rules
    # (`hypatia:ignore code_safety/believe_me structural_drift/SD008`)
    # and each call to this function honours just the rule_id it was
    # passed.
    on_line = ~r/hypatia:ignore\b.*\b#{Regex.escape(rule_id)}\b/

    # Two-pass: drop lines that contain the directive directly, *and*
    # drop the line immediately after a comment-only directive line
    # (so `// hypatia:ignore rule\nstatement` works the way the
    # convention reads).
    above = ~r/^\s*(?:\/\/|#|--)\s*hypatia:ignore\b.*\b#{Regex.escape(rule_id)}\b/

    lines
    |> Enum.with_index()
    |> Enum.reject(fn {line, idx} ->
      Regex.match?(on_line, line) or
        (idx > 0 and
           lines
           |> Enum.at(idx - 1, "")
           |> String.match?(above))
    end)
    |> Enum.map(fn {line, _} -> line end)
  end

  # Strip test sections from non-test source files so panic-shape rules
  # (.unwrap, .expect, panic!, todo!, unimplemented!) don't flag
  # conventional test scaffolding inside `#[cfg(test)] mod { … }` blocks
  # or `#[test]` functions that live alongside production code.
  #
  # For Rust: the standard convention is one `#[cfg(test)]` directive
  # near the bottom of the file with all test scaffolding below it, so
  # cutting from that marker to EOF removes the test code without
  # touching production code. For files where `#[cfg(test)]` is only
  # used to gate a single attribute, this is still safe — the rules we
  # care about (unwrap/expect/panic) are about runtime behaviour and
  # are appropriately suppressed for any code that only compiles into
  # the test profile.
  defp strip_test_sections(content, "rust") do
    case String.split(content, "#[cfg(test)]", parts: 2) do
      [only] -> only
      [head, _] -> head
    end
  end

  defp strip_test_sections(content, _other), do: content

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
      description: "Python file detected -- banned language"},
    %{id: :typescript_file, severity: :critical, glob: "*.ts",
      language: "TypeScript", replacement: "ReScript",
      description: "TypeScript file detected -- banned language"},
    %{id: :golang_file, severity: :critical, glob: "*.go",
      language: "Go", replacement: "Rust",
      description: "Go file detected -- banned language"}
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
        description: "Dockerfile detected -- must be named Containerfile",
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
  def shell_patterns, do: @shell_patterns
end
