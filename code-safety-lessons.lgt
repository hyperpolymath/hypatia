% SPDX-License-Identifier: PMPL-1.0-or-later
% Code Safety Lessons - Panic/Crash/Security Pattern Detection
% Fills the gap in hypatia for runtime safety issues

:- object(code_safety_lessons).

    :- info([
        version is 1.0,
        author is 'Hypatia Security Team',
        date is 2026-01-25,
        comment is 'Runtime safety patterns from vordr/svalinn security audits'
    ]).

    :- public([
        % Rust panic patterns
        has_unsafe_panic/2,
        classify_panic_severity/2,
        suggest_panic_fix/2,

        % ReScript crash patterns
        has_unsafe_crash/2,
        classify_crash_severity/2,
        suggest_crash_fix/2,

        % CORS security
        has_cors_misconfiguration/2,
        suggest_cors_fix/2,

        % Authentication patterns
        has_auth_bypass/2,
        suggest_auth_fix/2,

        % Privilege escalation
        has_privilege_escalation/2,
        suggest_privilege_fix/2,

        % Input validation
        missing_input_validation/2,
        suggest_validation_fix/2,

        % Comprehensive scan
        scan_file_for_safety_issues/2,

        % CWE mappings
        cwe_mapping/2
    ]).

    %% ============================================================
    %% RUST PANIC PATTERNS (DoS via unwrap/expect)
    %% ============================================================

    :- public(has_unsafe_panic/2).
    :- mode(has_unsafe_panic(+atom, -term), zero_or_more).
    :- info(has_unsafe_panic/2, [
        comment is 'Detect panic-inducing patterns in Rust code',
        argnames is ['FilePath', 'Issue']
    ]).

    % unwrap() without prior check
    has_unsafe_panic(Path, unwrap_without_check(Line, Context)) :-
        file_extension(Path, '.rs'),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, '.unwrap()', Line),
        \+ line_is_in_test(Path, LineNum),
        \+ previous_line_has_expect_check(Path, LineNum),
        extract_context(Line, Context),
        LineNum.

    % unwrap_or default to dangerous value (e.g., root UID)
    has_unsafe_panic(Path, unwrap_or_dangerous_default(Line, DefaultValue)) :-
        file_extension(Path, '.rs'),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, '.unwrap_or(0)', Line),  % Common for UID
        extract_default_value(Line, DefaultValue),
        member(DefaultValue, [0, '0', 'root']),
        LineNum.

    % expect() in hot path
    has_unsafe_panic(Path, expect_in_hot_path(Line, FuncName)) :-
        file_extension(Path, '.rs'),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, '.expect(', Line),
        function_is_hot_path(Path, LineNum, FuncName),
        LineNum.

    % RwLock/Mutex unwrap (poison handling)
    has_unsafe_panic(Path, lock_unwrap(Line, LockType)) :-
        file_extension(Path, '.rs'),
        read_code_line(Path, LineNum, Line),
        member(Pattern, ['.write().unwrap()', '.read().unwrap()', '.lock().unwrap()']),
        atom_concat(_, Pattern, Line),
        extract_lock_type(Line, LockType),
        LineNum.

    :- public(classify_panic_severity/2).
    :- mode(classify_panic_severity(+term, -atom), one).

    classify_panic_severity(unwrap_without_check(_, _), high).
    classify_panic_severity(unwrap_or_dangerous_default(_, _), critical).
    classify_panic_severity(expect_in_hot_path(_, _), medium).
    classify_panic_severity(lock_unwrap(_, _), high).

    :- public(suggest_panic_fix/2).
    :- mode(suggest_panic_fix(+term, -atom), one).

    suggest_panic_fix(unwrap_without_check(_, _),
        'Replace unwrap() with proper error propagation using ? operator or match/if let').

    suggest_panic_fix(unwrap_or_dangerous_default(_, DefaultValue),
        'Change unwrap_or(0) to return error or use safe default (e.g., 1000 for UID)').

    suggest_panic_fix(expect_in_hot_path(_, _),
        'Replace expect() with if let or match to avoid panic in critical path').

    suggest_panic_fix(lock_unwrap(_, _),
        'Replace lock().unwrap() with expect("RwLock poisoned - thread panic") with descriptive message').

    %% ============================================================
    %% RESCRIPT CRASH PATTERNS (DoS via getExn/Obj.magic)
    %% ============================================================

    :- public(has_unsafe_crash/2).
    :- mode(has_unsafe_crash(+atom, -term), zero_or_more).

    % getExn on external data
    has_unsafe_crash(Path, getexn_on_external_data(Line, Source)) :-
        file_extension(Path, '.res'),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'getExn', Line),
        line_handles_external_data(Path, LineNum, Source),
        LineNum.

    % Obj.magic bypassing type safety
    has_unsafe_crash(Path, obj_magic_bypass(Line, Context)) :-
        file_extension(Path, '.res'),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Obj.magic', Line),
        extract_context(Line, Context),
        LineNum.

    % Array access without bounds check
    has_unsafe_crash(Path, unsafe_array_access(Line)) :-
        file_extension(Path, '.res'),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, '[0]->Belt.Option.getExn', Line),
        LineNum.

    % JSON decode without validation
    has_unsafe_crash(Path, json_decode_getexn(Line)) :-
        file_extension(Path, '.res'),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'decodeObject->Belt.Option.getExn', Line),
        LineNum.

    :- public(classify_crash_severity/2).

    classify_crash_severity(getexn_on_external_data(_, _), critical).
    classify_crash_severity(obj_magic_bypass(_, _), high).
    classify_crash_severity(unsafe_array_access(_), high).
    classify_crash_severity(json_decode_getexn(_), critical).

    :- public(suggest_crash_fix/2).

    suggest_crash_fix(getexn_on_external_data(_, _),
        'Replace getExn with switch/match or getWithDefault with safe fallback').

    suggest_crash_fix(obj_magic_bypass(_, _),
        'Remove Obj.magic and use proper type conversions or external bindings').

    suggest_crash_fix(unsafe_array_access(_),
        'Use Belt.Array.get with pattern matching instead of getExn').

    suggest_crash_fix(json_decode_getexn(_),
        'Add validation: switch decodeObject { | Some(obj) => ... | None => raise(...) }').

    %% ============================================================
    %% CORS MISCONFIGURATION
    %% ============================================================

    :- public(has_cors_misconfiguration/2).
    :- mode(has_cors_misconfiguration(+atom, -term), zero_or_more).

    % Wildcard CORS allowing any origin
    has_cors_misconfiguration(Path, wildcard_cors(Line)) :-
        member(Ext, ['.res', '.js', '.ts', '.rs']),
        file_extension(Path, Ext),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Access-Control-Allow-Origin', Line),
        atom_concat(_, '"*"', Line),
        LineNum.

    % CORS with credentials and wildcard
    has_cors_misconfiguration(Path, cors_credentials_wildcard(Line)) :-
        file_extension(Path, _),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Access-Control-Allow-Credentials', Line),
        atom_concat(_, 'true', Line),
        nearby_line_has_wildcard_origin(Path, LineNum),
        LineNum.

    :- public(suggest_cors_fix/2).

    suggest_cors_fix(wildcard_cors(_),
        'Replace "*" with environment-based whitelist: check origin against ALLOWED_ORIGINS').

    suggest_cors_fix(cors_credentials_wildcard(_),
        'Cannot use credentials with wildcard - use specific origin list').

    %% ============================================================
    %% AUTHENTICATION BYPASS
    %% ============================================================

    :- public(has_auth_bypass/2).
    :- mode(has_auth_bypass(+atom, -term), zero_or_more).

    % Unverified JWT decode
    has_auth_bypass(Path, unverified_jwt_decode(Line)) :-
        file_extension(Path, _),
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'decodeJWT', Line),
        \+ next_lines_contain_verify(Path, LineNum, 5),
        LineNum.

    % Dev mode bypass without environment check
    has_auth_bypass(Path, dev_mode_no_check(Line, Comment)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'dev/testing', Line),  % Comment about dev mode
        \+ next_lines_contain_env_check(Path, LineNum, 3),
        extract_comment(Line, Comment),
        LineNum.

    % Authentication disabled check
    has_auth_bypass(Path, auth_disabled_path(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'config.enabled', Line),
        atom_concat(_, 'false', Line),
        atom_concat(_, 'return', Line),  % Early return bypassing auth
        LineNum.

    :- public(suggest_auth_fix/2).

    suggest_auth_fix(unverified_jwt_decode(_),
        'Add JWT signature verification using OIDC config or reject in production').

    suggest_auth_fix(dev_mode_no_check(_, _),
        'Add environment check: if env != "development" { raise(error) }').

    suggest_auth_fix(auth_disabled_path(_),
        'Remove auth bypass or require explicit ALLOW_INSECURE env var').

    %% ============================================================
    %% PRIVILEGE ESCALATION
    %% ============================================================

    :- public(has_privilege_escalation/2).
    :- mode(has_privilege_escalation(+atom, -term), zero_or_more).

    % Default to root UID
    has_privilege_escalation(Path, default_to_root(Line)) :-
        file_extension(Path, '.rs'),
        read_code_line(Path, LineNum, Line),
        member(Pattern, ['unwrap_or(0)', 'default_value = 0', 'uid = 0']),
        atom_concat(_, Pattern, Line),
        atom_concat(_, 'uid', Line),
        LineNum.

    % setuid without validation
    has_privilege_escalation(Path, setuid_unvalidated(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'setuid', Line),
        \+ previous_lines_contain_validation(Path, LineNum, 5),
        LineNum.

    :- public(suggest_privilege_fix/2).

    suggest_privilege_fix(default_to_root(_),
        'Change default UID from 0 (root) to 1000 or return error if UID parsing fails').

    suggest_privilege_fix(setuid_unvalidated(_),
        'Add UID validation: reject 0, validate against allowlist, check permissions').

    %% ============================================================
    %% INPUT VALIDATION
    %% ============================================================

    :- public(missing_input_validation/2).
    :- mode(missing_input_validation(+atom, -term), zero_or_more).

    % Path traversal risk
    missing_input_validation(Path, path_traversal_risk(Line, VarName)) :-
        read_code_line(Path, LineNum, Line),
        member(PathOp, ['Path::new', 'join', 'push', 'fs::read']),
        atom_concat(_, PathOp, Line),
        extract_variable_name(Line, VarName),
        \+ previous_lines_validate_path(Path, LineNum, VarName),
        LineNum.

    % Command injection risk
    missing_input_validation(Path, command_injection_risk(Line, Cmd)) :-
        read_code_line(Path, LineNum, Line),
        member(CmdPattern, ['Command::new', 'exec ', 'system(']),
        atom_concat(_, CmdPattern, Line),
        extract_command(Line, Cmd),
        \+ previous_lines_validate_command(Path, LineNum),
        LineNum.

    :- public(suggest_validation_fix/2).

    suggest_validation_fix(path_traversal_risk(_, _),
        'Validate path: reject "..", "/", "\\"; check path is within allowed root').

    suggest_validation_fix(command_injection_risk(_, _),
        'Validate command against allowlist; never use shell=true with user input').

    %% ============================================================
    %% COMPREHENSIVE SCAN
    %% ============================================================

    :- public(scan_file_for_safety_issues/2).
    :- mode(scan_file_for_safety_issues(+atom, -list), one).
    :- info(scan_file_for_safety_issues/2, [
        comment is 'Scan a source file for all safety issues',
        argnames is ['FilePath', 'Issues']
    ]).

    scan_file_for_safety_issues(Path, AllIssues) :-
        findall(panic(Issue), has_unsafe_panic(Path, Issue), PanicIssues),
        findall(crash(Issue), has_unsafe_crash(Path, Issue), CrashIssues),
        findall(cors(Issue), has_cors_misconfiguration(Path, Issue), CorsIssues),
        findall(auth(Issue), has_auth_bypass(Path, Issue), AuthIssues),
        findall(privesc(Issue), has_privilege_escalation(Path, Issue), PrivescIssues),
        findall(validation(Issue), missing_input_validation(Path, Issue), ValidationIssues),
        append([PanicIssues, CrashIssues, CorsIssues, AuthIssues, PrivescIssues, ValidationIssues], AllIssues).

    %% ============================================================
    %% CWE MAPPINGS
    %% ============================================================

    :- public(cwe_mapping/2).
    :- mode(cwe_mapping(+atom, -atom), one).

    cwe_mapping(unwrap_without_check, 'CWE-754: Improper Check for Unusual or Exceptional Conditions').
    cwe_mapping(getexn_on_external_data, 'CWE-754: Improper Check for Unusual or Exceptional Conditions').
    cwe_mapping(wildcard_cors, 'CWE-942: Permissive Cross-domain Policy with Untrusted Domains').
    cwe_mapping(unverified_jwt_decode, 'CWE-347: Improper Verification of Cryptographic Signature').
    cwe_mapping(default_to_root, 'CWE-250: Execution with Unnecessary Privileges').
    cwe_mapping(path_traversal_risk, 'CWE-22: Path Traversal').
    cwe_mapping(command_injection_risk, 'CWE-78: OS Command Injection').

    %% ============================================================
    %% PRIORITY LEVELS
    %% ============================================================

    :- public(issue_priority/2).
    :- mode(issue_priority(+term, -atom), one).

    issue_priority(panic(unwrap_or_dangerous_default(_, _)), critical).
    issue_priority(crash(getexn_on_external_data(_, _)), critical).
    issue_priority(crash(json_decode_getexn(_)), critical).
    issue_priority(cors(wildcard_cors(_)), critical).
    issue_priority(auth(unverified_jwt_decode(_)), critical).
    issue_priority(privesc(default_to_root(_)), critical).

    issue_priority(panic(unwrap_without_check(_, _)), high).
    issue_priority(panic(lock_unwrap(_, _)), high).
    issue_priority(crash(obj_magic_bypass(_, _)), high).
    issue_priority(crash(unsafe_array_access(_)), high).
    issue_priority(validation(command_injection_risk(_, _)), high).

    issue_priority(panic(expect_in_hot_path(_, _)), medium).
    issue_priority(validation(path_traversal_risk(_, _)), medium).
    issue_priority(auth(dev_mode_no_check(_, _)), medium).

    issue_priority(_, low).

    %% ============================================================
    %% HELPER PREDICATES (implemented in scanner.lgt)
    %% ============================================================

    :- private([
        read_code_line/3,
        file_extension/2,
        line_is_in_test/2,
        previous_line_has_expect_check/2,
        extract_context/2,
        extract_default_value/2,
        function_is_hot_path/3,
        extract_lock_type/2,
        line_handles_external_data/3,
        nearby_line_has_wildcard_origin/2,
        next_lines_contain_verify/3,
        next_lines_contain_env_check/3,
        extract_comment/2,
        previous_lines_contain_validation/3,
        extract_variable_name/2,
        previous_lines_validate_path/3,
        extract_command/2,
        previous_lines_validate_command/2
    ]).

:- end_object.
