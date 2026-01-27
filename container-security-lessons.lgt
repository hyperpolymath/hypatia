% SPDX-License-Identifier: PMPL-1.0-or-later
% Container Security Lessons - From Verified Container Spec Audit 2026-01-25
% Rules for secure container runtime integration and verification protocol implementation

:- object(container_security_lessons).

    :- info([
        version is 1.0,
        author is 'Verified Container Spec Security Team',
        date is 2026-01-25,
        comment is 'Lessons learned from verified-container-spec containerd shim security audit'
    ]).

    %% Environment Variable Injection Prevention

    :- public(has_env_var_injection_risk/2).
    :- mode(has_env_var_injection_risk(+atom, -term), zero_or_more).
    :- info(has_env_var_injection_risk/2, [
        comment is 'Detect environment variable injection vulnerabilities',
        argnames is ['CodePath', 'Vulnerability']
    ]).

    % Rust: std::env::var used directly in Command::new
    has_env_var_injection_risk(Path, env_var_in_command_new(Line, VarName)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'std::env::var', Line),
        atom_concat(_, 'Command::new', Line),
        extract_env_var_name(Line, VarName),
        LineNum.

    % Shell: exec with unvalidated environment variable
    has_env_var_injection_risk(Path, shell_exec_unvalidated_env(Line, VarName)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'exec ', Line),
        atom_concat(_, '$', Line),  % Environment variable reference
        extract_shell_var_name(Line, VarName),
        \+ line_has_allowlist_check(Path, LineNum, VarName),
        LineNum.

    :- public(safe_env_var_pattern/2).
    :- mode(safe_env_var_pattern(+atom, -atom), one).
    :- info(safe_env_var_pattern/2, [
        comment is 'Recommended patterns for safe environment variable usage',
        argnames is ['Language', 'Pattern']
    ]).

    safe_env_var_pattern(rust, 'Validate env var against allowlist before Command::new: if !allowed.contains(&val) { bail!(...) }').
    safe_env_var_pattern(bash, 'Validate against case statement allowlist before exec').
    safe_env_var_pattern(python, 'Use shlex.quote() after allowlist validation').

    %% Shell Variable Quoting Rules

    :- public(unquoted_shell_variable/2).
    :- mode(unquoted_shell_variable(+atom, -term), zero_or_more).
    :- info(unquoted_shell_variable/2, [
        comment is 'Detect unquoted variable expansions in shell scripts',
        argnames is ['CodePath', 'Issue']
    ]).

    % Unquoted variable in exec
    unquoted_shell_variable(Path, unquoted_exec_var(Line, VarName)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'exec ', Line),
        atom_concat(_, '$', Line),
        \+ atom_concat(_, '"$', Line),  % Not quoted with "$VAR"
        extract_shell_var_name(Line, VarName),
        LineNum.

    % Unquoted variable in command substitution
    unquoted_shell_variable(Path, unquoted_in_substitution(Line, VarName)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, '$(', Line),
        atom_concat(_, '$', Line),
        \+ atom_concat(_, '"$', Line),
        extract_shell_var_name(Line, VarName),
        LineNum.

    :- public(shell_quoting_rule/1).
    :- mode(shell_quoting_rule(-atom), one).

    shell_quoting_rule('Always quote variables: "$VAR" not $VAR. Unquoted expansion allows word splitting and glob expansion.').

    %% Allowlist Validation Rules

    :- public(missing_allowlist_validation/2).
    :- mode(missing_allowlist_validation(+atom, -term), zero_or_more).
    :- info(missing_allowlist_validation/2, [
        comment is 'Detect missing allowlist validation for untrusted input',
        argnames is ['CodePath', 'Issue']
    ]).

    % Runtime selection without validation
    missing_allowlist_validation(Path, runtime_no_allowlist(Line, RuntimeVar)) :-
        read_code_line(Path, LineNum, Line),
        member(Pattern, ['OCI_RUNTIME', 'CONTAINER_RUNTIME', 'RUNTIME']),
        atom_concat(_, Pattern, Line),
        atom_concat(_, 'Command::new', Line),
        \+ next_lines_contain_allowlist(Path, LineNum, 5),
        RuntimeVar = Pattern,
        LineNum.

    % Solver selection without validation (shell)
    missing_allowlist_validation(Path, solver_no_allowlist(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'SOLVER', Line),
        atom_concat(_, 'exec', Line),
        \+ previous_lines_contain_case_allowlist(Path, LineNum, 10),
        LineNum.

    :- public(allowlist_validation_pattern/2).
    :- mode(allowlist_validation_pattern(+atom, -atom), one).

    allowlist_validation_pattern(rust, 'const ALLOWED: &[&str] = &[...]; if !ALLOWED.contains(&input) { bail!(...) }').
    allowlist_validation_pattern(bash, 'case "$VAR" in allowed1|allowed2|allowed3) ;; *) echo "Error"; exit 1 ;; esac').
    allowlist_validation_pattern(python, 'ALLOWED = {...}; if input not in ALLOWED: raise ValueError(...)').

    %% Merkle Proof Verification Rules

    :- public(weak_merkle_verification/2).
    :- mode(weak_merkle_verification(+atom, -term), zero_or_more).
    :- info(weak_merkle_verification/2, [
        comment is 'Detect weak or incorrect Merkle tree proof verification',
        argnames is ['CodePath', 'Issue']
    ]).

    % Hashing hex strings instead of bytes
    weak_merkle_verification(Path, hash_hex_string_not_bytes(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'verify_merkle', Line),
        atom_concat(_, '.as_bytes()', Line),  % Treating hex as bytes
        atom_concat(_, 'hasher.update', Line),
        LineNum.

    % Missing left/right sibling distinction
    weak_merkle_verification(Path, no_left_right_distinction(FuncName)) :-
        function_definition(Path, FuncName, 'verify_merkle'),
        function_contains(Path, FuncName, 'hasher.update'),
        \+ function_contains(Path, FuncName, 'index % 2'),  % No left/right check
        FuncName.

    % Missing RFC 6962 node prefix (0x00 for leaf, 0x01 for node)
    weak_merkle_verification(Path, missing_node_prefix(FuncName)) :-
        function_definition(Path, FuncName, 'verify_merkle'),
        \+ function_contains(Path, FuncName, '0x01'),  % No node prefix
        \+ function_contains(Path, FuncName, '0x00'),  % No leaf prefix
        FuncName.

    :- public(rfc6962_merkle_requirements/1).
    :- mode(rfc6962_merkle_requirements(-list), one).

    rfc6962_merkle_requirements([
        'Decode hex hashes to bytes before hashing',
        'Use 0x00 prefix for leaf nodes, 0x01 for internal nodes',
        'Determine left/right child based on index % 2',
        'Validate log_index < tree_size',
        'Hash(0x01 || left || right) for internal nodes',
        'Reconstruct path from leaf to root following index bits'
    ]).

    %% Transparency Log Verification Rules

    :- public(missing_log_verification/2).
    :- mode(missing_log_verification(+atom, -term), zero_or_more).
    :- info(missing_log_verification/2, [
        comment is 'Detect missing transparency log verification steps',
        argnames is ['CodePath', 'Issue']
    ]).

    % Missing Signed Entry Timestamp verification
    missing_log_verification(Path, no_set_verification(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'signedEntryTimestamp', Line),
        atom_concat(_, 'Simplified', Line),  % Comment saying it's simplified
        \+ next_lines_contain_signature_verify(Path, LineNum, 10),
        LineNum.

    % Accepting single log entry (should require 2+)
    missing_log_verification(Path, single_log_accepted(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'log_entries.len()', Line),
        atom_concat(_, '< 1', Line),  % Only checking for at least 1
        \+ atom_concat(_, '< 2', Line),  % Should require 2+
        LineNum.

    :- public(transparency_log_best_practices/1).
    :- mode(transparency_log_best_practices(-list), one).

    transparency_log_best_practices([
        'Verify Signed Entry Timestamp (SET) signature from log operator',
        'Require 2+ distinct transparency logs for federated trust',
        'Verify Merkle inclusion proof per RFC 6962',
        'Check log public keys against trust store',
        'Validate timestamp is reasonable (not too old, not future)',
        'Cache verified results with TTL (1 hour recommended)'
    ]).

    %% Permissive Mode Security Rules

    :- public(insecure_permissive_mode/2).
    :- mode(insecure_permissive_mode(+atom, -term), zero_or_more).
    :- info(insecure_permissive_mode/2, [
        comment is 'Detect permissive modes that bypass security verification',
        argnames is ['CodePath', 'Issue']
    ]).

    % Permissive mode continues after verification failure
    insecure_permissive_mode(Path, permissive_continues(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Permissive', Line),
        atom_concat(_, 'continuing anyway', Line),
        LineNum.

    % Audit mode doesn't block execution
    insecure_permissive_mode(Path, audit_no_block(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Audit', Line),
        atom_concat(_, 'logged only', Line),
        \+ atom_concat(_, 'bail!', Line),
        LineNum.

    :- public(verification_mode_policy/1).
    :- mode(verification_mode_policy(-atom), one).

    verification_mode_policy('Production systems MUST use Strict mode. Permissive/Audit modes ONLY for development/testing.').

    %% Cryptographic Verification Best Practices

    :- public(simplified_crypto_implementation/2).
    :- mode(simplified_crypto_implementation(+atom, -term), zero_or_more).
    :- info(simplified_crypto_implementation/2, [
        comment is 'Detect simplified crypto implementations that need production hardening',
        argnames is ['CodePath', 'Issue']
    ]).

    % Comments admitting simplification
    simplified_crypto_implementation(Path, admitted_simplification(Line, Component)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'simplified', Line),
        atom_concat(_, 'production', Line),
        extract_component_name(Line, Component),
        LineNum.

    % Missing RFC compliance
    simplified_crypto_implementation(Path, missing_rfc_compliance(Line, RFC)) :-
        read_code_line(Path, LineNum, Line),
        member(RFC, ['RFC 6962', 'RFC 8032', 'RFC 9162']),
        atom_concat(_, RFC, Line),
        atom_concat(_, 'should', Line),  % "should follow RFC" comment
        LineNum.

    :- public(crypto_implementation_rule/1).
    :- mode(crypto_implementation_rule(-atom), one).

    crypto_implementation_rule('All cryptographic implementations MUST follow published RFCs exactly. Reference implementations must clearly document limitations.').

    %% Container Runtime Security Rules

    :- public(unsafe_runtime_delegation/2).
    :- mode(unsafe_runtime_delegation(+atom, -term), zero_or_more).
    :- info(unsafe_runtime_delegation/2, [
        comment is 'Detect unsafe delegation to container runtimes',
        argnames is ['CodePath', 'Issue']
    ]).

    % Trusting runtime path from environment
    unsafe_runtime_delegation(Path, runtime_from_env(Line, EnvVar)) :-
        read_code_line(Path, LineNum, Line),
        member(EnvVar, ['PATH', 'LD_LIBRARY_PATH', 'OCI_RUNTIME']),
        atom_concat(_, EnvVar, Line),
        atom_concat(_, 'Command::new', Line),
        LineNum.

    % No validation before exec
    unsafe_runtime_delegation(Path, exec_without_validation(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'exec ', Line),
        \+ previous_lines_contain(Path, LineNum, 'if [ ', 5),  % No validation
        LineNum.

    :- public(runtime_security_requirements/1).
    :- mode(runtime_security_requirements(-list), one).

    runtime_security_requirements([
        'Validate runtime name against allowlist before exec',
        'Never trust PATH from environment - use absolute paths or which(1)',
        'Run container runtimes with minimal privileges (no setuid)',
        'Enforce resource limits (cgroups, ulimits)',
        'Use read-only root filesystem where possible',
        'Disable network access unless required'
    ]).

    %% Security Lesson Summaries

    :- public(container_security_lesson/2).
    :- mode(container_security_lesson(+atom, -atom), one).

    container_security_lesson(env_var_injection,
        'Validate environment variables against allowlists before using in Command::new or exec.').

    container_security_lesson(shell_quoting,
        'Always quote shell variables: "$VAR" not $VAR. Unquoted expansion enables injection.').

    container_security_lesson(merkle_verification,
        'Follow RFC 6962 exactly: decode hex, use node prefixes, distinguish left/right, validate index.').

    container_security_lesson(transparency_logs,
        'Require 2+ logs, verify SET signatures, validate Merkle proofs, check timestamps.').

    container_security_lesson(permissive_mode,
        'Permissive modes that bypass verification MUST NOT be used in production.').

    container_security_lesson(simplified_crypto,
        'Simplified crypto is acceptable for reference implementations IF clearly documented.').

    %% CWE Mappings

    :- public(cwe_mapping/2).
    :- mode(cwe_mapping(+atom, -atom), one).

    cwe_mapping(env_var_injection, 'CWE-78: OS Command Injection').
    cwe_mapping(shell_unquoted, 'CWE-78: OS Command Injection').
    cwe_mapping(merkle_verification_weak, 'CWE-347: Improper Verification of Cryptographic Signature').
    cwe_mapping(set_verification_missing, 'CWE-347: Improper Verification of Cryptographic Signature').
    cwe_mapping(permissive_bypass, 'CWE-863: Incorrect Authorization').

    %% Priority Levels

    :- public(vulnerability_priority/2).
    :- mode(vulnerability_priority(+term, -atom), one).

    vulnerability_priority(env_var_in_command_new(_, _), critical).
    vulnerability_priority(shell_exec_unvalidated_env(_, _), critical).
    vulnerability_priority(unquoted_exec_var(_, _), critical).
    vulnerability_priority(weak_merkle_verification(_), high).
    vulnerability_priority(no_set_verification(_), high).
    vulnerability_priority(permissive_continues(_), high).
    vulnerability_priority(missing_allowlist_validation(_, _), medium).

    %% Integration with Automated Scanning

    :- public(scan_code_for_container_issues/2).
    :- mode(scan_code_for_container_issues(+atom, -list), one).
    :- info(scan_code_for_container_issues/2, [
        comment is 'Scan codebase for container security issues',
        argnames is ['CodePath', 'Issues']
    ]).

    scan_code_for_container_issues(Path, AllIssues) :-
        findall(env_injection(Issue),
            has_env_var_injection_risk(Path, Issue),
            EnvInjIssues),
        findall(shell_quoting(Issue),
            unquoted_shell_variable(Path, Issue),
            QuotingIssues),
        findall(allowlist(Issue),
            missing_allowlist_validation(Path, Issue),
            AllowlistIssues),
        findall(merkle(Issue),
            weak_merkle_verification(Path, Issue),
            MerkleIssues),
        findall(transparency(Issue),
            missing_log_verification(Path, Issue),
            LogIssues),
        findall(permissive(Issue),
            insecure_permissive_mode(Path, Issue),
            PermissiveIssues),
        append([EnvInjIssues, QuotingIssues, AllowlistIssues, MerkleIssues, LogIssues, PermissiveIssues], AllIssues).

    %% Remediation Actions

    :- public(remediation_action/2).
    :- mode(remediation_action(+term, -atom), one).
    :- info(remediation_action/2, [
        comment is 'Recommended remediation for each vulnerability type',
        argnames is ['Issue', 'Action']
    ]).

    remediation_action(env_var_in_command_new(_, _),
        'Validate environment variable against allowlist: if !["runc", "crun"].contains(&var) { bail!(...) }').

    remediation_action(shell_exec_unvalidated_env(_, _),
        'Add case statement allowlist before exec').

    remediation_action(unquoted_exec_var(_, _),
        'Quote variable: exec cmd "$VAR" not exec cmd $VAR').

    remediation_action(weak_merkle_verification(_),
        'Implement RFC 6962 compliant verification with node prefixes and left/right distinction').

    remediation_action(no_set_verification(_),
        'Verify signedEntryTimestamp Ed25519 signature using log public key').

    remediation_action(permissive_continues(_),
        'Remove permissive mode or require explicit authorization (check ALLOW_PERMISSIVE env var)').

    %% Helper Predicates (to be implemented by scanning tools)

    :- private(read_code_line/3).
    :- private(extract_env_var_name/2).
    :- private(extract_shell_var_name/2).
    :- private(line_has_allowlist_check/3).
    :- private(next_lines_contain_allowlist/3).
    :- private(previous_lines_contain_case_allowlist/3).
    :- private(function_definition/3).
    :- private(function_contains/3).
    :- private(next_lines_contain_signature_verify/3).
    :- private(extract_component_name/2).
    :- private(previous_lines_contain/4).

:- end_object.
