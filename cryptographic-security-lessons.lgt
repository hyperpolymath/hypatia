% SPDX-License-Identifier: PMPL-1.0-or-later
% Cryptographic Security Lessons - From Cerro Torre Security Audit 2026-01-25
% Rules for secure cryptographic operations and temporary file handling

:- object(cryptographic_security_lessons).

    :- info([
        version is 1.0,
        author is 'Cerro Torre Security Team',
        date is 2026-01-25,
        comment is 'Lessons learned from Cerro Torre Ed25519 signing security audit'
    ]).

    %% Command Injection Prevention Rules

    :- public(has_command_injection_risk/2).
    :- mode(has_command_injection_risk(+atom, -term), zero_or_more).
    :- info(has_command_injection_risk/2, [
        comment is 'Detect potential command injection vulnerabilities',
        argnames is ['CodePath', 'Vulnerability']
    ]).

    % Ada/GNAT: Argument_String_To_List with concatenated user input
    has_command_injection_risk(Path, argument_string_to_list_with_concat(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Argument_String_To_List', Line),
        atom_concat(_, '&', Line),  % String concatenation with &
        LineNum.

    % Shell command execution with string interpolation
    has_command_injection_risk(Path, shell_interpolation(Line, Command)) :-
        read_code_line(Path, LineNum, Line),
        member(Cmd, [system, popen, 'Spawn']),
        atom_concat(_, Cmd, Line),
        atom_concat(_, '&', Line),  % Concatenation present
        Command = Cmd,
        LineNum.

    % Python: subprocess with shell=True
    has_command_injection_risk(Path, subprocess_shell_true(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'subprocess.', Line),
        atom_concat(_, 'shell=True', Line),
        LineNum.

    :- public(safe_command_execution_pattern/2).
    :- mode(safe_command_execution_pattern(+atom, -atom), one).
    :- info(safe_command_execution_pattern/2, [
        comment is 'Recommended safe patterns for command execution',
        argnames is ['Language', 'Pattern']
    ]).

    safe_command_execution_pattern(ada, 'Use explicit Argument_List with new String arrays, not Argument_String_To_List').
    safe_command_execution_pattern(python, 'Use subprocess.run([cmd, arg1, arg2]) with shell=False (default)').
    safe_command_execution_pattern(rust, 'Use Command::new(cmd).args(&[arg1, arg2]), not sh -c with concatenation').
    safe_command_execution_pattern(bash, 'Use "$@" for array expansion, quote all variables: cmd "$var" not cmd $var').

    %% Temporary File Security Rules

    :- public(insecure_temp_file_pattern/2).
    :- mode(insecure_temp_file_pattern(+atom, -term), zero_or_more).
    :- info(insecure_temp_file_pattern/2, [
        comment is 'Detect insecure temporary file creation (TOCTOU attacks)',
        argnames is ['CodePath', 'Issue']
    ]).

    % Predictable paths in /tmp
    insecure_temp_file_pattern(Path, predictable_temp_path(Line, TempPath)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, '"/tmp/', Line),
        \+ atom_concat(_, 'XXXXXX', Line),  % Not using mktemp template
        extract_temp_path(Line, TempPath),
        LineNum.

    % Create without mode specification
    insecure_temp_file_pattern(Path, missing_file_mode(Line)) :-
        read_code_line(Path, LineNum, Line),
        member(CreateFunc, ['Create_Directory', 'Create', 'open', 'mktemp']),
        atom_concat(_, CreateFunc, Line),
        \+ atom_concat(_, 'mode', Line),  % No mode parameter
        \+ atom_concat(_, '0600', Line),  % No restrictive mode
        LineNum.

    % Time-of-check-time-of-use (TOCTOU) pattern
    insecure_temp_file_pattern(Path, toctou_check_then_create(CheckLine, CreateLine)) :-
        read_code_line(Path, CheckNum, CheckLine),
        read_code_line(Path, CreateNum, CreateLine),
        CreateNum =:= CheckNum + 1,  % Adjacent lines
        atom_concat(_, 'Exists', CheckLine),  % Check if exists
        atom_concat(_, 'Create', CreateLine). % Then create

    :- public(secure_temp_file_pattern/2).
    :- mode(secure_temp_file_pattern(+atom, -atom), one).
    :- info(secure_temp_file_pattern/2, [
        comment is 'Recommended secure temporary file patterns',
        argnames is ['Language', 'Pattern']
    ]).

    secure_temp_file_pattern(ada, 'Use mkdtemp via C FFI with mode 0700, or Ada.Directories.Create_Temporary_Directory (Ada 2022)').
    secure_temp_file_pattern(c, 'Use mkstemp() or mkdtemp(), never tmpnam() or tempnam()').
    secure_temp_file_pattern(python, 'Use tempfile.mkdtemp() or tempfile.NamedTemporaryFile(delete=True)').
    secure_temp_file_pattern(rust, 'Use tempfile crate: tempfile::tempdir() with proper permissions').

    %% Cryptographic Input Validation Rules

    :- public(weak_hex_validation/2).
    :- mode(weak_hex_validation(+atom, -term), zero_or_more).
    :- info(weak_hex_validation/2, [
        comment is 'Detect weak hex string validation that allows invalid input',
        argnames is ['CodePath', 'Issue']
    ]).

    % Silent failure on invalid hex (returns 0 instead of error)
    weak_hex_validation(Path, silent_invalid_hex_failure(Line)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Hex_Digit', Line),
        atom_concat(_, 'return 0', Line),  % Silent failure
        \+ atom_concat(_, 'raise', Line),  % No exception
        LineNum.

    % Missing length validation before parsing
    weak_hex_validation(Path, missing_hex_length_check(FuncName)) :-
        function_definition(Path, FuncName, 'Hex_To_'),
        \+ function_contains(Path, FuncName, 'Length'),
        FuncName.

    :- public(strict_hex_validation_pattern/1).
    :- mode(strict_hex_validation_pattern(-atom), one).

    strict_hex_validation_pattern('Validate hex length first, then raise exception on invalid characters, never return 0 or truncate').

    %% File Permission Security Rules

    :- public(missing_permission_restriction/2).
    :- mode(missing_permission_restriction(+atom, -term), zero_or_more).
    :- info(missing_permission_restriction/2, [
        comment is 'Detect files written without restrictive permissions',
        argnames is ['CodePath', 'Issue']
    ]).

    % Writing private keys without chmod
    missing_permission_restriction(Path, private_key_no_chmod(Line)) :-
        read_code_line(Path, LineNum, Line),
        member(KeyWord, ['private_key', 'Private_Key', 'secret', 'credential']),
        atom_concat(_, KeyWord, Line),
        member(WriteFunc, ['Create', 'Put', 'write', 'Write']),
        atom_concat(_, WriteFunc, Line),
        \+ next_lines_contain_chmod(Path, LineNum, 5),  % No chmod within 5 lines
        LineNum.

    :- public(required_file_permissions/2).
    :- mode(required_file_permissions(+atom, -atom), one).
    :- info(required_file_permissions/2, [
        comment is 'Required file permissions for sensitive data types',
        argnames is ['FileType', 'Mode']
    ]).

    required_file_permissions(private_key, '0600').  % Owner read/write only
    required_file_permissions(secret, '0600').
    required_file_permissions(credential, '0600').
    required_file_permissions(config_with_secrets, '0600').
    required_file_permissions(temp_directory, '0700').  % Owner all permissions only

    %% Resource Cleanup Rules

    :- public(incomplete_cleanup/2).
    :- mode(incomplete_cleanup(+atom, -term), zero_or_more).
    :- info(incomplete_cleanup/2, [
        comment is 'Detect incomplete resource cleanup in exception handlers',
        argnames is ['CodePath', 'Issue']
    ]).

    % Exception handler without cleanup
    incomplete_cleanup(Path, exception_handler_missing_cleanup(Line, Resource)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'exception', Line),
        find_resources_allocated_before(Path, LineNum, Resources),
        member(Resource, Resources),
        \+ exception_handler_cleans_up(Path, LineNum, Resource).

    % Temp files allocated without Finalization
    incomplete_cleanup(Path, temp_file_no_finalization(Line, TempPath)) :-
        read_code_line(Path, LineNum, Line),
        atom_concat(_, 'Temp_Dir', Line),
        atom_concat(_, '/tmp/', Line),
        extract_temp_path(Line, TempPath),
        \+ uses_finalization_pattern(Path, TempPath),
        LineNum.

    :- public(secure_cleanup_pattern/2).
    :- mode(secure_cleanup_pattern(+atom, -atom), one).

    secure_cleanup_pattern(ada, 'Use Ada.Finalization.Limited_Controlled with Finalize procedure for guaranteed cleanup').
    secure_cleanup_pattern(rust, 'Use Drop trait for automatic cleanup, or defer! macro for manual control').
    secure_cleanup_pattern(python, 'Use with statement for context managers (with open(...) as f:)').
    secure_cleanup_pattern(cpp, 'Use RAII (Resource Acquisition Is Initialization) with destructors').

    %% Entropy Source Rules

    :- public(weak_entropy_source/2).
    :- mode(weak_entropy_source(+atom, -term), zero_or_more).
    :- info(weak_entropy_source/2, [
        comment is 'Detect weak entropy sources for security-critical random values',
        argnames is ['CodePath', 'Issue']
    ]).

    % Using Clock/Time for security purposes
    weak_entropy_source(Path, time_based_random(Line, Usage)) :-
        read_code_line(Path, LineNum, Line),
        member(TimeFunc, ['Clock', 'time', 'gettimeofday', 'now']),
        atom_concat(_, TimeFunc, Line),
        security_critical_context(Path, LineNum, Usage),
        LineNum.

    % Math.random() or similar non-cryptographic RNG
    weak_entropy_source(Path, non_crypto_rng(Line, RNG)) :-
        read_code_line(Path, LineNum, Line),
        member(RNG, ['Math.random', 'rand()', 'srand', 'Random']),
        atom_concat(_, RNG, Line),
        security_critical_context(Path, LineNum, _),
        LineNum.

    :- public(secure_entropy_source/2).
    :- mode(secure_entropy_source(+atom, -atom), one).

    secure_entropy_source(ada, 'Ada.Numerics.Discrete_Random (seeded from /dev/urandom on Unix)').
    secure_entropy_source(c, '/dev/urandom or getrandom() syscall').
    secure_entropy_source(python, 'secrets module (secrets.token_bytes, secrets.token_hex)').
    secure_entropy_source(rust, 'rand crate with OsRng or ThreadRng').
    secure_entropy_source(javascript, 'crypto.getRandomValues() or crypto.randomBytes() in Node.js').

    %% Security Lesson Summaries

    :- public(crypto_security_lesson/2).
    :- mode(crypto_security_lesson(+atom, -atom), one).

    crypto_security_lesson(command_injection,
        'Never use string concatenation with shell commands. Use explicit argument arrays.').

    crypto_security_lesson(temp_files,
        'Use mkdtemp/mkstemp with mode 0700/0600. Predictable /tmp paths enable symlink attacks.').

    crypto_security_lesson(hex_validation,
        'Validate hex string length and raise exceptions on invalid characters. Never silently return 0.').

    crypto_security_lesson(file_permissions,
        'Set restrictive permissions (0600) on private keys immediately after creation, before writing data.').

    crypto_security_lesson(cleanup,
        'Use language-specific finalization patterns (RAII, Drop, Finalize) for guaranteed cleanup.').

    crypto_security_lesson(entropy,
        'Use cryptographically secure random sources (/dev/urandom, secrets module) for temp names and IDs.').

    %% CWE Mappings

    :- public(cwe_mapping/2).
    :- mode(cwe_mapping(+atom, -atom), one).

    cwe_mapping(command_injection, 'CWE-78: OS Command Injection').
    cwe_mapping(temp_file_toctou, 'CWE-377: Insecure Temporary File').
    cwe_mapping(predictable_temp, 'CWE-330: Use of Insufficiently Random Values').
    cwe_mapping(missing_permissions, 'CWE-732: Incorrect Permission Assignment').
    cwe_mapping(incomplete_cleanup, 'CWE-459: Incomplete Cleanup').
    cwe_mapping(weak_validation, 'CWE-20: Improper Input Validation').

    %% OWASP Top 10 2021 Mappings

    :- public(owasp_mapping/2).
    :- mode(owasp_mapping(+atom, -atom), one).

    owasp_mapping(command_injection, 'A03:2021 - Injection').
    owasp_mapping(temp_file_toctou, 'A04:2021 - Insecure Design').
    owasp_mapping(missing_permissions, 'A07:2021 - Identification and Authentication Failures').
    owasp_mapping(weak_entropy, 'A02:2021 - Cryptographic Failures').

    %% Priority Levels

    :- public(vulnerability_priority/2).
    :- mode(vulnerability_priority(+term, -atom), one).

    vulnerability_priority(command_injection(_), critical).
    vulnerability_priority(temp_file_toctou(_), critical).
    vulnerability_priority(private_key_no_chmod(_), high).
    vulnerability_priority(incomplete_cleanup(_), high).
    vulnerability_priority(weak_entropy(_), medium).
    vulnerability_priority(weak_hex_validation(_), medium).
    vulnerability_priority(missing_spdx, low).

    %% Integration with Automated Scanning

    :- public(scan_code_for_crypto_issues/2).
    :- mode(scan_code_for_crypto_issues(+atom, -list), one).
    :- info(scan_code_for_crypto_issues/2, [
        comment is 'Scan codebase for cryptographic security issues',
        argnames is ['CodePath', 'Issues']
    ]).

    scan_code_for_crypto_issues(Path, AllIssues) :-
        findall(command_injection(Issue),
            has_command_injection_risk(Path, Issue),
            CmdInjIssues),
        findall(temp_file(Issue),
            insecure_temp_file_pattern(Path, Issue),
            TempFileIssues),
        findall(hex_validation(Issue),
            weak_hex_validation(Path, Issue),
            HexIssues),
        findall(permissions(Issue),
            missing_permission_restriction(Path, Issue),
            PermIssues),
        findall(cleanup(Issue),
            incomplete_cleanup(Path, Issue),
            CleanupIssues),
        findall(entropy(Issue),
            weak_entropy_source(Path, Issue),
            EntropyIssues),
        append([CmdInjIssues, TempFileIssues, HexIssues, PermIssues, CleanupIssues, EntropyIssues], AllIssues).

    %% Remediation Actions

    :- public(remediation_action/2).
    :- mode(remediation_action(+term, -atom), one).
    :- info(remediation_action/2, [
        comment is 'Recommended remediation for each vulnerability type',
        argnames is ['Issue', 'Action']
    ]).

    remediation_action(command_injection(argument_string_to_list_with_concat(_)),
        'Replace Argument_String_To_List with explicit Argument_List array').

    remediation_action(temp_file(predictable_temp_path(_, _)),
        'Use mkdtemp with XXXXXX template and mode 0700').

    remediation_action(hex_validation(silent_invalid_hex_failure(_)),
        'Add raise Constraint_Error for invalid hex characters').

    remediation_action(permissions(private_key_no_chmod(_)),
        'Call chmod(path, 0600) immediately after Create, before writing data').

    remediation_action(cleanup(temp_file_no_finalization(_, _)),
        'Wrap temp dir in Finalization.Limited_Controlled for guaranteed cleanup').

    remediation_action(entropy(time_based_random(_, _)),
        'Replace Clock with Ada.Numerics.Discrete_Random (seeded from /dev/urandom)').

    %% Helper Predicates (to be implemented by scanning tools)

    :- private(read_code_line/3).
    :- private(extract_temp_path/2).
    :- private(next_lines_contain_chmod/3).
    :- private(function_definition/3).
    :- private(function_contains/3).
    :- private(security_critical_context/3).
    :- private(find_resources_allocated_before/3).
    :- private(exception_handler_cleans_up/3).
    :- private(uses_finalization_pattern/2).

:- end_object.
