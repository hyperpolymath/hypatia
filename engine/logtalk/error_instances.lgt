%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
%%
%% error_instances.lgt - Concrete error instance definitions for Hypatia PA rules
%%
%% Each PA rule (PA001-PA020) is defined as an object implementing the
%% error_instance protocol. Provides category metadata, severity classification,
%% CWE mappings, detection heuristics, triangle tier routing, and remediation
%% guidance used by the safety triangle pipeline.
%%
%% These instances are consumed by:
%%   - PatternRegistry (lib/pattern_registry.ex) for canonical pattern IDs
%%   - TriangleRouter (lib/triangle_router.ex) for tier routing decisions
%%   - RecipeMatcher (lib/recipe_matcher.ex) for fix recipe correlation
%%   - Neural Coordinator (lib/neural/coordinator.ex) for confidence aggregation


%% ---------------------------------------------------------------------------
%% Protocol: error_instance
%%
%% Defines the interface that all PA rule objects must implement.
%% ---------------------------------------------------------------------------

:- protocol(error_instance).

    :- public(pa_rule/1).
    :- mode(pa_rule(-atom), one).
    :- info(pa_rule/1, [
        comment is 'The PA rule identifier (e.g., pa001).',
        argnames is ['Rule']
    ]).

    :- public(category/1).
    :- mode(category(-atom), one).
    :- info(category/1, [
        comment is 'The canonical category name matching PatternRegistry.',
        argnames is ['Category']
    ]).

    :- public(severity/1).
    :- mode(severity(-atom), one).
    :- info(severity/1, [
        comment is 'Default severity: critical, high, medium, or low.',
        argnames is ['Severity']
    ]).

    :- public(cwe/1).
    :- mode(cwe(-atom), one_or_more).
    :- info(cwe/1, [
        comment is 'Associated CWE identifier(s).',
        argnames is ['CWE']
    ]).

    :- public(description/1).
    :- mode(description(-atom), one).
    :- info(description/1, [
        comment is 'Human-readable description of the error class.',
        argnames is ['Description']
    ]).

    :- public(triangle_tier/1).
    :- mode(triangle_tier(-atom), one).
    :- info(triangle_tier/1, [
        comment is 'Default safety triangle tier: eliminate, substitute, or control.',
        argnames is ['Tier']
    ]).

    :- public(auto_fixable/0).
    :- mode(auto_fixable, zero_or_one).
    :- info(auto_fixable/0, [
        comment is 'Succeeds if the error class can be auto-fixed.'
    ]).

    :- public(detection_pattern/1).
    :- mode(detection_pattern(-atom), one_or_more).
    :- info(detection_pattern/1, [
        comment is 'Regex or keyword pattern used for detection.',
        argnames is ['Pattern']
    ]).

    :- public(remediation/1).
    :- mode(remediation(-atom), one).
    :- info(remediation/1, [
        comment is 'Recommended remediation action.',
        argnames is ['Action']
    ]).

    :- public(proven_modules/1).
    :- mode(proven_modules(-list), one).
    :- info(proven_modules/1, [
        comment is 'List of proven-safe replacement modules (empty if none).',
        argnames is ['Modules']
    ]).

    :- public(affected_languages/1).
    :- mode(affected_languages(-list), one).
    :- info(affected_languages/1, [
        comment is 'Languages where this error class is relevant.',
        argnames is ['Languages']
    ]).

    :- public(classify_severity/2).
    :- mode(classify_severity(+atom, -atom), one).
    :- info(classify_severity/2, [
        comment is 'Context-aware severity: escalate in production, de-escalate in test.',
        argnames is ['Context', 'EffectiveSeverity']
    ]).

:- end_protocol.


%% ---------------------------------------------------------------------------
%% PA001 - Unchecked Allocation
%% Memory allocation without null/error checking
%% ---------------------------------------------------------------------------

:- object(pa001,
    implements(error_instance)).

    pa_rule('PA001').
    category('UncheckedAllocation').
    severity(high).
    cwe('CWE-690').
    cwe('CWE-252').
    description('Memory allocation without null or error checking. Unchecked malloc/calloc/realloc returns can lead to null pointer dereference or undefined behaviour.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('malloc\\s*\\([^)]*\\)\\s*;').
    detection_pattern('calloc\\s*\\(').
    detection_pattern('realloc\\s*\\([^)]*\\)\\s*;').
    detection_pattern('new\\s+\\w+\\s*[^(]').

    remediation('Wrap allocation calls in null-check guards. Use RAII or smart pointers in C++. In Rust, use Vec/Box which handle allocation failure via panic or try_reserve.').

    proven_modules([]).

    affected_languages([c, cpp, rust, zig]).

    classify_severity(production, critical) :- !.
    classify_severity(test, medium) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA002 - Unbounded Loop
%% Loop without guaranteed termination
%% ---------------------------------------------------------------------------

:- object(pa002,
    implements(error_instance)).

    pa_rule('PA002').
    category('UnboundedLoop').
    severity(medium).
    cwe('CWE-835').
    description('Loop without guaranteed termination condition. May cause infinite execution, CPU exhaustion, or denial of service.').
    triangle_tier(control).

    detection_pattern('while\\s*\\(\\s*true\\s*\\)').
    detection_pattern('while\\s*\\(\\s*1\\s*\\)').
    detection_pattern('loop\\s*\\{').
    detection_pattern('for\\s*\\(\\s*;\\s*;').

    remediation('Add explicit termination conditions, iteration limits, or timeout guards. Use bounded iterators where possible.').

    proven_modules([]).

    affected_languages([c, cpp, rust, java, javascript, python, elixir, shell]).

    classify_severity(production, high) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, medium).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA003 - Blocking IO
%% Synchronous blocking I/O in async or concurrent context
%% ---------------------------------------------------------------------------

:- object(pa003,
    implements(error_instance)).

    pa_rule('PA003').
    category('BlockingIO').
    severity(medium).
    cwe('CWE-400').
    cwe('CWE-770').
    description('Synchronous blocking I/O call in an async or concurrent context. Can starve thread pools, cause deadlocks, or degrade throughput.').
    triangle_tier(substitute).

    detection_pattern('File\\.read!').
    detection_pattern('File\\.write!').
    detection_pattern('IO\\.read').
    detection_pattern('std::fs::read').
    detection_pattern('readFileSync').
    detection_pattern('writeFileSync').

    remediation('Replace blocking I/O with async equivalents. In Elixir, use Task.async/Task.await or GenServer. In Rust, use tokio::fs. In JS, use fs/promises.').

    proven_modules([]).

    affected_languages([elixir, rust, javascript, java, python]).

    classify_severity(production, high) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, medium).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA004 - Unsafe Code
%% Unsafe blocks or operations bypassing type/memory safety
%% ---------------------------------------------------------------------------

:- object(pa004,
    implements(error_instance)).

    pa_rule('PA004').
    category('UnsafeCode').
    severity(high).
    cwe('CWE-704').
    cwe('CWE-188').
    description('Unsafe code block or operation bypassing language safety guarantees. Includes unsafe blocks without SAFETY comments, believe_me, unsafeCoerce, Obj.magic.').
    triangle_tier(control).

    detection_pattern('unsafe\\s*\\{').
    detection_pattern('believe_me').
    detection_pattern('assert_total').
    detection_pattern('unsafeCoerce').
    detection_pattern('unsafePerformIO').
    detection_pattern('Obj\\.magic').

    remediation('Replace unsafe operations with safe alternatives. In Rust, add // SAFETY: comment documenting invariants. In Idris2, replace believe_me with actual proofs. In Haskell, eliminate unsafeCoerce.').

    proven_modules([]).

    affected_languages([rust, idris2, haskell, ocaml, c, cpp, zig]).

    classify_severity(production, critical) :- !.
    classify_severity(test, medium) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA005 - Panic Path
%% Explicit panic, unwrap, or crash-on-failure patterns
%% ---------------------------------------------------------------------------

:- object(pa005,
    implements(error_instance)).

    pa_rule('PA005').
    category('PanicPath').
    severity(high).
    cwe('CWE-754').
    cwe('CWE-755').
    description('Explicit panic, unwrap, or crash-on-failure pattern. Causes abrupt process termination instead of graceful error handling.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('\\.unwrap\\(\\)').
    detection_pattern('panic!\\(').
    detection_pattern('todo!\\(').
    detection_pattern('unimplemented!\\(').
    detection_pattern('raise\\s+').
    detection_pattern('throw\\s+').

    remediation('Replace .unwrap() with pattern matching or .unwrap_or_default(). Replace panic! with Result/Option propagation. Use ? operator in Rust.').

    proven_modules([]).

    affected_languages([rust, elixir, python, java, javascript, haskell]).

    classify_severity(production, critical) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA006 - Race Condition
%% Data race or TOCTOU vulnerability
%% ---------------------------------------------------------------------------

:- object(pa006,
    implements(error_instance)).

    pa_rule('PA006').
    category('RaceCondition').
    severity(high).
    cwe('CWE-362').
    cwe('CWE-367').
    description('Potential data race or time-of-check-time-of-use (TOCTOU) vulnerability. Concurrent access to shared mutable state without proper synchronisation.').
    triangle_tier(control).

    detection_pattern('static\\s+mut\\s+').
    detection_pattern('global\\s+\\w+\\s*=').
    detection_pattern('File\\.exists\\?.*File\\.').
    detection_pattern('access\\(.*O_CREAT').
    detection_pattern(':ets\\.insert').

    remediation('Use mutexes, atomic operations, or message-passing. In Rust, use Arc<Mutex<T>>. In Elixir, use Agent or GenServer for shared state. Avoid TOCTOU by using atomic file operations.').

    proven_modules([]).

    affected_languages([c, cpp, rust, elixir, java, python]).

    classify_severity(production, critical) :- !.
    classify_severity(test, medium) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA007 - Deadlock Potential
%% Lock ordering violations or potential deadlocks
%% ---------------------------------------------------------------------------

:- object(pa007,
    implements(error_instance)).

    pa_rule('PA007').
    category('DeadlockPotential').
    severity(high).
    cwe('CWE-833').
    description('Potential deadlock from inconsistent lock ordering, nested locks, or blocking calls while holding a lock.').
    triangle_tier(control).

    detection_pattern('\\.lock\\(\\).*\\.lock\\(\\)').
    detection_pattern('Mutex::new.*Mutex::new').
    detection_pattern('synchronized.*synchronized').
    detection_pattern('GenServer\\.call.*GenServer\\.call').

    remediation('Enforce consistent lock ordering across all code paths. Use try_lock with timeout. In Elixir, avoid circular GenServer.call chains. Consider lock-free data structures.').

    proven_modules([]).

    affected_languages([rust, java, cpp, elixir, python]).

    classify_severity(production, critical) :- !.
    classify_severity(test, medium) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA008 - Resource Leak
%% File handles, sockets, or connections not properly closed
%% ---------------------------------------------------------------------------

:- object(pa008,
    implements(error_instance)).

    pa_rule('PA008').
    category('ResourceLeak').
    severity(medium).
    cwe('CWE-404').
    cwe('CWE-772').
    description('Resource leak: file handles, sockets, database connections, or other system resources not properly closed or released on all code paths.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('File\\.open.*[^c]$').
    detection_pattern('fopen\\(.*[^f]$').
    detection_pattern('socket\\(').
    detection_pattern('open\\(.*[^w]$').

    remediation('Use RAII, try-with-resources, or with-blocks to ensure cleanup. In Rust, rely on Drop trait. In Elixir, use File.open/2 with function callback. In C, pair every open with close in all paths.').

    proven_modules([]).

    affected_languages([c, cpp, rust, java, python, elixir, javascript]).

    classify_severity(production, high) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, medium).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA009 - Command Injection
%% Shell command construction from untrusted input
%% ---------------------------------------------------------------------------

:- object(pa009,
    implements(error_instance)).

    pa_rule('PA009').
    category('CommandInjection').
    severity(critical).
    cwe('CWE-78').
    cwe('CWE-77').
    description('Shell command injection: constructing shell commands from untrusted or unvalidated input. Allows arbitrary command execution.').
    triangle_tier(substitute).

    detection_pattern('System\\.cmd\\(').
    detection_pattern('os\\.system\\(').
    detection_pattern('subprocess\\.call.*shell=True').
    detection_pattern('eval\\s*\\(').
    detection_pattern('exec\\s*\\(').
    detection_pattern('\\$\\(.*\\$\\{').
    detection_pattern('system\\(').
    detection_pattern('popen\\(').

    remediation('Never construct shell commands from user input. Use parameterised APIs: System.cmd/3 in Elixir, Command::new in Rust, subprocess with list args in Python. Apply allowlist validation.').

    proven_modules(['SafeCommand', 'SafeSQL', 'SafeGit', 'SafeSSH']).

    affected_languages([shell, python, ruby, elixir, c, rust, javascript]).

    classify_severity(_, critical).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA010 - Unsafe Deserialization
%% Deserializing untrusted data without validation
%% ---------------------------------------------------------------------------

:- object(pa010,
    implements(error_instance)).

    pa_rule('PA010').
    category('UnsafeDeserialization').
    severity(critical).
    cwe('CWE-502').
    description('Unsafe deserialization of untrusted data. Can lead to remote code execution, denial of service, or data corruption when deserializing from external sources.').
    triangle_tier(substitute).

    detection_pattern('pickle\\.loads').
    detection_pattern('yaml\\.load\\(').
    detection_pattern('Marshal\\.load').
    detection_pattern('ObjectInputStream').
    detection_pattern(':erlang\\.binary_to_term').
    detection_pattern('JSON\\.parse.*eval').

    remediation('Use safe deserialization: yaml.safe_load in Python, Jason.decode in Elixir, serde with deny_unknown_fields in Rust. Never deserialize untrusted data with full object instantiation.').

    proven_modules(['SafeJson', 'SafeYAML', 'SafeXML']).

    affected_languages([python, ruby, java, elixir, javascript, rust]).

    classify_severity(_, critical).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA011 - Dynamic Code Execution
%% eval, exec, or dynamic code generation from strings
%% ---------------------------------------------------------------------------

:- object(pa011,
    implements(error_instance)).

    pa_rule('PA011').
    category('DynamicCodeExecution').
    severity(critical).
    cwe('CWE-95').
    cwe('CWE-94').
    description('Dynamic code execution via eval, exec, or code generation from strings. Enables code injection when input is untrusted.').
    triangle_tier(eliminate).

    detection_pattern('eval\\(').
    detection_pattern('exec\\(').
    detection_pattern('Function\\(').
    detection_pattern('Code\\.eval_string').
    detection_pattern('compile\\(.*exec').
    detection_pattern('load_string').

    remediation('Replace eval/exec with structured alternatives. Use pattern matching, lookup tables, or dedicated parsers. In Elixir, avoid Code.eval_string with user input. In JS, never eval user strings.').

    proven_modules([]).

    affected_languages([javascript, python, ruby, elixir, shell]).

    classify_severity(_, critical).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA012 - Unsafe FFI
%% Foreign function interface calls without proper safety wrappers
%% ---------------------------------------------------------------------------

:- object(pa012,
    implements(error_instance)).

    pa_rule('PA012').
    category('UnsafeFFI').
    severity(high).
    cwe('CWE-111').
    cwe('CWE-242').
    description('Unsafe foreign function interface usage without proper safety wrappers. Raw FFI calls bypass type checking, memory safety, and error handling of the host language.').
    triangle_tier(substitute).

    detection_pattern(':erlang\\.nif_error').
    detection_pattern('extern\\s+"C"').
    detection_pattern('foreign\\s+import').
    detection_pattern('ctypes\\.(cdll|windll)').
    detection_pattern('@external').
    detection_pattern('c_interop').

    remediation('Wrap all FFI calls in safe abstractions with input validation, null checks, and error translation. Use Zig FFI bridge (ffi/zig/) for C ABI compatibility. Validate all pointer arguments.').

    proven_modules([]).

    affected_languages([rust, elixir, idris2, haskell, python, ocaml, zig]).

    classify_severity(production, critical) :- !.
    classify_severity(test, medium) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA013 - Atom Exhaustion
%% Dynamic atom creation risking BEAM atom table exhaustion
%% ---------------------------------------------------------------------------

:- object(pa013,
    implements(error_instance)).

    pa_rule('PA013').
    category('AtomExhaustion').
    severity(high).
    cwe('CWE-400').
    cwe('CWE-770').
    description('Dynamic atom creation from untrusted input. The BEAM atom table is not garbage collected; unbounded atom creation causes VM crash via atom table exhaustion.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('String\\.to_atom').
    detection_pattern(':erlang\\.binary_to_atom').
    detection_pattern(':erlang\\.list_to_atom').
    detection_pattern('to_atom\\(').

    remediation('Replace String.to_atom/1 with String.to_existing_atom/1. Use an allowlist of permitted atoms. Never create atoms from user-supplied input. Consider using strings or maps instead.').

    proven_modules([]).

    affected_languages([elixir, erlang]).

    classify_severity(production, critical) :- !.
    classify_severity(test, medium) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA014 - Insecure Protocol
%% Use of HTTP, FTP, or other unencrypted protocols
%% ---------------------------------------------------------------------------

:- object(pa014,
    implements(error_instance)).

    pa_rule('PA014').
    category('InsecureProtocol').
    severity(medium).
    cwe('CWE-319').
    cwe('CWE-523').
    description('Use of insecure (unencrypted) protocol such as HTTP, FTP, or telnet where encrypted alternatives exist. Exposes data to interception.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('http://').
    detection_pattern('ftp://').
    detection_pattern('telnet://').
    detection_pattern('ws://').

    remediation('Replace http:// with https://, ftp:// with sftp://, telnet:// with ssh://, ws:// with wss://. Enforce TLS 1.2+ for all network connections.').

    proven_modules(['SafeUrl', 'SafeNetwork']).

    affected_languages([shell, javascript, python, rust, elixir, ruby, java]).

    classify_severity(production, high) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, medium).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA015 - Excessive Permissions
%% Overly broad file, workflow, or API permissions
%% ---------------------------------------------------------------------------

:- object(pa015,
    implements(error_instance)).

    pa_rule('PA015').
    category('ExcessivePermissions').
    severity(medium).
    cwe('CWE-732').
    cwe('CWE-276').
    description('Excessive permissions on files, workflows, or API tokens. Violates principle of least privilege and increases blast radius of compromise.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('chmod\\s+777').
    detection_pattern('chmod\\s+666').
    detection_pattern('permissions:\\s*write-all').
    detection_pattern('0o777').
    detection_pattern('umask\\s+000').
    detection_pattern('permissions:.*contents:\\s*write').

    remediation('Apply principle of least privilege. Use permissions: read-all at workflow level. Set file permissions to 0o644 or 0o755 maximum. Scope API tokens to minimum required permissions.').

    proven_modules([]).

    affected_languages([shell, yaml, javascript, python, rust, elixir]).

    classify_severity(production, high) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, medium).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA016 - Path Traversal
%% File path manipulation allowing directory traversal
%% ---------------------------------------------------------------------------

:- object(pa016,
    implements(error_instance)).

    pa_rule('PA016').
    category('PathTraversal').
    severity(critical).
    cwe('CWE-22').
    cwe('CWE-23').
    description('Path traversal vulnerability: user-controlled input used in file paths without sanitisation. Allows reading or writing arbitrary files via ../ sequences.').
    triangle_tier(substitute).

    detection_pattern('\\.\\./').
    detection_pattern('Path\\.join.*params').
    detection_pattern('os\\.path\\.join.*request').
    detection_pattern('File\\.read.*\\#\\{').
    detection_pattern('send_file.*params').

    remediation('Validate and canonicalise all file paths. Use Path.expand + prefix check in Elixir. Use std::path::Path::canonicalize in Rust. Never concatenate user input into file paths directly.').

    proven_modules(['SafePath']).

    affected_languages([elixir, python, ruby, javascript, rust, java, shell]).

    classify_severity(_, critical).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA017 - Hardcoded Secret
%% Credentials, API keys, or tokens embedded in source code
%% ---------------------------------------------------------------------------

:- object(pa017,
    implements(error_instance)).

    pa_rule('PA017').
    category('HardcodedSecret').
    severity(critical).
    cwe('CWE-798').
    cwe('CWE-259').
    description('Hardcoded secret, API key, token, or password in source code. Exposed credentials in version control allow unauthorised access.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('password\\s*=\\s*["\']').
    detection_pattern('api_key\\s*=\\s*["\']').
    detection_pattern('secret\\s*=\\s*["\']').
    detection_pattern('token\\s*=\\s*["\']').
    detection_pattern('AKIA[A-Z0-9]').
    detection_pattern('ghp_[a-zA-Z0-9]').
    detection_pattern('sk-[a-zA-Z0-9]').

    remediation('Move secrets to environment variables, .env files (gitignored), or a secrets manager. Use GitHub Secrets for CI/CD. Run TruffleHog or git-secrets as pre-commit hook.').

    proven_modules([]).

    affected_languages([shell, python, javascript, rust, elixir, ruby, java, yaml]).

    classify_severity(_, critical).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA018 - Unchecked Error
%% Error return values silently ignored
%% ---------------------------------------------------------------------------

:- object(pa018,
    implements(error_instance)).

    pa_rule('PA018').
    category('UncheckedError').
    severity(medium).
    cwe('CWE-252').
    cwe('CWE-754').
    description('Error return value silently ignored or discarded. Unhandled errors can lead to silent data corruption, resource leaks, or undefined program state.').
    triangle_tier(eliminate).
    auto_fixable.

    detection_pattern('_\\s*=.*!\\(').
    detection_pattern('let\\s+_\\s*=').
    detection_pattern('\\s+_\\s*<-').
    detection_pattern('catch\\s*\\{[^}]*\\}').
    detection_pattern('except:\\s*pass').
    detection_pattern('rescue\\s*_').
    detection_pattern('#\\[allow\\(unused_must_use\\)\\]').

    remediation('Handle all error returns explicitly. Use pattern matching on {:ok, _}/{:error, _} in Elixir. Use the ? operator in Rust. Log or propagate errors; never silently discard them.').

    proven_modules([]).

    affected_languages([rust, elixir, python, java, c, cpp, javascript]).

    classify_severity(production, high) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, medium).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA019 - Infinite Recursion
%% Recursive function without base case or depth limit
%% ---------------------------------------------------------------------------

:- object(pa019,
    implements(error_instance)).

    pa_rule('PA019').
    category('InfiniteRecursion').
    severity(medium).
    cwe('CWE-674').
    description('Recursive function without proper base case or depth limit. Causes stack overflow or OOM crash. Particularly dangerous in languages without tail-call optimisation.').
    triangle_tier(control).

    detection_pattern('def\\s+(\\w+).*\\1\\(').
    detection_pattern('fn\\s+(\\w+).*\\1\\(').
    detection_pattern('defp?\\s+(\\w+).*\\1\\(').
    detection_pattern('function\\s+(\\w+).*\\1\\(').

    remediation('Ensure all recursive functions have reachable base cases. Add explicit depth limits or use iterative alternatives. In Elixir, ensure tail-call position. Use Stream for lazy sequences.').

    proven_modules([]).

    affected_languages([elixir, rust, python, javascript, haskell, ocaml, idris2]).

    classify_severity(production, high) :- !.
    classify_severity(test, low) :- !.
    classify_severity(_, medium).

:- end_object.


%% ---------------------------------------------------------------------------
%% PA020 - Unsafe Type Coercion
%% Forced type conversion bypassing type system guarantees
%% ---------------------------------------------------------------------------

:- object(pa020,
    implements(error_instance)).

    pa_rule('PA020').
    category('UnsafeTypeCoercion').
    severity(high).
    cwe('CWE-704').
    cwe('CWE-681').
    description('Unsafe type coercion or cast bypassing type system guarantees. Includes transmute, believe_me, Obj.magic, unsafeCoerce, and unchecked numeric casts.').
    triangle_tier(eliminate).

    detection_pattern('transmute').
    detection_pattern('believe_me').
    detection_pattern('Obj\\.magic').
    detection_pattern('unsafeCoerce').
    detection_pattern('as\\s+\\*\\s*(const|mut)').
    detection_pattern('reinterpret_cast').
    detection_pattern('sorry').

    remediation('Replace unsafe casts with safe conversion functions. In Rust, use From/Into traits or TryFrom/TryInto. In Idris2, write actual proofs instead of believe_me. In Haskell, use proper newtype wrappers.').

    proven_modules([]).

    affected_languages([rust, idris2, haskell, ocaml, cpp, lean, coq]).

    classify_severity(production, critical) :- !.
    classify_severity(test, medium) :- !.
    classify_severity(_, high).

:- end_object.


%% ---------------------------------------------------------------------------
%% Utility object: error_catalog
%%
%% Aggregation queries over all PA rule instances.
%% ---------------------------------------------------------------------------

:- object(error_catalog).

    :- public(all_rules/1).
    :- mode(all_rules(-list), one).
    :- info(all_rules/1, [
        comment is 'Collect all PA rule objects.',
        argnames is ['Rules']
    ]).

    all_rules(Rules) :-
        findall(
            Obj,
            (   conforms_to_protocol(Obj, error_instance),
                Obj \== error_catalog
            ),
            Rules
        ).

    :- public(rules_by_severity/2).
    :- mode(rules_by_severity(+atom, -list), one).
    :- info(rules_by_severity/2, [
        comment is 'Find all PA rules at a given severity level.',
        argnames is ['Severity', 'Rules']
    ]).

    rules_by_severity(Severity, Rules) :-
        findall(
            Obj,
            (   conforms_to_protocol(Obj, error_instance),
                Obj::severity(Severity)
            ),
            Rules
        ).

    :- public(rules_by_tier/2).
    :- mode(rules_by_tier(+atom, -list), one).
    :- info(rules_by_tier/2, [
        comment is 'Find all PA rules routed to a given triangle tier.',
        argnames is ['Tier', 'Rules']
    ]).

    rules_by_tier(Tier, Rules) :-
        findall(
            Obj,
            (   conforms_to_protocol(Obj, error_instance),
                Obj::triangle_tier(Tier)
            ),
            Rules
        ).

    :- public(rules_for_language/2).
    :- mode(rules_for_language(+atom, -list), one).
    :- info(rules_for_language/2, [
        comment is 'Find all PA rules relevant to a given language.',
        argnames is ['Language', 'Rules']
    ]).

    rules_for_language(Language, Rules) :-
        findall(
            Obj,
            (   conforms_to_protocol(Obj, error_instance),
                Obj::affected_languages(Langs),
                member(Language, Langs)
            ),
            Rules
        ).

    :- public(auto_fixable_rules/1).
    :- mode(auto_fixable_rules(-list), one).
    :- info(auto_fixable_rules/1, [
        comment is 'Find all PA rules that are auto-fixable.',
        argnames is ['Rules']
    ]).

    auto_fixable_rules(Rules) :-
        findall(
            Obj,
            (   conforms_to_protocol(Obj, error_instance),
                Obj::auto_fixable
            ),
            Rules
        ).

    :- public(critical_rules/1).
    :- mode(critical_rules(-list), one).
    :- info(critical_rules/1, [
        comment is 'Find all critical-severity PA rules.',
        argnames is ['Rules']
    ]).

    critical_rules(Rules) :-
        rules_by_severity(critical, Rules).

    :- public(rule_summary/2).
    :- mode(rule_summary(+object_identifier, -compound), one).
    :- info(rule_summary/2, [
        comment is 'Get a summary tuple for a PA rule object.',
        argnames is ['RuleObj', 'Summary']
    ]).

    rule_summary(RuleObj, summary(PA, Cat, Sev, Tier, Desc)) :-
        RuleObj::pa_rule(PA),
        RuleObj::category(Cat),
        RuleObj::severity(Sev),
        RuleObj::triangle_tier(Tier),
        RuleObj::description(Desc).

    :- private(member/2).
    member(X, [X|_]).
    member(X, [_|T]) :- member(X, T).

    :- private(conforms_to_protocol/2).
    conforms_to_protocol(Obj, Proto) :-
        current_object(Obj),
        implements_protocol(Obj, Proto).

:- end_object.
