%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Scanner Implementation - Actual file scanning predicates
%% Implements the helper predicates that code-safety-lessons and container-security-lessons use

:- object(scanner).

    :- info([
        version is 1.0,
        author is 'Hypatia Scanning Engine',
        date is 2026-01-25,
        comment is 'Concrete implementation of code scanning predicates using ripgrep and filesystem'
    ]).

    :- public([
        read_code_line/3,
        read_first_line/2,
        file_extension/2,
        file_has_spdx_header/1,
        file_has_spdx_header/2,
        repo_has_file/2,
        repo_has_workflow/2,
        workflow_contains/2,
        repo_languages/2,
        scan_directory/2
    ]).

    %% ============================================================
    %% FILE READING PREDICATES
    %% ============================================================

    % Read a specific line from a file with line number
    % read_code_line(+FilePath, ?LineNum, ?LineContent)
    read_code_line(FilePath, LineNum, LineContent) :-
        atom(FilePath),
        exists_file(FilePath),
        open(FilePath, read, Stream),
        read_lines_from_stream(Stream, 1, LineNum, LineContent),
        close(Stream).

    % Helper to read lines from stream
    read_lines_from_stream(Stream, CurrentNum, TargetNum, Line) :-
        \+ at_end_of_stream(Stream),
        read_line_to_string(Stream, CurrentLine),
        ( CurrentNum = TargetNum ->
            atom_string(Line, CurrentLine)
        ; NextNum is CurrentNum + 1,
          read_lines_from_stream(Stream, NextNum, TargetNum, Line)
        ).

    % Read first line of file
    read_first_line(FilePath, Line) :-
        read_code_line(FilePath, 1, Line).

    %% ============================================================
    %% FILE PROPERTIES
    %% ============================================================

    % Check file extension
    file_extension(FilePath, Extension) :-
        atom(FilePath),
        atom_concat(_, Extension, FilePath),
        atom_concat('.', _, Extension).

    % Check if file has SPDX header
    file_has_spdx_header(FilePath) :-
        read_first_line(FilePath, FirstLine),
        atom_concat(_, 'SPDX-License-Identifier:', FirstLine).

    % Check file has specific SPDX license
    file_has_spdx_header(FilePath, License) :-
        read_first_line(FilePath, FirstLine),
        atom_concat('SPDX-License-Identifier:', Rest, FirstLine),
        atom_concat(' ', LicenseRaw, Rest),
        atom_string(LicenseRaw, LicenseStr),
        split_string(LicenseStr, " \t\n", " \t\n", [LicenseClean|_]),
        atom_string(License, LicenseClean).

    %% ============================================================
    %% CONTEXT ANALYSIS (simplified implementations)
    %% ============================================================

    % Check if line is in test code
    line_is_in_test(FilePath, _LineNum) :-
        ( atom_concat(_, '_test.rs', FilePath) -> true
        ; atom_concat(_, '/tests/', FilePath) -> true
        ; atom_concat(_, '/test/', FilePath) -> true
        ; fail
        ).

    % Check if previous line has validation/check
    previous_line_has_expect_check(FilePath, LineNum) :-
        LineNum > 1,
        PrevLineNum is LineNum - 1,
        read_code_line(FilePath, PrevLineNum, PrevLine),
        ( atom_concat(_, 'if ', PrevLine) -> true
        ; atom_concat(_, 'match ', PrevLine) -> true
        ; atom_concat(_, 'Some(_)', PrevLine) -> true
        ; fail
        ).

    % Check if nearby lines (within N) contain pattern
    nearby_line_has_pattern(FilePath, LineNum, Pattern, Range) :-
        between(1, Range, Offset),
        member(Direction, [-1, 1]),
        TargetLine is LineNum + (Offset * Direction),
        TargetLine > 0,
        read_code_line(FilePath, TargetLine, Line),
        atom_concat(_, Pattern, Line).

    nearby_line_has_wildcard_origin(FilePath, LineNum) :-
        nearby_line_has_pattern(FilePath, LineNum, '"*"', 5).

    next_lines_contain_verify(FilePath, LineNum, Range) :-
        nearby_line_has_pattern(FilePath, LineNum, 'verify', Range).

    next_lines_contain_env_check(FilePath, LineNum, Range) :-
        nearby_line_has_pattern(FilePath, LineNum, 'env', Range).

    previous_lines_contain_validation(FilePath, LineNum, Range) :-
        nearby_line_has_pattern(FilePath, LineNum, 'validate', Range).

    %% ============================================================
    %% EXTRACTION HELPERS
    %% ============================================================

    % Extract context from line (simplified - returns line itself)
    extract_context(Line, Line).

    % Extract default value from unwrap_or pattern
    extract_default_value(Line, Value) :-
        atom_concat(_, '.unwrap_or(', Rest),
        atom_concat(ValueStr, ')', Rest),
        atom_string(ValueStr, Value).
    extract_default_value(_, unknown).

    % Extract variable name (simplified)
    extract_variable_name(Line, VarName) :-
        % Try to extract first word before '='
        atom_concat(Prefix, ' =', Line),
        split_string(Prefix, " \t", " \t", Parts),
        last(Parts, VarStr),
        atom_string(VarName, VarStr).
    extract_variable_name(_, unknown).

    % Extract command from Command::new pattern
    extract_command(Line, Cmd) :-
        atom_concat(_, 'Command::new(', Rest),
        atom_concat(CmdStr, ')', Rest),
        atom_string(CmdStr, Cmd).
    extract_command(_, unknown).

    extract_comment(Line, Comment) :-
        ( atom_concat(_, '//', Rest) ->
            atom_string(Rest, Comment)
        ; atom_concat(_, '/*', Rest) ->
            atom_concat(CommentStr, '*/', Rest),
            atom_string(CommentStr, Comment)
        ; Comment = ''
        ).

    extract_lock_type(Line, LockType) :-
        ( atom_concat(_, 'RwLock', _) -> LockType = rwlock
        ; atom_concat(_, 'Mutex', _) -> LockType = mutex
        ; LockType = unknown
        ).

    %% ============================================================
    %% REPOSITORY QUERIES
    %% ============================================================

    % Check if repo has file
    repo_has_file(RepoPath, FileName) :-
        atom_concat(RepoPath, '/', Base),
        atom_concat(Base, FileName, FullPath),
        exists_file(FullPath).

    % Check if repo has workflow
    repo_has_workflow(RepoPath, WorkflowName) :-
        atom_concat(RepoPath, '/.github/workflows/', WorkflowDir),
        atom_concat(WorkflowDir, WorkflowName, WorkflowPath),
        exists_file(WorkflowPath).

    % Check if workflow contains pattern
    workflow_contains(WorkflowPath, Pattern) :-
        exists_file(WorkflowPath),
        open(WorkflowPath, read, Stream),
        read_stream_to_string(Stream, Content),
        close(Stream),
        atom_string(ContentAtom, Content),
        atom_concat(_, Pattern, ContentAtom).

    % Detect repository languages (simplified - checks for common file extensions)
    repo_languages(RepoPath, Languages) :-
        findall(Lang,
            (member(Ext-Lang, [
                ('.rs', rust),
                ('.res', rescript),
                ('.ml', ocaml),
                ('.hs', haskell),
                ('.jl', julia),
                ('.gleam', gleam),
                ('.js', javascript),
                ('.ts', typescript),
                ('.py', python),
                ('.go', go)
            ]),
            atom_concat(RepoPath, '/**/*', Pattern),
            atom_concat(Pattern, Ext, SearchPattern),
            expand_file_name(SearchPattern, Files),
            Files \== []),
            LanguagesWithDups),
        sort(LanguagesWithDups, Languages).

    %% ============================================================
    %% ADVANCED PATTERN DETECTION
    %% ============================================================

    % Check if line handles external data (heuristic)
    line_handles_external_data(FilePath, LineNum, Source) :-
        read_code_line(FilePath, LineNum, Line),
        ( atom_concat(_, 'Fetch.', Line) -> Source = http_request
        ; atom_concat(_, 'Request.', Line) -> Source = http_request
        ; atom_concat(_, 'json()', Line) -> Source = json_parse
        ; atom_concat(_, 'query', Line) -> Source = query_param
        ; atom_concat(_, 'body', Line) -> Source = request_body
        ; fail
        ).

    % Check if function is in hot path (heuristic)
    function_is_hot_path(FilePath, LineNum, FuncName) :-
        % Look backwards for function definition
        search_backwards_for_function(FilePath, LineNum, FuncName),
        % Hot path heuristics
        ( atom_concat(_, 'handle', FuncName) -> true
        ; atom_concat(_, 'process', FuncName) -> true
        ; atom_concat(_, 'execute', FuncName) -> true
        ; fail
        ).

    search_backwards_for_function(FilePath, LineNum, FuncName) :-
        LineNum > 0,
        read_code_line(FilePath, LineNum, Line),
        ( atom_concat(_, 'fn ', Line),
          extract_function_name(Line, FuncName) -> true
        ; PrevLine is LineNum - 1,
          search_backwards_for_function(FilePath, PrevLine, FuncName)
        ).

    extract_function_name(Line, FuncName) :-
        atom_concat(_, 'fn ', Rest),
        atom_concat(FuncNameRaw, '(', Rest),
        split_string(FuncNameRaw, " \t", " \t", [NameStr|_]),
        atom_string(FuncName, NameStr).

    % Path validation checks
    previous_lines_validate_path(FilePath, LineNum, _VarName) :-
        nearby_line_has_pattern(FilePath, LineNum, 'validate', 5).

    previous_lines_validate_command(FilePath, LineNum) :-
        nearby_line_has_pattern(FilePath, LineNum, 'allowlist', 5).

    %% ============================================================
    %% DIRECTORY SCANNING
    %% ============================================================

    % Scan directory for all source files
    scan_directory(DirPath, SourceFiles) :-
        findall(File,
            (member(Ext, ['.rs', '.res', '.ml', '.hs', '.jl', '.gleam']),
             atom_concat(DirPath, '/**/*', Pattern),
             atom_concat(Pattern, Ext, SearchPattern),
             expand_file_name(SearchPattern, Files),
             member(File, Files)),
            AllFiles),
        sort(AllFiles, SourceFiles).

:- end_object.
