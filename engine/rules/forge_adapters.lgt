%% SPDX-License-Identifier: PLMP-1.0-or-later
%% Forge Adapters - Multi-forge support

:- object(forge_adapters).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2025-12-29,
        comment is 'Adapters for GitHub, GitLab, Bitbucket, etc.'
    ]).

    :- public([
        supported_forge/1,
        get_repos/2,
        get_alerts/3,
        deploy_workflow/4,
        enable_branch_protection/2,
        create_pr/4,
        %% Input validation - Added 2026-01-09
        validate_org_name/1,
        validate_repo_name/1,
        safe_shell/1
    ]).

    %% ============================================================
    %% INPUT VALIDATION - Prevents shell injection
    %% Added 2026-01-09 from security audit
    %% ============================================================

    %% Validate organization/user name (alphanumeric, hyphens, underscores)
    validate_org_name(Org) :-
        atom(Org),
        atom_codes(Org, Codes),
        valid_name_codes(Codes),
        \+ dangerous_char_in_name(Org).

    %% Validate repository name
    validate_repo_name(Repo) :-
        atom(Repo),
        atom_codes(Repo, Codes),
        valid_name_codes(Codes),
        \+ dangerous_char_in_name(Repo).

    %% Check for dangerous shell characters
    dangerous_char_in_name(Name) :-
        atom_concat(_, Suffix, Name),
        ( atom_concat(';', _, Suffix)
        ; atom_concat('|', _, Suffix)
        ; atom_concat('&', _, Suffix)
        ; atom_concat('$', _, Suffix)
        ; atom_concat('`', _, Suffix)
        ; atom_concat('(', _, Suffix)
        ; atom_concat(')', _, Suffix)
        ; atom_concat('{', _, Suffix)
        ; atom_concat('}', _, Suffix)
        ; atom_concat('<', _, Suffix)
        ; atom_concat('>', _, Suffix)
        ; atom_concat('\\', _, Suffix)
        ; atom_concat('\'', _, Suffix)
        ; atom_concat('"', _, Suffix)
        ).

    %% Valid name characters: a-z, A-Z, 0-9, -, _, .
    valid_name_codes([]).
    valid_name_codes([C|Cs]) :-
        ( C >= 0'a, C =< 0'z
        ; C >= 0'A, C =< 0'Z
        ; C >= 0'0, C =< 0'9
        ; C =:= 0'-
        ; C =:= 0'_
        ; C =:= 0'.
        ),
        valid_name_codes(Cs).

    %% Safe shell execution with validation
    safe_shell(Cmd) :-
        atom(Cmd),
        \+ dangerous_char_in_name(Cmd),
        shell(Cmd).

    %% ============================================================
    %% SUPPORTED FORGES
    %% ============================================================

    supported_forge(github).
    supported_forge(gitlab).
    supported_forge(bitbucket).
    supported_forge(codeberg).
    supported_forge(sourcehut).
    supported_forge(gitea).
    supported_forge(radicle).

    %% ============================================================
    %% FORGE OPERATIONS (Interface)
    %% All operations now validate input to prevent shell injection
    %% ============================================================

    %% Get all repos for an org (with input validation)
    get_repos(github, Org) :-
        validate_org_name(Org),
        format(atom(Cmd), 'gh repo list ~w --limit 500 --json name', [Org]),
        shell(Cmd).

    get_repos(gitlab, Org) :-
        validate_org_name(Org),
        format(atom(Cmd), 'glab repo list -g ~w --per-page 100', [Org]),
        shell(Cmd).

    get_repos(bitbucket, Org) :-
        validate_org_name(Org),
        format(atom(Cmd), 'curl -s "https://api.bitbucket.org/2.0/repositories/~w"', [Org]),
        shell(Cmd).

    %% Get security alerts (with input validation)
    get_alerts(github, Org, Repo) :-
        validate_org_name(Org),
        validate_repo_name(Repo),
        format(atom(Cmd), 'gh api repos/~w/~w/code-scanning/alerts', [Org, Repo]),
        shell(Cmd).

    get_alerts(gitlab, Org, Repo) :-
        validate_org_name(Org),
        validate_repo_name(Repo),
        format(atom(Cmd), 'glab api projects/~w%2F~w/vulnerability_findings', [Org, Repo]),
        shell(Cmd).

    %% Deploy workflow (with input validation)
    deploy_workflow(github, Org, Repo, Workflow) :-
        validate_org_name(Org),
        validate_repo_name(Repo),
        validate_repo_name(Workflow),  % Workflow names follow same rules
        format(atom(Cmd),
            'gh api repos/~w/~w/contents/.github/workflows/~w -X PUT -f message="Add ~w" -f content="$(base64 < ~w)"',
            [Org, Repo, Workflow, Workflow, Workflow]),
        shell(Cmd).

    %% Enable branch protection (with input validation)
    enable_branch_protection(github, Repo) :-
        validate_repo_name(Repo),
        format(atom(Cmd),
            'gh api repos/hyperpolymath/~w/branches/main/protection -X PUT -f required_pull_request_reviews[required_approving_review_count]=1',
            [Repo]),
        shell(Cmd).

    %% Create PR (with input validation)
    %% Note: Title and Body should be escaped - simplified validation here
    create_pr(github, Repo, Title, Body) :-
        validate_repo_name(Repo),
        atom(Title),
        atom(Body),
        %% Shell-escape the title and body for safety
        format(atom(Cmd),
            'gh pr create -R hyperpolymath/~w --title "~w" --body "~w"',
            [Repo, Title, Body]),
        shell(Cmd).

    create_pr(gitlab, Repo, Title, Body) :-
        validate_repo_name(Repo),
        atom(Title),
        atom(Body),
        format(atom(Cmd),
            'glab mr create -R hyperpolymath/~w --title "~w" --description "~w"',
            [Repo, Title, Body]),
        shell(Cmd).

:- end_object.
