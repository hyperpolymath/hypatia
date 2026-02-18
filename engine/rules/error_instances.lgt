% SPDX-License-Identifier: PMPL-1.0-or-later
% Error Instance Log - Runtime error tracking
% Origin: security-audit/logtalk-db/error_instances.lgt

:- object(error_instances).

    :- info([
        version is 1:0:0,
        author is 'hyperpolymath',
        date is 2025-12-31,
        comment is 'Runtime log of security errors discovered'
    ]).

    % Error instance format: error(ID, Repo, Category, Severity, File, Line, Description, Status, FixCommit)
    :- public(error/9).
    :- dynamic(error/9).
    :- mode(error(?atom, ?atom, ?atom, ?atom, ?atom, ?integer, ?atom, ?atom, ?atom), zero_or_more).

    % Status values: open, fixed, dismissed, wont_fix

    % Add error instance
    :- public(add_error/8).
    :- mode(add_error(+atom, +atom, +atom, +atom, +atom, +integer, +atom, +atom), one).
    add_error(Repo, Category, Severity, File, Line, Description, Status, FixCommit) :-
        gensym(err_, ID),
        assertz(error(ID, Repo, Category, Severity, File, Line, Description, Status, FixCommit)).

    % Query helpers
    :- public(errors_by_repo/2).
    :- mode(errors_by_repo(+atom, -list), one).
    errors_by_repo(Repo, Errors) :-
        findall(E, error(E, Repo, _, _, _, _, _, _, _), Errors).

    :- public(open_errors/1).
    :- mode(open_errors(-list), one).
    open_errors(Errors) :-
        findall(error(ID, Repo, Cat, Sev, File, Line, Desc, open, _),
                error(ID, Repo, Cat, Sev, File, Line, Desc, open, _),
                Errors).

    :- public(errors_by_category/2).
    :- mode(errors_by_category(+atom, -list), one).
    errors_by_category(Category, Errors) :-
        findall(E, error(E, _, Category, _, _, _, _, _, _), Errors).

:- end_object.
