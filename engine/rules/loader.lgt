% SPDX-License-Identifier: PMPL-1.0-or-later
% Loader for Security Error Database (from security-audit)
% Origin: security-audit/logtalk-db/loader.lgt

:- initialization((
    logtalk_load([
        security_errors,
        error_instances,
        prevention_hooks,
        error_catalog
    ])
)).
