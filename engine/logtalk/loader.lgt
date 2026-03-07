%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
%%
%% loader.lgt - Logtalk loader for Hypatia rule engine modules
%%
%% Loads all Logtalk components in dependency order:
%%   1. error_instances.lgt  - Protocol + PA001-PA020 objects + error_catalog
%%   2. (future) approved rules, detection heuristics, learning feedback
%%
%% Usage from SWI-Prolog with Logtalk:
%%   ?- logtalk_load('engine/logtalk/loader').
%%
%% Usage from Elixir (via port or NIF):
%%   System.cmd("swipl", ["-g", "logtalk_load('engine/logtalk/loader')", ...])


%% ---------------------------------------------------------------------------
%% Load order matters: protocol must be defined before implementing objects.
%% error_instances.lgt contains both the protocol and all implementations,
%% so it is self-contained. Additional rule modules should be loaded after it.
%% ---------------------------------------------------------------------------

:- initialization((
    % Set the library alias for the engine directory
    logtalk_load(error_instances, [
        source_data(on),
        optimize(on),
        clean(on)
    ])
)).
