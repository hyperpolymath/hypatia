# SPDX-License-Identifier: MPL-2.0
# shellcheck shell=bash
#
# Sourced by detect.sh and remediate.sh: the bare `on: [push, pull_request]`
# trigger that D-BURN names, with an optional trailing comment. detect.sh
# reports a workflow on it, and remediate.sh selects and rewrites the same
# line, so every file the selector picks is one the rewrite changes.
# An ERE for grep -E and for awk (read there through ENVIRON, because
# awk -v would rewrite its backslashes).
# shellcheck disable=SC2034 # used by the scripts that source this file
BURN_TRIGGER_ERE='^on:[[:space:]]*\[[[:space:]]*push[[:space:]]*,[[:space:]]*pull_request[[:space:]]*\][[:space:]]*(#.*)?$'
