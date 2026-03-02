# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

# Bot Directives

This directory contains per-bot configuration for the gitbot-fleet.

Each subdirectory corresponds to a bot in the fleet:

- `rhodibot/` — Primary remediation bot (Rhodium Standard enforcement)
- `echidnabot/` — Formal verification auditor (believe_me, Admitted, sorry detection)
- `sustainabot/` — Sustainability and dependency health
- `glambot/` — Documentation and presentation quality
- `seambot/` — Integration seam and API boundary checker
- `finishbot/` — Completion and TODO tracker

Directives are read by the gitbot-fleet orchestrator and control bot
behaviour, thresholds, and scope within this repository.
