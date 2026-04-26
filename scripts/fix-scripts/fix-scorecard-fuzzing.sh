#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-scorecard-fuzzing.sh — Add fuzzing setup (SC-014)
# Recipe: recipe-scorecard-fuzzing (confidence: 0.80, auto_fixable: true)
#
# Adds cargo-fuzz setup for Rust repos, or a libfuzzer workflow stub for others.
# OSS-Fuzz integration requires separate registration; this creates the fuzz target.
#
# Usage: fix-scorecard-fuzzing.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-scorecard-fuzzing.sh <repo-path>}"

if [[ -f "${REPO}/Cargo.toml" ]]; then
  FUZZ_DIR="${REPO}/fuzz"

  if [[ -d "$FUZZ_DIR" ]]; then
    echo "[fix-scorecard-fuzzing] Fuzz directory already exists: ${FUZZ_DIR}"
    exit 0
  fi

  mkdir -p "${FUZZ_DIR}/fuzz_targets"

  cat > "${FUZZ_DIR}/Cargo.toml" <<'TOML'
# SPDX-License-Identifier: PMPL-1.0-or-later
[package]
name = "fuzz"
version = "0.0.0"
publish = false
edition = "2021"

[package.metadata]
cargo-fuzz = true

[dependencies]
libfuzzer-sys = "0.4"

[[bin]]
name = "fuzz_target_1"
path = "fuzz_targets/fuzz_target_1.rs"
test = false
doc = false
bench = false
TOML

  cat > "${FUZZ_DIR}/fuzz_targets/fuzz_target_1.rs" <<'RUST'
// SPDX-License-Identifier: PMPL-1.0-or-later
// Fuzz target template — replace with meaningful parsing entry point
#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // TODO: call your parsing/deserialisation entry point here
    let _ = std::str::from_utf8(data);
});
RUST

  mkdir -p "${REPO}/.github/workflows"
  cat > "${REPO}/.github/workflows/fuzz.yml" <<'YAML'
# SPDX-License-Identifier: PMPL-1.0-or-later
# Nightly fuzz run — 5 minutes per target
name: Fuzz

on:
  schedule:
    - cron: '0 2 * * 0'
  workflow_dispatch:

permissions: read-all

jobs:
  fuzz:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@de0fac2e4500dabe0009e67214ff5f5447ce83dd # v6.0.2
      - uses: dtolnay/rust-toolchain@4be9e76fd7c4901c61fb841f559994984270fce7 # stable
        with:
          toolchain: nightly
      - run: cargo install cargo-fuzz
      - run: cargo fuzz run fuzz_target_1 -- -max_total_time=300
YAML

  echo "[fix-scorecard-fuzzing] Created fuzz/ directory and nightly fuzz workflow"
else
  echo "[fix-scorecard-fuzzing] Non-Rust repo — creating fuzz workflow stub"
  mkdir -p "${REPO}/.github/workflows"
  cat > "${REPO}/.github/workflows/fuzz.yml" <<'YAML'
# SPDX-License-Identifier: PMPL-1.0-or-later
# TODO: configure fuzzing for this project's language
name: Fuzz

on:
  schedule:
    - cron: '0 2 * * 0'
  workflow_dispatch:

permissions: read-all

jobs:
  fuzz:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@de0fac2e4500dabe0009e67214ff5f5447ce83dd # v6.0.2
      - name: Run fuzz tests
        run: echo "TODO: add language-appropriate fuzzing"
YAML
  echo "[fix-scorecard-fuzzing] Created fuzz workflow stub — add language-specific fuzzing"
fi
