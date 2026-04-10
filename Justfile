# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# justfile — hypatia
# Run with: just <recipe>

set shell := ["bash", "-euo", "pipefail", "-c"]

# Default recipe: show help
default:
    @just --list

# Build Rust workspace
build:
    OPENSSL_NO_VENDOR=1 cargo build --release

# Run Rust workspace tests
test:
    OPENSSL_NO_VENDOR=1 cargo test

# Run Elixir tests
test-elixir:
    mix test

# Build everything (Rust + Elixir)
build-all: build
    mix deps.get && mix compile

# Format all code
fmt:
    cargo fmt
    mix format

# Check formatting without modifying
fmt-check:
    cargo fmt --all --check
# Run clippy lints
lint:
    cargo clippy -- -D warnings

# Run hypatia CLI
cli *ARGS:
    cargo run --bin hyper -- {{ARGS}}

# Run neurosymbolic scan on a target
scan target *ARGS:
    cargo run --bin hyper -- scan "{{target}}" {{ARGS}}

# Run Invariant Path overlay tools for this repository
invariant-path *ARGS:
    ./scripts/invariant-path.sh {{ARGS}}

# Compile Idris2 ABI definitions
compile-abi:
    cd src/abi && idris2 --build hypatia-abi.ipkg

# Compile Idris2 verification proofs
compile-verify:
    cd verify && idris2 --build hypatia-verify.ipkg

# Build Zig FFI bridge
build-ffi:
    cd ffi/zig && zig build

# Run panic-attack static analysis
panic-scan:
    @PANIC_BIN="$(command -v panic-attack 2>/dev/null || echo "")"; \
    if [ -z "$PANIC_BIN" ] && [ -n "${PANIC_ATTACKER_DIR:-}" ]; then \
        PANIC_BIN="${PANIC_ATTACKER_DIR}/target/release/panic-attack"; \
    fi; \
    if [ -x "$PANIC_BIN" ]; then \
        "$PANIC_BIN" assail . --verbose; \
    else \
        echo "panic-attack not found — install it or set PANIC_ATTACKER_DIR"; \
    fi

# Check license compliance
license-check:
    @echo "Checking for banned AGPL-3.0 headers..."
    @if grep -rl "AGPL-3.0" --include='*.rs' --include='*.ex' --include='*.exs' --include='*.idr' --include='*.zig' --include='*.yml' . 2>/dev/null; then \
        echo "FAIL: Found AGPL-3.0 headers"; \
        exit 1; \
    else \
        echo "PASS: No AGPL-3.0 headers found"; \
    fi

# Validate SCM files are in .machine_readable/ only
check-scm:
    @for f in STATE.scm META.scm ECOSYSTEM.scm; do \
        if [ -f "$$f" ]; then \
            echo "ERROR: $$f found in root"; exit 1; \
        fi; \
    done
    @echo "PASS: No SCM files in root"

# Clean all build artifacts
clean:
    cargo clean
    @echo "Cleaned."

# [AUTO-GENERATED] Multi-arch / RISC-V target
build-riscv:
	@echo "Building for RISC-V..."
	cross build --target riscv64gc-unknown-linux-gnu

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"

# ── Onboarding ────────────────────────────────────────────────

# Check that all required tools are installed and working
doctor:
    #!/usr/bin/env bash
    set -euo pipefail
    PASS=0; FAIL=0; WARN=0
    check() {
        local name="$1" cmd="$2"
        if command -v "$cmd" >/dev/null 2>&1; then
            ver=$("$cmd" --version 2>/dev/null | head -1 || echo "unknown")
            echo "  [OK] $name: $ver"
            PASS=$((PASS + 1))
        else
            echo "  [FAIL] $name: '$cmd' not found"
            FAIL=$((FAIL + 1))
        fi
    }
    check_optional() {
        local name="$1" cmd="$2"
        if command -v "$cmd" >/dev/null 2>&1; then
            ver=$("$cmd" --version 2>/dev/null | head -1 || echo "unknown")
            echo "  [OK] $name: $ver"
            PASS=$((PASS + 1))
        else
            echo "  [WARN] $name: '$cmd' not found (optional)"
            WARN=$((WARN + 1))
        fi
    }
    echo "=== hypatia: Doctor ==="
    echo ""
    echo "Required tools:"
    check "Elixir" "elixir"
    check "Erlang" "erl"
    check "Mix" "mix"
    check "Rust/cargo" "cargo"
    check "just" "just"
    check "pkg-config" "pkg-config"
    # Check openssl headers
    if pkg-config --exists openssl 2>/dev/null; then
        echo "  [OK] openssl-devel: $(pkg-config --modversion openssl)"
        PASS=$((PASS + 1))
    else
        echo "  [FAIL] openssl-devel: not found (install openssl-devel or libssl-dev)"
        FAIL=$((FAIL + 1))
    fi
    # Check Hex and rebar
    if mix local.hex --check 2>/dev/null; then
        echo "  [OK] Hex: installed"
        PASS=$((PASS + 1))
    else
        echo "  [WARN] Hex: not installed (run 'mix local.hex --force')"
        WARN=$((WARN + 1))
    fi
    echo ""
    echo "Optional tools (ABI/FFI layers):"
    check_optional "Idris2" "idris2"
    check_optional "Zig" "zig"
    check_optional "panic-attack" "panic-attack"
    echo ""
    echo "Elixir dependencies:"
    if [ -d "deps" ] && [ "$(ls -A deps 2>/dev/null)" ]; then
        echo "  [OK] deps/ populated ($(ls deps | wc -l) packages)"
    else
        echo "  [INFO] deps/ empty — run 'mix deps.get'"
    fi
    echo ""
    echo "Rust workspace:"
    if [ -f "target/release/hyper" ] || [ -d "target/debug" ]; then
        echo "  [OK] Rust workspace built"
    else
        echo "  [INFO] Rust workspace not built — run 'just build'"
    fi
    echo ""
    echo "=== Results: $PASS passed, $FAIL failed, $WARN warnings ==="
    if [ "$FAIL" -gt 0 ]; then
        echo "Run 'just heal' for install instructions."
        exit 1
    fi

# Show install instructions for missing tools
heal:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== hypatia: Heal ==="
    echo ""
    echo "Install missing tools:"
    echo ""
    if ! command -v elixir >/dev/null 2>&1; then
        echo "  Elixir + Erlang (REQUIRED):"
        echo "    asdf plugin add erlang && asdf install erlang latest"
        echo "    asdf plugin add elixir && asdf install elixir latest"
        echo "    mix local.hex --force && mix local.rebar --force"
        echo ""
    fi
    if ! command -v cargo >/dev/null 2>&1; then
        echo "  Rust (REQUIRED):"
        echo "    asdf install rust nightly"
        echo "    # Or: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
        echo ""
    fi
    if ! pkg-config --exists openssl 2>/dev/null; then
        echo "  OpenSSL development headers (REQUIRED):"
        echo "    Fedora: sudo dnf install openssl-devel pkg-config"
        echo "    Ubuntu: sudo apt install libssl-dev pkg-config"
        echo ""
    fi
    if ! command -v idris2 >/dev/null 2>&1; then
        echo "  Idris2 (for ABI definitions):"
        echo "    Install via pack: https://github.com/stefan-hoeck/idris2-pack"
        echo ""
    fi
    if ! command -v zig >/dev/null 2>&1; then
        echo "  Zig (for FFI bridge):"
        echo "    asdf plugin add zig && asdf install zig 0.13.0"
        echo ""
    fi
    if ! command -v panic-attack >/dev/null 2>&1; then
        echo "  panic-attack (pre-commit scans):"
        echo "    cargo install --git https://github.com/hyperpolymath/panic-attacker"
        echo ""
    fi
    echo "After installing, run 'just doctor' to verify."

# Guided tour of the repository structure
tour:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== Hypatia: Guided Tour ==="
    echo ""
    echo "Hypatia is the neurosymbolic CI/CD intelligence layer."
    echo "It coordinates gitbot-fleet via a safety triangle pipeline."
    echo ""
    echo "1. ELIXIR PIPELINE: lib/"
    echo "   Core OTP application with 8 GenServers."
    echo "     lib/pattern_analyzer.ex      - Full pipeline orchestrator"
    echo "     lib/verisimdb_connector.ex   - VQL-powered data access"
    echo "     lib/pattern_registry.ex      - Canonical patterns (PA001-PA020)"
    echo "     lib/triangle_router.ex       - Eliminate > Substitute > Control"
    echo "     lib/fleet_dispatcher.ex      - Confidence-gated dispatch"
    echo "     lib/outcome_tracker.ex       - Bayesian confidence updating"
    echo ""
    echo "2. NEURAL SUBSYSTEM: lib/neural/"
    echo "   5 neural networks + coordinator GenServer:"
    echo "     graph_of_trust.ex            - PageRank trust"
    echo "     mixture_of_experts.ex        - 7 expert domains"
    echo "     liquid_state_machine.ex      - Temporal anomaly detection"
    echo "     echo_state_network.ex        - Confidence forecasting"
    echo "     radial_neural_network.ex     - Similarity + novelty"
    echo ""
    echo "3. VQL QUERY LAYER: lib/vql/"
    echo "   Built-in parser + file executor for verisimdb-data queries."
    echo ""
    echo "4. RUST WORKSPACE: adapters/ cli/ data/ fixer/ integration/"
    echo "   Rust crates for adapters, CLI tools, data processing, fixes."
    echo ""
    echo "5. ABI/FFI:"
    echo "     src/abi/   - Idris2 types (GraphQL, gRPC, REST proofs)"
    echo "     ffi/zig/   - 7 exported C functions"
    echo ""
    echo "6. SAFETY SYSTEMS: lib/safety/"
    echo "     rate_limiter.ex   - Per-bot + global dispatch limits"
    echo "     quarantine.ex     - Auto-quarantine on repeated failures"
    echo "     batch_rollback.ex - Rollback entire dispatch batches"
    echo ""
    echo "Try: just build-all    (Rust + Elixir)"
    echo "     just test         (Rust tests)"
    echo "     just test-elixir  (Elixir tests)"
    echo "     just scan <repo>  (scan a repository)"

# Show available recipes with descriptions
help-me:
    #!/usr/bin/env bash
    echo "=== Hypatia: Help ==="
    echo ""
    echo "Onboarding:"
    echo "  just doctor         - Check required tools are installed"
    echo "  just heal           - Show install instructions for missing tools"
    echo "  just tour           - Guided walkthrough of repo structure"
    echo "  just help-me        - This help message"
    echo ""
    echo "Build:"
    echo "  just build          - Build Rust workspace (release)"
    echo "  just build-all      - Build Rust + Elixir"
    echo "  just compile-abi    - Compile Idris2 ABI definitions"
    echo "  just build-ffi      - Build Zig FFI bridge"
    echo ""
    echo "Testing:"
    echo "  just test           - Run Rust workspace tests"
    echo "  just test-elixir    - Run Elixir tests"
    echo ""
    echo "Quality:"
    echo "  just fmt            - Format Rust + Elixir code"
    echo "  just fmt-check      - Check formatting"
    echo "  just lint           - Run clippy lints"
    echo "  just license-check  - Check for banned license headers"
    echo "  just check-scm      - Validate SCM file locations"
    echo "  just assail         - panic-attacker pre-commit scan"
    echo "  just panic-scan     - panic-attack verbose analysis"
    echo ""
    echo "Running:"
    echo "  just cli <args>     - Run hypatia CLI"
    echo "  just scan <path>    - Scan a repo for weak points"
    echo ""
    echo "Housekeeping:"
    echo "  just clean          - Remove build artefacts"


# Print the current CRG grade (reads from READINESS.md '**Current Grade:** X' line)
crg-grade:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    echo "$$grade"

# Generate a shields.io badge markdown for the current CRG grade
# Looks for '**Current Grade:** X' in READINESS.md; falls back to X
crg-badge:
    @grade=$$(grep -oP '(?<=\*\*Current Grade:\*\* )[A-FX]' READINESS.md 2>/dev/null | head -1); \
    [ -z "$$grade" ] && grade="X"; \
    case "$$grade" in \
      A) color="brightgreen" ;; B) color="green" ;; C) color="yellow" ;; \
      D) color="orange" ;; E) color="red" ;; F) color="critical" ;; \
      *) color="lightgrey" ;; esac; \
    echo "[![CRG $$grade](https://img.shields.io/badge/CRG-$$grade-$$color?style=flat-square)](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)"
