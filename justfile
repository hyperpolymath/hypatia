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

# Run clippy lints
lint:
    cargo clippy -- -D warnings

# Run hypatia CLI
cli *ARGS:
    cargo run --bin hyper -- {{ARGS}}

# Run neurosymbolic scan on a target
scan target *ARGS:
    cargo run --bin hyper -- scan "{{target}}" {{ARGS}}

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
    @if [ -x "/var/mnt/eclipse/repos/panic-attacker/target/release/panic-attack" ]; then \
        /var/mnt/eclipse/repos/panic-attacker/target/release/panic-attack assail . --verbose; \
    else \
        echo "panic-attack not built — run 'cd /var/mnt/eclipse/repos/panic-attacker && cargo build --release'"; \
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
