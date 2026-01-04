# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a build automation

# Default recipe: show help
default:
    @just --list

# Build all components
build: build-rust build-haskell

# Build Rust workspace
build-rust:
    cargo build --workspace

# Build Haskell registry (requires ghcup in PATH)
build-haskell:
    cd registry && ~/.ghcup/bin/cabal build

# Run all tests
test: test-rust

# Run Rust tests
test-rust:
    cargo test --workspace

# Lint/check all code
lint: lint-rust

# Lint Rust code
lint-rust:
    cargo clippy --workspace -- -D warnings

# Format all code
fmt: fmt-rust

# Format Rust code
fmt-rust:
    cargo fmt --all

# Check formatting
check-fmt:
    cargo fmt --all -- --check

# Clean build artifacts
clean:
    cargo clean
    rm -rf registry/dist-newstyle

# Release build
release:
    cargo build --workspace --release

# Run the forge adapter CLI
run-adapter *ARGS:
    cargo run --bin forge-adapter -- {{ARGS}}

# Development: watch and rebuild
dev:
    cargo watch -x 'check --workspace'

# Docker: build all containers
docker-build:
    podman compose -f deploy/docker-compose.yml build

# Docker: start services
docker-up:
    podman compose -f deploy/docker-compose.yml up -d

# Docker: stop services
docker-down:
    podman compose -f deploy/docker-compose.yml down

# Security: run security checks
security:
    cargo audit
    cargo deny check

# Documentation: build docs
docs:
    cargo doc --workspace --no-deps
