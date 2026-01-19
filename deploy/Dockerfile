# SPDX-License-Identifier: PLMP-1.0-or-later
# Dockerfile for Hypatia CLI (hyper)
# Multi-stage build for minimal production image

# ============================================================
# Stage 1: Build Environment
# ============================================================
FROM rust:1.85-bookworm AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Copy workspace manifest and lock file
COPY Cargo.toml Cargo.lock ./

# Copy all workspace member Cargo.toml files
COPY cli/Cargo.toml ./cli/
COPY adapters/Cargo.toml ./adapters/
COPY data/Cargo.toml ./data/
COPY fixer/Cargo.toml ./fixer/
COPY integration/Cargo.toml ./integration/

# Create dummy source files for dependency compilation
RUN mkdir -p cli/src adapters/src data/src fixer/src integration/src && \
    echo "fn main() {}" > cli/src/main.rs && \
    echo "pub fn dummy() {}" > adapters/src/lib.rs && \
    echo "fn main() {}" > adapters/src/main.rs && \
    echo "pub fn dummy() {}" > data/src/lib.rs && \
    echo "pub fn dummy() {}" > fixer/src/lib.rs && \
    echo "pub fn dummy() {}" > integration/src/lib.rs

# Build dependencies only (this layer will be cached)
RUN cargo build --release --package hypatia-cli || true
RUN rm -rf cli/src adapters/src data/src fixer/src integration/src

# Copy actual source code
COPY cli/src ./cli/src
COPY adapters/src ./adapters/src
COPY data/src ./data/src
COPY fixer/src ./fixer/src
COPY integration/src ./integration/src

# Build the CLI
RUN cargo build --release --package hypatia-cli

# ============================================================
# Stage 2: Runtime Environment
# ============================================================
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --shell /bin/bash cicd

WORKDIR /app

# Copy built binary
COPY --from=builder /build/target/release/hyper /usr/local/bin/hyper

# Copy schemas and hooks if they exist
COPY schemas /app/schemas
COPY hooks /app/hooks

# Set ownership
RUN chown -R cicd:cicd /app

# Switch to non-root user
USER cicd

# Environment variables
ENV RUST_LOG=info \
    RUST_BACKTRACE=0

# Run the application
ENTRYPOINT ["hyper"]
CMD ["--help"]
