# SPDX-License-Identifier: PLMP-1.0-or-later
# Dockerfile for cicd-hyper-a API Service
# Multi-stage build for minimal production image

# ============================================================
# Stage 1: Build Environment
# ============================================================
FROM rust:1.75-bookworm AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Create a new empty shell project for dependency caching
RUN USER=root cargo new --bin cicd-hyper-a
WORKDIR /build/cicd-hyper-a

# Copy manifests for dependency caching
COPY Cargo.toml Cargo.lock ./
COPY adapters/Cargo.toml ./adapters/
COPY data/Cargo.toml ./data/
COPY fixer/Cargo.toml ./fixer/

# Create dummy source files for dependency compilation
RUN mkdir -p adapters/src data/src fixer/src && \
    echo "fn main() {}" > src/main.rs && \
    echo "pub fn dummy() {}" > adapters/src/lib.rs && \
    echo "pub fn dummy() {}" > data/src/lib.rs && \
    echo "pub fn dummy() {}" > fixer/src/lib.rs

# Build dependencies only (this layer will be cached)
RUN cargo build --release && \
    rm -rf src adapters/src data/src fixer/src

# Copy actual source code
COPY adapters/src ./adapters/src
COPY data/src ./data/src
COPY fixer/src ./fixer/src
COPY src ./src

# Touch source files to ensure rebuild
RUN touch adapters/src/lib.rs data/src/lib.rs fixer/src/lib.rs

# Build the application
RUN cargo build --release --locked

# ============================================================
# Stage 2: Runtime Environment
# ============================================================
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    libpq5 \
    curl \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --shell /bin/bash cicd

WORKDIR /app

# Copy built binary
COPY --from=builder /build/cicd-hyper-a/target/release/cicd-hyper-a /usr/local/bin/cicd-hyper-a

# Copy schemas and configuration
COPY schemas /app/schemas
COPY hooks /app/hooks

# Set ownership
RUN chown -R cicd:cicd /app

# Switch to non-root user
USER cicd

# Environment variables
ENV RUST_LOG=info \
    RUST_BACKTRACE=0 \
    PORT=8000

# Expose API port
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:${PORT}/health || exit 1

# Run the application
ENTRYPOINT ["cicd-hyper-a"]
CMD ["serve", "--port", "8000"]
