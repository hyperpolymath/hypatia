# Integration Tests

This directory contains integration tests for the cicd-hyper-a platform.

## Overview

The integration test framework validates:
- **Fleet Operations**: Bot orchestration and inter-bot communication
- **ArangoDB**: Graph database connectivity, queries, and data operations
- **Registry**: Haskell ruleset registry operations and validation
- **Git Hooks**: Pre-commit, pre-push, and post-receive hook execution
- **Forge Adapters**: GitHub, GitLab, and Bitbucket API integrations

## Directory Structure

```
integration/
├── Cargo.toml                    # Test crate configuration
├── docker-compose.test.yml       # Test environment containers
├── run-tests.sh                  # Test runner script
├── README.md                     # This file
├── tests/
│   ├── fleet_test.rs            # Bot fleet orchestration tests
│   ├── arangodb_test.rs         # ArangoDB connectivity tests
│   ├── registry_test.rs         # Haskell registry tests
│   ├── hooks_test.rs            # Git hooks execution tests
│   └── forge_test.rs            # Forge adapter tests (wiremock)
└── fixtures/
    ├── test-repo/               # Sample repository for testing
    │   ├── .github/workflows/   # Test workflow files
    │   ├── src/                 # Test source files
    │   └── index.html           # Test HTML with accessibility issues
    ├── sample-ruleset.json      # Sample ruleset for registry tests
    └── mock-responses/          # Mock API responses for forges
        ├── github-repo.json
        ├── github-workflows.json
        ├── github-alerts.json
        ├── gitlab-project.json
        └── bitbucket-repo.json
```

## Prerequisites

- Rust 1.75+ with cargo
- Docker and Docker Compose
- Git
- (Optional) redis-cli for health checks

## Quick Start

```bash
# Run all integration tests
./run-tests.sh

# Run with verbose output
./run-tests.sh --verbose

# Run specific test file
./run-tests.sh --test fleet_test

# Run without slow tests
./run-tests.sh --quick

# Generate coverage report
./run-tests.sh --coverage

# Keep containers running after tests
./run-tests.sh --keep
```

## Test Categories

### Fleet Tests (`fleet_test.rs`)

Tests bot fleet operations:
- Sequential bot execution pipeline
- Shared context propagation between bots
- Alert aggregation from multiple bots
- Fix deduplication
- Error handling and recovery
- Metrics collection

### ArangoDB Tests (`arangodb_test.rs`)

Tests database operations:
- Connection pooling and health checks
- Repository CRUD operations
- Alert storage and retrieval
- Rule management
- AQL query execution
- Bulk insert performance
- Graph traversal queries

### Registry Tests (`registry_test.rs`)

Tests Haskell registry:
- Ruleset registration and validation
- LiquidHaskell verification (mock)
- Duplicate rule detection
- Rule search by category and severity
- Fix template application
- JSON serialization

### Hooks Tests (`hooks_test.rs`)

Tests git hooks:
- Hook installation and permissions
- Pre-commit hook success/failure
- Hook bypass with --no-verify
- SPDX header validation
- SHA pin validation
- Permissions validation
- Multiple hooks execution
- Environment variable handling

### Forge Tests (`forge_test.rs`)

Tests forge adapters:
- GitHub API mock server
- GitLab API mock server
- Bitbucket API mock server
- Authentication headers
- Rate limiting responses
- Webhook payload handling
- Error handling (404, 500)
- Pagination handling
- Concurrent requests

## Test Environment

The test environment uses Docker containers:

| Service | Port | Purpose |
|---------|------|---------|
| ArangoDB | 8529 | Graph database |
| Dragonfly | 6379 | Redis-compatible cache |
| Mock GitHub | 1080 | GitHub API mock |
| Mock GitLab | 1081 | GitLab API mock |
| Mock Bitbucket | 1082 | Bitbucket API mock |
| Webhook Receiver | 8088 | Webhook testing |

## Environment Variables

The test runner sets these automatically:

| Variable | Default | Description |
|----------|---------|-------------|
| `ARANGODB_URL` | http://localhost:8529 | ArangoDB connection URL |
| `ARANGODB_DATABASE` | cicd_hyper_a_test | Test database name |
| `ARANGODB_USERNAME` | root | Database username |
| `ARANGODB_PASSWORD` | testpassword | Database password |
| `DRAGONFLY_URL` | redis://localhost:6379 | Dragonfly/Redis URL |
| `RUST_LOG` | info | Logging level |
| `RUST_BACKTRACE` | 1 | Enable backtraces |

## Features

Enable additional test categories:

```bash
# Run with live service tests (requires real credentials)
./run-tests.sh --live

# Run with slow tests (extended timeouts, performance tests)
cargo test --features slow-tests
```

## Writing New Tests

### Test Structure

Each test file follows this pattern:

```rust
// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Result;
use tracing::info;

mod common;
use common::setup_test_logging;

#[tokio::test]
async fn test_example() -> Result<()> {
    setup_test_logging();

    // Test implementation
    info!("Test completed");
    Ok(())
}

fn main() {
    // Custom test runner for standalone execution
}
```

### Using Mock Servers

```rust
use wiremock::{Mock, MockServer, ResponseTemplate};
use wiremock::matchers::{method, path};

#[tokio::test]
async fn test_with_mock() -> Result<()> {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/repos/owner/repo"))
        .respond_with(ResponseTemplate::new(200)
            .set_body_json(serde_json::json!({"name": "repo"})))
        .mount(&mock_server)
        .await;

    // Use mock_server.uri() for requests
    Ok(())
}
```

### Using Testcontainers

```rust
use testcontainers::{GenericImage, runners::AsyncRunner};

#[tokio::test]
async fn test_with_container() -> Result<()> {
    let container = GenericImage::new("arangodb", "3.11")
        .with_exposed_port(8529)
        .with_env_var("ARANGO_ROOT_PASSWORD", "test")
        .start()
        .await?;

    let port = container.get_host_port_ipv4(8529).await?;
    // Use container
    Ok(())
}
```

## Troubleshooting

### Container Issues

```bash
# Check container logs
docker logs cicd-hyper-a-test-arangodb

# Restart containers
docker-compose -f docker-compose.test.yml restart

# Clean up everything
docker-compose -f docker-compose.test.yml down -v
```

### Database Connection

```bash
# Check ArangoDB
curl http://localhost:8529/_api/version

# Check Dragonfly
redis-cli -h localhost -p 6379 ping
```

### Test Failures

```bash
# Run with maximum verbosity
RUST_LOG=debug ./run-tests.sh --verbose

# Run single test with output
cargo test test_name -- --nocapture
```

## CI/CD Integration

For GitHub Actions:

```yaml
jobs:
  integration-tests:
    runs-on: ubuntu-latest
    services:
      arangodb:
        image: arangodb:3.11
        env:
          ARANGO_ROOT_PASSWORD: testpassword
        ports:
          - 8529:8529
      dragonfly:
        image: docker.dragonflydb.io/dragonflydb/dragonfly
        ports:
          - 6379:6379
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo test --manifest-path integration/Cargo.toml
```

## License

AGPL-3.0-or-later
