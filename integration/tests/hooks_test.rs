// SPDX-License-Identifier: PMPL-1.0-or-later
//! Git Hooks Integration Tests
//!
//! Tests git hooks execution and validation:
//! - Pre-commit hook validation
//! - Pre-push security checks
//! - Post-receive webhook triggers
//! - Hook installation and removal
//! - Hook bypass handling

use anyhow::{Context, Result};
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;
use tracing::{debug, info, warn};

mod common;
use common::setup_test_logging;

// ============================================================================
// Test Constants
// ============================================================================

/// Path to hooks directory in the project
const HOOKS_SOURCE_DIR: &str = "hooks";

/// Standard git hooks we test
const HOOK_NAMES: &[&str] = &["pre-commit", "pre-push", "post-receive"];

/// Validation scripts
const VALIDATION_SCRIPTS: &[&str] = &[
    "validate-codeql.sh",
    "validate-permissions.sh",
    "validate-sha-pins.sh",
    "validate-spdx.sh",
];

// ============================================================================
// Test Fixtures
// ============================================================================

/// Test repository with hooks installed
struct HookTestRepo {
    temp_dir: TempDir,
    repo_path: PathBuf,
    hooks_path: PathBuf,
}

impl HookTestRepo {
    /// Create a new test repository with git initialized
    fn new() -> Result<Self> {
        let temp_dir = TempDir::new()?;
        let repo_path = temp_dir.path().to_path_buf();

        // Initialize git repository
        Command::new("git")
            .args(["init"])
            .current_dir(&repo_path)
            .output()
            .context("Failed to init git repo")?;

        // Configure git user
        Command::new("git")
            .args(["config", "user.email", "test@hyperpolymath.dev"])
            .current_dir(&repo_path)
            .output()?;
        Command::new("git")
            .args(["config", "user.name", "Test User"])
            .current_dir(&repo_path)
            .output()?;

        let hooks_path = repo_path.join(".git/hooks");

        Ok(Self {
            temp_dir,
            repo_path,
            hooks_path,
        })
    }

    /// Install a hook from source
    fn install_hook(&self, hook_name: &str, source_path: &Path) -> Result<()> {
        let dest = self.hooks_path.join(hook_name);
        fs::copy(source_path, &dest)?;

        // Make executable
        let mut perms = fs::metadata(&dest)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&dest, perms)?;

        Ok(())
    }

    /// Install a hook with custom content
    fn install_hook_content(&self, hook_name: &str, content: &str) -> Result<()> {
        let dest = self.hooks_path.join(hook_name);
        fs::write(&dest, content)?;

        let mut perms = fs::metadata(&dest)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&dest, perms)?;

        Ok(())
    }

    /// Create a test file in the repository
    fn create_file(&self, name: &str, content: &str) -> Result<()> {
        let path = self.repo_path.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&path, content)?;
        Ok(())
    }

    /// Stage a file
    fn stage_file(&self, name: &str) -> Result<()> {
        Command::new("git")
            .args(["add", name])
            .current_dir(&self.repo_path)
            .output()?;
        Ok(())
    }

    /// Attempt to commit (may fail due to hooks)
    fn try_commit(&self, message: &str) -> Result<(i32, String, String)> {
        let output = Command::new("git")
            .args(["commit", "-m", message])
            .current_dir(&self.repo_path)
            .output()?;

        Ok((
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stdout).to_string(),
            String::from_utf8_lossy(&output.stderr).to_string(),
        ))
    }

    /// Commit with hook bypass
    fn commit_no_verify(&self, message: &str) -> Result<(i32, String, String)> {
        let output = Command::new("git")
            .args(["commit", "--no-verify", "-m", message])
            .current_dir(&self.repo_path)
            .output()?;

        Ok((
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stdout).to_string(),
            String::from_utf8_lossy(&output.stderr).to_string(),
        ))
    }

    /// Get hook status
    fn hook_exists(&self, hook_name: &str) -> bool {
        self.hooks_path.join(hook_name).exists()
    }

    /// Check if hook is executable
    fn hook_is_executable(&self, hook_name: &str) -> Result<bool> {
        let path = self.hooks_path.join(hook_name);
        if !path.exists() {
            return Ok(false);
        }
        let metadata = fs::metadata(&path)?;
        Ok(metadata.permissions().mode() & 0o111 != 0)
    }
}

// ============================================================================
// Hook Content Generators
// ============================================================================

/// Generate a pre-commit hook that validates SPDX headers
fn spdx_validation_hook() -> String {
    r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Pre-commit hook to validate SPDX headers

set -e

echo "Checking SPDX headers..."

# Get staged files
files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(rs|hs|sh|yml|yaml)$' || true)

if [ -z "$files" ]; then
    echo "No relevant files to check"
    exit 0
fi

errors=0
for file in $files; do
    if ! head -5 "$file" | grep -q "SPDX-License-Identifier"; then
        echo "ERROR: Missing SPDX header in $file"
        errors=$((errors + 1))
    fi
done

if [ $errors -gt 0 ]; then
    echo "Found $errors files without SPDX headers"
    exit 1
fi

echo "All files have SPDX headers"
exit 0
"#
    .to_string()
}

/// Generate a pre-commit hook that validates SHA pins in workflows
fn sha_pin_validation_hook() -> String {
    r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Pre-commit hook to validate SHA pins in workflows

set -e

echo "Checking SHA pins in workflows..."

files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.github/workflows/.*\.ya?ml$' || true)

if [ -z "$files" ]; then
    echo "No workflow files to check"
    exit 0
fi

errors=0
for file in $files; do
    # Check for unpinned actions (uses: action@v1 or @main instead of @sha)
    if grep -E 'uses:\s+[^@]+@(v[0-9]+|main|master)(\s|$)' "$file" > /dev/null 2>&1; then
        echo "ERROR: Unpinned action in $file"
        grep -n -E 'uses:\s+[^@]+@(v[0-9]+|main|master)(\s|$)' "$file" || true
        errors=$((errors + 1))
    fi
done

if [ $errors -gt 0 ]; then
    echo "Found workflows with unpinned actions"
    exit 1
fi

echo "All actions are properly pinned"
exit 0
"#
    .to_string()
}

/// Generate a pre-commit hook that validates permissions in workflows
fn permissions_validation_hook() -> String {
    r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Pre-commit hook to validate permissions in workflows

set -e

echo "Checking permissions in workflows..."

files=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.github/workflows/.*\.ya?ml$' || true)

if [ -z "$files" ]; then
    echo "No workflow files to check"
    exit 0
fi

errors=0
for file in $files; do
    if ! grep -q '^permissions:' "$file"; then
        echo "ERROR: Missing permissions declaration in $file"
        errors=$((errors + 1))
    fi
done

if [ $errors -gt 0 ]; then
    echo "Found workflows without permissions"
    exit 1
fi

echo "All workflows have permissions declared"
exit 0
"#
    .to_string()
}

/// Generate a simple passing hook
fn passing_hook() -> String {
    r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
echo "Hook executed successfully"
exit 0
"#
    .to_string()
}

/// Generate a simple failing hook
fn failing_hook() -> String {
    r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
echo "Hook failed intentionally"
exit 1
"#
    .to_string()
}

// ============================================================================
// Test Cases
// ============================================================================

#[tokio::test]
async fn test_hook_installation() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;

    // Verify no hooks initially
    assert!(!repo.hook_exists("pre-commit"));

    // Install hook
    repo.install_hook_content("pre-commit", &passing_hook())?;

    // Verify hook exists and is executable
    assert!(repo.hook_exists("pre-commit"));
    assert!(repo.hook_is_executable("pre-commit")?);

    info!("Hook installation test passed");
    Ok(())
}

#[tokio::test]
async fn test_precommit_hook_success() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;
    repo.install_hook_content("pre-commit", &passing_hook())?;

    // Create and stage a file
    repo.create_file("test.txt", "Hello, World!")?;
    repo.stage_file("test.txt")?;

    // Commit should succeed
    let (code, stdout, stderr) = repo.try_commit("Test commit")?;
    debug!("stdout: {}", stdout);
    debug!("stderr: {}", stderr);
    assert_eq!(code, 0, "Commit should succeed with passing hook");

    info!("Pre-commit hook success test passed");
    Ok(())
}

#[tokio::test]
async fn test_precommit_hook_failure() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;
    repo.install_hook_content("pre-commit", &failing_hook())?;

    // Create and stage a file
    repo.create_file("test.txt", "Hello, World!")?;
    repo.stage_file("test.txt")?;

    // Commit should fail
    let (code, stdout, stderr) = repo.try_commit("Test commit")?;
    debug!("stdout: {}", stdout);
    debug!("stderr: {}", stderr);
    assert_ne!(code, 0, "Commit should fail with failing hook");

    info!("Pre-commit hook failure test passed");
    Ok(())
}

#[tokio::test]
async fn test_hook_bypass_with_no_verify() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;
    repo.install_hook_content("pre-commit", &failing_hook())?;

    // Create and stage a file
    repo.create_file("test.txt", "Hello, World!")?;
    repo.stage_file("test.txt")?;

    // Commit with --no-verify should succeed
    let (code, stdout, stderr) = repo.commit_no_verify("Test commit")?;
    debug!("stdout: {}", stdout);
    debug!("stderr: {}", stderr);
    assert_eq!(code, 0, "Commit with --no-verify should bypass hook");

    info!("Hook bypass test passed");
    Ok(())
}

#[tokio::test]
async fn test_spdx_validation_hook() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;
    repo.install_hook_content("pre-commit", &spdx_validation_hook())?;

    // Create file without SPDX header
    repo.create_file("bad.rs", "fn main() {}")?;
    repo.stage_file("bad.rs")?;

    // Commit should fail
    let (code, _, stderr) = repo.try_commit("Bad commit")?;
    debug!("stderr: {}", stderr);
    assert_ne!(code, 0, "Commit should fail without SPDX header");

    // Create file with SPDX header
    repo.create_file(
        "good.rs",
        "// SPDX-License-Identifier: PMPL-1.0-or-later\nfn main() {}",
    )?;
    repo.stage_file("good.rs")?;

    // Reset bad.rs
    Command::new("git")
        .args(["reset", "HEAD", "bad.rs"])
        .current_dir(&repo.repo_path)
        .output()?;

    // Commit should succeed
    let (code, _, _) = repo.try_commit("Good commit")?;
    assert_eq!(code, 0, "Commit should succeed with SPDX header");

    info!("SPDX validation hook test passed");
    Ok(())
}

#[tokio::test]
async fn test_sha_pin_validation_hook() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;
    repo.install_hook_content("pre-commit", &sha_pin_validation_hook())?;

    // Create workflow without SHA pins
    let bad_workflow = r#"
name: CI
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
"#;
    repo.create_file(".github/workflows/ci.yml", bad_workflow)?;
    repo.stage_file(".github/workflows/ci.yml")?;

    // Commit should fail
    let (code, _, stderr) = repo.try_commit("Bad workflow")?;
    debug!("stderr: {}", stderr);
    assert_ne!(code, 0, "Commit should fail with unpinned action");

    // Create workflow with SHA pins
    let good_workflow = r#"# SPDX-License-Identifier: PMPL-1.0-or-later
name: CI
on: [push]
permissions: read-all
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4
"#;
    repo.create_file(".github/workflows/ci.yml", good_workflow)?;
    repo.stage_file(".github/workflows/ci.yml")?;

    // Commit should succeed
    let (code, _, _) = repo.try_commit("Good workflow")?;
    assert_eq!(code, 0, "Commit should succeed with pinned action");

    info!("SHA pin validation hook test passed");
    Ok(())
}

#[tokio::test]
async fn test_permissions_validation_hook() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;
    repo.install_hook_content("pre-commit", &permissions_validation_hook())?;

    // Create workflow without permissions
    let bad_workflow = r#"name: CI
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11
"#;
    repo.create_file(".github/workflows/ci.yml", bad_workflow)?;
    repo.stage_file(".github/workflows/ci.yml")?;

    // Commit should fail
    let (code, _, stderr) = repo.try_commit("Bad workflow")?;
    debug!("stderr: {}", stderr);
    assert_ne!(code, 0, "Commit should fail without permissions");

    // Create workflow with permissions
    let good_workflow = r#"name: CI
on: [push]
permissions: read-all
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11
"#;
    repo.create_file(".github/workflows/ci.yml", good_workflow)?;
    repo.stage_file(".github/workflows/ci.yml")?;

    // Commit should succeed
    let (code, _, _) = repo.try_commit("Good workflow")?;
    assert_eq!(code, 0, "Commit should succeed with permissions");

    info!("Permissions validation hook test passed");
    Ok(())
}

#[tokio::test]
async fn test_multiple_hooks() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;

    // Install multiple hooks
    repo.install_hook_content("pre-commit", &passing_hook())?;
    repo.install_hook_content(
        "post-commit",
        &r#"#!/bin/bash
echo "Post-commit hook executed"
exit 0
"#
        .to_string(),
    )?;

    repo.create_file("test.txt", "content")?;
    repo.stage_file("test.txt")?;

    let (code, stdout, _) = repo.try_commit("Multi-hook test")?;
    assert_eq!(code, 0);
    debug!("stdout: {}", stdout);

    info!("Multiple hooks test passed");
    Ok(())
}

#[tokio::test]
async fn test_hook_environment_variables() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;

    // Hook that checks environment variables
    let env_hook = r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later

# Check that GIT_DIR is set
if [ -z "$GIT_DIR" ]; then
    echo "ERROR: GIT_DIR not set"
    exit 1
fi

# Check working directory
if [ ! -d ".git" ] && [ -z "$GIT_WORK_TREE" ]; then
    echo "ERROR: Not in git repository"
    exit 1
fi

echo "Environment check passed"
exit 0
"#;

    repo.install_hook_content("pre-commit", env_hook)?;

    repo.create_file("test.txt", "content")?;
    repo.stage_file("test.txt")?;

    let (code, stdout, stderr) = repo.try_commit("Env test")?;
    debug!("stdout: {}", stdout);
    debug!("stderr: {}", stderr);
    assert_eq!(code, 0, "Hook should have correct environment");

    info!("Hook environment variables test passed");
    Ok(())
}

#[tokio::test]
async fn test_hook_with_staged_changes_only() -> Result<()> {
    setup_test_logging();

    let repo = HookTestRepo::new()?;

    // Hook that only processes staged changes
    let staged_hook = r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later

staged=$(git diff --cached --name-only)
echo "Staged files: $staged"

if [ -z "$staged" ]; then
    echo "No staged changes"
    exit 0
fi

for file in $staged; do
    echo "Processing: $file"
done

exit 0
"#;

    repo.install_hook_content("pre-commit", staged_hook)?;

    // Create two files, only stage one
    repo.create_file("staged.txt", "staged content")?;
    repo.create_file("unstaged.txt", "unstaged content")?;
    repo.stage_file("staged.txt")?;

    let (code, stdout, _) = repo.try_commit("Partial staging test")?;
    assert_eq!(code, 0);
    assert!(stdout.contains("staged.txt"));
    assert!(!stdout.contains("unstaged.txt"));

    info!("Hook with staged changes only test passed");
    Ok(())
}

// ============================================================================
// Main Test Runner
// ============================================================================

fn main() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    println!("Running Git Hooks Integration Tests\n");
    println!("====================================\n");

    let tests: Vec<(&str, fn() -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>)> = vec![
        ("test_hook_installation", || Box::pin(test_hook_installation())),
        ("test_precommit_hook_success", || Box::pin(test_precommit_hook_success())),
        ("test_precommit_hook_failure", || Box::pin(test_precommit_hook_failure())),
        ("test_hook_bypass_with_no_verify", || Box::pin(test_hook_bypass_with_no_verify())),
        ("test_spdx_validation_hook", || Box::pin(test_spdx_validation_hook())),
        ("test_sha_pin_validation_hook", || Box::pin(test_sha_pin_validation_hook())),
        ("test_permissions_validation_hook", || Box::pin(test_permissions_validation_hook())),
        ("test_multiple_hooks", || Box::pin(test_multiple_hooks())),
        ("test_hook_environment_variables", || Box::pin(test_hook_environment_variables())),
        ("test_hook_with_staged_changes_only", || Box::pin(test_hook_with_staged_changes_only())),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, test_fn) in tests {
        print!("  {} ... ", name);
        match rt.block_on(test_fn()) {
            Ok(_) => {
                println!("ok");
                passed += 1;
            }
            Err(e) => {
                println!("FAILED");
                eprintln!("    Error: {}", e);
                failed += 1;
            }
        }
    }

    println!("\n====================================");
    println!("Results: {} passed, {} failed", passed, failed);

    if failed > 0 {
        std::process::exit(1);
    }
}

// ============================================================================
// Common Test Utilities
// ============================================================================

mod common {
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};

    pub fn setup_test_logging() {
        let _ = tracing_subscriber::registry()
            .with(fmt::layer().with_test_writer())
            .with(EnvFilter::from_default_env().add_directive("info".parse().unwrap()))
            .try_init();
    }
}
