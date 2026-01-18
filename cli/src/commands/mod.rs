// SPDX-License-Identifier: PLMP-1.0-or-later
//! Command implementations for the cicd-hyper-a CLI.
//!
//! Each submodule implements a specific CLI command with its own
//! arguments, validation, and execution logic.

#![allow(dead_code)]

pub mod batch;
pub mod deposit;
pub mod fleet;
pub mod hooks;
pub mod scan;
pub mod search;
pub mod withdraw;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use git2::Repository;

/// Validates that a path exists and is a directory
pub fn validate_directory_path(path: &Path) -> Result<PathBuf> {
    let canonical_path = path
        .canonicalize()
        .with_context(|| format!("Path does not exist: {}", path.display()))?;

    if !canonical_path.is_dir() {
        anyhow::bail!("Path is not a directory: {}", canonical_path.display());
    }

    Ok(canonical_path)
}

/// Validates that a path is a git repository
pub fn validate_git_repository(path: &Path) -> Result<Repository> {
    let repo_path = validate_directory_path(path)?;
    Repository::open(&repo_path)
        .with_context(|| format!("Not a git repository: {}", repo_path.display()))
}

/// Validates that a file exists and is readable
pub fn validate_file_path(path: &Path) -> Result<PathBuf> {
    let canonical_path = path
        .canonicalize()
        .with_context(|| format!("File does not exist: {}", path.display()))?;

    if !canonical_path.is_file() {
        anyhow::bail!("Path is not a file: {}", canonical_path.display());
    }

    Ok(canonical_path)
}

/// Common finding severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    /// Informational finding
    Info,
    /// Low severity - minor issue
    Low,
    /// Medium severity - should be addressed
    Medium,
    /// High severity - important issue
    High,
    /// Critical severity - must be addressed immediately
    Critical,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Info => write!(f, "info"),
            Severity::Low => write!(f, "low"),
            Severity::Medium => write!(f, "medium"),
            Severity::High => write!(f, "high"),
            Severity::Critical => write!(f, "critical"),
        }
    }
}

impl Severity {
    /// Returns the ANSI color for this severity level
    pub fn color(&self) -> colored::Color {
        use colored::Color;
        match self {
            Severity::Info => Color::Blue,
            Severity::Low => Color::Cyan,
            Severity::Medium => Color::Yellow,
            Severity::High => Color::Red,
            Severity::Critical => Color::Magenta,
        }
    }
}

/// Common finding category
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Category {
    /// Security-related issue
    Security,
    /// Policy violation
    Policy,
    /// Code quality issue
    Quality,
    /// Performance concern
    Performance,
    /// Maintainability issue
    Maintainability,
    /// Documentation issue
    Documentation,
    /// Configuration issue
    Configuration,
    /// Custom category
    #[serde(untagged)]
    Custom(String),
}

impl std::fmt::Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Category::Security => write!(f, "security"),
            Category::Policy => write!(f, "policy"),
            Category::Quality => write!(f, "quality"),
            Category::Performance => write!(f, "performance"),
            Category::Maintainability => write!(f, "maintainability"),
            Category::Documentation => write!(f, "documentation"),
            Category::Configuration => write!(f, "configuration"),
            Category::Custom(s) => write!(f, "{}", s),
        }
    }
}
