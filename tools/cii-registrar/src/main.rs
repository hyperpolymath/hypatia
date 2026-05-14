// SPDX-License-Identifier: PMPL-1.0-or-later
//! CII Registrar - CLI for OpenSSF Best Practices Badge Registration
//!
//! Registers GitHub repositories with the bestpractices.dev platform
//! using their REST API for batch badge registration.
//!
//! # Environment Variables
//!
//! * `BESTPRACTICES_API_KEY` — API key from bestpractices.coreinfrastructure.org
//!   Required for `register` and `batch` commands.
//! * `GITHUB_TOKEN` — GitHub personal access token (for `list` command).

#![forbid(unsafe_code)]
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use serde::Deserialize;
use std::io::BufRead;

/// Base URL for the OpenSSF Best Practices API
const API_BASE: &str = "https://bestpractices.coreinfrastructure.org";

/// OpenSSF Best Practices Badge Registration CLI
#[derive(Parser)]
#[command(name = "cii-registrar")]
#[command(about = "Register repos with OpenSSF Best Practices Badge", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Register a new project
    Register {
        /// GitHub repository URL
        #[arg(short, long)]
        repo: String,
    },
    /// Check badge status for a project
    Status {
        /// Project ID on bestpractices.dev
        #[arg(short, long)]
        id: u64,
    },
    /// Batch register multiple repos from a file
    Batch {
        /// File containing repo URLs (one per line)
        #[arg(short, long)]
        file: String,
    },
    /// List ready repos from hyperpolymath org
    List {
        /// Only show repos with all prerequisites
        #[arg(short, long)]
        ready: bool,
    },
}

/// Response from the bestpractices.dev API for a badge project
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct BadgeProject {
    id: u64,
    name: Option<String>,
    repo_url: Option<String>,
    badge_level: Option<String>,
    tiered_percentage: Option<u32>,
    badge_percentage_0: Option<u32>,
}

/// Response from the bestpractices.dev API for project creation
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct CreateResponse {
    id: u64,
    repo_url: Option<String>,
}

/// GitHub repository listing entry (minimal fields)
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct GhRepo {
    full_name: String,
    html_url: String,
    has_issues: bool,
    license: Option<GhLicense>,
    description: Option<String>,
}

/// GitHub license info
#[derive(Debug, Deserialize)]
struct GhLicense {
    spdx_id: Option<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Register { repo } => register_project(repo),
        Commands::Status { id } => check_status(*id),
        Commands::Batch { file } => batch_register(file),
        Commands::List { ready } => list_repos(*ready),
    }
}

/// Register a single project with bestpractices.dev
fn register_project(repo_url: &str) -> Result<()> {
    let api_key = std::env::var("BESTPRACTICES_API_KEY")
        .context("BESTPRACTICES_API_KEY env var required for registration")?;

    let client = reqwest::blocking::Client::new();
    let url = format!("{}/en/projects.json", API_BASE);

    println!("Registering: {}", repo_url);

    let response = client
        .post(&url)
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&serde_json::json!({
            "project": {
                "repo_url": repo_url
            }
        }))
        .send()
        .context("Failed to send registration request")?;

    if response.status().is_success() {
        let created: CreateResponse = response
            .json()
            .context("Failed to parse registration response")?;
        println!("Registered successfully — project ID: {}", created.id);
        println!("Badge URL: {}/en/projects/{}/badge", API_BASE, created.id);
        println!("Edit URL: {}/en/projects/{}/edit", API_BASE, created.id);
    } else {
        let status = response.status();
        let body = response.text().unwrap_or_default();
        anyhow::bail!("Registration failed ({}): {}", status, body);
    }

    Ok(())
}

/// Check badge status for a project by ID
fn check_status(project_id: u64) -> Result<()> {
    let client = reqwest::blocking::Client::new();
    let url = format!("{}/en/projects/{}.json", API_BASE, project_id);

    println!("Checking status for project ID: {}", project_id);

    let response = client
        .get(&url)
        .send()
        .context("Failed to fetch project status")?;

    if response.status().is_success() {
        let project: BadgeProject = response
            .json()
            .context("Failed to parse project response")?;

        println!(
            "Name:       {}",
            project.name.as_deref().unwrap_or("(unnamed)")
        );
        println!(
            "Repo:       {}",
            project.repo_url.as_deref().unwrap_or("(none)")
        );
        println!(
            "Level:      {}",
            project.badge_level.as_deref().unwrap_or("in_progress")
        );
        println!("Progress:   {}%", project.badge_percentage_0.unwrap_or_default());
        println!("Tiered:     {}%", project.tiered_percentage.unwrap_or_default());
        println!("Badge:      {}/en/projects/{}/badge", API_BASE, project_id);
    } else if response.status().as_u16() == 404 {
        println!("Project {} not found on bestpractices.dev", project_id);
    } else {
        let status = response.status();
        let body = response.text().unwrap_or_default();
        anyhow::bail!("Status check failed ({}): {}", status, body);
    }

    Ok(())
}

/// Batch register multiple repos from a file (one URL per line)
fn batch_register(file_path: &str) -> Result<()> {
    let file = std::fs::File::open(file_path)
        .with_context(|| format!("Failed to open batch file: {}", file_path))?;

    let reader = std::io::BufReader::new(file);
    let urls: Vec<String> = reader
        .lines()
        .map_while(Result::ok)
        .map(|line| line.trim().to_string())
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .collect();

    println!("Batch registering {} repositories...", urls.len());

    let mut successes = 0;
    let mut failures = 0;

    for url in &urls {
        match register_project(url) {
            Ok(()) => successes += 1,
            Err(e) => {
                eprintln!("  FAILED: {} — {}", url, e);
                failures += 1;
            }
        }
        println!();
    }

    println!(
        "Batch complete: {} succeeded, {} failed",
        successes, failures
    );

    Ok(())
}

/// List repos from the hyperpolymath GitHub org, optionally filtering for readiness
fn list_repos(ready_only: bool) -> Result<()> {
    let token =
        std::env::var("GITHUB_TOKEN").context("GITHUB_TOKEN env var required for listing repos")?;

    let client = reqwest::blocking::Client::builder()
        .user_agent("cii-registrar/0.1.0")
        .build()?;

    let url =
        "https://api.github.com/orgs/hyperpolymath/repos?per_page=100&sort=updated&type=public";

    let response = client
        .get(url)
        .header("Authorization", format!("Bearer {}", token))
        .header("Accept", "application/vnd.github+json")
        .send()
        .context("Failed to fetch repos from GitHub")?;

    if !response.status().is_success() {
        let status = response.status();
        let body = response.text().unwrap_or_default();
        anyhow::bail!("GitHub API error ({}): {}", status, body);
    }

    let repos: Vec<GhRepo> = response
        .json()
        .context("Failed to parse GitHub repos response")?;

    println!("Found {} public repos for hyperpolymath", repos.len());
    println!();

    for repo in &repos {
        let has_license = repo
            .license
            .as_ref()
            .and_then(|l| l.spdx_id.as_deref())
            .map(|id| id != "NOASSERTION")
            .unwrap_or(false);

        let is_ready = repo.has_issues && has_license;

        if ready_only && !is_ready {
            continue;
        }

        let status_icon = if is_ready { "+" } else { "-" };
        let license_str = repo
            .license
            .as_ref()
            .and_then(|l| l.spdx_id.as_deref())
            .unwrap_or("none");

        println!(
            "[{}] {} (license: {}, issues: {})",
            status_icon,
            repo.full_name,
            license_str,
            if repo.has_issues { "yes" } else { "no" }
        );

        if let Some(desc) = &repo.description {
            println!("    {}", desc);
        }
    }

    Ok(())
}
