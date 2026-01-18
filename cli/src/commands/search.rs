// SPDX-License-Identifier: AGPL-3.0-or-later
//! Search command implementation.
//!
//! Search for rulesets in the registry by various criteria.

use std::collections::HashMap;

use anyhow::Result;
use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};

use super::deposit::RulesetCategory;
use crate::config::Config;
use crate::output::{OutputFormat, Outputter};

/// Arguments for the search command
#[derive(Args, Debug)]
pub struct SearchArgs {
    /// Free-text search query
    #[arg(default_value = "")]
    pub query: String,

    /// Filter by effect type (check, fix, both)
    #[arg(long)]
    pub effect: Option<EffectFilter>,

    /// Filter by target language
    #[arg(short, long)]
    pub language: Option<String>,

    /// Filter by category
    #[arg(short, long)]
    pub category: Option<RulesetCategory>,

    /// Filter by tag
    #[arg(short, long)]
    pub tag: Option<String>,

    /// Filter by author
    #[arg(short, long)]
    pub author: Option<String>,

    /// Include private rulesets (requires authentication)
    #[arg(long)]
    pub include_private: bool,

    /// Sort results by (name, downloads, updated, relevance)
    #[arg(long, default_value = "relevance")]
    pub sort: SortField,

    /// Sort order
    #[arg(long, default_value = "desc")]
    pub order: SortOrder,

    /// Maximum number of results
    #[arg(long, default_value = "20")]
    pub limit: usize,

    /// Offset for pagination
    #[arg(long, default_value = "0")]
    pub offset: usize,

    /// Registry URL to search
    #[arg(long)]
    pub registry: Option<String>,
}

/// Effect filter options
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum EffectFilter {
    Check,
    Fix,
    Both,
}

impl std::fmt::Display for EffectFilter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EffectFilter::Check => write!(f, "check"),
            EffectFilter::Fix => write!(f, "fix"),
            EffectFilter::Both => write!(f, "both"),
        }
    }
}

/// Sort field options
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum SortField {
    Name,
    Downloads,
    Updated,
    Relevance,
    Stars,
}

/// Sort order options
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum SortOrder {
    Asc,
    Desc,
}

/// A search result item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResultItem {
    /// Unique ruleset ID
    pub id: String,
    /// Ruleset name
    pub name: String,
    /// Latest version
    pub version: String,
    /// Description
    pub description: String,
    /// Category
    pub category: String,
    /// Author(s)
    pub authors: Vec<String>,
    /// Target languages
    pub languages: Vec<String>,
    /// Tags
    pub tags: Vec<String>,
    /// Number of rules in the ruleset
    pub rules_count: usize,
    /// Effect types available
    pub effects: Vec<String>,
    /// Download count
    pub downloads: u64,
    /// Star count
    pub stars: u64,
    /// Last updated
    pub updated_at: DateTime<Utc>,
    /// Whether this is private
    pub private: bool,
    /// License
    pub license: String,
}

/// Search results with pagination
#[derive(Debug, Serialize, Deserialize)]
pub struct SearchResults {
    /// Query that was searched
    pub query: String,
    /// Filters that were applied
    pub filters: HashMap<String, String>,
    /// Total number of matching results
    pub total_count: usize,
    /// Current offset
    pub offset: usize,
    /// Results limit
    pub limit: usize,
    /// Result items
    pub items: Vec<SearchResultItem>,
    /// Search took (milliseconds)
    pub took_ms: u64,
}

/// Execute the search command
pub async fn execute(args: SearchArgs, config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);
    let start_time = std::time::Instant::now();

    let registry_url = args
        .registry
        .clone()
        .or_else(|| config.registry_url.clone())
        .unwrap_or_else(|| "https://registry.cicd-hyper-a.dev".to_string());

    info!("Searching registry: {}", registry_url);

    // Build filters map for output
    let mut filters = HashMap::new();
    if !args.query.is_empty() {
        filters.insert("query".to_string(), args.query.clone());
    }
    if let Some(ref effect) = args.effect {
        filters.insert("effect".to_string(), effect.to_string());
    }
    if let Some(ref lang) = args.language {
        filters.insert("language".to_string(), lang.clone());
    }
    if let Some(ref cat) = args.category {
        filters.insert("category".to_string(), cat.to_string());
    }
    if let Some(ref tag) = args.tag {
        filters.insert("tag".to_string(), tag.clone());
    }
    if let Some(ref author) = args.author {
        filters.insert("author".to_string(), author.clone());
    }

    // Perform search
    let items = search_registry(&args, &registry_url).await?;

    let took_ms = start_time.elapsed().as_millis() as u64;

    let results = SearchResults {
        query: args.query.clone(),
        filters,
        total_count: items.len(), // In real implementation, this would be total from server
        offset: args.offset,
        limit: args.limit,
        items,
        took_ms,
    };

    // Output results
    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&results)?;
        }
        OutputFormat::Table => {
            print_table_format(&results)?;
        }
        OutputFormat::Plain => {
            print_plain_format(&results)?;
        }
    }

    Ok(())
}

/// Search the registry
async fn search_registry(args: &SearchArgs, registry_url: &str) -> Result<Vec<SearchResultItem>> {
    debug!("Searching {} with query: '{}'", registry_url, args.query);

    // Simulate network delay
    tokio::time::sleep(std::time::Duration::from_millis(100)).await;

    // Generate mock results based on filters
    let mut items = generate_mock_results();

    // Apply filters
    if !args.query.is_empty() {
        let query_lower = args.query.to_lowercase();
        items.retain(|item| {
            item.name.to_lowercase().contains(&query_lower)
                || item.description.to_lowercase().contains(&query_lower)
                || item.tags.iter().any(|t| t.to_lowercase().contains(&query_lower))
        });
    }

    if let Some(ref effect) = args.effect {
        let effect_str = effect.to_string();
        items.retain(|item| item.effects.contains(&effect_str) || item.effects.contains(&"both".to_string()));
    }

    if let Some(ref language) = args.language {
        let lang_lower = language.to_lowercase();
        items.retain(|item| {
            item.languages.iter().any(|l| l.to_lowercase() == lang_lower)
                || item.languages.contains(&"all".to_string())
        });
    }

    if let Some(ref category) = args.category {
        let cat_str = category.to_string();
        items.retain(|item| item.category == cat_str);
    }

    if let Some(ref tag) = args.tag {
        let tag_lower = tag.to_lowercase();
        items.retain(|item| item.tags.iter().any(|t| t.to_lowercase() == tag_lower));
    }

    if let Some(ref author) = args.author {
        let author_lower = author.to_lowercase();
        items.retain(|item| {
            item.authors
                .iter()
                .any(|a| a.to_lowercase().contains(&author_lower))
        });
    }

    if !args.include_private {
        items.retain(|item| !item.private);
    }

    // Sort results
    match args.sort {
        SortField::Name => {
            items.sort_by(|a, b| a.name.cmp(&b.name));
        }
        SortField::Downloads => {
            items.sort_by(|a, b| b.downloads.cmp(&a.downloads));
        }
        SortField::Updated => {
            items.sort_by(|a, b| b.updated_at.cmp(&a.updated_at));
        }
        SortField::Stars => {
            items.sort_by(|a, b| b.stars.cmp(&a.stars));
        }
        SortField::Relevance => {
            // For mock data, sort by downloads as proxy for relevance
            items.sort_by(|a, b| b.downloads.cmp(&a.downloads));
        }
    }

    if matches!(args.order, SortOrder::Asc) {
        items.reverse();
    }

    // Apply pagination
    let start = args.offset.min(items.len());
    let end = (args.offset + args.limit).min(items.len());
    items = items[start..end].to_vec();

    Ok(items)
}

/// Generate mock search results
fn generate_mock_results() -> Vec<SearchResultItem> {
    vec![
        SearchResultItem {
            id: "security/workflow-pins/1.2.0".to_string(),
            name: "workflow-pins".to_string(),
            version: "1.2.0".to_string(),
            description: "Pin all GitHub Actions to SHA for supply chain security".to_string(),
            category: "security".to_string(),
            authors: vec!["cicd-hyper-a".to_string()],
            languages: vec!["all".to_string()],
            tags: vec!["security".to_string(), "github-actions".to_string(), "supply-chain".to_string()],
            rules_count: 3,
            effects: vec!["both".to_string()],
            downloads: 15420,
            stars: 234,
            updated_at: Utc::now() - chrono::Duration::days(2),
            private: false,
            license: "AGPL-3.0-or-later".to_string(),
        },
        SearchResultItem {
            id: "security/workflow-permissions/2.0.1".to_string(),
            name: "workflow-permissions".to_string(),
            version: "2.0.1".to_string(),
            description: "Enforce minimal workflow permissions for GitHub Actions".to_string(),
            category: "security".to_string(),
            authors: vec!["cicd-hyper-a".to_string()],
            languages: vec!["all".to_string()],
            tags: vec!["security".to_string(), "github-actions".to_string(), "permissions".to_string()],
            rules_count: 5,
            effects: vec!["both".to_string()],
            downloads: 12340,
            stars: 189,
            updated_at: Utc::now() - chrono::Duration::days(5),
            private: false,
            license: "AGPL-3.0-or-later".to_string(),
        },
        SearchResultItem {
            id: "policy/spdx-headers/1.0.0".to_string(),
            name: "spdx-headers".to_string(),
            version: "1.0.0".to_string(),
            description: "Ensure all source files have SPDX license identifiers".to_string(),
            category: "policy".to_string(),
            authors: vec!["hyperpolymath".to_string()],
            languages: vec!["rust".to_string(), "javascript".to_string(), "typescript".to_string()],
            tags: vec!["license".to_string(), "spdx".to_string(), "compliance".to_string()],
            rules_count: 2,
            effects: vec!["both".to_string()],
            downloads: 8765,
            stars: 156,
            updated_at: Utc::now() - chrono::Duration::days(10),
            private: false,
            license: "MIT".to_string(),
        },
        SearchResultItem {
            id: "quality/rust-best-practices/3.1.0".to_string(),
            name: "rust-best-practices".to_string(),
            version: "3.1.0".to_string(),
            description: "Comprehensive Rust code quality rules and linting".to_string(),
            category: "quality".to_string(),
            authors: vec!["rustacean-collective".to_string()],
            languages: vec!["rust".to_string()],
            tags: vec!["rust".to_string(), "quality".to_string(), "linting".to_string(), "clippy".to_string()],
            rules_count: 42,
            effects: vec!["check".to_string()],
            downloads: 23456,
            stars: 567,
            updated_at: Utc::now() - chrono::Duration::days(1),
            private: false,
            license: "Apache-2.0".to_string(),
        },
        SearchResultItem {
            id: "documentation/readme-quality/1.5.0".to_string(),
            name: "readme-quality".to_string(),
            version: "1.5.0".to_string(),
            description: "Validate README files for completeness and quality".to_string(),
            category: "documentation".to_string(),
            authors: vec!["glambot".to_string()],
            languages: vec!["all".to_string()],
            tags: vec!["documentation".to_string(), "readme".to_string(), "quality".to_string()],
            rules_count: 8,
            effects: vec!["check".to_string(), "fix".to_string()],
            downloads: 5432,
            stars: 87,
            updated_at: Utc::now() - chrono::Duration::days(7),
            private: false,
            license: "AGPL-3.0-or-later".to_string(),
        },
        SearchResultItem {
            id: "workflow/codeql-config/2.0.0".to_string(),
            name: "codeql-config".to_string(),
            version: "2.0.0".to_string(),
            description: "Optimal CodeQL configuration for language detection".to_string(),
            category: "workflow".to_string(),
            authors: vec!["cicd-hyper-a".to_string()],
            languages: vec!["all".to_string()],
            tags: vec!["codeql".to_string(), "security".to_string(), "scanning".to_string()],
            rules_count: 4,
            effects: vec!["both".to_string()],
            downloads: 9876,
            stars: 145,
            updated_at: Utc::now() - chrono::Duration::days(3),
            private: false,
            license: "AGPL-3.0-or-later".to_string(),
        },
        SearchResultItem {
            id: "dependencies/npm-audit/1.1.0".to_string(),
            name: "npm-audit".to_string(),
            version: "1.1.0".to_string(),
            description: "Automated npm dependency security auditing".to_string(),
            category: "dependencies".to_string(),
            authors: vec!["node-security".to_string()],
            languages: vec!["javascript".to_string(), "typescript".to_string()],
            tags: vec!["npm".to_string(), "security".to_string(), "dependencies".to_string()],
            rules_count: 6,
            effects: vec!["check".to_string()],
            downloads: 7654,
            stars: 123,
            updated_at: Utc::now() - chrono::Duration::days(14),
            private: false,
            license: "MIT".to_string(),
        },
        SearchResultItem {
            id: "performance/bundle-size/1.0.0".to_string(),
            name: "bundle-size".to_string(),
            version: "1.0.0".to_string(),
            description: "Monitor and limit JavaScript bundle sizes".to_string(),
            category: "performance".to_string(),
            authors: vec!["web-perf-team".to_string()],
            languages: vec!["javascript".to_string(), "typescript".to_string()],
            tags: vec!["performance".to_string(), "bundle".to_string(), "webpack".to_string()],
            rules_count: 3,
            effects: vec!["check".to_string()],
            downloads: 4321,
            stars: 76,
            updated_at: Utc::now() - chrono::Duration::days(21),
            private: false,
            license: "MIT".to_string(),
        },
    ]
}

/// Print results in table format
fn print_table_format(results: &SearchResults) -> Result<()> {
    use tabled::{settings::Style, Table, Tabled};

    #[derive(Tabled)]
    struct ResultRow {
        #[tabled(rename = "Name")]
        name: String,
        #[tabled(rename = "Version")]
        version: String,
        #[tabled(rename = "Category")]
        category: String,
        #[tabled(rename = "Rules")]
        rules: usize,
        #[tabled(rename = "Downloads")]
        downloads: String,
        #[tabled(rename = "Stars")]
        stars: u64,
    }

    if results.items.is_empty() {
        println!("No rulesets found matching your criteria.");
        return Ok(());
    }

    let rows: Vec<ResultRow> = results
        .items
        .iter()
        .map(|item| ResultRow {
            name: item.name.clone(),
            version: item.version.clone(),
            category: item.category.clone(),
            rules: item.rules_count,
            downloads: format_count(item.downloads),
            stars: item.stars,
        })
        .collect();

    let table = Table::new(rows).with(Style::rounded()).to_string();
    println!("{}", table);

    println!(
        "\nShowing {} of {} results ({} ms)",
        results.items.len(),
        results.total_count,
        results.took_ms
    );

    Ok(())
}

/// Print results in plain format
fn print_plain_format(results: &SearchResults) -> Result<()> {
    if results.items.is_empty() {
        println!("No rulesets found matching your criteria.");
        return Ok(());
    }

    println!(
        "{} ({} results in {} ms)",
        "Search Results".bold(),
        results.total_count,
        results.took_ms
    );
    println!("{}", "═".repeat(60));

    for item in &results.items {
        let category_colored = match item.category.as_str() {
            "security" => item.category.red(),
            "quality" => item.category.green(),
            "policy" => item.category.yellow(),
            "documentation" => item.category.blue(),
            "workflow" => item.category.cyan(),
            "dependencies" => item.category.magenta(),
            "performance" => item.category.bright_yellow(),
            _ => item.category.normal(),
        };

        println!();
        println!(
            "{} {} {}",
            item.name.bold(),
            format!("v{}", item.version).dimmed(),
            format!("[{}]", category_colored)
        );
        println!("  {}", item.description);
        println!(
            "  {} {} | {} {} | {} rules",
            "↓".dimmed(),
            format_count(item.downloads),
            "★".yellow(),
            item.stars,
            item.rules_count
        );
        println!(
            "  {}: {}",
            "Languages".dimmed(),
            item.languages.join(", ")
        );
        println!("  {}: {}", "Tags".dimmed(), item.tags.join(", ").dimmed());
        println!(
            "  {}",
            format!("hyper withdraw {}", item.id).cyan().dimmed()
        );
    }

    println!();

    Ok(())
}

/// Format a large count nicely
fn format_count(count: u64) -> String {
    if count >= 1_000_000 {
        format!("{:.1}M", count as f64 / 1_000_000.0)
    } else if count >= 1_000 {
        format!("{:.1}K", count as f64 / 1_000.0)
    } else {
        count.to_string()
    }
}
