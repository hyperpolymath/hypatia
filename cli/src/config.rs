// SPDX-License-Identifier: PMPL-1.0-or-later
//! Configuration handling for the cicd-hyper-a CLI.
//!
//! Supports loading configuration from:
//! - Default locations (~/.config/cicd-hyper-a/config.toml)
//! - Environment variables (HYPER_*)
//! - Command-line arguments (--config)
//! - Project-local files (.hyper.toml, .hyper.yaml)

#![allow(dead_code)]

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use directories::ProjectDirs;
use serde::{Deserialize, Serialize};
use tracing::debug;

/// Main configuration struct
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct Config {
    /// General settings
    pub general: GeneralConfig,

    /// Registry settings
    pub registry_url: Option<String>,

    /// Scan settings
    pub scan: ScanConfig,

    /// Fleet settings
    pub fleet: FleetConfig,

    /// Hooks settings
    pub hooks: HooksConfig,

    /// Output settings
    pub output: OutputConfig,

    /// Custom settings (key-value pairs)
    #[serde(flatten)]
    pub custom: HashMap<String, toml::Value>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            general: GeneralConfig::default(),
            registry_url: None,
            scan: ScanConfig::default(),
            fleet: FleetConfig::default(),
            hooks: HooksConfig::default(),
            output: OutputConfig::default(),
            custom: HashMap::new(),
        }
    }
}

/// General configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct GeneralConfig {
    /// Log level (trace, debug, info, warn, error)
    pub log_level: String,

    /// Enable telemetry
    pub telemetry: bool,

    /// Cache directory
    pub cache_dir: Option<PathBuf>,

    /// Data directory
    pub data_dir: Option<PathBuf>,
}

impl Default for GeneralConfig {
    fn default() -> Self {
        Self {
            log_level: "warn".to_string(),
            telemetry: false,
            cache_dir: None,
            data_dir: None,
        }
    }
}

/// Scan configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct ScanConfig {
    /// Default minimum severity
    pub min_severity: String,

    /// Default categories to scan
    pub categories: Vec<String>,

    /// Checks to skip by default
    pub skip_checks: Vec<String>,

    /// Include hidden files
    pub include_hidden: bool,

    /// Custom patterns to scan
    pub patterns: Vec<String>,

    /// Ignore patterns
    pub ignore_patterns: Vec<String>,
}

impl Default for ScanConfig {
    fn default() -> Self {
        Self {
            min_severity: "low".to_string(),
            categories: vec!["all".to_string()],
            skip_checks: vec![],
            include_hidden: false,
            patterns: vec![],
            ignore_patterns: vec![
                "target/**".to_string(),
                "node_modules/**".to_string(),
                "dist/**".to_string(),
                ".git/**".to_string(),
                "vendor/**".to_string(),
            ],
        }
    }
}

/// Fleet configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct FleetConfig {
    /// Default bots to run
    pub default_bots: Vec<String>,

    /// Bots to exclude by default
    pub exclude_bots: Vec<String>,

    /// Enable parallel execution
    pub parallel: bool,

    /// Continue on error
    pub continue_on_error: bool,

    /// Timeout in seconds
    pub timeout: u64,

    /// Auto-fix by default
    pub auto_fix: bool,
}

impl Default for FleetConfig {
    fn default() -> Self {
        Self {
            default_bots: vec![],
            exclude_bots: vec![],
            parallel: false,
            continue_on_error: false,
            timeout: 600,
            auto_fix: false,
        }
    }
}

/// Hooks configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct HooksConfig {
    /// Hooks to install by default
    pub default_hooks: Vec<String>,

    /// Create backups when installing
    pub backup: bool,

    /// Custom hook scripts directory
    pub custom_hooks_dir: Option<PathBuf>,
}

impl Default for HooksConfig {
    fn default() -> Self {
        Self {
            default_hooks: vec!["pre-commit".to_string(), "pre-push".to_string()],
            backup: true,
            custom_hooks_dir: None,
        }
    }
}

/// Output configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct OutputConfig {
    /// Default output format
    pub format: String,

    /// Use colors
    pub color: bool,

    /// Show progress indicators
    pub progress: bool,

    /// Verbose output
    pub verbose: bool,
}

impl Default for OutputConfig {
    fn default() -> Self {
        Self {
            format: "plain".to_string(),
            color: true,
            progress: true,
            verbose: false,
        }
    }
}

impl Config {
    /// Load configuration from default locations
    pub fn load() -> Result<Self> {
        let mut config = Config::default();

        // Try to load from XDG config directory
        if let Some(proj_dirs) = ProjectDirs::from("dev", "hyperpolymath", "cicd-hyper-a") {
            let config_file = proj_dirs.config_dir().join("config.toml");
            if config_file.exists() {
                config = Config::from_file(&config_file)?;
                debug!("Loaded config from: {}", config_file.display());
            }
        }

        // Try to load from home directory
        if let Some(home) = dirs::home_dir() {
            let home_config = home.join(".hyper.toml");
            if home_config.exists() {
                config = config.merge_file(&home_config)?;
                debug!("Merged config from: {}", home_config.display());
            }
        }

        // Try to load from current directory
        let local_configs = [".hyper.toml", ".hyper.yaml", ".hyper.json"];
        for filename in &local_configs {
            let local_config = PathBuf::from(filename);
            if local_config.exists() {
                config = config.merge_file(&local_config)?;
                debug!("Merged config from: {}", local_config.display());
                break;
            }
        }

        // Apply environment variables
        config = config.apply_env_vars()?;

        Ok(config)
    }

    /// Load configuration from a specific file
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read config file: {}", path.display()))?;

        let extension = path.extension().and_then(|e| e.to_str()).unwrap_or("");

        match extension {
            "toml" => {
                toml::from_str(&content).with_context(|| "Failed to parse TOML config")
            }
            "yaml" | "yml" => {
                serde_yaml::from_str(&content).with_context(|| "Failed to parse YAML config")
            }
            "json" => {
                serde_json::from_str(&content).with_context(|| "Failed to parse JSON config")
            }
            _ => {
                // Try TOML first, then YAML, then JSON
                if let Ok(config) = toml::from_str(&content) {
                    return Ok(config);
                }
                if let Ok(config) = serde_yaml::from_str(&content) {
                    return Ok(config);
                }
                serde_json::from_str(&content).with_context(|| "Failed to parse config file")
            }
        }
    }

    /// Merge configuration from a file
    pub fn merge_file(self, path: &Path) -> Result<Self> {
        let other = Config::from_file(path)?;
        Ok(self.merge(other))
    }

    /// Merge two configurations (other takes precedence)
    pub fn merge(mut self, other: Config) -> Self {
        // Registry URL
        if other.registry_url.is_some() {
            self.registry_url = other.registry_url;
        }

        // General settings
        if other.general.log_level != GeneralConfig::default().log_level {
            self.general.log_level = other.general.log_level;
        }
        if other.general.telemetry {
            self.general.telemetry = true;
        }
        if other.general.cache_dir.is_some() {
            self.general.cache_dir = other.general.cache_dir;
        }
        if other.general.data_dir.is_some() {
            self.general.data_dir = other.general.data_dir;
        }

        // Scan settings
        if other.scan.min_severity != ScanConfig::default().min_severity {
            self.scan.min_severity = other.scan.min_severity;
        }
        if !other.scan.skip_checks.is_empty() {
            self.scan.skip_checks.extend(other.scan.skip_checks);
        }

        // Fleet settings
        if !other.fleet.default_bots.is_empty() {
            self.fleet.default_bots = other.fleet.default_bots;
        }
        if other.fleet.parallel {
            self.fleet.parallel = true;
        }
        if other.fleet.auto_fix {
            self.fleet.auto_fix = true;
        }

        // Custom settings
        self.custom.extend(other.custom);

        self
    }

    /// Apply environment variables
    pub fn apply_env_vars(mut self) -> Result<Self> {
        // HYPER_REGISTRY_URL
        if let Ok(url) = std::env::var("HYPER_REGISTRY_URL") {
            self.registry_url = Some(url);
        }

        // HYPER_LOG_LEVEL
        if let Ok(level) = std::env::var("HYPER_LOG_LEVEL") {
            self.general.log_level = level;
        }

        // HYPER_OUTPUT_FORMAT
        if let Ok(format) = std::env::var("HYPER_OUTPUT_FORMAT") {
            self.output.format = format;
        }

        // HYPER_NO_COLOR
        if std::env::var("HYPER_NO_COLOR").is_ok() || std::env::var("NO_COLOR").is_ok() {
            self.output.color = false;
        }

        // HYPER_CACHE_DIR
        if let Ok(dir) = std::env::var("HYPER_CACHE_DIR") {
            self.general.cache_dir = Some(PathBuf::from(dir));
        }

        // HYPER_DATA_DIR
        if let Ok(dir) = std::env::var("HYPER_DATA_DIR") {
            self.general.data_dir = Some(PathBuf::from(dir));
        }

        Ok(self)
    }

    /// Initialize default configuration file
    pub fn init_default() -> Result<PathBuf> {
        let proj_dirs = ProjectDirs::from("dev", "hyperpolymath", "cicd-hyper-a")
            .context("Could not determine config directory")?;

        let config_dir = proj_dirs.config_dir();
        std::fs::create_dir_all(config_dir)
            .with_context(|| format!("Failed to create config directory: {}", config_dir.display()))?;

        let config_file = config_dir.join("config.toml");

        let default_content = r#"# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a CLI Configuration
# https://github.com/hyperpolymath/cicd-hyper-a

# Registry URL for ruleset operations
# registry_url = "https://registry.cicd-hyper-a.dev"

[general]
# Log level: trace, debug, info, warn, error
log_level = "warn"

# Enable telemetry (anonymous usage statistics)
telemetry = false

# Cache directory (default: ~/.cache/cicd-hyper-a)
# cache_dir = "~/.cache/cicd-hyper-a"

[scan]
# Minimum severity to report: info, low, medium, high, critical
min_severity = "low"

# Categories to scan (or ["all"])
categories = ["all"]

# Checks to skip by default
skip_checks = []

# Include hidden files in scan
include_hidden = false

# Patterns to ignore
ignore_patterns = [
    "target/**",
    "node_modules/**",
    "dist/**",
    ".git/**",
    "vendor/**",
]

[fleet]
# Default bots to run (empty means all)
default_bots = []

# Bots to exclude by default
exclude_bots = []

# Run bots in parallel where possible
parallel = false

# Continue even if a bot fails
continue_on_error = false

# Timeout in seconds
timeout = 600

# Apply fixes automatically
auto_fix = false

[hooks]
# Hooks to install by default
default_hooks = ["pre-commit", "pre-push"]

# Create backups when installing hooks
backup = true

[output]
# Default output format: plain, json, yaml, table
format = "plain"

# Use colored output
color = true

# Show progress indicators
progress = true

# Verbose output by default
verbose = false
"#;

        std::fs::write(&config_file, default_content)
            .with_context(|| format!("Failed to write config file: {}", config_file.display()))?;

        Ok(config_file)
    }

    /// Save configuration to default location
    pub fn save(&self) -> Result<()> {
        let proj_dirs = ProjectDirs::from("dev", "hyperpolymath", "cicd-hyper-a")
            .context("Could not determine config directory")?;

        let config_dir = proj_dirs.config_dir();
        std::fs::create_dir_all(config_dir)?;

        let config_file = config_dir.join("config.toml");
        let content = toml::to_string_pretty(self)?;
        std::fs::write(&config_file, content)?;

        Ok(())
    }

    /// Get a configuration value by dot-separated key
    pub fn get(&self, key: &str) -> Option<String> {
        let parts: Vec<&str> = key.split('.').collect();

        match parts.as_slice() {
            ["registry", "url"] | ["registry_url"] => self.registry_url.clone(),
            ["general", "log_level"] => Some(self.general.log_level.clone()),
            ["general", "telemetry"] => Some(self.general.telemetry.to_string()),
            ["scan", "min_severity"] => Some(self.scan.min_severity.clone()),
            ["scan", "include_hidden"] => Some(self.scan.include_hidden.to_string()),
            ["fleet", "parallel"] => Some(self.fleet.parallel.to_string()),
            ["fleet", "timeout"] => Some(self.fleet.timeout.to_string()),
            ["fleet", "auto_fix"] => Some(self.fleet.auto_fix.to_string()),
            ["output", "format"] => Some(self.output.format.clone()),
            ["output", "color"] => Some(self.output.color.to_string()),
            _ => {
                // Check custom settings
                if let Some(value) = self.custom.get(key) {
                    Some(value.to_string())
                } else {
                    None
                }
            }
        }
    }

    /// Set a configuration value by dot-separated key
    pub fn set(&mut self, key: &str, value: &str) -> Result<()> {
        let parts: Vec<&str> = key.split('.').collect();

        match parts.as_slice() {
            ["registry", "url"] | ["registry_url"] => {
                self.registry_url = Some(value.to_string());
            }
            ["general", "log_level"] => {
                self.general.log_level = value.to_string();
            }
            ["general", "telemetry"] => {
                self.general.telemetry = value.parse().context("Invalid boolean value")?;
            }
            ["scan", "min_severity"] => {
                self.scan.min_severity = value.to_string();
            }
            ["scan", "include_hidden"] => {
                self.scan.include_hidden = value.parse().context("Invalid boolean value")?;
            }
            ["fleet", "parallel"] => {
                self.fleet.parallel = value.parse().context("Invalid boolean value")?;
            }
            ["fleet", "timeout"] => {
                self.fleet.timeout = value.parse().context("Invalid integer value")?;
            }
            ["fleet", "auto_fix"] => {
                self.fleet.auto_fix = value.parse().context("Invalid boolean value")?;
            }
            ["output", "format"] => {
                self.output.format = value.to_string();
            }
            ["output", "color"] => {
                self.output.color = value.parse().context("Invalid boolean value")?;
            }
            _ => {
                // Store as custom setting
                self.custom.insert(key.to_string(), toml::Value::String(value.to_string()));
            }
        }

        Ok(())
    }

    /// Get the cache directory
    pub fn cache_dir(&self) -> PathBuf {
        self.general.cache_dir.clone().unwrap_or_else(|| {
            dirs::cache_dir()
                .map(|d| d.join("cicd-hyper-a"))
                .unwrap_or_else(|| PathBuf::from(".cache/cicd-hyper-a"))
        })
    }

    /// Get the data directory
    pub fn data_dir(&self) -> PathBuf {
        self.general.data_dir.clone().unwrap_or_else(|| {
            dirs::data_dir()
                .map(|d| d.join("cicd-hyper-a"))
                .unwrap_or_else(|| PathBuf::from(".local/share/cicd-hyper-a"))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.general.log_level, "warn");
        assert!(!config.general.telemetry);
        assert_eq!(config.scan.min_severity, "low");
    }

    #[test]
    fn test_config_get_set() {
        let mut config = Config::default();

        config.set("registry_url", "https://example.com").unwrap();
        assert_eq!(
            config.get("registry_url"),
            Some("https://example.com".to_string())
        );

        config.set("fleet.parallel", "true").unwrap();
        assert_eq!(config.get("fleet.parallel"), Some("true".to_string()));
        assert!(config.fleet.parallel);
    }

    #[test]
    fn test_config_from_toml() {
        let toml_content = r#"
registry_url = "https://test.example.com"

[general]
log_level = "debug"

[scan]
min_severity = "high"
"#;

        let config: Config = toml::from_str(toml_content).unwrap();
        assert_eq!(
            config.registry_url,
            Some("https://test.example.com".to_string())
        );
        assert_eq!(config.general.log_level, "debug");
        assert_eq!(config.scan.min_severity, "high");
    }

    #[test]
    fn test_config_merge() {
        let base = Config::default();
        let mut override_config = Config::default();
        override_config.registry_url = Some("https://override.example.com".to_string());
        override_config.fleet.parallel = true;

        let merged = base.merge(override_config);
        assert_eq!(
            merged.registry_url,
            Some("https://override.example.com".to_string())
        );
        assert!(merged.fleet.parallel);
    }
}
