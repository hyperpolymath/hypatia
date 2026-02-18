// SPDX-License-Identifier: PMPL-1.0-or-later
//! Output formatting for the cicd-hyper-a CLI.
//!
//! Supports multiple output formats:
//! - Plain text (default, human-readable)
//! - JSON (machine-readable)
//! - YAML (human and machine-readable)
//! - Table (formatted tables for terminal)

#![allow(dead_code)]


use anyhow::Result;
use colored::Colorize;
use serde::Serialize;

use crate::config::Config;

/// Output format options
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputFormat {
    /// Plain text output (human-readable)
    #[default]
    Plain,
    /// JSON output (machine-readable)
    Json,
    /// YAML output
    Yaml,
    /// Table output (formatted tables)
    Table,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "plain" | "text" | "human" => Ok(OutputFormat::Plain),
            "json" => Ok(OutputFormat::Json),
            "yaml" | "yml" => Ok(OutputFormat::Yaml),
            "table" | "tbl" => Ok(OutputFormat::Table),
            _ => Err(format!(
                "Unknown output format: {}. Valid formats: plain, json, yaml, table",
                s
            )),
        }
    }
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Plain => write!(f, "plain"),
            OutputFormat::Json => write!(f, "json"),
            OutputFormat::Yaml => write!(f, "yaml"),
            OutputFormat::Table => write!(f, "table"),
        }
    }
}

/// Output handler for consistent formatting across commands
#[derive(Debug, Clone)]
pub struct Outputter {
    format: OutputFormat,
}

impl Outputter {
    /// Create a new outputter with the specified format
    pub fn new(format: OutputFormat) -> Self {
        Self { format }
    }

    /// Create an outputter from config
    pub fn from_config(config: &Config) -> Self {
        let format = config
            .output
            .format
            .parse()
            .unwrap_or(OutputFormat::Plain);
        Self { format }
    }

    /// Get the current output format
    pub fn format(&self) -> OutputFormat {
        self.format
    }

    /// Output a serializable value in the configured format
    pub fn output<T: Serialize>(&self, value: &T) -> Result<()> {
        match self.format {
            OutputFormat::Json => {
                let output = serde_json::to_string_pretty(value)?;
                println!("{}", output);
            }
            OutputFormat::Yaml => {
                let output = serde_yaml::to_string(value)?;
                print!("{}", output);
            }
            OutputFormat::Plain | OutputFormat::Table => {
                // For plain/table format, serialize to JSON then output
                // Commands should handle their own plain/table formatting
                let output = serde_json::to_string_pretty(value)?;
                println!("{}", output);
            }
        }
        Ok(())
    }

    /// Output the configuration
    pub fn output_config(&self, config: &Config) -> Result<()> {
        match self.format {
            OutputFormat::Json => {
                let output = serde_json::to_string_pretty(config)?;
                println!("{}", output);
            }
            OutputFormat::Yaml => {
                let output = serde_yaml::to_string(config)?;
                print!("{}", output);
            }
            _ => {
                let output = toml::to_string_pretty(config)?;
                println!("{}", output);
            }
        }
        Ok(())
    }

    /// Output a simple message
    pub fn plain(&self, message: &str) -> Result<()> {
        println!("{}", message);
        Ok(())
    }

    /// Output a success message
    pub fn success(&self, message: &str) -> Result<()> {
        match self.format {
            OutputFormat::Json => {
                println!(r#"{{"status": "success", "message": "{}"}}"#, escape_json(message));
            }
            OutputFormat::Yaml => {
                println!("status: success\nmessage: \"{}\"", message);
            }
            _ => {
                println!("{} {}", "Success:".green().bold(), message);
            }
        }
        Ok(())
    }

    /// Output a warning message
    pub fn warn(&self, message: &str) -> Result<()> {
        match self.format {
            OutputFormat::Json => {
                println!(r#"{{"status": "warning", "message": "{}"}}"#, escape_json(message));
            }
            OutputFormat::Yaml => {
                println!("status: warning\nmessage: \"{}\"", message);
            }
            _ => {
                eprintln!("{} {}", "Warning:".yellow().bold(), message);
            }
        }
        Ok(())
    }

    /// Output an error message
    pub fn error(&self, message: &str) -> Result<()> {
        match self.format {
            OutputFormat::Json => {
                eprintln!(r#"{{"status": "error", "message": "{}"}}"#, escape_json(message));
            }
            OutputFormat::Yaml => {
                eprintln!("status: error\nmessage: \"{}\"", message);
            }
            _ => {
                eprintln!("{} {}", "Error:".red().bold(), message);
            }
        }
        Ok(())
    }

    /// Output an informational message
    pub fn info(&self, message: &str) -> Result<()> {
        match self.format {
            OutputFormat::Json => {
                println!(r#"{{"status": "info", "message": "{}"}}"#, escape_json(message));
            }
            OutputFormat::Yaml => {
                println!("status: info\nmessage: \"{}\"", message);
            }
            _ => {
                println!("{} {}", "Info:".blue().bold(), message);
            }
        }
        Ok(())
    }

    /// Output a debug message (only in plain/table formats)
    pub fn debug(&self, message: &str) -> Result<()> {
        match self.format {
            OutputFormat::Plain | OutputFormat::Table => {
                eprintln!("{} {}", "Debug:".dimmed(), message.dimmed());
            }
            _ => {}
        }
        Ok(())
    }

    /// Check if the format is machine-readable
    pub fn is_machine_readable(&self) -> bool {
        matches!(self.format, OutputFormat::Json | OutputFormat::Yaml)
    }

    /// Check if the format is human-readable
    pub fn is_human_readable(&self) -> bool {
        matches!(self.format, OutputFormat::Plain | OutputFormat::Table)
    }
}

/// Escape special characters for JSON string
fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Progress indicator for long-running operations
pub struct Progress {
    bar: Option<indicatif::ProgressBar>,
    format: OutputFormat,
}

impl Progress {
    /// Create a new spinner progress indicator
    pub fn spinner(format: OutputFormat, message: &str) -> Self {
        let bar = if matches!(format, OutputFormat::Plain | OutputFormat::Table) {
            let pb = indicatif::ProgressBar::new_spinner();
            pb.set_style(
                indicatif::ProgressStyle::default_spinner()
                    .template("{spinner:.green} {msg}")
                    .unwrap(),
            );
            pb.set_message(message.to_string());
            Some(pb)
        } else {
            None
        };

        Self { bar, format }
    }

    /// Create a new bar progress indicator
    pub fn bar(format: OutputFormat, total: u64, message: &str) -> Self {
        let bar = if matches!(format, OutputFormat::Plain | OutputFormat::Table) {
            let pb = indicatif::ProgressBar::new(total);
            pb.set_style(
                indicatif::ProgressStyle::default_bar()
                    .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}/{len} {msg}")
                    .unwrap()
                    .progress_chars("#>-"),
            );
            pb.set_message(message.to_string());
            Some(pb)
        } else {
            None
        };

        Self { bar, format }
    }

    /// Update the progress message
    pub fn set_message(&self, message: &str) {
        if let Some(ref bar) = self.bar {
            bar.set_message(message.to_string());
        }
    }

    /// Increment the progress bar
    pub fn inc(&self, delta: u64) {
        if let Some(ref bar) = self.bar {
            bar.inc(delta);
        }
    }

    /// Finish the progress with a message
    pub fn finish(&self, message: &str) {
        if let Some(ref bar) = self.bar {
            bar.finish_with_message(message.to_string());
        }
    }

    /// Finish and clear the progress bar
    pub fn finish_and_clear(&self) {
        if let Some(ref bar) = self.bar {
            bar.finish_and_clear();
        }
    }
}

/// Helper trait for printable types
pub trait Printable {
    fn to_plain(&self) -> String;
}

/// Format a duration in human-readable form
pub fn format_duration(millis: u64) -> String {
    if millis < 1000 {
        format!("{} ms", millis)
    } else if millis < 60_000 {
        format!("{:.1} s", millis as f64 / 1000.0)
    } else if millis < 3_600_000 {
        let minutes = millis / 60_000;
        let seconds = (millis % 60_000) / 1000;
        format!("{}m {}s", minutes, seconds)
    } else {
        let hours = millis / 3_600_000;
        let minutes = (millis % 3_600_000) / 60_000;
        format!("{}h {}m", hours, minutes)
    }
}

/// Format a file size in human-readable form
pub fn format_size(bytes: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;

    if bytes < KB {
        format!("{} B", bytes)
    } else if bytes < MB {
        format!("{:.1} KB", bytes as f64 / KB as f64)
    } else if bytes < GB {
        format!("{:.1} MB", bytes as f64 / MB as f64)
    } else {
        format!("{:.2} GB", bytes as f64 / GB as f64)
    }
}

/// Format a count with appropriate suffix
pub fn format_count(count: u64) -> String {
    if count >= 1_000_000 {
        format!("{:.1}M", count as f64 / 1_000_000.0)
    } else if count >= 1_000 {
        format!("{:.1}K", count as f64 / 1_000.0)
    } else {
        count.to_string()
    }
}

/// Print a horizontal separator
pub fn separator() {
    println!("{}", "─".repeat(60));
}

/// Print a double horizontal separator
pub fn double_separator() {
    println!("{}", "═".repeat(60));
}

/// Print a section header
pub fn section_header(title: &str) {
    println!();
    println!("{}", title.bold());
    separator();
}

/// Print a key-value pair
pub fn key_value(key: &str, value: &str) {
    println!("  {}: {}", key.dimmed(), value);
}

/// Print a list item
pub fn list_item(item: &str) {
    println!("  - {}", item);
}

/// Print a numbered list item
pub fn numbered_item(index: usize, item: &str) {
    println!("  {}. {}", index, item);
}

/// Print a check mark item
pub fn check_item(item: &str, checked: bool) {
    let mark = if checked {
        "✓".green()
    } else {
        "○".dimmed()
    };
    println!("  {} {}", mark, item);
}

/// Print an error item
pub fn error_item(item: &str) {
    println!("  {} {}", "✗".red(), item);
}

/// Print a warning item
pub fn warning_item(item: &str) {
    println!("  {} {}", "⚠".yellow(), item);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_output_format_parsing() {
        assert_eq!("plain".parse::<OutputFormat>().unwrap(), OutputFormat::Plain);
        assert_eq!("json".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
        assert_eq!("yaml".parse::<OutputFormat>().unwrap(), OutputFormat::Yaml);
        assert_eq!("table".parse::<OutputFormat>().unwrap(), OutputFormat::Table);
    }

    #[test]
    fn test_format_duration() {
        assert_eq!(format_duration(500), "500 ms");
        assert_eq!(format_duration(1500), "1.5 s");
        assert_eq!(format_duration(65000), "1m 5s");
        assert_eq!(format_duration(3665000), "1h 1m");
    }

    #[test]
    fn test_format_size() {
        assert_eq!(format_size(500), "500 B");
        assert_eq!(format_size(1536), "1.5 KB");
        assert_eq!(format_size(1572864), "1.5 MB");
        assert_eq!(format_size(1610612736), "1.50 GB");
    }

    #[test]
    fn test_format_count() {
        assert_eq!(format_count(500), "500");
        assert_eq!(format_count(1500), "1.5K");
        assert_eq!(format_count(1500000), "1.5M");
    }

    #[test]
    fn test_escape_json() {
        assert_eq!(escape_json("hello"), "hello");
        assert_eq!(escape_json("hello\nworld"), "hello\\nworld");
        assert_eq!(escape_json("say \"hi\""), "say \\\"hi\\\"");
    }
}
