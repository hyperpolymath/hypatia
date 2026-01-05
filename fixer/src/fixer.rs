// SPDX-License-Identifier: AGPL-3.0-or-later
//! Workflow fixer - applies fixes to CI/CD issues

use crate::scanner::IssueCategory;
use crate::{FixResult, Issue, Result, ShaPins};
use regex::Regex;
use std::fs;
use std::path::Path;

/// Workflow fixer that applies fixes
pub struct WorkflowFixer {
    spdx_header: String,
    permissions_block: String,
}

impl WorkflowFixer {
    pub fn new() -> Self {
        Self {
            spdx_header: "# SPDX-License-Identifier: AGPL-3.0-or-later\n".to_string(),
            permissions_block: "\npermissions: read-all\n".to_string(),
        }
    }

    /// Fix a single issue
    pub fn fix_issue(
        &self,
        _repo_path: &Path,
        issue: &Issue,
        sha_pins: &ShaPins,
        dry_run: bool,
    ) -> Result<FixResult> {
        let file_path = Path::new(&issue.file_path);

        if !file_path.exists() {
            return Ok(FixResult {
                issue_id: issue.id.clone(),
                file_path: issue.file_path.clone(),
                success: false,
                applied: false,
                message: "File does not exist".to_string(),
            });
        }

        let content = fs::read_to_string(file_path)?;
        let fixed_content = match issue.category {
            IssueCategory::MissingSpdx => self.fix_missing_spdx(&content),
            IssueCategory::MissingPermissions => self.fix_missing_permissions(&content),
            IssueCategory::UnpinnedAction => self.fix_unpinned_action(&content, sha_pins),
            IssueCategory::MissingToolchainInput => self.fix_missing_toolchain(&content),
            _ => None,
        };

        match fixed_content {
            Some(new_content) => {
                if dry_run {
                    Ok(FixResult {
                        issue_id: issue.id.clone(),
                        file_path: issue.file_path.clone(),
                        success: true,
                        applied: false,
                        message: "Would apply fix (dry-run)".to_string(),
                    })
                } else {
                    fs::write(file_path, new_content)?;
                    Ok(FixResult {
                        issue_id: issue.id.clone(),
                        file_path: issue.file_path.clone(),
                        success: true,
                        applied: true,
                        message: "Fix applied successfully".to_string(),
                    })
                }
            }
            None => Ok(FixResult {
                issue_id: issue.id.clone(),
                file_path: issue.file_path.clone(),
                success: false,
                applied: false,
                message: "No fix available for this issue type".to_string(),
            }),
        }
    }

    /// Add SPDX header to file
    fn fix_missing_spdx(&self, content: &str) -> Option<String> {
        if content.starts_with("# SPDX-") {
            return None;
        }
        Some(format!("{}{}", self.spdx_header, content))
    }

    /// Add permissions: read-all after on: block
    fn fix_missing_permissions(&self, content: &str) -> Option<String> {
        if content.contains("permissions:") {
            return None;
        }

        // Find the on: block and add permissions after it
        let on_re = Regex::new(r"(?m)(^on:\s*\n(?:\s+.*\n)*)").ok()?;
        if let Some(cap) = on_re.captures(content) {
            let on_block = cap.get(1)?;
            let end_pos = on_block.end();
            let mut new_content = content.to_string();
            new_content.insert_str(end_pos, &self.permissions_block);
            return Some(new_content);
        }

        // Fallback: insert after name: line
        let name_re = Regex::new(r"(?m)(^name:.*\n)").ok()?;
        if let Some(cap) = name_re.captures(content) {
            let name_line = cap.get(1)?;
            let end_pos = name_line.end();
            let mut new_content = content.to_string();
            new_content.insert_str(end_pos, &self.permissions_block);
            return Some(new_content);
        }

        None
    }

    /// Pin unpinned actions to SHA
    fn fix_unpinned_action(&self, content: &str, sha_pins: &ShaPins) -> Option<String> {
        let uses_re = Regex::new(r"uses:\s*([a-zA-Z0-9_-]+/[a-zA-Z0-9_/-]+)@(v[0-9]+[a-zA-Z0-9.-]*|main|master)").ok()?;
        let mut new_content = content.to_string();
        let mut made_change = false;

        // Find all matches and their positions
        let matches: Vec<_> = uses_re.captures_iter(content).collect();

        // Process in reverse order to maintain positions
        for cap in matches.into_iter().rev() {
            let full_match = cap.get(0)?;
            let action = cap.get(1)?.as_str();
            let version = cap.get(2)?.as_str();

            if let Some((sha, ver)) = sha_pins.get_pin(action) {
                let replacement = format!("uses: {}@{} # {}", action, sha, ver);
                new_content.replace_range(full_match.range(), &replacement);
                made_change = true;
            } else {
                // Keep version as comment if no SHA known
                // No SHA known for this action, skip
                let _replacement = format!("uses: {}@{} # {}", action, version, version);
                // Don't actually change since we don't have the SHA
            }
        }

        if made_change {
            Some(new_content)
        } else {
            None
        }
    }

    /// Add toolchain input to dtolnay/rust-toolchain
    fn fix_missing_toolchain(&self, content: &str) -> Option<String> {
        let toolchain_re = Regex::new(r"(?m)(uses:\s*dtolnay/rust-toolchain@[a-f0-9]+(?:\s*#[^\n]*)?)").ok()?;
        let mut new_content = content.to_string();
        let mut made_change = false;

        let lines: Vec<&str> = content.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            if line.contains("dtolnay/rust-toolchain@") {
                // Check if next line has with:
                let has_with = lines.get(i + 1).map_or(false, |l| l.trim().starts_with("with:"));
                let has_toolchain = lines.iter()
                    .skip(i + 1)
                    .take(3)
                    .any(|l| l.contains("toolchain:"));

                if !has_with && !has_toolchain {
                    // Need to add with: toolchain:
                    // Find indentation of uses: line
                    let indent = line.len() - line.trim_start().len();
                    let with_block = format!("\n{}  with:\n{}    toolchain: stable", " ".repeat(indent), " ".repeat(indent));

                    // Find position to insert
                    if let Some(cap) = toolchain_re.captures(line) {
                        let full_match = cap.get(0)?;
                        let match_end = full_match.end();
                        let line_start = content.lines()
                            .take(i)
                            .map(|l| l.len() + 1)
                            .sum::<usize>();
                        let insert_pos = line_start + match_end;

                        new_content.insert_str(insert_pos, &with_block);
                        made_change = true;
                        break; // Only fix one at a time to avoid position issues
                    }
                }
            }
        }

        if made_change {
            Some(new_content)
        } else {
            None
        }
    }
}

impl Default for WorkflowFixer {
    fn default() -> Self {
        Self::new()
    }
}
