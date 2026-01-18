// SPDX-License-Identifier: PLMP-1.0-or-later
//! Build script for cicd-hyper-a CLI.
//!
//! Captures build-time information like git commit hash and build date.
//! Optionally generates shell completions and man pages during release builds.

use std::path::Path;
use std::process::Command;
use std::{env, fs};

fn main() {
    // Re-run if .git/HEAD changes (e.g., new commit)
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/refs/heads/");
    println!("cargo:rerun-if-changed=man/");

    // Get git commit hash
    let git_commit = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .and_then(|output| {
            if output.status.success() {
                String::from_utf8(output.stdout).ok()
            } else {
                None
            }
        })
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    println!("cargo:rustc-env=GIT_COMMIT={}", git_commit);

    // Get build date using the date command
    let build_date = Command::new("date")
        .args(["+%Y-%m-%d"])
        .output()
        .ok()
        .and_then(|output| {
            if output.status.success() {
                String::from_utf8(output.stdout).ok()
            } else {
                None
            }
        })
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    println!("cargo:rustc-env=BUILD_DATE={}", build_date);

    // Get target triple
    let target = env::var("TARGET").unwrap_or_else(|_| "unknown".to_string());
    println!("cargo:rustc-env=TARGET={}", target);

    // Set man page directory path for installation
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let man_dir = Path::new(&manifest_dir).join("man");
    if man_dir.exists() {
        println!("cargo:rustc-env=MAN_DIR={}", man_dir.display());
    }

    // Copy man pages to OUT_DIR for packaging (release builds)
    if env::var("PROFILE").map(|p| p == "release").unwrap_or(false) {
        let out_dir = env::var("OUT_DIR").unwrap();
        let target_man_dir = Path::new(&out_dir).join("man");

        if man_dir.exists() {
            let _ = fs::create_dir_all(&target_man_dir);
            if let Ok(entries) = fs::read_dir(&man_dir) {
                for entry in entries.filter_map(|e| e.ok()) {
                    let path = entry.path();
                    if path.is_file() {
                        let dest = target_man_dir.join(entry.file_name());
                        let _ = fs::copy(&path, &dest);
                    }
                }
            }
            println!(
                "cargo:warning=Man pages copied to: {}",
                target_man_dir.display()
            );
        }
    }
}
