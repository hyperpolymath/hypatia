// SPDX-License-Identifier: AGPL-3.0-or-later
//! cicd-hyper-a forge adapter CLI

use adapters::github::GitHubAdapter;
use adapters::ForgeAdapter;
use std::env;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        return Ok(());
    }

    match args[1].as_str() {
        "list-repos" => {
            if args.len() < 3 {
                eprintln!("Usage: forge-adapter list-repos <owner>");
                return Ok(());
            }
            let adapter = get_github_adapter()?;
            let repos = adapter.list_repos(&args[2]).await?;
            for repo in repos {
                println!("{}/{} ({:?})", repo.owner, repo.name, repo.visibility);
            }
        }
        "get-alerts" => {
            if args.len() < 4 {
                eprintln!("Usage: forge-adapter get-alerts <owner> <repo>");
                return Ok(());
            }
            let adapter = get_github_adapter()?;
            let alerts = adapter.get_alerts(&args[2], &args[3]).await?;
            for alert in alerts {
                println!(
                    "[{:?}] {}: {} ({})",
                    alert.severity,
                    alert.rule_id,
                    alert.description,
                    alert.file.unwrap_or_default()
                );
            }
        }
        "list-workflows" => {
            if args.len() < 4 {
                eprintln!("Usage: forge-adapter list-workflows <owner> <repo>");
                return Ok(());
            }
            let adapter = get_github_adapter()?;
            let workflows = adapter.list_workflows(&args[2], &args[3]).await?;
            for wf in workflows {
                println!("{}: {} ({:?})", wf.id, wf.name, wf.state);
            }
        }
        "trigger" => {
            if args.len() < 5 {
                eprintln!("Usage: forge-adapter trigger <owner> <repo> <workflow>");
                return Ok(());
            }
            let adapter = get_github_adapter()?;
            adapter
                .trigger_workflow(&args[2], &args[3], &args[4], "main")
                .await?;
            println!("Triggered workflow: {}", args[4]);
        }
        "help" | "--help" | "-h" => {
            print_usage();
        }
        _ => {
            eprintln!("Unknown command: {}", args[1]);
            print_usage();
        }
    }

    Ok(())
}

fn get_github_adapter() -> Result<GitHubAdapter, Box<dyn std::error::Error>> {
    let token = env::var("GITHUB_TOKEN")
        .or_else(|_| env::var("GH_TOKEN"))
        .map_err(|_| "GITHUB_TOKEN or GH_TOKEN environment variable not set")?;
    Ok(GitHubAdapter::new(&token)?)
}

fn print_usage() {
    println!("cicd-hyper-a Forge Adapter");
    println!();
    println!("Usage: forge-adapter <command> [args...]");
    println!();
    println!("Commands:");
    println!("  list-repos <owner>              List repositories");
    println!("  get-alerts <owner> <repo>       Get security alerts");
    println!("  list-workflows <owner> <repo>   List workflows");
    println!("  trigger <owner> <repo> <wf>     Trigger a workflow");
    println!("  help                            Show this help");
    println!();
    println!("Environment:");
    println!("  GITHUB_TOKEN or GH_TOKEN        GitHub API token");
}
