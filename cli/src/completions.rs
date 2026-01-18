// SPDX-License-Identifier: PLMP-1.0-or-later
//! Shell completion generation for the cicd-hyper-a CLI.
//!
//! Generates completion scripts for bash, zsh, fish, elvish, and PowerShell.

use clap::CommandFactory;
use clap_complete::{generate, Shell};
use std::io;
use std::path::PathBuf;

use crate::Cli;

/// Arguments for generating shell completions
#[derive(clap::Args, Debug)]
pub struct CompletionsArgs {
    /// Shell to generate completions for
    #[arg(value_enum)]
    pub shell: Shell,

    /// Output directory (stdout if not specified)
    #[arg(short, long)]
    pub output: Option<PathBuf>,
}

/// Generate shell completion scripts
pub fn generate_completions(args: &CompletionsArgs) -> anyhow::Result<()> {
    let mut cmd = Cli::command();

    if let Some(ref output_dir) = args.output {
        std::fs::create_dir_all(output_dir)?;

        let bin_name = cmd.get_name().to_string();
        let filename = match args.shell {
            Shell::Bash => format!("{}.bash", bin_name),
            Shell::Zsh => format!("_{}", bin_name),
            Shell::Fish => format!("{}.fish", bin_name),
            Shell::Elvish => format!("{}.elv", bin_name),
            Shell::PowerShell => format!("_{}.ps1", bin_name),
            _ => format!("{}.completion", bin_name),
        };

        let path = output_dir.join(&filename);
        let mut file = std::fs::File::create(&path)?;
        generate(args.shell, &mut cmd, bin_name, &mut file);
        eprintln!("Generated: {}", path.display());
    } else {
        let bin_name = cmd.get_name().to_string();
        generate(args.shell, &mut cmd, bin_name, &mut io::stdout());
    }

    Ok(())
}

/// Print installation instructions for completions
#[allow(dead_code)]
pub fn print_completion_instructions(shell: Shell) {
    let bin_name = "hyper";

    match shell {
        Shell::Bash => {
            println!(
                r#"# Bash completions for {bin_name}
# Add to ~/.bashrc or ~/.bash_profile:

# Option 1: Direct evaluation (slower startup)
eval "$({bin_name} completions bash)"

# Option 2: Pre-generate to file (recommended)
{bin_name} completions bash > ~/.local/share/bash-completion/completions/{bin_name}

# Or system-wide:
sudo {bin_name} completions bash > /etc/bash_completion.d/{bin_name}
"#,
                bin_name = bin_name
            );
        }
        Shell::Zsh => {
            println!(
                r#"# Zsh completions for {bin_name}
# Add to ~/.zshrc:

# Ensure completion directory exists
mkdir -p ~/.zsh/completions

# Generate completions
{bin_name} completions zsh > ~/.zsh/completions/_{bin_name}

# Add to fpath (before compinit)
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit && compinit

# Or with oh-my-zsh, place in $ZSH_CUSTOM/plugins/{bin_name}/
"#,
                bin_name = bin_name
            );
        }
        Shell::Fish => {
            println!(
                r#"# Fish completions for {bin_name}
# Run once to install:
{bin_name} completions fish > ~/.config/fish/completions/{bin_name}.fish

# Or for all users:
sudo {bin_name} completions fish > /usr/share/fish/vendor_completions.d/{bin_name}.fish
"#,
                bin_name = bin_name
            );
        }
        Shell::Elvish => {
            println!(
                r#"# Elvish completions for {bin_name}
# Add to ~/.elvish/rc.elv:
eval ({bin_name} completions elvish | slurp)
"#,
                bin_name = bin_name
            );
        }
        Shell::PowerShell => {
            println!(
                r#"# PowerShell completions for {bin_name}
# Add to $PROFILE:
Invoke-Expression (& {bin_name} completions powershell | Out-String)

# Or pre-generate:
{bin_name} completions powershell > ~\Documents\PowerShell\Completions\_{bin_name}.ps1
"#,
                bin_name = bin_name
            );
        }
        _ => {
            println!("Unknown shell. See documentation for completion setup.");
        }
    }
}
