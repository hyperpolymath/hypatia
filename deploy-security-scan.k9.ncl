K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia Security Scanning Deployment
#
# This K9 component deploys Hypatia's neurosymbolic security scanning workflow
# across the hyperpolymath organization, integrating with robot-repo-automaton.

leash = 'Hunt

pedigree = {
  schema_version = "1.0.0",
  component_type = "security-scan-deployment",
  author = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>",
  description = "Automated deployment of Hypatia security scanning workflows",
  created = "2026-01-30",
  k9_spec_version = "1.0.0",
}

config = {
  # GitHub organization settings
  github_org | String = "hyperpolymath",
  workflow_file_name | String = "hypatia-scan.yml",

  # Scan schedule
  scan_schedule | {
    weekly | String,
    on_push | Bool,
    on_pull_request | Bool,
  } = {
    weekly = "0 0 * * 0",  # Sunday midnight UTC
    on_push = true,
    on_pull_request = true,
  },

  # Hypatia integration endpoints
  hypatia_endpoints = {
    ruleset_registry | String = "https://ruleset.hypatia.reposystem.dev",
    learning_api | String = "https://learn.hypatia.reposystem.dev",
    graph_db | String = "tcp://arangodb.hypatia.internal:8529",
    cache | String = "redis://dragonfly.hypatia.internal:6379",
  },

  # Security scanning configuration
  scan_config = {
    # Rulesets to apply
    enabled_rulesets | Array String = [
      "workflow-security-v1",
      "dependency-safety-v1",
      "code-quality-v1",
      "license-compliance-v1",
      "secret-detection-v1",
    ],

    # Scan depth
    scan_depth | [| 'Quick, 'Standard, 'Deep |] = 'Standard,

    # Auto-fix configuration
    auto_fix_enabled | Bool = false,
    auto_fix_confidence_threshold | Number = 0.95,  # Only apply fixes with >95% confidence

    # Reporting
    report_to_pr | Bool = true,
    create_issues | Bool = false,
    send_notifications | Bool = false,
  },

  # Neural learning settings
  learning = {
    enable_learning | Bool = true,
    send_telemetry | Bool = true,  # Send anonymized patterns to improve rulesets
    privacy_mode | [| 'Full, 'Metadata, 'None |] = 'Metadata,
  },

  # Deployment settings
  deployment = {
    repos_per_batch | Number = 10,
    dry_run | Bool = true,
    create_branch | Bool = true,
    branch_name | String = "hypatia/security-scan",
    create_pr | Bool = true,
    pr_title | String = "feat: add Hypatia neurosymbolic security scanning",
  },
}

# Generate the Hypatia workflow YAML
hypatia_workflow = {
  spdx_header = "# SPDX-License-Identifier: PMPL-1.0-or-later",
  name = "Hypatia Security Scan",
  permissions = "permissions: read-all",

  on_triggers =
    "on:\n" ++
    (if config.scan_schedule.on_push then "  push:\n    branches: [main, develop]\n" else "") ++
    (if config.scan_schedule.on_pull_request then "  pull_request:\n    branches: [main]\n" else "") ++
    "  schedule:\n    - cron: '%{config.scan_schedule.weekly}'\n" ++
    "  workflow_dispatch:\n",

  jobs = {
    security_scan = {
      runs_on = "ubuntu-latest",
      steps = [
        { name = "Checkout code", uses = "actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11" },
        {
          name = "Download Hypatia rulesets",
          run = "curl -sSL %{config.hypatia_endpoints.ruleset_registry}/download/%{std.string.join "," config.scan_config.enabled_rulesets} -o rulesets.tar.gz && tar xzf rulesets.tar.gz",
        },
        {
          name = "Run Hypatia scan",
          run = "hypatia scan --depth %{std.string.from config.scan_config.scan_depth} --rulesets ./rulesets/ --report scan-report.json",
        },
        {
          name = "Upload findings",
          uses = "github/codeql-action/upload-sarif@a4784f2dad6682d68cce8299ef20b1ca931bbdfb",
          with_ = { sarif_file = "scan-report.sarif" },
        },
        {
          name = "Send learning data",
          if_ = config.learning.enable_learning,
          run = "hypatia learn --privacy %{std.string.from config.learning.privacy_mode} --endpoint %{config.hypatia_endpoints.learning_api}",
        },
      ],
    },
  },
}

# Just recipes for deployment
recipes = {
  default = {
    recipe = "deploy-workflows",
    description = "Deploy Hypatia scanning to all repos",
  },

  "check-prerequisites" = {
    description = "Verify prerequisites for deployment",
    commands = [
      # Check GitHub CLI
      "command -v gh >/dev/null 2>&1 || { echo 'Error: gh CLI not found'; exit 1; }",
      "gh auth status",

      # Check Hypatia CLI
      "command -v hypatia >/dev/null 2>&1 || { echo 'Installing Hypatia CLI...'; cargo install hypatia-cli; }",
      "hypatia --version",

      # Test ruleset registry connectivity
      "curl -sSf %{config.hypatia_endpoints.ruleset_registry}/health || { echo 'Error: Cannot connect to Hypatia ruleset registry'; exit 1; }",

      "echo 'Prerequisites check complete!'",
    ],
  },

  "generate-workflow" = {
    description = "Generate Hypatia workflow YAML",
    commands = [
      "mkdir -p .k9-output",

      # Generate workflow file
      "cat > .k9-output/%{config.workflow_file_name} <<'WORKFLOW_EOF'\n" ++
      "%{hypatia_workflow.spdx_header}\n" ++
      "name: %{hypatia_workflow.name}\n\n" ++
      "%{hypatia_workflow.on_triggers}\n" ++
      "%{hypatia_workflow.permissions}\n\n" ++
      "jobs:\n" ++
      "  security-scan:\n" ++
      "    runs-on: ubuntu-latest\n" ++
      "    steps:\n" ++
      "      - name: Checkout code\n" ++
      "        uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11\n\n" ++
      "      - name: Install Hypatia CLI\n" ++
      "        run: cargo install hypatia-cli\n\n" ++
      "      - name: Download rulesets\n" ++
      "        run: |\n" ++
      "          curl -sSL %{config.hypatia_endpoints.ruleset_registry}/download/%{std.string.join "," config.scan_config.enabled_rulesets} -o rulesets.tar.gz\n" ++
      "          tar xzf rulesets.tar.gz\n\n" ++
      "      - name: Run security scan\n" ++
      "        run: |\n" ++
      "          hypatia scan \\\\\n" ++
      "            --depth %{std.string.from config.scan_config.scan_depth} \\\\\n" ++
      "            --rulesets ./rulesets/ \\\\\n" ++
      "            --report scan-report.json \\\\\n" ++
      "            --sarif scan-report.sarif\n\n" ++
      "      - name: Upload findings to GitHub Security\n" ++
      "        uses: github/codeql-action/upload-sarif@a4784f2dad6682d68cce8299ef20b1ca931bbdfb\n" ++
      "        with:\n" ++
      "          sarif_file: scan-report.sarif\n\n" ++
      (if config.learning.enable_learning then
        "      - name: Send learning data to Hypatia\n" ++
        "        run: |\n" ++
        "          hypatia learn \\\\\n" ++
        "            --privacy %{std.string.from config.learning.privacy_mode} \\\\\n" ++
        "            --endpoint %{config.hypatia_endpoints.learning_api}\n"
      else
        "") ++
      "WORKFLOW_EOF",

      "echo 'Workflow generated: .k9-output/%{config.workflow_file_name}'",
      "cat .k9-output/%{config.workflow_file_name}",
    ],
  },

  "deploy-to-repo" = {
    description = "Deploy workflow to a single repository (for testing)",
    dependencies = ["check-prerequisites", "generate-workflow"],
    commands = [
      "echo 'Deploying to current repository...'",

      # Create .github/workflows directory if it doesn't exist
      "mkdir -p .github/workflows",

      # Copy workflow file
      "cp .k9-output/%{config.workflow_file_name} .github/workflows/",

      # Commit and push if not dry run
      "if [ %{std.string.from config.deployment.dry_run} = 'false' ]; then git add .github/workflows/%{config.workflow_file_name} && git commit -m 'feat: add Hypatia security scanning' && git push; else echo 'DRY RUN: No changes committed'; fi",

      "echo 'Deployment to single repo complete!'",
    ],
  },

  "deploy-to-organization" = {
    description = "Deploy workflow to all repos in organization",
    dependencies = ["check-prerequisites", "generate-workflow"],
    commands = [
      "echo 'Deploying Hypatia scanning to organization: %{config.github_org}'",
      "echo 'Dry run: %{std.string.from config.deployment.dry_run}'",
      "echo ''",

      # Get list of repos
      "gh repo list %{config.github_org} --limit 1000 --json name --jq '.[].name' > .k9-output/repos.txt",
      "echo 'Found' $(wc -l < .k9-output/repos.txt) 'repositories'",

      # Deploy to each repo
      "while read repo; do " ++
        "echo 'Processing: %{config.github_org}/$repo'; " ++
        "gh repo clone %{config.github_org}/$repo .k9-temp/$repo 2>/dev/null || true; " ++
        "if [ -d .k9-temp/$repo ]; then " ++
          "cd .k9-temp/$repo; " ++
          (if config.deployment.create_branch then "git checkout -b %{config.deployment.branch_name}; " else "") ++
          "mkdir -p .github/workflows; " ++
          "cp ../../.k9-output/%{config.workflow_file_name} .github/workflows/; " ++
          (if !config.deployment.dry_run then
            "git add .github/workflows/%{config.workflow_file_name}; " ++
            "git commit -m 'feat: add Hypatia security scanning'; " ++
            "git push origin %{config.deployment.branch_name}; " ++
            (if config.deployment.create_pr then
              "gh pr create --title '%{config.deployment.pr_title}' --body 'Automated deployment of Hypatia neurosymbolic security scanning'; "
            else "") ++
            "cd ../..; "
          else
            "echo 'DRY RUN: Would commit and push'; cd ../..; "
          ) ++
        "fi; " ++
      "done < .k9-output/repos.txt",

      "echo ''",
      "echo 'Organization-wide deployment complete!'",
    ],
  },

  "deploy-workflows" = {
    description = "Deploy Hypatia workflows (default)",
    dependencies = ["deploy-to-organization"],
    commands = [
      "echo ''",
      "echo '╔══════════════════════════════════════════════════════════╗'",
      "echo '║  ✅ Hypatia Security Scanning Deployment Complete!     ║'",
      "echo '╚══════════════════════════════════════════════════════════╝'",
      "echo ''",
      "echo 'Rulesets: %{std.string.join ", " config.scan_config.enabled_rulesets}'",
      "echo 'Scan depth: %{std.string.from config.scan_config.scan_depth}'",
      "echo 'Learning enabled: %{std.string.from config.learning.enable_learning}'",
      "echo ''",
    ],
  },
}

validation = {
  # Ensure scan depth is valid
  valid_scan_depth =
    config.scan_config.scan_depth == 'Quick
    || config.scan_config.scan_depth == 'Standard
    || config.scan_config.scan_depth == 'Deep
    | doc "Scan depth must be Quick, Standard, or Deep",

  # Ensure confidence threshold is between 0 and 1
  valid_confidence =
    config.scan_config.auto_fix_confidence_threshold >= 0.0
    && config.scan_config.auto_fix_confidence_threshold <= 1.0
    | doc "Auto-fix confidence threshold must be between 0.0 and 1.0",

  # Ensure at least one ruleset is enabled
  has_rulesets =
    std.array.length config.scan_config.enabled_rulesets > 0
    | doc "At least one ruleset must be enabled",

  # Ensure privacy mode is valid
  valid_privacy_mode =
    config.learning.privacy_mode == 'Full
    || config.learning.privacy_mode == 'Metadata
    || config.learning.privacy_mode == 'None
    | doc "Privacy mode must be Full, Metadata, or None",
}
