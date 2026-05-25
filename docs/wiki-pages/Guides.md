<!-- SPDX-License-Identifier: MPL-2.0 -->
# Guides

Task-oriented walkthroughs. For end-to-end tutorials see the in-tree guides under [`docs/guides/`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/).

## By role

- **Users** → [`docs/guides/user-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/user-guide.adoc)
- **Developers** → [`docs/guides/developer-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/developer-guide.adoc)
- **Admins / SRE** → [`docs/guides/admin-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/admin-guide.adoc)

## By task

### Add a new detection rule

1. Define the pattern in `lib/rules/<family>.ex`. For language-specific content checks, append to `code_safety.ex` under the right `@<lang>_banned` or `@<lang>_patterns` list.
2. Add a soundness fixture under `test/soundness/fixtures/<rule_module>/<rule_id>.<ext>` and an entry in `test/soundness/manifest.json`. The fixture is a tiny snippet of the bad pattern; the manifest pins the rule's `expected_severity`.
3. Run `mix test --only soundness` to confirm the new rule fires on its fixture.
4. (Optional) write a fix script in `scripts/fix-scripts/fix-<rule>.sh` and add a recipe JSON in `data/verisim/recipes/`.

See [`docs/guides/developer-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/developer-guide.adoc) for the deep version.

### Write a recipe (mapping a rule to a fix)

Recipes live in `data/verisim/recipes/recipe-*.json`. Minimal shape:

```json
{
  "id": "recipe-my-rule",
  "target_categories": ["MyCategory"],
  "languages": ["any"],
  "triangle_tier": "eliminate",
  "confidence": 0.95,
  "fix_script": "fix-my-rule.sh"
}
```

`languages` accepts `"*"` or `"any"` as the language-agnostic sentinel. Workflow-file recipes use `"yaml"` and the matcher auto-remaps repo language to `"yaml"` for scorecard-source patterns (PR #309 commit `d2bbf75`).

### Configure cross-org federation

Add to `config/runtime.exs`:

```elixir
config :hypatia, :cross_org_policies, [
  %{
    peer_id: "rhodicorp-prod",
    base_url: "https://hypatia.rhodicorp.example",
    trust_level: :medium,
    accept_severities: [:critical, :high],
    remap_medium_to: :low,
    override_dismissals: true,
    max_age_days: 14,
    require_re_scan: true,
    bearer_token_env: "HYPATIA_PEER_RHODICORP_TOKEN",
    drift_threshold: 0.30
  }
]
```

Set `HYPATIA_PEER_RHODICORP_TOKEN` in your release env. The next `LearningScheduler` tick (5 min) will start polling. See [`lib/vcl/cross_org.ex`](https://github.com/hyperpolymath/hypatia/blob/main/lib/vcl/cross_org.ex) for full policy semantics.

### Enable alert sinks

Three pluggable sinks:

```bash
# Slack-compatible webhook (Slack incoming-webhook URL works directly)
export HYPATIA_ALERT_WEBHOOK_URL=https://hooks.slack.com/services/...

# Append-only JSONL audit log
export HYPATIA_ALERT_LOG_FILE=/var/log/hypatia-alerts.jsonl

# Cross-host federation peers (comma-separated URLs, shared bearer required)
export HYPATIA_FEDERATION_PEERS=https://hypatia-peer1,https://hypatia-peer2
```

The Log sink is always-on; the others activate on env var presence.

### Bulk-close PR-invalidated issues

When a noise-reduction PR lands, hundreds of estate issues become false positives. The triage script:

```bash
# Dry-run (default)
HYPATIA_DISPATCH_PAT=$TOKEN mix hypatia.triage_issues

# Apply per class
HYPATIA_DISPATCH_PAT=$TOKEN mix hypatia.triage_issues --apply --confirm --classes workflow_audit
HYPATIA_DISPATCH_PAT=$TOKEN mix hypatia.triage_issues --apply --confirm --classes code_safety_low
HYPATIA_DISPATCH_PAT=$TOKEN mix hypatia.triage_issues --apply --confirm --classes migration_rules_fixture
HYPATIA_DISPATCH_PAT=$TOKEN mix hypatia.triage_issues --apply --confirm --classes honest_completion_namespaced
```

Each closed issue gets a comment explaining the classification. Reopen with the `hypatia-triage-disputed` label if misclassified.

### Switch neural rebalancer strategies

```elixir
# In config/runtime.exs
config :hypatia, :neural_rebalance_strategy, :rotate    # A → B → C across cycles
# Or fix to a single strategy
config :hypatia, :neural_rebalance_strategy, :b         # adversarial perturbation
```

Run `mix hypatia.strategy_effectiveness --days 30` periodically to compare strategies once data accumulates.

### Run the soundness gate

```bash
mix test --only soundness                          # 13 in-process fixtures
bash test/soundness/run-escript-soundness.sh       # end-to-end (rebuilds escript)
```

The escript variant catches packaging regressions — the exact PR #278 bug class where the escript silently dropped entire rule families.

## CI/CD integration

Drop this into any consuming repo as `.github/workflows/hypatia-scan.yml`:

```yaml
# See https://github.com/hyperpolymath/hypatia/blob/main/.github/workflows/hypatia-scan.yml
# for the canonical version; copy it as-is.
```

The workflow clones Hypatia, builds the scanner, runs it against the repo, uploads SARIF to GitHub Code Scanning, and (in Phase 2) submits medium+ findings to gitbot-fleet for learning. Low-severity findings stay advisory-only and don't generate cross-repo dispatches.

## More

- **All in-tree docs** → [`docs/README.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/README.adoc)
- **Operational runbooks** → [`docs/operations/`](https://github.com/hyperpolymath/hypatia/blob/main/docs/operations/)
- **VCL reference** → [`docs/guides/vcl-usage-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/vcl-usage-guide.adoc)
