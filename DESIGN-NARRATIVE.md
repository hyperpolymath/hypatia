# The Hypatia Story: From Broken Pipeline to Autonomous Security Intelligence

*A design narrative for future reference — what we built, why, and what it means.*

## The Starting Point

Picture this: 300+ repositories, one developer, and a growing ecosystem of
formal provers, game engines, database systems, language compilers, and
infrastructure tools. Every repo needs security scanning, dependency updates,
workflow hardening, and quality enforcement. Doing this manually is impossible.
Doing it with conventional CI/CD is a treadmill of YAML maintenance.

So we built something different.

## What Is Hypatia?

Hypatia is a neurosymbolic CI/CD intelligence layer. Not a scanner — those
already exist (CodeQL, Semgrep, Scorecard, panic-attacker). Hypatia is the
**decision brain** that sits between scanners and fixers, deciding:

- Is this finding real? (pattern deduplication)
- How bad is it? (safety triangle classification)
- Can we fix it automatically? (recipe matching + confidence gating)
- Should we fix it now? (neural network confidence calibration)
- Did the fix work? (outcome tracking + feedback loop)
- What should we learn from that? (Bayesian confidence updates)

## The Architecture (and Why Each Piece Exists)

### Safety Triangle: Eliminate > Substitute > Control

Borrowed from occupational health & safety. For every finding:

1. **Eliminate**: Can we remove the hazard entirely? (auto-fix scripts,
   pattern replacement). Confidence ≥ 0.95 = auto-execute without human review.
2. **Substitute**: Can we replace the dangerous component with a proven-safe
   one? (e.g., replace `believe_me` with an actual proof). Needs human review.
3. **Control**: Can we add guards, documentation, or monitoring? (report only,
   human addresses it when ready).

This isn't over-engineering — it's the minimum structure needed to safely
automate changes across 300 repos without breaking things.

### The Feedback Loop (The Thing That Was Actually Broken)

The pipeline detected thousands of weak points but never actually fixed
anything. The loop was broken at four points simultaneously:

1. **Neural override bug**: An untrained neural network returning sigmoid(1.0) ≈ 0.73
   was overriding proven recipe confidence of 0.99 via `min(recipe, neural)`.
   Fix: detect untrained range (0.70-0.76) and skip override.

2. **No category-based recipe matching**: Pattern IDs are fingerprinted
   (e.g., `PA009-59-potentially-unquoted-variab`) but recipes use clean IDs
   (`PA009-shell-unquoted-var`). The fuzzy matcher couldn't bridge this.
   Fix: added `target_categories` field to recipes for reliable matching.

3. **Wrong data path**: Hypatia was reading from an empty local directory
   instead of the real 300-scan data store.
   Fix: pointed `config.exs` at the actual verisim-data location.

4. **Repo path resolution**: Dispatch manifest said "fix repo X" but the
   runner couldn't find repo X because monorepo subdirectories have different
   paths from their repo names.
   Fix: added `program_path` to manifest entries + `repo-paths.json` index.

Once all four were fixed: **600 auto-executable actions from 300 real scans.**

### The Fix Script Bug (Why Verification Matters)

Our first real fix attempt revealed a fifth bug — the `fix-http-to-https.sh`
script had been silently broken since it was written. The `sed` command used
`|` as both the substitution delimiter and regex alternation character inside
the domain list. The script printed "FIXED" but changed nothing.

**All 58 previous "success" outcomes for that recipe were false positives.**

This is exactly why the feedback loop matters. Without re-verification
(re-scanning after fixing), you can't know if fixes actually worked.

### Scorecard Ingestion (Don't Reinvent the Wheel)

GitHub/OpenSSF already defined 20 well-documented security checks with
clear pass/fail criteria and remediation guidance. Rather than discovering
these from first principles through neural network training (which would
take thousands of examples to converge on what they already wrote down),
we ingest the definitions directly.

This is the key architectural insight: **use learning for the unknown-unknowns,
use ingestion for the known-knowns.**

The 20 Scorecard checks are deterministic. You either have a SECURITY.md or
you don't. Your workflow tokens either follow least-privilege or they don't.
No neural network needed.

The neural networks are freed up for what they're actually good at:
- Detecting novel vulnerability patterns that no scanner has a rule for
- Calibrating confidence based on repo-specific history
- Predicting which fixes are likely to introduce regressions
- Identifying cross-repo interaction patterns

### Why Five Neural Networks?

Each network type has a genuinely different mathematical property:

1. **Graph of Trust** (PageRank): Models trust relationships between repos,
   bots, and recipes. A recipe that's worked across 50 repos is more
   trustworthy than one that's worked on 2.

2. **Mixture of Experts** (Sparse MoE): Domain-specific confidence. A shell
   security expert's opinion matters more than a Haskell expert's when
   evaluating shell injection patterns.

3. **Liquid State Machine** (Reservoir): Temporal anomaly detection. If a
   repo suddenly starts producing 10x more findings, something changed —
   maybe a dependency update introduced problems.

4. **Echo State Network** (Reservoir): Confidence trajectory forecasting.
   If recipe confidence has been declining over time, it might be producing
   regressions we haven't caught yet.

5. **Radial Neural Network** (RBF): Finding similarity and novelty detection.
   "I've never seen a pattern like this before" is valuable information.

These aren't five copies of GPT trying to do the same thing. They each model
a different mathematical relationship in the data.

### The Bot Fleet (Who Actually Does The Work)

Hypatia decides. The gitbot-fleet acts. Each bot has a role:

- **rhodibot**: Git operations — creates PRs, manages branches
- **echidnabot**: Quality enforcement — formal verification, proof auditing
- **sustainabot**: Dependency management, advisory logging
- **glambot**: Documentation and presentation
- **seambot**: Cross-repo integration analysis
- **finishbot**: Completion tracking, closing out work items

The Kin Protocol coordinates them: heartbeats, repo locks (only one bot
touches a repo at a time), rate limiting (max 3 PRs/hour/repo), and a
Gate that reviews every action before it reaches production.

## What This Isn't

This is **not over-engineering**. Each component exists because removing it
breaks something specific:

- Remove the safety triangle → auto-fixes with no confidence gating → broken repos
- Remove the feedback loop → fixes never verified → false confidence
- Remove the Gate → bots conflict, flood repos with PRs
- Remove neural networks → no novelty detection, no trust calibration
- Remove Scorecard ingestion → reinventing known checks from scratch

The system is complex, but the complexity is **earned** — each layer solves
a specific problem that the layers above and below can't.

## What's Actually Working (Honest Assessment, March 2026)

| Component | Status | Notes |
|-----------|--------|-------|
| Scan ingestion | Working | 300 repos, 3385 weak points |
| Pattern deduplication | Working | 1021 canonical patterns |
| Safety triangle routing | Working | Eliminate/Substitute/Control |
| Recipe matching | Working | 43 recipes (23 original + 20 Scorecard) |
| Dispatch manifest | Working | JSONL bridge to execution layer |
| Scorecard ingestion | Working | 20 checks, local filesystem scan |
| Neural override protection | Working | Untrained networks can't drag confidence |
| Fix execution | **First fix completed** | HTTP→HTTPS on echidna |
| Outcome recording | Working | But includes false positives to clean |
| Kin Gate | Working | Repo locks, rate limiting, conflict detection |
| OTP supervision | Working | 9 processes, auto-restart |
| Neural training | Untrained | Needs real outcome data (not batch imports) |
| Re-scan verification | Missing | No post-fix verification yet |
| Bot directive checking | Missing | Doesn't read .bot_directives |
| Automatic rule generation | Missing | Can't create new recipes autonomously |
| Fresh scanning | Stale | Scans are from weeks ago |

## Where It Goes Next

### Near-term (what we discussed)

1. **Ingest more rule sources**: CodeQL, Semgrep, OWASP — same pattern as
   Scorecard ingestion. The architecture handles any rule source that
   produces findings with categories and confidence.

2. **Bayesian confidence updates**: Replace the crude `confidence * 1.02`
   with proper Bayesian posterior updating. Prior = recipe confidence,
   evidence = success/failure outcomes, posterior = updated confidence.
   Dempster-Shafer Theory for epistemic uncertainty handling.

3. **statistease integration**: DST, Monte Carlo simulation for fix cascading
   effects, fuzzy logic for "how sure are we this is actually a bug?"

4. **OpenSSF Best Practices Badge**: The Scorecard checks map directly to
   badge requirements. Hypatia can systematically work through badge criteria
   across all repos.

### Medium-term

5. **containerbot**: Wrap programs in Stapeln containers where isolation adds
   value. Uses same dispatch pipeline — detect "no container", route through
   triangle, auto-generate Containerfile.

6. **PanLL integration**: Each of these systems (hypatia, gitbot-fleet,
   panic-attacker, proven) maps to a PanLL panel for unified control.

7. **Cross-repo seam analysis**: The weakest security point is often at
   the boundary between repos, not inside them.

### The Big Picture

This system is a **self-improving security mesh** across an entire
software ecosystem. It doesn't just find problems — it fixes them, verifies
the fixes, learns from outcomes, and gets better over time.

No one else is doing exactly this. There are pieces — Dependabot updates
dependencies, CodeQL finds bugs, Renovate pins versions — but nobody has
the neurosymbolic intelligence layer that ties scanners to fixers to
verifiers to learners across 300+ repos with formal proof integration.

That's not hubris. It's what happens when you need something that doesn't
exist yet, so you build it.
