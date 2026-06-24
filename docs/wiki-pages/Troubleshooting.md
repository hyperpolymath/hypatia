<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Troubleshooting

Symptoms → diagnosis → fix, ordered roughly by how often each comes up.

## "Hundreds of hypatia-labelled issues in my repos"

**Cause.** Almost always Hypatia's scanner running an older ruleset that's now over-noisy. The biggest historical classes:

- `workflow_audit/missing_workflow` for 12 retired workflows (consolidated by `governance.yml`)
- Low-severity unwrap/expect findings in cli/bin/fixer paths (advisory only — shouldn't dispatch)
- `migration_rules/deprecated_api` firing on soundness fixtures
- `honest_completion/no_state_file` flagging the namespaced layout

All four classes are suppressed since PR #314.

**Fix.** Wait for the next sweep to run with the fixed scanner, then bulk-close the stale issues:

```bash
HYPATIA_DISPATCH_PAT=$TOKEN mix hypatia.triage_issues               # dry-run
HYPATIA_DISPATCH_PAT=$TOKEN mix hypatia.triage_issues --apply --confirm --classes workflow_audit
# ... then per-class repeat
```

## hypatia-scan workflow shows ~100 findings on every PR

**Cause.** The workflow clones Hypatia from `main` (per line 64), not from the PR branch. So fixes to the noise floor only take effect once a PR with rule changes lands on main. Before that, the scan's PR comment shows the OLD ruleset against NEW code.

**Fix.** This is self-correcting. The post-merge run uses the fixed scanner. If you need to test a rule change against the PR's own scanner, run `./hypatia scan .` locally.

## `mergeable_state: dirty` keeps appearing on my PR

**Cause.** This branch's earlier squash-merges landed on main, leaving file-content overlap that git sees as divergent. Common when GitHub auto-merges a partial slice of an active branch.

**Fix.**
```bash
git fetch origin main
git merge origin/main
# Resolve conflicts (usually "take ours" since the branch is the strict superset)
git checkout --ours <conflicting-files>
git add <conflicting-files>
git commit
git push
```

## `governance / Language / package anti-pattern policy` check fails

**Cause.** A file in a banned language exists in the tree. The ban is **total** since PR #280 — no path-based suppression, including `test/soundness/fixtures/`. Banned: Python (`.py`), Go (`.go`), TypeScript (`.ts`), ReScript (`.res`), Makefile, package-lock.json.

**Fix.** Either delete the file or port it. If it's a soundness fixture, port the test to an inline string (see `test/code_safety_test.exs:118` for the pattern).

## Recipe shows `:insufficient_data` forever

**Cause.** The verification metric needs `verification` markers on outcome records. If the dispatch-runner isn't calling `mix hypatia.record_outcome` (which sets the marker), all outcomes log as `unverified` and the rate stays uncomputed.

**Fix.** Wire the dispatch-runner per [`docs/operations/dispatch-runner-contract.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/operations/dispatch-runner-contract.adoc). Lives in gitbot-fleet, not here.

## Watcher dashboard empty / "loading..." forever

**Diagnose.**
```bash
curl http://localhost:9090/api/status              # should return JSON
curl http://localhost:9090/health                  # should return {"status":"ok"}
iex -S mix                                         # then: Process.whereis(Hypatia.Watcher)
```

If `:status` returns `{"status":"unavailable"}`, the Watcher GenServer isn't running. Check supervisor logs.

If `/api/status` returns 403, you're not on loopback — set `HYPATIA_API_BEARER_TOKEN` and pass `Authorization: Bearer …` on the request.

## Alerts not firing despite obvious triggers

**Diagnose.**
```bash
iex -S mix
> Hypatia.Watcher.Alerts.recent()
> Hypatia.Watcher.Alerts.tick_now()           # force evaluation
```

If `recent/0` returns `[]` but you expect alerts:
- Check the rule kind matches `[:hypatia, :quarantine, :triggered] | [:hypatia, :soundness, :violation] | [:hypatia, :anomaly, :detected]` — those are the only telemetry events that emit alerts directly.
- Tick rules (`queue_depth_high`, `events_dropped`) only fire on the 30-second tick — try `tick_now/0`.

## Neural networks "cold" after every restart

**Cause.** Either the persist dir isn't writable, or the network's state isn't in the hydrated list.

**Diagnose.** Check `data/verisim/neural-states/` for fresh `.json` files. The boot log should say which networks restored:
```
Neural Coordinator initialised: blackboard architecture, 8 networks, 6 phases
(RBF restored from persisted state) (MoE restored: 3 experts ...) (trust scores restored ...)
(gnn/vae/sequence restored)
```

If the parenthetical is empty after multiple cycles, the save side is failing. Permissions and disk space first.

## "Failed to lookup telemetry handlers" log spam

**Cause.** `:telemetry` application isn't fully booted. Usually harmless in tests / one-off scripts; benign warning.

**Fix.** In CI / production it should never appear because the OTP application starts `:telemetry` as a dependency. If it appears in production logs, check `:telemetry` is in `extra_applications` in `mix.exs`.

## VCL query returns empty unexpectedly

**Diagnose.**
```bash
iex -S mix
> Hypatia.VCL.Client.execute("SELECT * FROM scans LIMIT 1;")
> Hypatia.VerisimConnector.fetch_all_scans() |> length()
```

If `fetch_all_scans/0` returns 0, the verisim-data store isn't populated locally. Check `:hypatia, :verisimdb_data_path` config and the path's contents.

## Cross-org federation gives 401 from peer

**Cause.** Peers don't share `HYPATIA_API_BEARER_TOKEN`, OR the peer's auth_gate plug is enabled but you're sending no bearer.

**Fix.** Set the same token env on every peer; verify with:
```bash
curl -H "Authorization: Bearer $TOKEN" $PEER_URL/api/status
```

## `mix compile` warnings about Jason undefined

**Cause.** Running the compiler outside `mix` (e.g. direct `elixirc`) without dependencies vendored.

**Fix.** Use `mix compile` (resolves deps via Hex). The warning is benign in `mix` builds.

## More

- Check the dashboard's alert ribbon for any auto-triggered notifications you may have missed.
- For deep neural-layer issues, see [`docs/architecture/NEURAL-ARCHITECTURE.md`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/NEURAL-ARCHITECTURE.md).
- For boundary / SNIF / verisimdb architecture questions, see [`docs/architecture/boundary-design-options.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/boundary-design-options.adoc) and issue #294.
