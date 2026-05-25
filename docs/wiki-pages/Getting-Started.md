<!-- SPDX-License-Identifier: MPL-2.0 -->
# Getting Started

Five minutes from clone to your first scan.

## Prerequisites

- Elixir 1.18+ and Erlang/OTP 27 (for the Elixir scanner + supervision tree)
- Rust 1.94+ (for the high-throughput CLI workers)
- `git` (every clone path)
- Optional: `panic-attack` on `$PATH` if you want closed-loop verification of fixes

## Install

```bash
git clone https://github.com/hyperpolymath/hypatia.git
cd hypatia
mix deps.get
mix escript.build       # builds the `hypatia` escript scanner
```

## Run your first scan

```bash
# Scan the current directory
./hypatia scan . --format text

# JSON output for piping
./hypatia scan . --format json | jq '.[] | select(.severity == "high")'

# Native SARIF for IDE / GitHub Code Scanning
./hypatia scan . --format sarif > findings.sarif

# GitHub Actions annotation format
./hypatia scan . --format github
```

`--severity` controls the noise floor (`critical` | `high` | `medium` | `low`). Default is `medium`.

## Start the supervision tree (full Hypatia, with watcher and dashboards)

```bash
iex -S mix
```

Then visit:

| URL | What |
|---|---|
| `http://localhost:9090/` | Live HTML dashboard |
| `http://localhost:9090/api/status` | Watcher snapshot JSON |
| `http://localhost:9090/metrics` | Prometheus exposition |
| `http://localhost:9090/api/events` | Server-Sent Events stream |

The HTML dashboard is loopback-only by default. For remote access:

```bash
export HYPATIA_API_BEARER_TOKEN=$(openssl rand -hex 32)
```

Then call with `Authorization: Bearer $HYPATIA_API_BEARER_TOKEN`.

## Terminal dashboard

```bash
mix hypatia.watch
```

Refreshes every 2 seconds. ANSI-based, no external dependencies. `--url http://host:9090/api/status` to watch a remote instance.

## Recipe health

```bash
mix hypatia.recipe_health
mix hypatia.recipe_health --only-actionable
mix hypatia.recipe_health --format json
```

Shows per-recipe dispatch counts, success rate, and **verification rate** (fraction of "successes" that re-scan confirmed clean). Recipes below 0.30 over ≥ 5 verifiable outcomes are auto-quarantined by `FleetDispatcher`.

## Closed-loop outcome recording

If you're integrating Hypatia from a bash dispatcher:

```bash
mix hypatia.record_outcome \
  --recipe   recipe-pin-action-sha \
  --repo     hyperpolymath/007-lang \
  --file     .github/workflows/ci.yml \
  --outcome  success
```

Exit codes:
- `0` — recorded; verified clean (or scan unavailable)
- `2` — recorded; re-scan still finds the weak point → caller should rollback
- `1` — bad args / unrecoverable error

See [`docs/operations/dispatch-runner-contract.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/operations/dispatch-runner-contract.adoc) for the full contract.

## Next steps

- **End-user usage** → [`docs/guides/user-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/user-guide.adoc)
- **Adding a rule** → [`docs/guides/developer-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/developer-guide.adoc)
- **Deployment** → [`docs/guides/admin-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/admin-guide.adoc)
- **VCL queries** → [`docs/guides/vcl-usage-guide.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/vcl-usage-guide.adoc)
- **API surface** → [`docs/guides/api-reference.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/guides/api-reference.adoc)
- **Architecture** → [Architecture wiki page](Architecture)
