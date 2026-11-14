---
# SPDX-License-Identifier: CC-BY-SA-4.0
name: estate-gates
description: The checklist to run before a push or a pull request in hypatia, covering language, formats, licence, scanner hygiene, counts and the checks to run. Use before you commit, push or open a pull request, and when a CI gate fails.
license: CC-BY-SA-4.0
---

# Gates before a push

Reach green by meeting the gate. Do not skip a test, weaken a check or ask
for an override. If a gate looks wrong, report it and stop.

## 1. What must not change

- `LICENSE`, `LICENSES/`, `NOTICE`, and each existing `SPDX-License-Identifier`
  line. Licence edits are manual and owner-only. A licence finding is a report,
  never a fix.
- `.github/workflows/actions.lock`. A tool owns it. Do not edit or reformat it.
- `.hypatia-baseline.json`. A new baseline entry is the last resort and needs
  the owner. A ratchet check rejects growth without a `Ratchet-exception:` line.

## 2. New files

- Code and configuration carry `SPDX-License-Identifier: MPL-2.0`.
- Documents (`.adoc`, `.md`) carry `SPDX-License-Identifier: CC-BY-SA-4.0`.
- Documents are `.adoc`. Use `.md` only where a tool fixes the name
  (`SKILL.md`, agent files, `CLAUDE.md`).
- No new `.a2ml` file. No new Python, TypeScript, ReScript, Go or Java source.
- A `.json` file is in canonical form: sorted keys, no spaces, one final line feed.
- No secret. A test credential starts with `test-`, `dummy-`, `fake-`,
  `example-` or `placeholder-`. Links use HTTPS.

## 3. Checks to run

Run the checks that match your change. Copy each result line for the receipt.

| You changed | Run |
|---|---|
| Elixir | `mix compile --warnings-as-errors`, `mix test`, and `mix format --check-formatted <file>` for each file that was formatted before your change |
| `src/Hypatia/ABI/` | `bash scripts/check-verdicts.sh` (needs Idris 2) |
| Rust | `cargo fmt --all -- --check`, `cargo clippy`, `cargo test --workspace --locked` |
| `ffi/zig/` | `zig build test` in `ffi/zig` |
| a shell script | `shellcheck` on the file |
| an `.adoc` file | `asciidoctor --failure-level WARN -o /dev/null <file>` |
| README, EXPLAINME, docs index, `lib/rules`, Mix tasks, the Justfile | `bash scripts/check-readme-claims.sh` |
| anything | `mix escript.build && ./hypatia scan . --format json` |

Hypatia scans itself in CI. For a finding on a file that you changed, use
this order: fix the code; then an inline directive with a reason
(`# hypatia: allow <module>/<rule> -- <reason>`); then `.hypatia-ignore`.
The word `eval` in a shell script and a literal temporary directory path are
2 common findings. Rewrite the line.

## 4. Counts are claims

`.claude/CLAUDE.md` and `README.adoc` quote counts (rule modules, Mix tasks,
recipes, exported functions). If you add or remove one of those things,
measure again and change the number in the same pull request.

## 5. The commit and the pull request

- Use the conventional form: `fix(scope): what changed`.
- No model name in a commit message or a pull request body.
- End a commit with `Co-Authored-By: Claude <noreply@anthropic.com>`.
- Open the pull request as a draft, with What, Why, Evidence and Not done.
- Do not merge. Do not approve. Do not close an issue. Do not ask a review
  bot for a new review unless the owner asked for it.
- A commit without a signature in a cloud session is a platform matter. Do
  not add a key to a container or a repository to change it.

## 6. A red gate

Find the cause before you change anything. A failure is not yours only when
it is red on `main` too, or when it names a service that your diff does not
touch. Say which, with the run link. Re-run a job once at most.
