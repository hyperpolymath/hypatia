<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Wiki page sources

This directory holds the **canonical source** for every page in the public GitHub wiki at <https://github.com/hyperpolymath/hypatia/wiki>.

GitHub wikis are a separate git repo (`hypatia.wiki.git`) that we can't push to from this repo's CI. To update the wiki:

## Option A — copy-paste through the wiki UI

For each `.md` file in this directory:

1. Open the corresponding wiki page on github.com (e.g. `wiki/Home`, `wiki/Architecture`).
2. Click "Edit".
3. Paste the file's contents.
4. Save.

The file names map 1:1 to wiki page slugs:

| Source | Wiki page |
|---|---|
| `Home.md` | <https://github.com/hyperpolymath/hypatia/wiki/Home> |
| `Architecture.md` | <https://github.com/hyperpolymath/hypatia/wiki/Architecture> |
| `Getting-Started.md` | <https://github.com/hyperpolymath/hypatia/wiki/Getting-Started> |
| `Guides.md` | <https://github.com/hyperpolymath/hypatia/wiki/Guides> |
| `FAQ.md` | <https://github.com/hyperpolymath/hypatia/wiki/FAQ> |
| `Troubleshooting.md` | <https://github.com/hyperpolymath/hypatia/wiki/Troubleshooting> |

## Option B — clone the wiki repo and `cp` across

```bash
git clone https://github.com/hyperpolymath/hypatia.wiki.git
cd hypatia.wiki

# Copy every page source from the main repo
for f in /path/to/hypatia/docs/wiki-pages/*.md; do
  name=$(basename "$f")
  if [ "$name" != "README.md" ]; then
    cp "$f" "./$name"
  fi
done

git add .
git commit -m "Sync wiki from hypatia/docs/wiki-pages/"
git push
```

## Why keep wiki sources in the main repo?

- **Single source of truth.** Wiki edits via the UI are not version-controlled in the main repo. Keeping the source here means changes go through PR review and ship alongside related code changes.
- **Discoverability.** A future contributor checking out the main repo sees the wiki content immediately.
- **Sync auditing.** Diff `docs/wiki-pages/*.md` against the live wiki to detect drift.

## Maintenance pattern

When you change a wiki page, change the source here in the same PR. The PR description should say "wiki sync needed" so the maintainer remembers to copy across after merge. Drift between this directory and the live wiki should be treated as a bug — file an issue if you spot one.

## Current pages

- [Home.md](Home.md) — landing page, key concepts, navigation
- [Architecture.md](Architecture.md) — supervision tree, data flow, 8 neural networks, VCL
- [Getting-Started.md](Getting-Started.md) — clone → first scan in 5 minutes
- [Guides.md](Guides.md) — task-oriented walkthroughs (add a rule, configure federation, etc.)
- [FAQ.md](FAQ.md) — common questions
- [Troubleshooting.md](Troubleshooting.md) — symptom → diagnosis → fix
