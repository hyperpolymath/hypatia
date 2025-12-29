# Hyperpolymath Release Candidates

Generated: 2025-12-29

## Summary

Analysis of hyperpolymath organization repositories for release readiness.

---

## Rust Projects (Cargo.toml)

### bunsenite
- **Current Version:** 1.0.2
- **Description:** Nickel configuration file parser with multi-language FFI bindings
- **RSR Tier:** Bronze
- **Status:** ACTIVE - Has existing releases
- **Recommendation:** Check for recent commits since 1.0.2 to determine if patch/minor bump needed

### czech-file-knife
- **Current Version:** 0.1.0 (workspace)
- **Description:** Multi-provider file management toolkit with FUSE VFS
- **Structure:** Workspace with 8 crates (cfk-core, cfk-providers, cfk-cache, cfk-search, cfk-vfs, cfk-cli, cfk-integrations, cfk-ios)
- **Recommendation:** READY FOR INITIAL RELEASE - If functionality is complete, tag v0.1.0

### echidna
- **Current Version:** 0.1.0
- **Description:** Extensible Cognitive Hybrid Intelligence for Deductive Neural Assistance
- **Keywords:** theorem-proving, neural, neurosymbolic, formal-verification
- **Recommendation:** READY FOR INITIAL RELEASE - If MVP is complete

### ubicity
- **Current Version:** 0.3.0 (wasm crate: ubicity-wasm)
- **Description:** WASM-based utility library
- **Note:** Main package.json and Cargo.toml not found in root - wasm/ directory contains the Rust code
- **Recommendation:** Check if 0.3.0 is released; if not, tag it

### idaptik
- **Current Version:** 0.1.0 (idaptik-core)
- **Description:** Bevy-based game/simulation (uses bevy 0.17, bevy_rapier2d 0.31)
- **Recommendation:** NEEDS WORK - Likely in early development

### project-wharf
- **Current Version:** 0.1.0 (workspace)
- **Description:** The Sovereign Web Hypervisor - Immutable CMS Infrastructure
- **Structure:** Workspace with crates (wharf-core, wharf-cli, yacht-agent, wharf-ebpf)
- **Recommendation:** READY FOR INITIAL RELEASE - Complex project, verify MVP completion

### disinfo-nesy-detector
- **Current Version:** 0.1.0
- **Description:** Neuro-Symbolic AI Disinformation Detector
- **License:** Apache-2.0
- **Recommendation:** READY FOR INITIAL RELEASE - Verify NATS/Prometheus integration is working

---

## JavaScript/TypeScript/Deno Projects

### supernorma
- **Config:** Uses deno.json (following RSR - no Node/npm)
- **Status:** Unknown version - deno.json check failed
- **Recommendation:** INVESTIGATE - Check deno.json for version field

### llm-verify
- **Status:** No Cargo.toml or package.json found in root
- **Recommendation:** NOT APPLICABLE - May be specification/documentation repo

### polyglot-i18n
- **Status:** API rate limit prevented checking
- **Recommendation:** INVESTIGATE - Check manually

### thejeffparadox
- **Status:** API rate limit prevented checking
- **Notes:** Uses GitHub Rulesets (not branch protection), per CLAUDE.md
- **Recommendation:** INVESTIGATE - Check manually

---

## Repos Without Version Files

The following repos from the org do not appear to have Cargo.toml or package.json in the root:
- gitvisor
- januskey (specification repo)
- hackenbush-ssg
- git-seo
- llm-verify
- thejeffparadox
- raze-tui
- safe-brute-force
- obli-fs
- tree-navigator
- alkahest-shell-transmuter
- bebop-v-ffi
- fogbinder
- laminar
- megadog
- volumod
- slopctl
- synapse
- phronesis
- refugia
- kith
- neurosym-scm
- llm-antidote

These may be:
1. Specification/documentation repos (no code)
2. Early-stage repos (code not yet started)
3. Using different build systems (Gleam, Julia, OCaml, etc. per RSR)
4. Nested structure (version file in subdirectory)

---

## Release Recommendations Summary

| Repo | Version | Recommendation | Priority |
|------|---------|----------------|----------|
| bunsenite | 1.0.2 | Check for updates since last release | Medium |
| czech-file-knife | 0.1.0 | Initial release candidate | High |
| echidna | 0.1.0 | Initial release candidate | High |
| ubicity | 0.3.0 | Verify if released | Medium |
| idaptik | 0.1.0 | Needs more development | Low |
| project-wharf | 0.1.0 | Initial release candidate | High |
| disinfo-nesy-detector | 0.1.0 | Initial release candidate | High |

---

## Next Steps

1. **For initial releases (v0.1.0):**
   - Verify all CI checks pass
   - Ensure CHANGELOG.md exists
   - Create GitHub release with release notes
   - Consider publishing to crates.io (for Rust)

2. **For bunsenite (existing releases):**
   - Check commits since v1.0.2
   - Review for breaking changes vs. patches
   - Follow semver for version bump

3. **Manual verification needed:**
   - supernorma - check deno.json
   - polyglot-i18n - check package.json
   - thejeffparadox - check package.json

---

## Data Collection Note

This report was generated with partial data due to GitHub API rate limiting on the unauthenticated MCP GitHub tool. For complete analysis, run:

```bash
gh release list -R hyperpolymath/REPO_NAME
gh api repos/hyperpolymath/REPO_NAME/commits?since=DATE
```

Replace REPO_NAME with each repository name.
