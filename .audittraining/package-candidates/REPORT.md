# Hyperpolymath Package Publishing Candidates

Analysis of hyperpolymath repositories suitable for publishing to package registries.

## 1. Rust/Cargo Crates (crates.io)

### bunsenite
- **Package Type**: Rust crate (library + binary)
- **Current Version**: 1.0.2
- **Publish Status**: READY - Has proper crates.io metadata
- **Crate Name**: `bunsenite`
- **Description**: Nickel configuration file parser with multi-language FFI bindings
- **Features**: cli, wasm, watch, repl, schema, full
- **Keywords**: nickel, config, parser, ffi, wasm
- **Categories**: config, parsing, wasm, api-bindings
- **License**: MIT OR Palimpsest-0.8
- **Dependencies**: nickel-lang-core, serde, serde_json, anyhow, thiserror, miette, wasm-bindgen
- **Blockers**: None - fully configured for crates.io
- **Recommended Registry**: crates.io
- **Notes**: Has WASM support, can also publish to npm via wasm-pack

### czech-file-knife
- **Package Type**: Rust workspace (multiple crates)
- **Current Version**: 0.1.0
- **Publish Status**: NEEDS WORK - Workspace needs per-crate publish config
- **Workspace Members**:
  - cfk-core
  - cfk-providers
  - cfk-cache
  - cfk-search
  - cfk-vfs
  - cfk-cli
  - cfk-integrations
  - cfk-ios
- **Description**: Universal file management tool with cloud provider integration
- **License**: AGPL-3.0-or-later
- **Dependencies**: tokio, serde, reqwest, oauth2, blake3, lz4_flex, fuser, tantivy
- **Blockers**:
  - Each workspace member needs its own Cargo.toml with publish metadata
  - Need to add description, repository, license to each sub-crate
  - fuser dependency may cause platform-specific issues
- **Recommended Registry**: crates.io (multiple crates in workspace)
- **Notes**: Multi-platform file manager, iOS support via cfk-ios

### ubicity (wasm component)
- **Package Type**: Rust WASM crate
- **Location**: wasm/Cargo.toml
- **Current Version**: 0.3.0
- **Publish Status**: NEEDS WORK - Missing crates.io metadata
- **Crate Name**: `ubicity-wasm`
- **Description**: (Missing - needs to be added)
- **Dependencies**: wasm-bindgen, serde, serde_json, getrandom
- **Blockers**:
  - Missing description, repository, license in Cargo.toml
  - Missing keywords and categories
- **Recommended Registry**: crates.io + npm (via wasm-pack)
- **Notes**: WASM component for ubicity learning capture system

### polyglot-i18n (wasm component)
- **Package Type**: Rust WASM crate
- **Location**: wasm/Cargo.toml
- **Current Version**: 1.0.0
- **Publish Status**: NEEDS WORK - Missing crates.io metadata
- **Crate Name**: `i18n-wasm`
- **Description**: (Missing - needs to be added)
- **Dependencies**: wasm-bindgen, serde, serde_json, serde-wasm-bindgen
- **Blockers**:
  - Missing description, repository, license, authors in Cargo.toml
  - Missing keywords and categories
- **Recommended Registry**: crates.io + npm (via wasm-pack)
- **Notes**: WASM component for polyglot-i18n internationalization library

---

## 2. Deno/JSR Packages

### ubicity
- **Package Type**: Deno module
- **Current Version**: 0.3.0
- **Publish Status**: READY - Has JSR-compatible deno.json
- **Package Name**: `@ubicity/core`
- **Description**: Learning capture system for urban informal education
- **Exports**:
  - `.` -> `./src/index.ts`
- **Dependencies**: @std/assert, @std/path, @std/fs, @std/crypto, @std/testing, zod
- **Blockers**: None - properly configured for JSR
- **Recommended Registry**: JSR (jsr.io)
- **Notes**: Has both Deno and Node.js compatibility

### polyglot-i18n
- **Package Type**: Deno module (ReScript compiled)
- **Current Version**: 2.0.0-beta
- **Publish Status**: READY - Has JSR publish config
- **Package Name**: `@polyglot/i18n`
- **Exports**:
  - `.` -> `./src/lib/es6/src/mod.res.mjs`
  - `./i18n` -> I18n module
  - `./locale` -> Locale module
  - `./catalog` -> Catalog module
  - `./plural` -> Plural module
  - `./relative-time` -> RelativeTime module
- **Publish Includes**: src/lib/es6/, wasm/pkg/, locales/, LICENSE.txt, README.adoc
- **Dependencies**: @std/assert, @std/testing, @std/fs, @std/path, @std/json, @std/yaml, @std/toml
- **Blockers**: None - has proper publish configuration
- **Recommended Registry**: JSR (jsr.io)
- **Notes**: ReScript-based i18n library with WASM acceleration

---

## 3. npm Packages

### ubicity
- **Package Type**: npm module
- **Current Version**: 0.2.0
- **Publish Status**: NEEDS REVIEW - Has package.json but minimal config
- **Package Name**: `ubicity`
- **Description**: Learning capture system for urban informal education
- **Main**: `src/index.js`
- **Binary**: `ubicity` -> `./src/cli.js`
- **Dependencies**: zod
- **Dev Dependencies**: eslint, prettier
- **Blockers**:
  - Consider if npm publish is needed given JSR version exists
  - May need to add files field to limit published content
- **Recommended Registry**: npm (if Node.js compatibility needed) or JSR
- **Notes**: Dual-publish to npm and JSR possible but may cause confusion

---

## 4. Container Images (GHCR)

### czech-file-knife
- **Package Type**: Container image
- **Containerfile**: Yes (Containerfile at root)
- **Publish Status**: READY
- **Base Image**: cgr.dev/chainguard/wolfi-base
- **Build Image**: rust:1.83-slim
- **Description**: Universal file management tool container
- **Entrypoint**: `/usr/local/bin/cfk`
- **User**: Non-root (cfk, uid 1000)
- **Dependencies**: fuse3, fuse3-libs, ca-certificates
- **Recommended Registry**: ghcr.io/hyperpolymath/czech-file-knife
- **Notes**: Multi-stage build, minimal attack surface with Wolfi base

### ubicity
- **Package Type**: Container image
- **Containerfile**: Yes (Containerfile at root)
- **Publish Status**: READY
- **Base Image**: node:20-alpine
- **Description**: UbiCity learning capture system container
- **Volumes**: /app/ubicity-data
- **Command**: `node src/cli.js help`
- **Recommended Registry**: ghcr.io/hyperpolymath/ubicity
- **Notes**: Simple Node.js container, consider migrating to Deno for RSR compliance

### gitvisor
- **Package Type**: Container image
- **Containerfile**: Yes (Containerfile at root)
- **Publish Status**: READY - Complex multi-stage build
- **Base Image**: cgr.dev/chainguard/wolfi-base
- **Components**:
  - Elixir/Phoenix backend
  - ReScript frontend (compiled)
  - Ada TUI
- **Exposed Port**: 4000
- **Health Check**: Yes (HTTP check on /api/v1/health)
- **User**: Non-root (gitvisor, uid 1000)
- **Recommended Registry**: ghcr.io/hyperpolymath/gitvisor
- **Notes**: Multi-language polyglot container, includes backend + frontend + TUI

### polyglot-i18n
- **Package Type**: Container image
- **Containerfile**: Yes (container/Containerfile)
- **Publish Status**: READY
- **Additional**: Has apko.yaml for distroless builds
- **Recommended Registry**: ghcr.io/hyperpolymath/polyglot-i18n
- **Notes**: Multiple container formats supported (Containerfile + apko)

### thejeffparadox
- **Package Type**: Container image
- **Containerfile**: Yes (container/Containerfile)
- **Publish Status**: READY
- **Additional**: Has podman-compose.yml for orchestration
- **Recommended Registry**: ghcr.io/hyperpolymath/thejeffparadox
- **Notes**: Multi-node distributed system container

---

## 5. Haskell Packages (Hackage)

### llm-verify (claude-verify)
- **Package Type**: Haskell library + executable
- **Current Version**: 0.1.0.0
- **Publish Status**: NEEDS WORK - Ready structure but needs testing
- **Package Name**: `claude-verify`
- **Description**: Real verification for LLM-generated code via ECHIDNA integration
- **Library Modules**: ClaudeVerify, ClaudeVerify.EchidnaClient, ClaudeVerify.PropertyExtraction, etc.
- **Executable**: claude-verify
- **License**: MIT
- **Tested With**: GHC 9.4.8, 9.6.4, 9.8.2
- **Dependencies**: aeson, megaparsec, sbv, what4, sqlite-simple, optparse-applicative
- **Blockers**:
  - Needs thorough testing before Hackage upload
  - May need to verify all dependencies are on Hackage
  - sbv and what4 are SMT-related; verify compatibility
- **Recommended Registry**: Hackage
- **Notes**: Formal verification library integrating with ECHIDNA theorem prover

---

## 6. Julia Packages (General Registry)

### thejeffparadox/engine
- **Package Type**: Julia package
- **Location**: engine/Project.toml
- **Publish Status**: NEEDS INVESTIGATION
- **Description**: Likely a Julia-based engine for thejeffparadox
- **Recommended Registry**: Julia General Registry
- **Blockers**:
  - Need to review Project.toml for proper UUID and version
  - Julia packages require specific registration process
- **Notes**: Julia component within larger multi-language project

---

## 7. Ada/Alire Packages

### thejeffparadox/tui
- **Package Type**: Ada application (Alire)
- **Manifest**: tui/alire.toml
- **Project File**: tui/jeff_tui.gpr
- **Publish Status**: NEEDS REVIEW
- **Description**: TUI component for thejeffparadox
- **Recommended Registry**: Alire (Ada package manager)
- **Blockers**: Need to verify alire.toml completeness
- **Notes**: SPARK/Ada TUI with formal verification potential

### polyglot-i18n/tui
- **Package Type**: Ada application
- **Project File**: tui/polyglot_tui.gpr
- **Publish Status**: NEEDS ALIRE CONFIG
- **Description**: TUI for polyglot-i18n
- **Recommended Registry**: Alire
- **Blockers**: Missing alire.toml manifest
- **Notes**: Needs Alire manifest for package publishing

### gitvisor/tui
- **Package Type**: Ada application
- **Project File**: tui/gitvisor_tui.gpr
- **Publish Status**: NEEDS ALIRE CONFIG
- **Description**: TUI for gitvisor
- **Recommended Registry**: Alire
- **Blockers**: Missing alire.toml manifest
- **Notes**: Part of larger gitvisor ecosystem

---

## 8. GitHub Actions (Marketplace)

No standalone reusable GitHub Actions were identified. The repositories contain project-specific workflows but no packaged actions suitable for GitHub Marketplace.

**Potential Action Candidates** (if extracted as standalone actions):
- RSR antipattern checker (supernorma/.github/workflows/rsr-antipattern.yml)
- TypeScript blocker (supernorma/.github/workflows/ts-blocker.yml)
- npm/Bun blocker (supernorma/.github/workflows/npm-bun-blocker.yml)
- Well-known enforcement (supernorma/.github/workflows/wellknown-enforcement.yml)

These could be extracted into reusable actions in a dedicated repo like `hyperpolymath/actions`.

---

## Summary Table

| Repository | Package Type | Registry | Status | Priority |
|------------|-------------|----------|--------|----------|
| bunsenite | Rust crate | crates.io | READY | HIGH |
| czech-file-knife | Rust workspace | crates.io | NEEDS WORK | MEDIUM |
| ubicity | Deno module | JSR | READY | HIGH |
| polyglot-i18n | Deno module | JSR | READY | HIGH |
| ubicity-wasm | Rust WASM | crates.io + npm | NEEDS WORK | LOW |
| i18n-wasm | Rust WASM | crates.io + npm | NEEDS WORK | LOW |
| czech-file-knife | Container | GHCR | READY | HIGH |
| ubicity | Container | GHCR | READY | MEDIUM |
| gitvisor | Container | GHCR | READY | HIGH |
| polyglot-i18n | Container | GHCR | READY | MEDIUM |
| thejeffparadox | Container | GHCR | READY | MEDIUM |
| llm-verify | Haskell | Hackage | NEEDS WORK | LOW |
| thejeffparadox/engine | Julia | General | NEEDS INVESTIGATION | LOW |
| thejeffparadox/tui | Ada | Alire | NEEDS REVIEW | LOW |

---

## Recommended Next Steps

1. **Immediate (READY packages)**:
   - Publish `bunsenite` to crates.io
   - Publish `@ubicity/core` to JSR
   - Publish `@polyglot/i18n` to JSR
   - Set up GHCR publishing workflows for container images

2. **Short-term (NEEDS WORK)**:
   - Add missing metadata to czech-file-knife workspace crates
   - Add crates.io metadata to ubicity-wasm and i18n-wasm
   - Complete testing for llm-verify before Hackage submission

3. **Long-term**:
   - Extract reusable GitHub Actions from supernorma
   - Set up Julia package registration for thejeffparadox/engine
   - Add Alire manifests to Ada TUI components
