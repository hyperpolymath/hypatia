;; SPDX-License-Identifier: MPL-2.0
;; STANDARDIZATION-CHECKLIST.scm - Machine-readable checklist for repo standardization
;; Media-Type: application/checklist+scheme

(standardization-checklist
  (version "1.0")
  (last-updated "2025-01-04")
  (campaign "RSR 2026 Standardization")

  (repo-requirements
    (structure
      (required
        (".machine_readable/6scm/STATE.scm" "Project state")
        (".machine_readable/6scm/META.scm" "Meta-level info")
        (".machine_readable/6scm/ECOSYSTEM.scm" "Ecosystem position")
        (".machine_readable/6scm/PLAYBOOK.scm" "Operational runbook")
        (".machine_readable/6scm/AGENTIC.scm" "AI interaction patterns")
        (".machine_readable/6scm/NEUROSYM.scm" "Neurosymbolic config")
        ("tasks/Justfile" "Build tasks")
        ("licenses/MPL-2.0-EN.txt" "License file")
        ("README.adoc" "Documentation")
        ("SECURITY.md" "Security policy")
        ("LICENSE" "License"))
      (recommended
        ("CONTRIBUTING.adoc" "Contribution guide")
        ("CODE_OF_CONDUCT.md" "Community standards")
        ("AGENTS.md" "AI instructions")
        ("DISCOVERY.json" "Machine manifest")
        (".editorconfig" "Editor config")))

    (banned
      ("STATE.scm at root" "Move to .machine_readable/6scm/")
      ("META.scm at root" "Move to .machine_readable/6scm/")
      ("ECOSYSTEM.scm at root" "Move to .machine_readable/6scm/")
      ("Dockerfile" "Use Containerfile with nerdctl/podman")
      ("docker-compose.yml" "Use compose.yml with nerdctl/podman")
      ("package.json for runtime" "Use deno.json imports")
      ("node_modules/" "Use Deno caching")
      (".ts files" "Convert to ReScript")
      (".go files" "Convert to Rust")))

  (workflow-requirements
    (security
      ("SPDX header" "First line of all source files")
      ("permissions: read-all" "All workflow files")
      ("SHA-pinned actions" "No @v1, @main tags")
      ("CodeQL enabled" "Appropriate language matrix")
      ("Scorecard enabled" "OpenSSF Scorecard"))
    (ci
      ("Build verification" "cargo build / deno check / etc")
      ("Test execution" "cargo test / deno test / etc")
      ("Lint checking" "clippy / rescript format / etc")))

  (bot-repos
    (completed
      ("glambot" "Presentation quality" "compiles")
      ("finishing-bot" "Release readiness" "compiles"))
    (existing
      ("rhodibot" "RSR compliance" "active")
      ("echidnabot" "Formal verification" "active")
      ("oikos" "Eco/economic standards" "active")
      ("seambot" "Integration testing" "active"))
    (pending
      ("gitbot-fleet" "Fleet orchestration" "needs-docs")))

  (satellite-protocol
    (satellite-commits
      ("Update STATE.scm with changes")
      ("Note parent repo in commit message")
      ("Document in ECOSYSTEM.scm relationship"))
    (parent-responsibilities
      ("Track satellites in ECOSYSTEM.scm")
      ("Review satellite updates")
      ("Update documentation as needed")))

  (crypto-requirements
    (reference "CRYPTO-STANDARD.scm")
    (principle "Post-quantum ready, proven algorithms, flat prime distributions")

    (password-hashing
      (required "argon2id")
      (parameters
        (memory-cost "64 MiB minimum")
        (time-cost "3 iterations minimum")
        (parallelism "4 threads minimum"))
      (banned "bcrypt" "scrypt" "pbkdf2" "sha256-crypt" "md5-crypt"))

    (general-hashing
      (required "blake3" "shake3-256")
      (acceptable "sha3-256" "sha3-512")
      (banned "md5" "sha1" "sha256" "sha512"))

    (post-quantum-signatures
      (required "dilithium5")
      (nist-name "ML-DSA-87")
      (nist-level 5))

    (classical-signatures
      (required "ed448")
      (acceptable "ed25519" "SSH keys only")
      (banned "rsa" "ecdsa" "dsa"))

    (post-quantum-key-exchange
      (required "kyber-1024")
      (nist-name "ML-KEM-1024")
      (nist-level 5))

    (classical-key-exchange
      (required "x448")
      (acceptable "x25519")
      (banned "ecdh-p256" "ecdh-p384" "rsa-kex"))

    (symmetric-encryption
      (required "chacha20-poly1305" "xchacha20-poly1305")
      (acceptable "aes-256-gcm")
      (banned "aes-128-*" "3des" "rc4" "ecb-mode"))

    (prime-requirements
      (source "CSPRNG flat distribution")
      (verification "Proven primality, not probable")
      (banned-sources "Non-cryptographic PRNGs" "Time-based seeds only"))

    (compliance-status
      (full-compliance
        ("tma-mark2" "Full PQC stack")
        ("reasonably-good-token-vault" "Full PQC stack")
        ("academic-workflow-suite" "Updated: Argon2id, BLAKE3, Dilithium5, Kyber-1024, Ed25519"))
      (classical-compliant
        ("januskey" "Argon2id, Ed25519, AES-256-GCM, SHA-256 - needs PQC for signatures/KEX"))
      (acceptable-gaps
        ("SSH workflows" "Ed25519 acceptable - Ed448 not widely supported")
        ("JWKS/JWT" "RSA acceptable for legacy interop with documentation")
        ("Content addressing" "SHA-256 acceptable for integrity verification"))))

  (progress
    (repos-processed 109)
    (repos-remaining "~300")
    (jekyll-removed 26)
    (bots-created 2)
    (bots-updated 1)))
