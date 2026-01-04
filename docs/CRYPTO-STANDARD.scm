;; SPDX-License-Identifier: MPL-2.0
;; CRYPTO-STANDARD.scm - Hyperpolymath Cryptographic Standards
;; Media-Type: application/crypto-standard+scheme

(crypto-standard
  (version "1.0")
  (last-updated "2025-01-04")
  (principle "Post-quantum ready, proven algorithms, flat prime distributions")

  ;; ==========================================================================
  ;; REQUIRED ALGORITHMS - Use these or higher
  ;; ==========================================================================

  (password-hashing
    (required "argon2id")
    (parameters
      (memory-cost "64 MiB minimum")
      (time-cost "3 iterations minimum")
      (parallelism "4 threads minimum")
      (output-length "32 bytes minimum"))
    (banned
      ("bcrypt" "Limited to 72 bytes, no memory hardness")
      ("scrypt" "Less tunable than Argon2id")
      ("pbkdf2" "No memory hardness, vulnerable to ASICs")
      ("sha256-crypt" "Obsolete")
      ("md5-crypt" "Broken")))

  (general-hashing
    (required
      ("blake3" "Primary - fast, parallel, secure")
      ("shake3-256" "When XOF (extensible output) needed"))
    (acceptable
      ("sha3-256" "NIST standard, when BLAKE3 unavailable")
      ("sha3-512" "When 512-bit output required"))
    (banned
      ("md5" "Broken - collisions trivial")
      ("sha1" "Broken - practical collision attacks")
      ("sha256" "Use BLAKE3 instead for new code")
      ("sha512" "Use BLAKE3 instead for new code")))

  (post-quantum-signatures
    (required "dilithium5")
    (aliases "ML-DSA-87")
    (nist-level 5)
    (notes "NIST FIPS 204 standardized 2024"))

  (classical-signatures
    (required "ed448")
    (curve "Curve448-Goldilocks")
    (security-level "224-bit")
    (acceptable
      ("ed25519" "For SSH keys where Ed448 not supported")
      ("ed25519" "For legacy systems requiring smaller signatures"))
    (banned
      ("rsa" "Quantum vulnerable, use only for legacy interop with documentation")
      ("ecdsa" "Nonce reuse vulnerabilities, prefer EdDSA")
      ("dsa" "Obsolete")))

  (post-quantum-key-exchange
    (required "kyber-1024")
    (aliases "ML-KEM-1024")
    (nist-level 5)
    (notes "NIST FIPS 203 standardized 2024"))

  (classical-key-exchange
    (required "x448")
    (curve "Curve448")
    (acceptable
      ("x25519" "When Curve448 unavailable or interop required"))
    (banned
      ("ecdh-p256" "NSA curve, prefer Curve448/25519")
      ("ecdh-p384" "NSA curve")
      ("rsa-kex" "Quantum vulnerable")))

  (symmetric-encryption
    (required
      ("chacha20-poly1305" "Primary AEAD")
      ("xchacha20-poly1305" "When nonce reuse risk exists"))
    (acceptable
      ("aes-256-gcm" "Hardware acceleration available"))
    (banned
      ("aes-128-*" "Insufficient security margin")
      ("3des" "Obsolete")
      ("rc4" "Broken")
      ("ecb-mode" "Never use ECB for any cipher")))

  ;; ==========================================================================
  ;; PRIME NUMBER REQUIREMENTS
  ;; ==========================================================================

  (prime-requirements
    (source "Flat distribution from CSPRNG")
    (verification "Proven primality, not probable")
    (size-minimum "2048 bits for RSA legacy, 256+ bits for ECC")
    (banned-sources
      ("Non-cryptographic PRNGs")
      ("Time-based seeds only")
      ("Predictable sequences")))

  ;; ==========================================================================
  ;; IMPLEMENTATION REQUIREMENTS
  ;; ==========================================================================

  (implementation-requirements
    (constant-time "All comparisons must be constant-time")
    (memory-zeroing "Use zeroize crate or equivalent")
    (side-channels "Mitigate timing, cache, power analysis")
    (nonce-handling
      ("Never reuse nonces")
      ("Use XChaCha20 for random nonces")
      ("Counter mode with unique prefix per key")))

  ;; ==========================================================================
  ;; COMPLIANT REPOSITORIES
  ;; ==========================================================================

  (compliant-repos
    (full-compliance
      ("tma-mark2" "Dilithium5, Kyber-1024, BLAKE3, Argon2id, Ed448")
      ("reasonably-good-token-vault" "Full PQC stack implemented"))
    (classical-only
      ("januskey" "Needs Dilithium/Kyber addition")
      ("academic-workflow-suite" "PBKDF2 -> Argon2id, add PQC"))
    (acceptable-gaps
      ("SSH workflows" "Ed25519 acceptable - Ed448 not widely supported")
      ("JWKS/JWT" "RSA acceptable for legacy interop with documentation")))

  ;; ==========================================================================
  ;; RUST CRATE RECOMMENDATIONS
  ;; ==========================================================================

  (rust-dependencies
    (password-hashing "argon2 = \"0.5\"")
    (hashing "blake3 = \"1.5\"")
    (shake "sha3 = \"0.10\" ;; for SHAKE256")
    (signatures-pqc "pqcrypto-dilithium = \"0.5\"")
    (signatures-classical "ed448-goldilocks = \"0.9\"")
    (kex-pqc "pqcrypto-kyber = \"0.8\"")
    (kex-classical "x25519-dalek = \"2.0\"")
    (symmetric "chacha20poly1305 = \"0.10\"")
    (symmetric-aes "aes-gcm = \"0.10\"")
    (memory-safety "zeroize = \"1.8\"")
    (nist-2024-naming
      ("pqcrypto-mlkem" "ML-KEM (Kyber) with NIST names")
      ("pqcrypto-mldsa" "ML-DSA (Dilithium) with NIST names"))))
