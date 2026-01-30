;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Meta-level information for hypatia
;; Media type: application/vnd.meta+scm

(define meta
  `((metadata
      ((version . "1.0.0")
       (schema-version . "1.0")
       (created . "2026-01-30")
       (updated . "2026-01-30")
       (project . "hypatia")))
    (architecture-decisions
      ((adr-001 . ((status . "accepted")
                   (date . "2026-01-30")
                   (context . "Template validation and registry")
                   (decision . "Use Rust for performance and safety")
                   (consequences . "Faster execution, memory safety, steep learning curve")))))
    (development-practices
      ((code-style . "rustfmt default")
       (security . "cargo-audit, ClusterFuzzLite")
       (testing . "cargo test, fuzzing")
       (versioning . "semantic versioning")
       (documentation . "rustdoc")
       (branching . "GitHub Flow")))
    (design-rationale
      ((why-rust . "Performance, safety, ecosystem")
       (why-github-actions . "CI/CD automation, security scanning")))))
