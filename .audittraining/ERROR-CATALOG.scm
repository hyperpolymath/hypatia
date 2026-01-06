;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Security Error Catalog for cicd-hyper-a Training Data
;; Format: Guile Scheme (per RSR)
;; Generated: 2025-12-29

(define-module (audittraining error-catalog)
  #:export (error-types
            severity-levels
            fix-categories
            error-catalog))

;; Severity levels (aligned with CVSS)
(define severity-levels
  '((critical . 9.0)    ; Immediate action required
    (high . 7.0)        ; Fix within 24 hours
    (medium . 4.0)      ; Fix within 1 week
    (low . 1.0)         ; Fix when convenient
    (info . 0.0)))      ; Informational only

;; Error type categories
(define error-types
  '(code-security        ; Vulnerabilities in code
    workflow-security    ; GitHub Actions security issues
    dependency-vuln      ; Vulnerable dependencies
    process-hygiene      ; Missing policies/processes
    code-quality         ; Non-security code issues
    missing-tests        ; No test coverage
    missing-sast         ; No static analysis
    missing-fuzzing))    ; No fuzz testing

;; Fix categories
(define fix-categories
  '(auto-fixable         ; Can be fixed by script
    manual-code          ; Requires code changes
    manual-process       ; Requires process changes
    organic-activity))   ; Cannot be automated

;; Error Catalog
;; Format: (error-id type severity auto-fixable? description fix-pattern)
(define error-catalog
  '(
    ;; ===== Workflow Security =====
    (token-permissions-id
     workflow-security high #t
     "Workflow missing explicit permissions declaration"
     "Add `permissions: read-all` at workflow level")

    (pinned-dependencies-id
     workflow-security medium #t
     "GitHub Actions not SHA-pinned"
     "Replace @v4 with @SHA # v4 format")

    (workflow-linter-self-detection
     workflow-security low #t
     "Workflow linter grep matches its own comments containing 'uses:'"
     "Add grep -v filters for comment lines and specific patterns")

    (missing-action-input
     workflow-security medium #t
     "GitHub Action missing required input parameter"
     "Add 'with:' section with required inputs (e.g., toolchain: stable)")

    (codeql-language-mismatch
     workflow-security medium #t
     "CodeQL configured for languages not present in repository"
     "Update language matrix to match repo contents; use 'actions' for workflow scanning")

    (missing-workflow-permissions
     workflow-security high #t
     "Workflow does not contain permissions"
     "Same fix as token-permissions-id")

    ;; ===== Code Security =====
    (hard-coded-cryptographic-value
     code-security critical #f
     "Hard-coded secret, key, or token in source code"
     "Use environment variables or secrets manager")

    (remote-property-injection
     code-security high #f
     "Dynamic property access without validation"
     "Add allowlist validation for property names")

    (unused-local-variable
     code-quality low #t
     "Unused variable, import, function or class"
     "Remove unused code or prefix with underscore")

    (syntax-error
     code-quality medium #f
     "JavaScript/TypeScript syntax error"
     "Fix syntax error in source file")

    ;; ===== Dependency Vulnerabilities =====
    (vulnerabilities-id
     dependency-vuln high #f
     "Known vulnerabilities in dependencies"
     "Run cargo audit / npm audit and update deps")

    (unmaintained-crate
     dependency-vuln medium #f
     "Dependency is unmaintained"
     "Find alternative or fork and maintain")

    ;; ===== Process Hygiene =====
    (security-policy-id
     process-hygiene medium #t
     "Missing SECURITY.md file"
     "Add SECURITY.md with reporting instructions")

    (maintained-id
     process-hygiene low #f
     "Repository shows low activity"
     "Organic commit activity - cannot automate")

    (code-review-id
     process-hygiene medium #t
     "Pull requests merged without review"
     "Enable branch protection with required reviews")

    (branch-protection-id
     process-hygiene medium #t
     "Branch protection not enabled"
     "Enable via GitHub API or UI")

    (cii-best-practices-id
     process-hygiene low #f
     "Not registered for CII Best Practices badge"
     "Register at bestpractices.coreinfrastructure.org")

    ;; ===== Testing & Analysis =====
    (fuzzing-id
     missing-fuzzing medium #t
     "No fuzzing infrastructure configured"
     "Add ClusterFuzzLite or cargo-fuzz setup")

    (sast-id
     missing-sast medium #t
     "No static analysis configured"
     "Add CodeQL workflow with correct language matrix")

    (ci-tests-id
     missing-tests medium #t
     "No automated test workflow detected"
     "Add test workflow for project language")))

;; Helper: Get error by ID
(define (get-error id)
  (assoc id error-catalog))

;; Helper: Get all errors of a type
(define (errors-by-type type)
  (filter (lambda (e)
            (eq? (cadr e) type))
          error-catalog))

;; Helper: Get auto-fixable errors
(define (auto-fixable-errors)
  (filter (lambda (e)
            (cadddr e))
          error-catalog))

;; Helper: Get errors by severity
(define (errors-by-severity sev)
  (filter (lambda (e)
            (eq? (caddr e) sev))
          error-catalog))
