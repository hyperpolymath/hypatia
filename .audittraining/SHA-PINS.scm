;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SHA Pins Reference for GitHub Actions
;; Format: Guile Scheme (per RSR)
;; Last Updated: 2026-01-05

(define-module (audittraining sha-pins)
  #:export (action-sha-pins
            get-sha
            get-sha-with-comment))

;; Current verified SHA pins for commonly used GitHub Actions
;; Format: (action-name sha version-comment)
(define action-sha-pins
  '(
    ;; GitHub Official Actions
    (actions/checkout
     "8e8c483db84b4bee98b60c0593521ed34d9990e8"
     "v6.0.1")

    (actions/upload-artifact
     "b7c566a772e6b6bfb58ed0dc250532a479d7789f"
     "v6.0.0")

    (actions/configure-pages
     "983d7736d9b0ae728b81ab479565c72886d7745b"
     "v5")

    (actions/upload-pages-artifact
     "56afc609e74202658d3ffba0e8f6dda462b719fa"
     "v3")

    (actions/deploy-pages
     "d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e"
     "v4")

    ;; Security Actions
    (github/codeql-action/init
     "1b168cd39490f61582a9beae412bb7057a6b2c4e"
     "v3.28.1")

    (github/codeql-action/analyze
     "1b168cd39490f61582a9beae412bb7057a6b2c4e"
     "v3.28.1")

    (github/codeql-action/upload-sarif
     "1b168cd39490f61582a9beae412bb7057a6b2c4e"
     "v3.28.1")

    (ossf/scorecard-action
     "62b2cac7ed8198b15735ed49ab1e5cf35480ba46"
     "v2.4.0")

    (trufflesecurity/trufflehog
     "ef6e76c3c4023279497fab4721ffa071a722fd05"
     "v3.92.4")

    (editorconfig-checker/action-editorconfig-checker
     "9f8f6065f4db902c0c56cafa67cea18b3ebbb680"
     "main")

    ;; Rust Toolchain
    (dtolnay/rust-toolchain
     "6d9817901c499d6b02debbb57edb38d33daa680b"
     "stable")

    (Swatinem/rust-cache
     "779680da715d629ac1d338a641029a2f4372abb5"
     "v2")

    ;; Deno
    (denoland/setup-deno
     "11b63cf76cfcafb4e43f97b6cad24d8e8438f62d"
     "v1")

    (denoland/setup-deno-v2
     "e95548e56dfa95d4e1a28d6f422fafe75c4c26fb"
     "v2.0.3")

    ;; Other Tools
    (extractions/setup-just
     "e33e0265a09d6d736e2ee1e0eb685ef1de4669ff"
     "v3.0.0")

    (extractions/setup-crate
     "4993624604c307fbca528d28a3c8b60fa5ecc859"
     "v1")

    (ruby/setup-ruby
     "4a9ddd6f338a97768b8006bf671dfbad383215f4"
     "v1.207.0")

    (ocaml/setup-ocaml
     "4c1df9105efb7a8b996c21e052e4fb8b64a8f2fc"
     "v3")

    (webfactory/ssh-agent
     "dc588b651fe13675774614f8e6a936a468676387"
     "v0.9.0")

    (softprops/action-gh-release
     "da05d552573ad5aba039eaac05058a918a7bf631"
     "v2")

    (google/clusterfuzzlite/actions/build_fuzzers
     "884713a6c30a92e5e8544c39945cd7cb630abcd1"
     "v1")

    (google/clusterfuzzlite/actions/run_fuzzers
     "884713a6c30a92e5e8544c39945cd7cb630abcd1"
     "v1")))

;; Helper: Get SHA for an action
(define (get-sha action-name)
  (let ((entry (assoc action-name action-sha-pins)))
    (if entry
        (cadr entry)
        #f)))

;; Helper: Get SHA with comment for workflow file
(define (get-sha-with-comment action-name)
  (let ((entry (assoc action-name action-sha-pins)))
    (if entry
        (format #f "~a@~a # ~a" action-name (cadr entry) (caddr entry))
        #f)))
