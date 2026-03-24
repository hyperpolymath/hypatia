;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
;;
;; Guix development environment for hypatia.
;; Usage: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (gnu packages erlang)
             (gnu packages elixir)
             (gnu packages rust)
             (gnu packages crates-io)
             (gnu packages idris)
             (gnu packages zig)
             (gnu packages pkg-config)
             (gnu packages tls))

(package
  (name "hypatia")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
   (list elixir
         erlang
         rust
         rust-cargo
         idris2
         zig
         pkg-config
         openssl))
  (synopsis "Neurosymbolic CI/CD intelligence platform")
  (description
   "Hypatia is a neurosymbolic CI/CD intelligence platform providing
security scanning, code analysis, and automated quality enforcement
across the hyperpolymath ecosystem.")
  (license #f))
