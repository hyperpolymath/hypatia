# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Nix flake development environment for hypatia.
# Usage: nix develop
{
  description = "Hypatia — neurosymbolic CI/CD intelligence platform";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Elixir/Erlang — BEAM-based rule engine
            elixir
            erlang

            # Rust — CLI scanner and core analysis
            rustc
            cargo
            clippy
            rustfmt

            # Idris2 — formal verification ABI
            idris2

            # Zig — FFI implementation
            zig

            # System dependencies
            pkg-config
            openssl
          ];

          shellHook = ''
            echo "hypatia dev shell — elixir + cargo + idris2 + zig"
          '';
        };
      });
}
