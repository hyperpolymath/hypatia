#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
#
# install-toolchain.sh: install the pinned hypatia toolchain where mise is absent.
#
# For containers and cloud sessions on Ubuntu 24.04 x86_64. A workstation uses
# `mise install erlang elixir zig` (README.adoc). This script reads the same
# pins, so it adds no second version list:
#   erlang, elixir, zig    mise.toml
#   Zig checksum           .github/workflows/zig.yml (ZIG_SHA256)
#   Idris 2 version        .github/workflows/verify-proofs.yml (IDRIS2_VERSION)
# Every archive is checked against a pinned SHA-256 before it is unpacked.
# A version with no checksum below stops the script. It never installs unchecked.
#
# Usage: scripts/dev/install-toolchain.sh [--with-idris2]
#   HYPATIA_TOOLS  install root  (default: /opt/hypatia-tools as root, else ~/.local/hypatia-tools)
#   HYPATIA_BIN    link target   (default: /usr/local/bin as root, else ~/.local/bin)
# Idempotent: a tool already installed at the pinned version is skipped.

set -eu

die() { printf 'install-toolchain: %s\n' "$1" >&2; exit 1; }

repo=$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)
with_idris2=0
[ "${1:-}" = "--with-idris2" ] && with_idris2=1

# shellcheck source=/dev/null
. /etc/os-release 2>/dev/null || die "cannot read /etc/os-release"
[ "${ID:-}-${VERSION_ID:-}-$(uname -m)" = "ubuntu-24.04-x86_64" ] ||
  die "only Ubuntu 24.04 x86_64 has pinned builds here; use mise on other systems"
for tool in curl tar unzip xz sha256sum; do
  command -v "$tool" >/dev/null 2>&1 || die "missing required tool: $tool"
done

if [ "$(id -u)" = 0 ]; then def_root=/opt/hypatia-tools; def_bin=/usr/local/bin
else def_root="$HOME/.local/hypatia-tools"; def_bin="$HOME/.local/bin"; fi
root=${HYPATIA_TOOLS:-$def_root}
bin=${HYPATIA_BIN:-$def_bin}
mkdir -p "$root" "$bin"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

pin() { awk -F'"' -v k="$1" '$0 ~ "^[[:space:]]*" k "[[:space:]]*=" { print $2; exit }' "$repo/mise.toml"; }
otp=$(pin erlang); elixir=$(pin elixir); zig=$(pin zig)
if [ -z "$otp" ] || [ -z "$elixir" ] || [ -z "$zig" ]; then
  die "mise.toml must pin erlang, elixir and zig"
fi
just=1.34.0  # the version .github/workflows/build-gossamer-gui.yml installs

# Pinned SHA-256 of each archive, and the commit of the Idris 2 tag. Sources:
# builds.hex.pm builds.txt (OTP, Elixir), the taiki-e/install-action manifest
# (just), and `git ls-remote` on idris-lang/Idris2 (the peeled tag).
sum_for() {
  case "$1" in
    otp-27.3.4.19) echo 8ce4332d7b90fef51f1ecaf4b9fe2a2e363b2c1b6025b0ff044ee1bf7d346f87 ;;
    elixir-1.17.3-otp-27) echo 867ba03a0e012861f739b27030d06ab23d73245200c4e3fed3e8385fb98ba692 ;;
    just-1.34.0) echo 42df46f7ce727bdb2c3b5357017d885bd2033e8dbb154ca8e8f229e75093aeca ;;
    idris2-v0.7.0) echo 27780073c8631826d846499840b3857d9b9a4fd5 ;;
    zig-*) awk -F"'" '/ZIG_SHA256:/ { print $2; exit }' "$repo/.github/workflows/zig.yml" ;;
  esac
}

# fetch NAME URL: download to $work/NAME and stop unless the checksum matches.
fetch() {
  want=$(sum_for "$1")
  [ -n "$want" ] || die "no pinned checksum for $1: add it to sum_for() before you install it"
  curl -fsSL --retry 3 -o "$work/$1" "$2" || die "download failed: $2"
  got=$(sha256sum "$work/$1" | cut -d' ' -f1)
  [ "$got" = "$want" ] || die "checksum mismatch for $1: want $want, got $got"
}

link_all() { for f in "$1"/*; do [ -f "$f" ] && ln -sf "$f" "$bin/$(basename "$f")"; done; }

if [ ! -x "$root/otp-$otp/bin/erl" ]; then
  fetch "otp-$otp" "https://builds.hex.pm/builds/otp/amd64/ubuntu-24.04/OTP-$otp.tar.gz"
  mkdir -p "$root/otp-$otp"
  tar -xzf "$work/otp-$otp" -C "$root/otp-$otp" --strip-components=1
  "$root/otp-$otp/Install" -minimal "$root/otp-$otp" >/dev/null
fi
link_all "$root/otp-$otp/bin"

if [ ! -x "$root/elixir-$elixir/bin/mix" ]; then
  fetch "elixir-$elixir" "https://builds.hex.pm/builds/elixir/v$elixir.zip"
  mkdir -p "$root/elixir-$elixir"
  unzip -q -o "$work/elixir-$elixir" -d "$root/elixir-$elixir"
fi
link_all "$root/elixir-$elixir/bin"
PATH="$bin:$PATH"; export PATH
mix local.hex --force >/dev/null
mix local.rebar --force >/dev/null

if [ ! -x "$root/zig-$zig/zig" ]; then
  fetch "zig-$zig" "https://ziglang.org/download/$zig/zig-x86_64-linux-$zig.tar.xz"
  mkdir -p "$root/zig-$zig"
  tar -xJf "$work/zig-$zig" -C "$root/zig-$zig" --strip-components=1
fi
ln -sf "$root/zig-$zig/zig" "$bin/zig"

if [ ! -x "$root/just-$just/just" ]; then
  fetch "just-$just" "https://github.com/casey/just/releases/download/$just/just-$just-x86_64-unknown-linux-musl.tar.gz"
  mkdir -p "$root/just-$just"
  tar -xzf "$work/just-$just" -C "$root/just-$just" just
fi
ln -sf "$root/just-$just/just" "$bin/just"

# Idris 2 builds from source in about 10 minutes, so it is opt-in. The steps
# are the ones verify-proofs.yml runs: one prefix, $HOME/.idris2.
if [ "$with_idris2" = 1 ] && [ ! -x "$HOME/.idris2/bin/idris2" ]; then
  tag=$(awk '/IDRIS2_VERSION:/ { print $2; exit }' "$repo/.github/workflows/verify-proofs.yml")
  [ -n "$tag" ] || die "verify-proofs.yml names no IDRIS2_VERSION"
  # The bootstrap needs Chez Scheme and the GMP headers (GitHub runners ship both).
  if ! command -v chezscheme >/dev/null 2>&1 || [ ! -f /usr/include/x86_64-linux-gnu/gmp.h ]; then
    { apt-get update -qq && apt-get install -y -qq chezscheme libgmp-dev; } >/dev/null ||
      die "Idris 2 needs chezscheme and libgmp-dev, and apt-get could not install them"
  fi
  want=$(sum_for "idris2-$tag")
  [ -n "$want" ] || die "no pinned commit for Idris 2 $tag: add it to sum_for() before you build it"
  git -c advice.detachedHead=false clone -q --depth 1 --branch "$tag" \
    https://github.com/idris-lang/Idris2 "$work/idris2"
  got=$(git -C "$work/idris2" rev-parse HEAD)
  [ "$got" = "$want" ] || die "Idris 2 $tag is commit $got, want $want"
  make -C "$work/idris2" bootstrap SCHEME=chezscheme >/dev/null
  make -C "$work/idris2" install PREFIX="$HOME/.idris2" >/dev/null
fi
if [ -x "$HOME/.idris2/bin/idris2" ]; then
  # A wrapper, not a link: the launcher finds idris2_app beside its own path.
  printf '#!/bin/sh\nexec "%s/.idris2/bin/idris2" "$@"\n' "$HOME" > "$bin/idris2"
  chmod 755 "$bin/idris2"
fi

echo "hypatia toolchain ready in $root (links in $bin):"
echo "  erlang/otp $(cat "$root/otp-$otp"/releases/*/OTP_VERSION)"
echo "  $(elixir --version | grep '^Elixir')"
echo "  zig $(zig version)"
echo "  $(just --version)"
if command -v idris2 >/dev/null 2>&1; then echo "  $(idris2 --version)"
else echo "  idris2: not installed (run with --with-idris2)"; fi
