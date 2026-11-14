#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
#
# install-toolchain.sh: install the pinned hypatia toolchain where mise is absent.
#
# For containers and cloud sessions on Ubuntu 24.04 x86_64. A workstation uses
# `mise install erlang elixir zig` (README.adoc). This script reads the pins
# that the repository already has, so it adds no second version list:
#   erlang, elixir, zig    mise.toml
#   just                   .github/workflows/build-gossamer-gui.yml (tool: just@...)
#   Zig checksum           .github/workflows/zig.yml (ZIG_SHA256)
#   Idris 2 tag            .github/workflows/verify-proofs.yml (IDRIS2_VERSION)
# Every archive is checked against a pinned SHA-256 before it is unpacked.
# A version with no checksum below stops the script. It never installs unchecked.
#
# Usage: scripts/dev/install-toolchain.sh [--with-idris2]
#   HYPATIA_TOOLS  install root  (default: /opt/hypatia-tools as root, else ~/.local/hypatia-tools)
#   HYPATIA_BIN    link target   (default: /usr/local/bin as root, else ~/.local/bin)
# Idempotent: a tool is skipped only when its completion marker exists, so an
# interrupted run is repeated in full. Hex and Rebar follow the newest release
# for the pinned Elixir, as the CI does.

set -eu

die() {
  msg=$1
  printf 'install-toolchain: %s\n' "$msg" >&2
  exit 1
}

repo=$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)
with_idris2=0
case "$#:${1:-}" in
  0:) ;;
  1:--with-idris2) with_idris2=1 ;;
  *) die "usage: install-toolchain.sh [--with-idris2]" ;;
esac

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
# Absolute paths: a link with a relative target resolves from the link directory.
root=$(CDPATH='' cd -- "$root" && pwd)
bin=$(CDPATH='' cd -- "$bin" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
path_before=$PATH

# pin KEY: the quoted value of KEY in mise.toml.
pin() {
  key=$1
  awk -F'"' -v k="$key" '$0 ~ "^[[:space:]]*" k "[[:space:]]*=" { print $2; exit }' "$repo/mise.toml"
}
otp=$(pin erlang); elixir=$(pin elixir); zig=$(pin zig)
if [ -z "$otp" ] || [ -z "$elixir" ] || [ -z "$zig" ]; then
  die "mise.toml must pin erlang, elixir and zig"
fi
just=$(awk -F'@' '/tool:[[:space:]]*just@/ { print $2; exit }' "$repo/.github/workflows/build-gossamer-gui.yml")
[ -n "$just" ] || die "build-gossamer-gui.yml names no just version"

# Pinned SHA-256 of each archive, and the commit of the Idris 2 tag. Sources:
# builds.hex.pm builds.txt (OTP, Elixir), the taiki-e/install-action manifest
# (just), and `git ls-remote` on idris-lang/Idris2 (the peeled tag).
sum_for() {
  name=$1
  case "$name" in
    otp-27.3.4.19) echo 8ce4332d7b90fef51f1ecaf4b9fe2a2e363b2c1b6025b0ff044ee1bf7d346f87 ;;
    elixir-1.17.3-otp-27) echo 867ba03a0e012861f739b27030d06ab23d73245200c4e3fed3e8385fb98ba692 ;;
    just-1.34.0) echo 42df46f7ce727bdb2c3b5357017d885bd2033e8dbb154ca8e8f229e75093aeca ;;
    idris2-v0.7.0) echo 27780073c8631826d846499840b3857d9b9a4fd5 ;;
    zig-*) awk -F"'" '/ZIG_SHA256:/ { print $2; exit }' "$repo/.github/workflows/zig.yml" ;;
    *) ;; # no pin: the caller stops
  esac
}

# fetch NAME URL: download to $work/NAME and stop unless the checksum matches.
fetch() {
  name=$1
  url=$2
  want=$(sum_for "$name")
  [ -n "$want" ] || die "no pinned checksum for $name: add it to sum_for() before you install it"
  curl -fsSL --retry 3 -o "$work/$name" "$url" || die "download failed: $url"
  got=$(sha256sum "$work/$name" | cut -d' ' -f1)
  [ "$got" = "$want" ] || die "checksum mismatch for $name: want $want, got $got"
}

# fresh DIR: remove a directory that has no completion marker, and make it again.
fresh() {
  dir=$1
  rm -rf "$dir"
  mkdir -p "$dir"
}

link_all() {
  from=$1
  for f in "$from"/*; do [ -f "$f" ] && ln -sf "$f" "$bin/$(basename "$f")"; done
}

# as_root CMD...: run a command as root, with sudo where it is needed and present.
as_root() {
  if [ "$(id -u)" = 0 ]; then "$@"
  elif command -v sudo >/dev/null 2>&1; then sudo "$@"
  else return 1; fi
}

d="$root/otp-$otp"
if [ ! -f "$d/.installed" ]; then
  fetch "otp-$otp" "https://builds.hex.pm/builds/otp/amd64/ubuntu-24.04/OTP-$otp.tar.gz"
  fresh "$d"
  tar -xzf "$work/otp-$otp" -C "$d" --strip-components=1
  # Install writes the final path into the launchers, so it runs in place.
  "$d/Install" -minimal "$d" >/dev/null
  : > "$d/.installed"
fi
link_all "$d/bin"

d="$root/elixir-$elixir"
if [ ! -f "$d/.installed" ]; then
  fetch "elixir-$elixir" "https://builds.hex.pm/builds/elixir/v$elixir.zip"
  fresh "$d"
  unzip -q -o "$work/elixir-$elixir" -d "$d"
  : > "$d/.installed"
fi
link_all "$d/bin"
PATH="$bin:$PATH"; export PATH
mix local.hex --force --if-missing >/dev/null
mix local.rebar --force --if-missing >/dev/null

d="$root/zig-$zig"
if [ ! -f "$d/.installed" ]; then
  fetch "zig-$zig" "https://ziglang.org/download/$zig/zig-x86_64-linux-$zig.tar.xz"
  fresh "$d"
  tar -xJf "$work/zig-$zig" -C "$d" --strip-components=1
  : > "$d/.installed"
fi
ln -sf "$d/zig" "$bin/zig"

d="$root/just-$just"
if [ ! -f "$d/.installed" ]; then
  fetch "just-$just" "https://github.com/casey/just/releases/download/$just/just-$just-x86_64-unknown-linux-musl.tar.gz"
  fresh "$d"
  tar -xzf "$work/just-$just" -C "$d" just
  : > "$d/.installed"
fi
ln -sf "$d/just" "$bin/just"

# Idris 2 builds from source in about 10 minutes, so it is opt-in. The steps
# are the ones verify-proofs.yml runs: one prefix, $HOME/.idris2.
if [ "$with_idris2" = 1 ]; then
  tag=$(awk '/IDRIS2_VERSION:/ { print $2; exit }' "$repo/.github/workflows/verify-proofs.yml")
  [ -n "$tag" ] || die "verify-proofs.yml names no IDRIS2_VERSION"
  idris="$HOME/.idris2/bin/idris2"
  # `make install` puts the launcher in place before the libraries. The marker
  # is written after it ends, so an install that stopped part-way is built
  # again. An Idris 2 tree that has no marker is built again once.
  mark="$HOME/.idris2/.installed-$tag"
  have=""
  [ -f "$mark" ] && [ -x "$idris" ] && have=$("$idris" --version 2>/dev/null || true)
  case "$have" in
    *"version ${tag#v}"*) ;; # the pinned release is already there
    *)
      want=$(sum_for "idris2-$tag")
      [ -n "$want" ] || die "no pinned commit for Idris 2 $tag: add it to sum_for() before you build it"
      # The bootstrap needs Chez Scheme, the GMP headers, GNU make and a C
      # compiler (GitHub runners ship all 4).
      if ! command -v chezscheme >/dev/null 2>&1 || ! command -v make >/dev/null 2>&1 ||
        ! command -v cc >/dev/null 2>&1 || [ ! -f /usr/include/x86_64-linux-gnu/gmp.h ]; then
        { as_root apt-get update -qq && as_root apt-get install -y -qq chezscheme libgmp-dev make gcc; } >/dev/null ||
          die "Idris 2 needs chezscheme, libgmp-dev, make and gcc: install them as root, then run this script again"
      fi
      git -c advice.detachedHead=false clone -q --depth 1 --branch "$tag" \
        https://github.com/idris-lang/Idris2 "$work/idris2"
      got=$(git -C "$work/idris2" rev-parse HEAD)
      [ "$got" = "$want" ] || die "Idris 2 $tag is commit $got, want $want"
      make -C "$work/idris2" bootstrap SCHEME=chezscheme >/dev/null
      make -C "$work/idris2" install PREFIX="$HOME/.idris2" >/dev/null
      : > "$mark"
      ;;
  esac
  # A wrapper, not a link: the launcher finds idris2_app beside its own path.
  # No wrapper when the link target is the Idris 2 directory itself.
  if [ "$(CDPATH='' cd -- "$bin" && pwd -P)" != "$(CDPATH='' cd -- "$HOME/.idris2/bin" && pwd -P)" ]; then
    # Write a new file and rename it over the name. A redirect to the name
    # follows an existing link, and would overwrite the launcher itself.
    tmp="$bin/.idris2.$$"
    printf '#!/bin/sh\nexec "%s" "$@"\n' "$idris" > "$tmp"
    chmod 755 "$tmp"
    mv -f "$tmp" "$bin/idris2"
  fi
fi

echo "hypatia toolchain ready in $root (links in $bin):"
echo "  erlang/otp $(cat "$root/otp-$otp"/releases/*/OTP_VERSION)"
echo "  $(elixir --version | grep '^Elixir')"
echo "  zig $(zig version)"
echo "  $(just --version)"
if command -v idris2 >/dev/null 2>&1; then echo "  $(idris2 --version)"
else echo "  idris2: not installed (run with --with-idris2)"; fi
case ":$path_before:" in
  *":$bin:"*) ;;
  *) echo "Add the tools to this shell and to your profile: export PATH=\"$bin:\$PATH\"" ;;
esac
