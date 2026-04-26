#!/usr/bin/env sh
# GocciaScript nightly installer — macOS & Linux.
#
# Usage:
#   curl -fsSL https://gocciascript.dev/install-nightly | sh
#
# Pulls the rolling `nightly` GitHub release instead of the latest
# tagged stable. Use this when you want unreleased fixes / features
# but accept that anything in nightly can change between hours.
#
# Honors the same env overrides as `install.sh`:
#   INSTALL_DIR  — where to drop the binaries (default: /usr/local/bin)
#   GOCCIA_REPO  — GitHub owner/repo (default: frostney/GocciaScript)
#
# Side note: this is a thin wrapper around install.sh that pins
# GOCCIA_VERSION=nightly. The release artifacts under the `nightly`
# tag follow the same naming convention as the stable releases:
#   gocciascript-nightly-{macos,linux}-{arm64,x64}.{zip,tar.gz}

set -e

REPO="${GOCCIA_REPO:-frostney/GocciaScript}"
INSTALL_DIR="${INSTALL_DIR:-/usr/local/bin}"

err() { printf 'install-nightly.sh: %s\n' "$*" >&2; exit 1; }

case "$(uname -s)" in
  Darwin) OS="macos"; EXT="zip" ;;
  Linux)  OS="linux"; EXT="tar.gz" ;;
  *) err "unsupported OS: $(uname -s) — try the Windows installer (install-nightly.ps1)" ;;
esac

case "$(uname -m)" in
  arm64|aarch64) ARCH="arm64" ;;
  x86_64|amd64)  ARCH="x64"   ;;
  *) err "unsupported arch: $(uname -m)" ;;
esac

TAG="nightly"
# Nightly archives use the literal version string `nightly` rather
# than a semver number — the tag and the archive name use the same
# token, which keeps the URL pattern identical to stable.
VERSION="nightly"
ASSET="gocciascript-${VERSION}-${OS}-${ARCH}.${EXT}"
URL="https://github.com/${REPO}/releases/download/${TAG}/${ASSET}"

TMPDIR="$(mktemp -d 2>/dev/null || mktemp -d -t goccia-install)"
trap 'rm -rf "$TMPDIR"' EXIT INT TERM

printf 'Downloading %s (rolling nightly)\n' "$ASSET"
command -v curl >/dev/null 2>&1 || err "curl is required"
curl -fsSL -o "${TMPDIR}/${ASSET}" "$URL"

cd "$TMPDIR"
case "$EXT" in
  zip)    command -v unzip >/dev/null 2>&1 || err "unzip is required"; unzip -q "$ASSET" ;;
  tar.gz) tar xzf "$ASSET" ;;
esac

SUDO=""
if [ ! -w "$INSTALL_DIR" ]; then
  if command -v sudo >/dev/null 2>&1; then
    SUDO="sudo"
  else
    err "$INSTALL_DIR is not writable and sudo is not available — set INSTALL_DIR to a writable location"
  fi
fi

for bin in GocciaScriptLoader GocciaTestRunner GocciaREPL; do
  src="build/${bin}"
  if [ ! -f "$src" ]; then
    printf 'install-nightly.sh: %s not found in archive — skipping\n' "$src" >&2
    continue
  fi
  chmod +x "$src"
  $SUDO mv "$src" "${INSTALL_DIR}/${bin}"
done

printf '\nGocciaScript nightly installed to %s\n' "$INSTALL_DIR"
"${INSTALL_DIR}/GocciaScriptLoader" --version 2>/dev/null \
  || printf 'Add %s to your PATH if it is not already there.\n' "$INSTALL_DIR"
