#!/usr/bin/env sh
# GocciaScript installer — macOS & Linux.
#
# Usage:
#   curl -fsSL https://gocciascript.dev/install.sh | sh
#
# Honors the following environment variables:
#   INSTALL_DIR  — where to drop the binaries (default: /usr/local/bin)
#   GOCCIA_VERSION — tag to install (default: latest release)
#   GOCCIA_REPO  — GitHub owner/repo (default: frostney/GocciaScript)
#
# The release ships a single archive per OS / arch; we extract it
# under a temp dir, then move the three executables (GocciaScriptLoader,
# GocciaTestRunner, GocciaREPL) into INSTALL_DIR.

set -e

REPO="${GOCCIA_REPO:-frostney/GocciaScript}"
INSTALL_DIR="${INSTALL_DIR:-/usr/local/bin}"

err() { printf 'install.sh: %s\n' "$*" >&2; exit 1; }

# --- detect OS -------------------------------------------------------
case "$(uname -s)" in
  Darwin) OS="macos"; EXT="zip" ;;
  Linux)  OS="linux"; EXT="tar.gz" ;;
  *) err "unsupported OS: $(uname -s) — try the Windows installer (install.ps1)" ;;
esac

# --- detect arch -----------------------------------------------------
case "$(uname -m)" in
  arm64|aarch64) ARCH="arm64" ;;
  x86_64|amd64)  ARCH="x64"   ;;
  *) err "unsupported arch: $(uname -m)" ;;
esac

# --- resolve version -------------------------------------------------
if [ -n "$GOCCIA_VERSION" ]; then
  TAG="$GOCCIA_VERSION"
else
  command -v curl >/dev/null 2>&1 || err "curl is required"
  TAG="$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" \
    | grep -E '"tag_name":' | head -n1 | sed -E 's/.*"([^"]+)".*/\1/')"
  [ -n "$TAG" ] || err "could not resolve latest release"
fi
VERSION="${TAG#v}"

ASSET="gocciascript-${VERSION}-${OS}-${ARCH}.${EXT}"
URL="https://github.com/${REPO}/releases/download/${TAG}/${ASSET}"

# --- download + extract ---------------------------------------------
TMPDIR="$(mktemp -d 2>/dev/null || mktemp -d -t goccia-install)"
trap 'rm -rf "$TMPDIR"' EXIT INT TERM

printf 'Downloading %s\n' "$ASSET"
curl -fsSL -o "${TMPDIR}/${ASSET}" "$URL"

cd "$TMPDIR"
case "$EXT" in
  zip)    command -v unzip >/dev/null 2>&1 || err "unzip is required"; unzip -q "$ASSET" ;;
  tar.gz) tar xzf "$ASSET" ;;
esac

# --- install ---------------------------------------------------------
SUDO=""
if [ ! -w "$INSTALL_DIR" ]; then
  if command -v sudo >/dev/null 2>&1; then
    SUDO="sudo"
  else
    err "$INSTALL_DIR is not writable and sudo is not available — set INSTALL_DIR to a writable location"
  fi
fi

mkdir -p build  # paranoia: archive should already create this
for bin in GocciaScriptLoader GocciaTestRunner GocciaREPL; do
  src="build/${bin}"
  if [ ! -f "$src" ]; then
    printf 'install.sh: %s not found in archive — skipping\n' "$src" >&2
    continue
  fi
  chmod +x "$src"
  $SUDO mv "$src" "${INSTALL_DIR}/${bin}"
done

printf '\nGocciaScript %s installed to %s\n' "$VERSION" "$INSTALL_DIR"
"${INSTALL_DIR}/GocciaScriptLoader" --version 2>/dev/null \
  || printf 'Add %s to your PATH if it is not already there.\n' "$INSTALL_DIR"
