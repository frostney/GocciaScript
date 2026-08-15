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

# Trim every trailing slash, not just one: "/usr/local/bin//" must match the
# PATH entry "/usr/local/bin" or the installer prints a false "add it to your
# PATH" hint. A bare "/" is left intact — reducing it to "" would report an
# empty install location and probe PATH for "".
while [ "$INSTALL_DIR" != "/" ] && [ "$INSTALL_DIR" != "${INSTALL_DIR%/}" ]; do
  INSTALL_DIR="${INSTALL_DIR%/}"
done

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

# The archive expands into a single top-level directory named after the
# release (gocciascript-<version>-<os>-<arch>) with the executables sitting
# at its root. Resolve it by glob rather than by exact name, and keep the
# legacy build/ and flat layouts as fallbacks.
#
# A candidate qualifies only when it holds all three executables. Matching on
# the loader alone would commit to the first directory that happens to have
# it and then hard-fail on the missing sibling, even when a later candidate
# is complete.
SRC_DIR=""
PARTIAL_DIR=""
PARTIAL_MISSING=""
for candidate in "gocciascript-${VERSION}-${OS}-${ARCH}" gocciascript-*/ build .; do
  candidate="${candidate%/}"
  [ -d "$candidate" ] || continue
  missing=""
  for bin in GocciaScriptLoader GocciaTestRunner GocciaREPL; do
    [ -f "${candidate}/${bin}" ] || missing="${missing} ${bin}"
  done
  if [ -z "$missing" ]; then
    SRC_DIR="$candidate"
    break
  fi
  # Keep the first loader-bearing candidate for diagnostics: it is the one
  # that looked like a GocciaScript layout, so its missing files are what
  # the user needs to hear about if nothing else qualifies.
  if [ -z "$PARTIAL_DIR" ] && [ -f "${candidate}/GocciaScriptLoader" ]; then
    PARTIAL_DIR="$candidate"
    PARTIAL_MISSING="${missing# }"
  fi
done

if [ -z "$SRC_DIR" ]; then
  # Every release archive carries all three; a missing one means a broken
  # download or a layout change, so fail rather than report a partial
  # install as success.
  [ -z "$PARTIAL_DIR" ] || err "incomplete archive ${ASSET}: ${PARTIAL_DIR} is missing ${PARTIAL_MISSING}"
  err "could not find GocciaScriptLoader, GocciaTestRunner and GocciaREPL in $ASSET"
fi

for bin in GocciaScriptLoader GocciaTestRunner GocciaREPL; do
  src="${SRC_DIR}/${bin}"
  chmod +x "$src"
  $SUDO mv "$src" "${INSTALL_DIR}/${bin}"
done

printf '\nGocciaScript %s installed to %s\n' "$VERSION" "$INSTALL_DIR"
case ":${PATH}:" in
  *":${INSTALL_DIR}:"*) ;;
  *) printf 'Add %s to your PATH if it is not already there.\n' "$INSTALL_DIR" ;;
esac
