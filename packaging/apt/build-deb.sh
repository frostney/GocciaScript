#!/usr/bin/env sh
# Build a Debian binary package from a released GocciaScript archive.
#
# Usage:
#   ./packaging/apt/build-deb.sh <version> <arch>
#
# Where:
#   <version> — release version without the leading `v` (e.g. 0.6.1)
#   <arch>    — Debian arch name: amd64 (x64) or arm64
#
# Pulls the archive from the GitHub release, extracts the toolchain
# from `build/`, lays it out under a staging directory, writes a
# minimal `DEBIAN/control`, and runs `dpkg-deb --build`.

set -e

VERSION="${1:-}"
ARCH="${2:-amd64}"
[ -n "$VERSION" ] || { printf 'Usage: %s <version> <arch>\n' "$0" >&2; exit 1; }

case "$ARCH" in
  amd64) GOCCIA_ARCH="x64" ;;
  arm64) GOCCIA_ARCH="arm64" ;;
  *) printf 'unsupported arch: %s (use amd64 or arm64)\n' "$ARCH" >&2; exit 1 ;;
esac

REPO="${GOCCIA_REPO:-frostney/GocciaScript}"
ASSET="gocciascript-${VERSION}-linux-${GOCCIA_ARCH}.tar.gz"
URL="https://github.com/${REPO}/releases/download/v${VERSION}/${ASSET}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Fetch + extract the release archive.
curl -fsSL -o "${WORK}/${ASSET}" "$URL"
tar xzf "${WORK}/${ASSET}" -C "$WORK"

# The release tarball is built by the `release` job in `.github/workflows/ci.yml`
# with a single top-level directory matching the archive base name — the
# binaries land at `<root>/GocciaScriptLoader`, not `<root>/build/...`.
# (See `.github/scripts/stage-build-artifacts.sh`, which copies binaries
# directly into the staging dir before `tar -czf` archives it.)
ROOT="${WORK}/gocciascript-${VERSION}-linux-${GOCCIA_ARCH}"
[ -d "$ROOT" ] || {
  printf 'expected extracted root at %s\n' "$ROOT" >&2
  printf 'archive contents:\n' >&2
  ls -la "$WORK" >&2
  exit 1
}

# Lay out the package tree.
PKG="${WORK}/pkg"
mkdir -p "${PKG}/DEBIAN" "${PKG}/usr/bin"

cp "${ROOT}/GocciaScriptLoader" "${PKG}/usr/bin/"
cp "${ROOT}/GocciaTestRunner" "${PKG}/usr/bin/"
cp "${ROOT}/GocciaREPL" "${PKG}/usr/bin/"
chmod 755 "${PKG}/usr/bin/Goccia"*

cat > "${PKG}/DEBIAN/control" <<EOF
Package: gocciascript
Version: ${VERSION}
Section: interpreters
Priority: optional
Architecture: ${ARCH}
Maintainer: GocciaScript contributors <hello@gocciascript.dev>
Homepage: https://gocciascript.dev
Description: Sandbox-first ECMAScript runtime
 GocciaScript is a strict subset of ECMAScript 2027+ implemented from
 scratch in FreePascal. Single self-contained binary; no Node.js, no
 toolchain, no global state. Designed for tinkerers, embedding, and
 AI agents.
EOF

OUT="gocciascript_${VERSION}_${ARCH}.deb"
dpkg-deb --root-owner-group --build "$PKG" "$OUT"
printf 'Built %s\n' "$OUT"
