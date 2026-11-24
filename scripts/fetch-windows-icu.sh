#!/usr/bin/env bash
# scripts/fetch-windows-icu.sh TARGET DEST: puts ICU's own Windows build (icuin, icuuc and icudt DLLs, and ICU's
# license as ICU-LICENSE.txt) into DEST for a Windows TARGET (x86_64-win64, i386-win32); any other target is a
# no-op. Windows' own icu.dll exports only part of ICU's C API (no udtitvfmt_formatCalendarToResult, which a
# proleptic Gregorian formatRange needs), so Windows builds carry the same full ICU the other platforms load from the
# system; source/shared/ICU.pas prefers these DLLs beside the executable. The release is pinned by version and
# SHA-256 (docs/built-ins-intl.md, "Platform ICU details").
set -euo pipefail

target=$1
dest=$2
version=79.1
major=79
case "$target" in
  x86_64-win64)
    asset="icu4c-${version}-Win64-MSVC2026.zip"; bin=bin64
    sha256=c4ae1cbadb4187007f76f8b1bcf3a10deb699395ab3c74df01362bfdfa0f1ac2 ;;
  i386-win32)
    asset="icu4c-${version}-Win32-MSVC2026.zip"; bin=bin
    sha256=c5fe98270549d9b8f5f5cad339facdc8a589cc8666dfbc47c6a1c75d2044b44a ;;
  *)
    exit 0 ;;
esac

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
url="https://github.com/unicode-org/icu/releases/download/release-${version}/${asset}"
curl -fsSL --retry 3 -o "$work/$asset" "$url"
echo "$sha256  $work/$asset" | sha256sum -c - > /dev/null || {
  echo "::error::$asset does not have the pinned SHA-256" >&2
  exit 1
}
mkdir -p "$dest"
unzip -q -o -j "$work/$asset" "$bin/icuin${major}.dll" "$bin/icuuc${major}.dll" "$bin/icudt${major}.dll" -d "$dest"
unzip -q -o -p "$work/$asset" LICENSE > "$dest/ICU-LICENSE.txt"
echo "ICU ${version} for ${target}: $(cd "$dest" && ls icu*${major}.dll | tr '\n' ' ')"
