#!/usr/bin/env bash

# Checks a packaged release archive before it is published: every binary
# derived from the source/app/*.dpr entrypoints must sit at the archive's top
# level, <archive-base-name>/<name>, and every one without an .exe suffix must
# carry the executable bit for user, group and other. Windows binaries (.exe)
# are only checked for presence, since Windows ignores Unix file modes.
#
# Run from the repository root, like stage-build-artifacts.sh, so the
# entrypoint list matches the binaries that script stages.

set -euo pipefail

usage() {
  echo "Usage: $0 <archive.tar.gz|archive.zip>..." >&2
  exit 1
}

if [ "$#" -lt 1 ]; then
  usage
fi

shopt -s nullglob

entrypoints=(./source/app/*.dpr)
if [ "${#entrypoints[@]}" -eq 0 ]; then
  echo "::error::No .dpr entrypoints found in source/app/"
  exit 1
fi

status=0

for archive in "$@"; do
  if [ ! -f "$archive" ]; then
    echo "::error::Archive not found: $archive"
    status=1
    continue
  fi

  base="$(basename "$archive")"
  # GNU tar, bsdtar and zipinfo all print the mode first and the path last;
  # zipinfo's header and trailer lines never match a binary path below.
  case "$base" in
    *.tar.gz)
      root="${base%.tar.gz}"
      list_command=(tar -tvzf "$archive")
      ;;
    *.zip)
      root="${base%.zip}"
      list_command=(zipinfo "$archive")
      ;;
    *)
      echo "::error::Unsupported archive type: $archive"
      status=1
      continue
      ;;
  esac

  if ! listing="$("${list_command[@]}" | awk '{ print $1, $NF }')"; then
    echo "::error::Could not list $archive"
    status=1
    continue
  fi

  echo "Checking $archive"
  for src in "${entrypoints[@]}"; do
    name="$(basename "${src%.dpr}")"
    mode=""
    entry=""
    for candidate in "$root/$name" "$root/$name.exe"; do
      mode="$(awk -v path="$candidate" '$2 == path { print $1; exit }' <<< "$listing")"
      if [ -n "$mode" ]; then
        entry="$candidate"
        break
      fi
    done

    if [ -z "$entry" ]; then
      echo "::error::$archive does not contain $root/$name"
      status=1
      continue
    fi

    case "$entry" in
      *.exe)
        echo "  $mode $entry (Windows binary, mode not checked)"
        ;;
      *)
        if [[ "$mode" =~ ^-..x..x..x ]]; then
          echo "  $mode $entry"
        else
          echo "::error::$entry in $archive has mode $mode; release binaries must be executable (-rwxr-xr-x)"
          status=1
        fi
        ;;
    esac
  done
done

exit "$status"
