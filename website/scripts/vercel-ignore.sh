#!/usr/bin/env bash
# Vercel "Ignored Build Step" (wired from website/vercel.json → ignoreCommand).
#   exit 0 → skip the build
#   exit 1 → build
#
# Two reasons to build:
#   1. Something under website/ changed between the last built commit and this
#      one — the ordinary source-change case.
#   2. The newest published stable release is not among the tags the *live*
#      site vendored. Release commits touch only CHANGELOG.md, so reason 1
#      never fires for them, and the playground kept serving whatever engine
#      set the last website commit happened to vendor (that is how 0.11.0
#      shipped without ever reaching the site). The release workflow POSTs a
#      Vercel deploy hook once the release assets exist; this check is what
#      lets that deployment through, and it self-corrects any other way the
#      site falls behind a release.
#
# Reason 2 compares two public sources and needs no credentials:
#   - GitHub's `releases/latest`, which excludes drafts and prereleases, so
#     the rolling `nightly` never counts as the newest stable.
#   - This project's own `/api/versions`, which reports what the deployed
#     build actually staged under vendor/.
# Comparing against the live site rather than against git history is what
# makes it idempotent: once a deployment has vendored the tag the check goes
# quiet, however many deployments the release triggered on the way.
#
# GITHUB_TOKEN is used when set. Unauthenticated GitHub API access is 60
# requests/hour per IP; on exhaustion this script fails toward building.
set -u

target="${VERCEL_GIT_COMMIT_SHA:-HEAD}"
base="${VERCEL_GIT_PREVIOUS_SHA:-HEAD^}"
website_path="./"

repo="${GOCCIA_REPO:-frostney/GocciaScript}"
latest_release_url="${GOCCIA_LATEST_RELEASE_URL:-https://api.github.com/repos/${repo}/releases/latest}"
site_versions_url="${GOCCIA_SITE_VERSIONS_URL:-https://www.gocciascript.dev/api/versions}"

if [ -d "website" ] && [ -f "website/package.json" ]; then
  website_path="website/"
fi

ensure_commit() {
  git cat-file -e "$1^{commit}" 2>/dev/null && return 0
  git fetch --depth=1 origin "$1" >/dev/null 2>&1 &&
    git cat-file -e "$1^{commit}" 2>/dev/null &&
    return 0

  if [ "$(git rev-parse --is-shallow-repository 2>/dev/null)" = "true" ]; then
    git fetch --unshallow >/dev/null 2>&1 &&
      git cat-file -e "$1^{commit}" 2>/dev/null &&
      return 0
  fi

  return 1
}

# Echo a URL's body, or nothing on any failure. The GitHub token goes to the
# GitHub API only, never to the site.
fetch_url() {
  case "$1" in
    https://api.github.com/*)
      if [ -n "${GITHUB_TOKEN:-}" ]; then
        curl -fsS --max-time 10 --retry 1 \
          -H "Accept: application/vnd.github+json" \
          -H "Authorization: Bearer ${GITHUB_TOKEN}" "$1" 2>/dev/null
        return
      fi
      curl -fsS --max-time 10 --retry 1 \
        -H "Accept: application/vnd.github+json" "$1" 2>/dev/null
      ;;
    *)
      curl -fsS --max-time 10 --retry 1 "$1" 2>/dev/null
      ;;
  esac
}

# First `"tag_name": "..."` value in a GitHub release payload, `v` stripped.
extract_tag_name() {
  grep -o '"tag_name"[[:space:]]*:[[:space:]]*"[^"]*"' |
    head -n 1 |
    sed -e 's/.*:[[:space:]]*"//' -e 's/"$//' -e 's/^v//'
}

# 0 = the live site is missing the newest stable release (build), 1 = current.
release_is_missing_from_site() {
  if [ "${GOCCIA_SKIP_RELEASE_CHECK:-}" = "1" ]; then
    echo "GOCCIA_SKIP_RELEASE_CHECK=1; not checking release freshness."
    return 1
  fi

  latest_body="$(fetch_url "$latest_release_url")"
  latest_tag="$(printf '%s' "$latest_body" | extract_tag_name)"
  if [ -z "$latest_tag" ]; then
    echo "Could not read the newest stable release from ${latest_release_url}; building."
    return 0
  fi

  site_body="$(fetch_url "$site_versions_url")"
  if [ -z "$site_body" ]; then
    echo "Could not read vendored versions from ${site_versions_url}; building."
    return 0
  fi

  # Fixed-string, both quoted forms. A regex would let `.` match anything, so
  # a live `0x11y0` would satisfy a search for `0.11.0` and skip a build the
  # site actually needs.
  if printf '%s' "$site_body" |
    grep -qF -e "\"${latest_tag}\"" -e "\"v${latest_tag}\""; then
    echo "Live site already vendors release ${latest_tag}."
    return 1
  fi

  echo "Release ${latest_tag} is not vendored by the live site; building."
  return 0
}

if ! ensure_commit "$base"; then
  echo "Previous commit $base is not available; building."
  exit 1
fi

if ! ensure_commit "$target"; then
  echo "Target commit $target is not available; building."
  exit 1
fi

if ! git diff --quiet "$base" "$target" -- "$website_path"; then
  echo "Website changes detected; building."
  exit 1
fi

echo "No website changes detected; checking release freshness."
if release_is_missing_from_site; then
  exit 1
fi

echo "Skipping build."
exit 0
