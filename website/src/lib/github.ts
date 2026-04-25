export const GITHUB_REPO_URL = "https://github.com/frostney/GocciaScript";
export const GITHUB_RELEASES_URL =
  "https://github.com/frostney/GocciaScript/releases";
const GITHUB_API_URL = "https://api.github.com/repos/frostney/GocciaScript";
const GITHUB_RELEASES_API_URL =
  "https://api.github.com/repos/frostney/GocciaScript/releases/latest";
const GITHUB_RELEASES_LIST_URL =
  "https://api.github.com/repos/frostney/GocciaScript/releases?per_page=20";

export type ReleaseInfo = {
  tagName: string;
  name: string | null;
  htmlUrl: string;
  publishedAt: string | null;
};

// Pre-1.0 = the major version is 0. While in 0.x.y the public API is not
// stabilized; breaking changes can land between any two releases.
export function isPreStable(release: ReleaseInfo | null | undefined): boolean {
  if (!release?.tagName) return false;
  const m = release.tagName.replace(/^v/, "").match(/^(\d+)\./);
  if (!m) return false;
  return Number(m[1]) === 0;
}

export function formatStars(count: number): string {
  if (count >= 1_000_000) return `${(count / 1_000_000).toFixed(1)}m`;
  if (count >= 1_000) return `${(count / 1_000).toFixed(1)}k`;
  return String(count);
}

export async function fetchLatestRelease(): Promise<ReleaseInfo | null> {
  try {
    const headers: Record<string, string> = {
      Accept: "application/vnd.github+json",
      "X-GitHub-Api-Version": "2022-11-28",
    };
    if (process.env.GITHUB_TOKEN) {
      headers.Authorization = `Bearer ${process.env.GITHUB_TOKEN}`;
    }
    const res = await fetch(GITHUB_RELEASES_API_URL, {
      headers,
      next: { revalidate: 1800 },
    });
    if (!res.ok) return null;
    const data = (await res.json()) as {
      tag_name?: string;
      name?: string | null;
      html_url?: string;
      published_at?: string | null;
    };
    if (
      typeof data.tag_name !== "string" ||
      typeof data.html_url !== "string"
    ) {
      return null;
    }
    return {
      tagName: data.tag_name,
      name: data.name ?? null,
      htmlUrl: data.html_url,
      publishedAt: data.published_at ?? null,
    };
  } catch {
    return null;
  }
}

type Semver = { major: number; minor: number; patch: number; tag: string };

function parseSemverTag(t: string): Semver | null {
  const s = t.replace(/^v/, "");
  const m = s.match(/^(\d+)\.(\d+)\.(\d+)/);
  if (!m) return null;
  return { major: +m[1], minor: +m[2], patch: +m[3], tag: t };
}

function compareSemver(a: Semver, b: Semver): number {
  return a.major - b.major || a.minor - b.minor || a.patch - b.patch;
}

/** Pick up to three release tags by precedence, newest-first.
 *
 *  Each slot points at the **latest patch** within its target line —
 *  never an `x.y.0` "starter" tag if a newer patch in that same line
 *  exists. Concretely:
 *
 *  1. **Patch**: the latest semver overall.
 *  2. **Minor**: the latest semver from a *different* `(major, minor)`
 *     line than the patch pin — i.e. the previous minor cycle's most
 *     recent patch. Example: with `0.6.1, 0.6.0, 0.5.1, 0.5.0`, this
 *     resolves to `0.5.1` (not `0.5.0`).
 *  3. **Major**: the latest semver from a *different* `major` than the
 *     patch pin — i.e. the previous major line's most recent patch.
 *     When no other major exists in history (or it would collide with
 *     the minor pin), falls back to the previous minor cycle relative
 *     to the minor pin: the latest semver whose `(major, minor)`
 *     differs from **both** the patch and minor pins.
 *
 *  Duplicates collapse (e.g. when the latest release is itself a major
 *  bump and there's only one minor line in that major); result is
 *  sorted newest-first. */
export function pickPrecedenceVersions(tags: string[]): string[] {
  const parsed = tags
    .map(parseSemverTag)
    .filter((s): s is Semver => s !== null)
    .sort((a, b) => compareSemver(b, a));
  if (parsed.length === 0) return [];

  const patchPin = parsed[0];
  // Minor: most recent patch from a different (major, minor) line than
  // the patch pin's. Walking the desc-sorted list, the first such tag
  // is by definition the highest patch in that previous minor cycle.
  const minorPin = parsed.find(
    (s) => s.major !== patchPin.major || s.minor !== patchPin.minor,
  );
  // Major: most recent patch from a different major line. When that's
  // unavailable or duplicates the minor pin, step further back to a
  // (major, minor) line distinct from both pins.
  let majorPin = parsed.find((s) => s.major !== patchPin.major);
  if (!majorPin || (minorPin && majorPin.tag === minorPin.tag)) {
    majorPin = minorPin
      ? parsed.find(
          (s) =>
            (s.major !== patchPin.major || s.minor !== patchPin.minor) &&
            (s.major !== minorPin.major || s.minor !== minorPin.minor),
        )
      : undefined;
  }

  const picks: Semver[] = [];
  for (const p of [patchPin, minorPin, majorPin]) {
    if (p && !picks.some((q) => q.tag === p.tag)) picks.push(p);
  }
  picks.sort((a, b) => compareSemver(b, a));
  return picks.map((s) => s.tag);
}

/** Fetch up to `count` recent stable release tags, newest first. The
 *  rolling `nightly` tag (and any GitHub-flagged prerelease/draft) is
 *  filtered out so the caller can append `nightly` separately. */
export async function fetchRecentStableTags(count = 3): Promise<string[]> {
  try {
    const headers: Record<string, string> = {
      Accept: "application/vnd.github+json",
      "X-GitHub-Api-Version": "2022-11-28",
    };
    if (process.env.GITHUB_TOKEN) {
      headers.Authorization = `Bearer ${process.env.GITHUB_TOKEN}`;
    }
    const res = await fetch(GITHUB_RELEASES_LIST_URL, {
      headers,
      next: { revalidate: 1800 },
    });
    if (!res.ok) return [];
    const list = (await res.json()) as Array<{
      tag_name?: string;
      prerelease?: boolean;
      draft?: boolean;
    }>;
    if (!Array.isArray(list)) return [];
    const tags: string[] = [];
    for (const r of list) {
      if (r.draft || r.prerelease) continue;
      const tag = r.tag_name;
      if (typeof tag !== "string") continue;
      if (tag.toLowerCase() === "nightly") continue;
      tags.push(tag);
      if (tags.length >= count) break;
    }
    return tags;
  } catch {
    return [];
  }
}

export async function fetchStars(): Promise<number | null> {
  try {
    const headers: Record<string, string> = {
      Accept: "application/vnd.github+json",
      "X-GitHub-Api-Version": "2022-11-28",
    };
    if (process.env.GITHUB_TOKEN) {
      headers.Authorization = `Bearer ${process.env.GITHUB_TOKEN}`;
    }
    const res = await fetch(GITHUB_API_URL, {
      headers,
      next: { revalidate: 3600 },
    });
    if (!res.ok) return null;
    const data = (await res.json()) as { stargazers_count?: number };
    return typeof data.stargazers_count === "number"
      ? data.stargazers_count
      : null;
  } catch {
    return null;
  }
}
