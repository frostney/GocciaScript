import type { ReleaseInfo } from "@/lib/github";

/** "Latest version v0.6.1 released April 25, 2026" — shared between
 *  the homepage hero and the dedicated /install page so the wording
 *  + formatting stay in one place. The release date is formatted on
 *  the server with the locale resolved from `Accept-Language` so SSR
 *  and the browser render identical text (no hydration mismatch). */
export function LatestVersion({
  release,
  locale,
}: {
  release: ReleaseInfo | null;
  /** BCP-47 locale tag used for date formatting. */
  locale: string;
}) {
  if (!release) {
    return <p className="install-latest">Latest version —</p>;
  }
  return (
    <p className="install-latest">
      Latest version{" "}
      <a
        href={release.htmlUrl}
        target="_blank"
        rel="noopener noreferrer"
        title={release.name ?? release.tagName}
      >
        <strong>{release.tagName}</strong>
      </a>
      {release.publishedAt && (
        <>
          {" "}
          released{" "}
          {/* `timeZone: "UTC"` keeps the calendar date stable across
              visitor timezones — release publish times come from
              GitHub in UTC. */}
          {new Date(release.publishedAt).toLocaleDateString(locale, {
            year: "numeric",
            month: "long",
            day: "numeric",
            timeZone: "UTC",
          })}
        </>
      )}
    </p>
  );
}
