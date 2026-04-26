/**
 * Pick a usable BCP-47 locale tag from an `Accept-Language` header.
 *
 * The header format is a comma-separated list of language tags with
 * optional quality factors:
 *
 *   en-US,en;q=0.9,fr-FR;q=0.8,*;q=0.5
 *
 * We sort by quality (default `q=1`), drop `*` (any), and validate the
 * winner via `Intl.DateTimeFormat`. Invalid tags fall back to the next
 * candidate; an empty / fully-invalid header falls back to `en-US`.
 */
export const DEFAULT_LOCALE = "en-US";

export function parseAcceptLanguage(header: string | null | undefined): string {
  if (!header) return DEFAULT_LOCALE;
  const candidates = header
    .split(",")
    .map((part) => {
      const [tag, ...attrs] = part.trim().split(";");
      const qPart = attrs.find((a) => a.trim().startsWith("q="));
      const qRaw = qPart ? Number(qPart.trim().slice(2)) : 1;
      const q = Number.isFinite(qRaw) ? qRaw : 0;
      return { tag: tag.trim(), q };
    })
    .filter((c) => c.tag && c.tag !== "*")
    // Stable sort by descending quality — `Array.sort` in modern
    // engines is stable, so equal-q tags keep header order.
    .sort((a, b) => b.q - a.q);

  for (const c of candidates) {
    try {
      // Round-trip through `Intl.DateTimeFormat` to validate the tag.
      // Returns the canonical form (e.g. `en-us` → `en-US`).
      return new Intl.DateTimeFormat(c.tag).resolvedOptions().locale;
    } catch {
      // try next candidate
    }
  }
  return DEFAULT_LOCALE;
}
