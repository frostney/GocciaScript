// Fetches the project's own site, page by page, the way a crawl does: one GET
// after another, each with a 15 second deadline, and fails if any of them does
// not complete. Run by GocciaRunner itself, so the request goes through the
// platform's TLS backend against a real HTTPS server. live-fetch.yml runs it
// on macOS against the published nightly build.
//
// A local fixture cannot stand in on macOS: SecureTransport trusts only the
// system keychain, which a CI runner cannot be made to trust without a person
// approving it. Issue #1558 was such a failure: 30-45% of these GETs stalled
// on macOS while curl and Linux got every page.
//
//   GocciaRunner scripts/check-live-fetch.js --source-type=module \
//     --allow-net=www.gocciascript.dev --timeout=900s

const SITE = "https://www.gocciascript.dev";
const PAGE_LIMIT = 40;
const DEADLINE_MILLISECONDS = 15000;

const sitemapResponse = await fetch(`${SITE}/sitemap.xml`, {
  signal: AbortSignal.timeout(DEADLINE_MILLISECONDS),
});
if (!sitemapResponse.ok) throw new Error(`sitemap.xml: HTTP ${sitemapResponse.status}`);
const sitemap = await sitemapResponse.text();
const pages = [...sitemap.matchAll(/<loc>([^<]+)<\/loc>/g)]
  .map((match) => match[1])
  .slice(0, PAGE_LIMIT);
if (pages.length === 0) throw new Error("sitemap.xml lists no pages");

const failures = [];
for (const page of pages) {
  const started = performance.now();
  try {
    const response = await fetch(page, { signal: AbortSignal.timeout(DEADLINE_MILLISECONDS) });
    const body = await response.text();
    const framing = response.headers.get("content-length") !== null
      ? `content-length ${response.headers.get("content-length")}`
      : "chunked";
    const elapsed = Math.round(performance.now() - started);
    if (!response.ok) failures.push(`${page}: HTTP ${response.status}`);
    console.log(`${response.status} ${framing}, ${body.length} characters, ${elapsed} ms: ${page}`);
  } catch (error) {
    const elapsed = Math.round(performance.now() - started);
    failures.push(`${page}: ${error.message}`);
    console.log(`FAILED after ${elapsed} ms: ${page}: ${error.message}`);
  }
}

console.log(`${pages.length - failures.length} of ${pages.length} pages fetched`);
if (failures.length > 0) throw new Error(`${failures.length} of ${pages.length} GETs failed:\n${failures.join("\n")}`);
