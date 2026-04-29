const DEFAULT_SITE_URL = "https://gocciascript.dev";

function getSiteUrl(): string {
  const siteUrl = (process.env.NEXT_PUBLIC_SITE_URL ?? DEFAULT_SITE_URL).trim();

  try {
    return new URL(siteUrl || DEFAULT_SITE_URL).href.replace(/\/+$/, "");
  } catch {
    return DEFAULT_SITE_URL;
  }
}

export function GET() {
  const body = [
    "User-agent: *",
    "Content-Signal: ai-train=no, search=yes, ai-input=no",
    "Allow: /",
    "Disallow: /api/",
    `Sitemap: ${getSiteUrl()}/sitemap.xml`,
    "",
  ].join("\n");

  return new Response(body, {
    headers: {
      "content-type": "text/plain; charset=utf-8",
    },
  });
}
