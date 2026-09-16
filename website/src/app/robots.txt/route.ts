import { getSiteUrl } from "@/lib/site-url";

export function GET() {
  const siteUrl = getSiteUrl();
  const sitemapUrl = new URL("/sitemap.xml", siteUrl).toString();
  
  const body = [
    "User-agent: *",
    "Content-Signal: search=yes, ai-input=yes, ai-train=yes",
    "Allow: /",
    "Disallow: /api/",
    `Sitemap: ${sitemapUrl}`,
    "",
  ].join("\n");

  return new Response(body, {
    headers: {
      "content-type": "text/plain; charset=utf-8",
    },
  });
}
