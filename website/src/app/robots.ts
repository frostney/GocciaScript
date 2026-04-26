import type { MetadataRoute } from "next";

// Strip any trailing slashes so the constructed `${SITE_URL}/sitemap.xml`
// can't end up with a `//` separator.
const SITE_URL = (
  process.env.NEXT_PUBLIC_SITE_URL ?? "https://gocciascript.dev"
).replace(/\/+$/, "");

export default function robots(): MetadataRoute.Robots {
  return {
    rules: [
      {
        userAgent: "*",
        allow: "/",
        disallow: ["/api/"],
      },
    ],
    sitemap: `${SITE_URL}/sitemap.xml`,
  };
}
