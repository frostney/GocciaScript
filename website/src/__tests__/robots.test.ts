import { describe, expect, test } from "bun:test";
import { GET } from "@/app/robots.txt/route";

describe("robots.txt", () => {
  test("allows search, AI input, and AI training with existing crawl rules", async () => {
    const previousSiteUrl = process.env.NEXT_PUBLIC_SITE_URL;
    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com///";

    try {
      const response = GET();

      expect(response.status).toBe(200);
      expect(response.headers.get("content-type")).toBe(
        "text/plain; charset=utf-8",
      );
      expect(await response.text()).toBe(
        [
          "User-agent: *",
          "Content-Signal: search=yes, ai-input=yes, ai-train=yes",
          "Allow: /",
          "Disallow: /api/",
          "Sitemap: https://example.com/sitemap.xml",
          "",
        ].join("\n"),
      );
    } finally {
      if (previousSiteUrl === undefined) {
        delete process.env.NEXT_PUBLIC_SITE_URL;
      } else {
        process.env.NEXT_PUBLIC_SITE_URL = previousSiteUrl;
      }
    }
  });

  test("falls back to the canonical URL when the site URL is invalid", async () => {
    const previousSiteUrl = process.env.NEXT_PUBLIC_SITE_URL;
    process.env.NEXT_PUBLIC_SITE_URL = "gocciascript.dev";

    try {
      const response = GET();

      expect(await response.text()).toContain(
        "Sitemap: https://www.gocciascript.dev/sitemap.xml",
      );
    } finally {
      if (previousSiteUrl === undefined) {
        delete process.env.NEXT_PUBLIC_SITE_URL;
      } else {
        process.env.NEXT_PUBLIC_SITE_URL = previousSiteUrl;
      }
    }
  });

  test("normalizes the apex production host to the canonical www host", async () => {
    const previousSiteUrl = process.env.NEXT_PUBLIC_SITE_URL;
    process.env.NEXT_PUBLIC_SITE_URL = "https://gocciascript.dev";

    try {
      const response = GET();

      expect(await response.text()).toContain(
        "Sitemap: https://www.gocciascript.dev/sitemap.xml",
      );
    } finally {
      if (previousSiteUrl === undefined) {
        delete process.env.NEXT_PUBLIC_SITE_URL;
      } else {
        process.env.NEXT_PUBLIC_SITE_URL = previousSiteUrl;
      }
    }
  });
});
