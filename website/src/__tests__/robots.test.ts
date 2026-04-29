import { describe, expect, test } from "bun:test";
import { GET } from "@/app/robots.txt/route";

describe("robots.txt", () => {
  test("declares Content Signals with existing crawl rules", async () => {
    const previousSiteUrl = process.env.NEXT_PUBLIC_SITE_URL;
    process.env.NEXT_PUBLIC_SITE_URL = "https://example.com///";

    try {
      const response = GET();

      expect(response.headers.get("content-type")).toBe(
        "text/plain; charset=utf-8",
      );
      expect(await response.text()).toBe(
        [
          "User-agent: *",
          "Content-Signal: ai-train=no, search=yes, ai-input=no",
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
});
