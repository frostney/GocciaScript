import type { MetadataRoute } from "next";
import { DOC_PAGES } from "@/lib/docs-data";

const SITE_URL = process.env.NEXT_PUBLIC_SITE_URL ?? "https://gocciascript.dev";

export default function sitemap(): MetadataRoute.Sitemap {
  const lastModified = new Date();
  const top = ["/", "/install", "/docs", "/playground", "/sandbox"].map(
    (path) => ({
      url: `${SITE_URL}${path}`,
      lastModified,
      changeFrequency: "weekly" as const,
      priority: path === "/" ? 1.0 : 0.8,
    }),
  );
  const docs = DOC_PAGES.filter((p) => p.id !== "readme").map((p) => ({
    url: `${SITE_URL}/docs/${p.id}`,
    lastModified,
    changeFrequency: "weekly" as const,
    priority: 0.6,
  }));
  return [...top, ...docs];
}
