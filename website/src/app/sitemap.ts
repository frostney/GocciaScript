import type { MetadataRoute } from "next";
import { type DocsPage, docsSource } from "@/lib/docs-source";
import { getSiteUrl } from "@/lib/site-url";

const APP_LAST_MODIFIED = new Date();

export function getDocLastModified(page: DocsPage): Date {
  return page.data.lastModified ?? APP_LAST_MODIFIED;
}

export function buildSitemap(
  siteUrl = getSiteUrl(),
  appLastModified = APP_LAST_MODIFIED,
): MetadataRoute.Sitemap {
  const normalizedSiteUrl = siteUrl.replace(/\/+$/, "");

  const top = [
    "/",
    "/installation",
    "/compatibility",
    "/performance",
    "/playground",
    "/sandbox",
  ].map((path) => ({
    url: `${normalizedSiteUrl}${path}`,
    lastModified: appLastModified,
    changeFrequency: "weekly" as const,
    priority: path === "/" ? 1.0 : 0.8,
  }));
  const docs = docsSource.getPages().map((page) => ({
    url: `${normalizedSiteUrl}${page.url}`,
    lastModified: getDocLastModified(page),
    changeFrequency: "weekly" as const,
    priority: page.url === "/docs" ? 0.8 : 0.6,
  }));
  return [...top, ...docs];
}

export default function sitemap(): MetadataRoute.Sitemap {
  return buildSitemap();
}
