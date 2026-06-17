import fs from "node:fs";
import path from "node:path";
import type { MetadataRoute } from "next";
import { DOC_PAGES, type DocPage } from "@/lib/docs-data";
import { getSiteUrl } from "@/lib/site-url";

const APP_LAST_MODIFIED = new Date();

function statMtime(filePath: string): Date | null {
  try {
    return fs.statSync(filePath).mtime;
  } catch {
    return null;
  }
}

function docSourceCandidates(page: DocPage): string[] {
  const websiteRoot = process.cwd();
  const repoRoot = path.resolve(websiteRoot, "..");
  const syncedPath = path.join(websiteRoot, "content", "docs", page.file);
  const sourcePath =
    page.file === "readme.md"
      ? path.join(repoRoot, "README.md")
      : path.join(repoRoot, "docs", page.file);
  return [sourcePath, syncedPath];
}

export function getDocLastModified(page: DocPage): Date {
  for (const candidate of docSourceCandidates(page)) {
    const mtime = statMtime(candidate);
    if (mtime) return mtime;
  }
  return APP_LAST_MODIFIED;
}

export function buildSitemap(
  siteUrl = getSiteUrl(),
  appLastModified = APP_LAST_MODIFIED,
): MetadataRoute.Sitemap {
  const normalizedSiteUrl = siteUrl.replace(/\/+$/, "");

  const top = [
    "/",
    "/installation",
    "/docs",
    "/compatibility",
    "/playground",
    "/sandbox",
  ].map((path) => ({
    url: `${normalizedSiteUrl}${path}`,
    lastModified: appLastModified,
    changeFrequency: "weekly" as const,
    priority: path === "/" ? 1.0 : 0.8,
  }));
  const docs = DOC_PAGES.filter((p) => p.id !== "readme").map((p) => ({
    url: `${normalizedSiteUrl}/docs/${p.id}`,
    lastModified: getDocLastModified(p),
    changeFrequency: "weekly" as const,
    priority: 0.6,
  }));
  return [...top, ...docs];
}

export default function sitemap(): MetadataRoute.Sitemap {
  return buildSitemap();
}
