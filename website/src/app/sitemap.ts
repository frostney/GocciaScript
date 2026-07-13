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
  const cwd = process.cwd();
  const parent = path.resolve(cwd, "..");
  const sourceFromWebsiteRoot =
    page.file === "readme.md"
      ? path.join(parent, "README.md")
      : path.join(parent, "docs", page.file);
  const sourceFromRepoRoot =
    page.file === "readme.md"
      ? path.join(cwd, "README.md")
      : path.join(cwd, "docs", page.file);

  return [
    sourceFromWebsiteRoot,
    sourceFromRepoRoot,
    path.join(cwd, "content", "docs", page.file),
    path.join(cwd, "website", "content", "docs", page.file),
  ];
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
    "/performance",
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
