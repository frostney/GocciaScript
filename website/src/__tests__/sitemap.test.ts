import { describe, expect, test } from "bun:test";
import fs from "node:fs";
import path from "node:path";
import { buildSitemap, getDocLastModified } from "@/app/sitemap";
import { DOC_PAGES } from "@/lib/docs-data";

describe("sitemap", () => {
  test("uses a build date for app pages and file mtimes for doc pages", () => {
    const appLastModified = new Date("2026-06-17T00:00:00.000Z");
    const entries = buildSitemap("https://example.test///", appLastModified);
    const home = entries.find((entry) => entry.url === "https://example.test/");
    const languagePage = DOC_PAGES.find((page) => page.id === "language");
    if (!languagePage) throw new Error("missing language doc page");

    const languageEntry = entries.find(
      (entry) => entry.url === "https://example.test/docs/language",
    );
    const languageSource = path.resolve(
      process.cwd(),
      "..",
      "docs",
      languagePage.file,
    );

    expect(home?.lastModified).toEqual(appLastModified);
    expect(languageEntry?.lastModified).toEqual(
      fs.statSync(languageSource).mtime,
    );
    expect(getDocLastModified(languagePage)).toEqual(
      fs.statSync(languageSource).mtime,
    );
  });
});
