import { describe, expect, test } from "bun:test";
import { buildSitemap, getDocLastModified } from "@/app/sitemap";
import { docsSource } from "@/lib/docs-source";

describe("Fumadocs repository source", () => {
  test("derives the established routes from repository Markdown paths", () => {
    expect(docsSource.getPage(undefined)?.path).toBe("README.md");
    expect(docsSource.getPage(["language"])?.path).toBe("docs/language.md");
    expect(docsSource.getPage(["contributing-workflow"])?.path).toBe(
      "docs/contributing/workflow.md",
    );
    expect(docsSource.getPage(["adr"])?.path).toBe("docs/adr/README.md");
  });
});

describe("sitemap", () => {
  test("publishes each app and repository-backed docs route once", () => {
    const appLastModified = new Date("2026-06-17T00:00:00.000Z");
    const entries = buildSitemap("https://example.test///", appLastModified);
    const urls = entries.map((entry) => entry.url);
    const home = entries.find((entry) => entry.url === "https://example.test/");
    const docs = entries.find(
      (entry) => entry.url === "https://example.test/docs",
    );
    const languagePage = docsSource.getPage(["language"]);
    if (!languagePage) throw new Error("missing language doc page");
    const language = entries.find(
      (entry) => entry.url === "https://example.test/docs/language",
    );

    expect(new Set(urls).size).toBe(urls.length);
    expect(home?.lastModified).toEqual(appLastModified);
    expect(docs?.priority).toBe(0.8);
    expect(language?.lastModified).toEqual(getDocLastModified(languagePage));
  });
});
