import { describe, expect, test } from "bun:test";
import {
  acceptsMarkdown,
  estimateMarkdownTokens,
  MARKDOWN_CONTENT_TYPE,
  markdownResponseHeaders,
} from "@/lib/markdown-negotiation";
import { createSiteMarkdown, resolveMarkdownRoute } from "@/lib/site-markdown";

describe("acceptsMarkdown", () => {
  test("requires an explicit text/markdown accept entry", () => {
    expect(acceptsMarkdown(null)).toBe(false);
    expect(acceptsMarkdown("")).toBe(false);
    expect(
      acceptsMarkdown(
        "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      ),
    ).toBe(false);
    expect(acceptsMarkdown("*/*")).toBe(false);
  });

  test("accepts text/markdown with optional parameters", () => {
    expect(acceptsMarkdown("text/markdown")).toBe(true);
    expect(acceptsMarkdown("text/html, text/markdown;q=0.7")).toBe(true);
    expect(acceptsMarkdown("TEXT/MARKDOWN; charset=utf-8")).toBe(true);
  });

  test("ignores text/markdown when q=0", () => {
    expect(acceptsMarkdown("text/markdown;q=0")).toBe(false);
  });

  test("normalizes text/markdown quality values", () => {
    expect(acceptsMarkdown("text/markdown;q=1.5")).toBe(true);
    expect(acceptsMarkdown("text/markdown;q=-1")).toBe(false);
    expect(acceptsMarkdown("text/markdown;q=abc")).toBe(false);
  });
});

describe("markdown response headers", () => {
  test("set markdown content type, vary, and token count", () => {
    const markdown = "# Title\n\nSome markdown content.";
    const headers = markdownResponseHeaders(markdown);

    expect(headers.get("content-type")).toBe(MARKDOWN_CONTENT_TYPE);
    expect(headers.get("vary")).toBe("Accept");
    expect(headers.get("x-markdown-tokens")).toBe(
      String(estimateMarkdownTokens(markdown)),
    );
  });
});

describe("resolveMarkdownRoute", () => {
  test("maps HTML page paths to markdown routes", () => {
    expect(resolveMarkdownRoute(undefined)).toEqual({ kind: "home" });
    expect(resolveMarkdownRoute([])).toEqual({ kind: "home" });
    expect(resolveMarkdownRoute(["docs"])).toEqual({
      kind: "docs",
      id: "readme",
    });
    expect(resolveMarkdownRoute(["docs", "language"])).toEqual({
      kind: "docs",
      id: "language",
    });
    expect(resolveMarkdownRoute(["installation"])).toEqual({
      kind: "installation",
    });
    expect(resolveMarkdownRoute(["playground"])).toEqual({
      kind: "playground",
    });
    expect(resolveMarkdownRoute(["sandbox"])).toEqual({ kind: "sandbox" });
  });

  test("does not invent markdown for unknown routes", () => {
    expect(resolveMarkdownRoute(["api", "execute"])).toBeNull();
    expect(resolveMarkdownRoute(["missing"])).toBeNull();
    expect(resolveMarkdownRoute(["docs", "missing"])).toBeNull();
  });
});

describe("createSiteMarkdown", () => {
  test("returns markdown for the playground selected example", async () => {
    const markdown = await createSiteMarkdown(
      ["playground"],
      new URLSearchParams("example=coffee"),
    );

    expect(markdown).toContain("# Playground");
    expect(markdown).toContain("CoffeeShop");
    expect(markdown).toContain("```js");
  });
});
