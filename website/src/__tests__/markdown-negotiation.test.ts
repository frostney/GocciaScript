import { describe, expect, test } from "bun:test";
import {
  acceptsMarkdown,
  estimateMarkdownTokens,
  MARKDOWN_CONTENT_TYPE,
  markdownResponseHeaders,
} from "@/lib/markdown-negotiation";
import {
  createSiteMarkdown,
  renderCompatibilityMarkdown,
  resolveMarkdownRoute,
  yamlScalar,
} from "@/lib/site-markdown";
import type { Test262DashboardData } from "@/lib/test262-dashboard";

const compatibilityData = {
  status: "ready",
  generatedAt: "2026-07-16T06:44:17.371Z",
  source: {
    repositoryUrl: "https://github.com/frostney/GocciaScript",
    workflowUrl:
      "https://github.com/frostney/GocciaScript/actions/workflows/ci.yml",
    artifactName: "test262-results",
    reportName: "test262-results.json",
    minGroupTests: 25,
  },
  latest: {
    runId: 100,
    runNumber: 829,
    title: "CI",
    headSha: "0466e9bc9dbe787f3846c783ad823eacf879f186",
    shortSha: "0466e9bc",
    runUrl: "https://github.com/frostney/GocciaScript/actions/runs/100",
    createdAt: "2026-07-16T05:48:46.000Z",
    updatedAt: "2026-07-16T06:18:39.420Z",
    artifactId: 100,
    artifactCreatedAt: "2026-07-16T06:18:39.420Z",
    jsonUrl: "/api/test262/results/100",
    summary: {
      totalDiscovered: 100,
      totalRun: 100,
      passed: 99,
      failed: 1,
      wrapperInfraFailures: 0,
      timeouts: 0,
      durationSeconds: 10,
      byCategory: [
        {
          category: "language",
          run: 50,
          passed: 50,
          failed: 0,
          wrapperInfra: 0,
          timeouts: 0,
        },
        {
          category: "staging",
          run: 50,
          passed: 49,
          failed: 1,
          wrapperInfra: 0,
          timeouts: 0,
        },
      ],
    },
  },
  timeline: [],
  leastCovered: [],
  mostCovered: [],
} satisfies Test262DashboardData;

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
    expect(resolveMarkdownRoute(["compatibility"])).toEqual({
      kind: "compatibility",
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
  test("serializes frontmatter scalars with YAML-safe quoting", () => {
    expect(yamlScalar('Title: "quoted"\nnext')).toBe(
      '"Title: \\"quoted\\"\\nnext"',
    );
  });

  test("returns markdown for the playground selected example", async () => {
    const markdown = await createSiteMarkdown(
      ["playground"],
      new URLSearchParams("example=coffee"),
    );

    expect(markdown).toContain("# Playground");
    expect(markdown).toContain("CoffeeShop");
    expect(markdown).toContain("```js");
  });

  test("renders a concise live compatibility alternate from dashboard data", () => {
    const markdown = renderCompatibilityMarkdown(compatibilityData);

    expect(markdown).toContain(
      "concise Markdown representation of the authoritative GocciaScript compatibility page",
    );
    expect(markdown).toContain(
      "Overall test262 corpus pass rate: **99.0%** (99 passed / 100 run)",
    );
    expect(markdown).toContain("| language | 100.0% | 50 / 50 |");
    expect(markdown).toContain("`0466e9bc`");
    expect(markdown).not.toContain("/api/test262/latest");
  });
});
