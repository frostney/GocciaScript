import { describe, expect, test } from "bun:test";
import {
  acceptsMarkdown,
  estimateMarkdownTokens,
  MARKDOWN_CONTENT_TYPE,
  markdownResponseHeaders,
} from "@/lib/markdown-negotiation";
import type {
  PerformanceDashboardData,
  PerformanceSuiteData,
} from "@/lib/performance-dashboard";
import {
  createSiteMarkdown,
  renderCompatibilityMarkdown,
  renderPerformanceMarkdown,
  resolveMarkdownRoute,
  yamlScalar,
} from "@/lib/site-markdown";
import type { Test262DashboardData } from "@/lib/test262-dashboard";

const emptyPerformanceSuite: PerformanceSuiteData = {
  latest: null,
  latestComplete: null,
  timeline: [],
  targets: [],
};

const awfyPoint = {
  suite: "awfy" as const,
  runId: 200,
  runNumber: 912,
  artifactId: 200,
  runUrl: "https://github.com/frostney/GocciaScript/actions/runs/200",
  headSha: "1234567890abcdef1234567890abcdef12345678",
  shortSha: "12345678",
  createdAt: "2026-09-20T04:00:00.000Z",
  complete: true,
  stale: false,
  quickjsRatio: 1.5,
  nodeRatio: 12.25,
  failedWorkloadCount: 0,
  workloadCount: 14,
  repetitions: 3,
  engineVersions: {},
  corpusCommit: "abc",
  driverVersion: 1,
  compatibilityKey: "k",
};

const performanceSource = {
  repositoryUrl: "https://github.com/frostney/GocciaScript",
  workflowUrl:
    "https://github.com/frostney/GocciaScript/actions/workflows/ci.yml",
};

const performanceData: PerformanceDashboardData = {
  status: "ready",
  generatedAt: "2026-09-20T05:00:00.000Z",
  source: performanceSource,
  awfy: {
    latest: awfyPoint,
    latestComplete: awfyPoint,
    timeline: [awfyPoint],
    targets: [
      {
        name: "Richards",
        status: "degraded",
        failure: "goccia: timeout",
        unit: "microseconds",
        goccia: null,
        quickjs: 10,
        node: 1,
        quickjsRatio: null,
        nodeRatio: null,
      },
    ],
  },
  jetstream: emptyPerformanceSuite,
};

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
    expect(resolveMarkdownRoute(["performance"])).toEqual({
      kind: "performance",
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

  test("describes the sandbox flow with the inputs sandbox mode accepts", async () => {
    const markdown = (await createSiteMarkdown(["sandbox"])) ?? "";
    const flow = markdown.slice(
      markdown.indexOf("## Agent flow"),
      markdown.indexOf("## Virtual filesystem runner"),
    );

    expect(flow).toContain("`--copy` inputs");
    expect(flow).toContain("`--modules`");
    // Sandbox mode rejects --globals and ignores config globals.
    expect(flow).not.toMatch(/globals/i);
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
    expect(markdown).toContain("- Commit: `0466e9bc`");
    expect(markdown).toContain(
      "- CI run: [#829](https://github.com/frostney/GocciaScript/actions/runs/100)",
    );
    expect(markdown).not.toContain("/api/test262/latest");
  });

  test("renders a performance alternate from dashboard data", () => {
    const markdown = renderPerformanceMarkdown(performanceData);

    expect(markdown).toContain("# Performance Barometer");
    expect(markdown).toContain("## Are We Fast Yet");
    expect(markdown).toContain("- QuickJS reference ratio: **1.50x**");
    expect(markdown).toContain("- Node.js reference ratio: **12.25x**");
    expect(markdown).toContain("- Workloads: 14 (0 failed)");
    expect(markdown).toContain(
      "- CI run: [#912](https://github.com/frostney/GocciaScript/actions/runs/200)",
    );
    expect(markdown).toContain(
      "- Degraded workloads in the latest report: Richards",
    );
    expect(markdown).toContain("## JetStream 3");
    expect(markdown).toContain("No complete report has been retained yet.");
  });

  test("names the run degraded workloads come from when it is incomplete", () => {
    const incomplete = {
      ...awfyPoint,
      runId: 201,
      runNumber: 913,
      runUrl: "https://github.com/frostney/GocciaScript/actions/runs/201",
      createdAt: "2026-09-21T04:00:00.000Z",
      complete: false,
    };
    const markdown = renderPerformanceMarkdown({
      ...performanceData,
      awfy: {
        ...performanceData.awfy,
        latest: incomplete,
        latestComplete: { ...awfyPoint, stale: true },
      },
    });

    expect(markdown).toContain(
      "- CI run: [#912](https://github.com/frostney/GocciaScript/actions/runs/200)",
    );
    expect(markdown).toContain(
      "- Degraded workloads in the latest report ([#913](https://github.com/frostney/GocciaScript/actions/runs/201),",
    );
    expect(markdown).toContain("): Richards");
  });

  test.each([
    [
      "needs-blob-credentials",
      "Failed to read performance reports: No blob credentials found.",
    ],
    ["empty", "No performance barometer reports were found."],
    ["error", "Failed to read performance reports: fetch failed"],
  ] as const)("keeps the performance alternate and the dashboard's diagnosis when %s", (status, message) => {
    const markdown = renderPerformanceMarkdown({
      status,
      message,
      generatedAt: "2026-09-20T05:00:00.000Z",
      source: performanceSource,
      awfy: emptyPerformanceSuite,
      jetstream: emptyPerformanceSuite,
    });

    // The HTML dashboard shows the loader's message; so does the Markdown.
    expect(markdown).toContain(message);
    expect(markdown).toContain("[Open the dashboard](/performance)");
  });

  test("keeps a generic diagnosis when the unavailable data has no message", () => {
    const markdown = renderPerformanceMarkdown({
      status: "error",
      generatedAt: "2026-09-20T05:00:00.000Z",
      source: performanceSource,
      awfy: emptyPerformanceSuite,
      jetstream: emptyPerformanceSuite,
    });

    expect(markdown).toContain("temporarily unavailable");
    expect(markdown).not.toContain("undefined");
  });

  test("carries the test262 dashboard's diagnosis when no result is available", () => {
    const message =
      "Failed to read test262 reports from Vercel Blob: fetch failed";
    const markdown = renderCompatibilityMarkdown({
      ...compatibilityData,
      status: "error",
      message,
      latest: null,
    } as Parameters<typeof renderCompatibilityMarkdown>[0]);

    expect(markdown).toContain(message);
    expect(markdown).toContain("Open the HTML dashboard or CI workflow");
  });

  test("keeps a generic test262 diagnosis when there is no message", () => {
    const markdown = renderCompatibilityMarkdown({
      ...compatibilityData,
      status: "error",
      latest: null,
    } as Parameters<typeof renderCompatibilityMarkdown>[0]);

    expect(markdown).toContain("temporarily unavailable");
    expect(markdown).not.toContain("undefined");
  });
});
