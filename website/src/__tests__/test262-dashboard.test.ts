import { describe, expect, test } from "bun:test";
import { deflateRawSync } from "node:zlib";
import { createElement } from "react";
import { renderToStaticMarkup } from "react-dom/server";
import {
  formatTest262CategoryLabel,
  formatTest262DurationTooltip,
  formatTest262TimelineTooltip,
  Test262Dashboard,
} from "@/components/test262-dashboard";
import type { Test262BlobRun } from "@/lib/test262-blob-store";
import {
  createTest262DashboardData,
  createTest262DashboardFallback,
  extractJsonFromZip,
  groupCoverage,
  latestTest262Summary,
  normalizeTest262Report,
  rankGroupCoverage,
  readNewestValidTest262Report,
  type Test262DashboardData,
  type Test262Record,
  type Test262SuiteSummary,
  test262RunIncludesAnnexB,
} from "@/lib/test262-dashboard";
import { test262ModeRates } from "@/lib/test262-modes";

function buildZip(name: string, text: string): Uint8Array {
  const nameBytes = Buffer.from(name);
  const raw = Buffer.from(text);
  const compressed = deflateRawSync(raw);
  const local = Buffer.alloc(30 + nameBytes.length);
  local.writeUInt32LE(0x04034b50, 0);
  local.writeUInt16LE(20, 4);
  local.writeUInt16LE(8, 8);
  local.writeUInt32LE(compressed.length, 18);
  local.writeUInt32LE(raw.length, 22);
  local.writeUInt16LE(nameBytes.length, 26);
  nameBytes.copy(local, 30);

  const central = Buffer.alloc(46 + nameBytes.length);
  central.writeUInt32LE(0x02014b50, 0);
  central.writeUInt16LE(20, 4);
  central.writeUInt16LE(20, 6);
  central.writeUInt16LE(8, 10);
  central.writeUInt32LE(compressed.length, 20);
  central.writeUInt32LE(raw.length, 24);
  central.writeUInt16LE(nameBytes.length, 28);
  nameBytes.copy(central, 46);

  const eocd = Buffer.alloc(22);
  eocd.writeUInt32LE(0x06054b50, 0);
  eocd.writeUInt16LE(1, 8);
  eocd.writeUInt16LE(1, 10);
  eocd.writeUInt32LE(central.length, 12);
  eocd.writeUInt32LE(local.length + compressed.length, 16);

  return Buffer.concat([local, compressed, central, eocd]);
}

describe("latest test262 summary selection", () => {
  const latest = {
    runId: 1,
    runNumber: 1,
    title: "CI",
    headSha: "abc1234",
    shortSha: "abc1234",
    runUrl: "https://example.test/runs/1",
    createdAt: "2026-06-10T01:00:00.000Z",
    updatedAt: "2026-06-10T01:05:00.000Z",
    artifactId: 1001,
    artifactCreatedAt: "2026-06-10T01:05:00.000Z",
    jsonUrl: "/api/test262/results/1001",
    summary: {
      totalDiscovered: 3,
      totalRun: 3,
      passed: 2,
      failed: 1,
      wrapperInfraFailures: 0,
      timeouts: 0,
      durationSeconds: 1,
      byCategory: [],
    },
  };
  const data: Test262DashboardData = {
    ...createTest262DashboardFallback("empty", "No reports"),
    status: "ready",
    latest,
    timeline: [{ ...latest, summary: { ...latest.summary, passed: 3 } }],
  };

  test("uses the ready dashboard's readable latest report, not the timeline", () => {
    expect(latestTest262Summary(data)).toEqual(latest.summary);
  });

  test("returns null when ready data has no latest report", () => {
    expect(latestTest262Summary({ ...data, latest: null })).toBeNull();
  });

  test("reports whether the run covered the annexB category", () => {
    const category = {
      run: 1,
      passed: 1,
      failed: 0,
      wrapperInfra: 0,
      timeouts: 0,
    };
    expect(test262RunIncludesAnnexB(latest.summary)).toBe(false);
    expect(
      test262RunIncludesAnnexB({
        ...latest.summary,
        byCategory: [{ category: "language", ...category }],
      }),
    ).toBe(false);
    expect(
      test262RunIncludesAnnexB({
        ...latest.summary,
        byCategory: [
          { category: "language", ...category },
          { category: "annexB", ...category },
        ],
      }),
    ).toBe(true);
  });

  test.each([
    "error",
    "empty",
    "needs-blob-credentials",
  ] as const)("returns null for %s even if a latest report is present", (status) => {
    expect(latestTest262Summary({ ...data, status })).toBeNull();
  });
});

describe("test262 dashboard data helpers", () => {
  test("normalizes report JSON and ranks path groups", () => {
    const report = normalizeTest262Report({
      summary: {
        totalDiscovered: 100,
        totalRun: 100,
        passed: 75,
        failed: 25,
        wrapperInfraFailures: 0,
        timeouts: 0,
        durationSeconds: 12,
        byCategory: [
          {
            category: "built-ins",
            run: 50,
            passed: 40,
            failed: 10,
            wrapperInfra: 0,
            timeouts: 0,
          },
        ],
      },
      results: [
        ...Array.from({ length: 30 }, (_, i) => ({
          id: `built-ins/Array/test-${i}.js`,
          status: i < 30 ? "PASS" : "FAIL",
          durationMs: 1,
          message: "ok",
        })),
        ...Array.from({ length: 30 }, (_, i) => ({
          id: `language/statements/test-${i}.js`,
          status: i < 6 ? "PASS" : "FAIL",
          durationMs: 1,
          message: "ok",
        })),
      ],
    });

    expect(report).not.toBeNull();
    if (!report) throw new Error("expected normalized report");
    const ranked = rankGroupCoverage(report);
    expect(ranked.mostCovered[0]).toMatchObject({
      key: "built-ins/Array",
      attempted: 30,
      passed: 30,
    });
    expect(ranked.leastCovered[0]).toMatchObject({
      key: "language/statements",
      attempted: 30,
      passed: 6,
    });
  });

  const modeCounts = (run: number, passed: number) => ({
    run,
    passed,
    failed: run - passed,
    wrapperInfra: 0,
    timeouts: 0,
  });

  test("keeps mode and byMode, and counts both runs of a file in group coverage", () => {
    const report = normalizeTest262Report({
      summary: {
        totalDiscovered: 30,
        totalRun: 60,
        passed: 45,
        failed: 15,
        byMode: {
          "non-strict": modeCounts(30, 30),
          strict: modeCounts(30, 15),
        },
        byCategory: [
          {
            category: "language",
            run: 60,
            passed: 45,
            failed: 15,
            byMode: {
              "non-strict": modeCounts(30, 30),
              strict: modeCounts(30, 15),
            },
          },
        ],
      },
      results: Array.from({ length: 30 }, (_, i) => [
        { id: `language/types/t-${i}.js`, mode: "non-strict", status: "PASS" },
        {
          id: `language/types/t-${i}.js`,
          mode: "strict",
          status: i < 15 ? "PASS" : "FAIL",
        },
      ]).flat(),
    });
    if (!report) throw new Error("expected normalized report");

    expect(report.summary.totalDiscovered).toBe(30);
    expect(report.summary.totalRun).toBe(60);
    expect(report.summary.byMode?.strict).toEqual(modeCounts(30, 15));
    expect(report.summary.byCategory[0]?.byMode?.["non-strict"].passed).toBe(
      30,
    );
    expect(report.results.filter((r) => r.mode === "strict")).toHaveLength(30);

    // 30 files x 2 runs clears the 25-run floor that 30 runs alone would not
    // for a 15-file group.
    const [group] = groupCoverage(report);
    expect(group).toMatchObject({
      key: "language/types",
      attempted: 60,
      passed: 45,
      failed: 15,
    });
    expect(group?.byMode?.["non-strict"]).toEqual(modeCounts(30, 30));
    expect(group?.byMode?.strict).toEqual(modeCounts(30, 15));
    expect(groupCoverage(report, 61)).toEqual([]);
  });

  test("does not invent modes for a report that predates them", () => {
    const report = normalizeTest262Report({
      summary: {
        totalDiscovered: 40,
        totalRun: 40,
        passed: 30,
        failed: 10,
        byCategory: [{ category: "language", run: 40, passed: 30, failed: 10 }],
      },
      results: Array.from({ length: 40 }, (_, i) => ({
        id: `language/types/t-${i}.js`,
        status: i < 30 ? "PASS" : "FAIL",
      })),
    });
    if (!report) throw new Error("expected normalized report");

    expect(report.summary.byMode).toBeUndefined();
    expect(report.summary.byCategory[0]?.byMode).toBeUndefined();
    expect("byMode" in report.summary).toBe(false);
    expect(report.results.every((r) => r.mode === undefined)).toBe(true);
    const [group] = groupCoverage(report);
    expect(group).toMatchObject({ attempted: 40, passed: 30 });
    expect(group?.byMode).toBeUndefined();
    // The published (re-serialized) report must not gain a mode either.
    expect(JSON.stringify(report)).not.toContain("byMode");
    expect(JSON.stringify(report)).not.toContain('"mode"');
  });

  test("treats a partial byMode or an unknown mode as unavailable", () => {
    const report = normalizeTest262Report({
      summary: {
        totalRun: 2,
        passed: 2,
        byMode: { strict: modeCounts(1, 1) },
        byCategory: [
          { category: "language", run: 2, passed: 2, byMode: "strict" },
        ],
      },
      results: [
        { id: "language/a.js", mode: "module", status: "PASS" },
        { id: "language/b.js", mode: "strict", status: "PASS" },
      ],
    });
    if (!report) throw new Error("expected normalized report");

    expect(report.summary.byMode).toBeUndefined();
    expect(report.summary.byCategory[0]?.byMode).toBeUndefined();
    expect(report.results.map((r) => r.mode)).toEqual([undefined, "strict"]);
  });

  test("extracts the JSON report from a GitHub artifact ZIP", () => {
    const json = JSON.stringify({ summary: { totalRun: 1 }, results: [] });
    const zip = buildZip("test262-results.json", json);
    expect(extractJsonFromZip(zip, "test262-results.json")).toBe(json);
  });

  test("falls back to the newest readable report when the newest blob is bad", async () => {
    const older: Test262BlobRun = {
      runId: 1,
      runNumber: 1,
      title: "CI",
      headSha: "older",
      shortSha: "older",
      runUrl: "https://example.test/runs/1",
      createdAt: "2026-06-10T01:00:00.000Z",
      updatedAt: "2026-06-10T01:05:00.000Z",
      artifactId: 1001,
      artifactCreatedAt: "2026-06-10T01:05:00.000Z",
      jsonUrl: "/api/test262/results/1001",
      reportPath: "test262/runs/1001.json.gz",
      reportUrl: "https://blob.test/test262/runs/1001.json.gz",
      reportDownloadUrl:
        "https://blob.test/test262/runs/1001.json.gz?download=1",
      reportCompressedSize: 128,
      publishedAt: "2026-06-10T01:06:00.000Z",
      summary: {
        totalDiscovered: 1,
        totalRun: 1,
        passed: 1,
        failed: 0,
        wrapperInfraFailures: 0,
        timeouts: 0,
        durationSeconds: 1,
        byCategory: [],
      },
    };
    const newest: Test262BlobRun = {
      ...older,
      runId: 2,
      runNumber: 2,
      headSha: "newest",
      shortSha: "newest",
      runUrl: "https://example.test/runs/2",
      createdAt: "2026-06-11T01:00:00.000Z",
      updatedAt: "2026-06-11T01:05:00.000Z",
      artifactId: 1002,
      artifactCreatedAt: "2026-06-11T01:05:00.000Z",
      jsonUrl: "/api/test262/results/1002",
      reportPath: "test262/runs/1002.json.gz",
      reportUrl: "https://blob.test/test262/runs/1002.json.gz",
      reportDownloadUrl:
        "https://blob.test/test262/runs/1002.json.gz?download=1",
      publishedAt: "2026-06-11T01:06:00.000Z",
    };

    const result = await readNewestValidTest262Report(
      [older, newest],
      async (run) => {
        if (run.artifactId === 1002) {
          throw new Error("simulated blob read error");
        }
        return JSON.stringify({
          summary: older.summary,
          results: [{ id: "built-ins/Array/a.js", status: "PASS" }],
        });
      },
    );

    expect(result.latestReport?.summary.totalRun).toBe(1);
    expect(result.usableTimeline.map((run) => run.artifactId)).toEqual([1001]);
  });
});

describe("test262 dashboard component helpers", () => {
  test("formats timeline tooltips from generated category summaries", () => {
    const point = {
      runId: 622,
      runNumber: 622,
      title: "CI",
      headSha: "1699f7b7abcdef",
      shortSha: "1699f7b7",
      runUrl: "https://example.test/runs/622",
      createdAt: "2026-06-09T12:00:00.000Z",
      updatedAt: "2026-06-09T12:21:00.000Z",
      artifactId: 262622,
      artifactCreatedAt: "2026-06-09T12:21:00.000Z",
      jsonUrl: "/api/test262/results/262622",
      summary: {
        totalDiscovered: 52_233,
        totalRun: 52_233,
        passed: 41_765,
        failed: 10_460,
        wrapperInfraFailures: 0,
        timeouts: 8,
        durationSeconds: 1_242,
        byCategory: [
          {
            category: "built-ins",
            run: 20_000,
            passed: 15_000,
            failed: 4_997,
            wrapperInfra: 0,
            timeouts: 3,
          },
          {
            category: "future-category",
            run: 25,
            passed: 5,
            failed: 20,
            wrapperInfra: 0,
            timeouts: 0,
          },
        ],
      },
    };
    const tooltip = formatTest262TimelineTooltip(point);

    expect(tooltip).toBe(
      [
        "Jun 9, 2026 - run #622 - 1699f7b7",
        "Total: 80.0% (41,765 passed / 52,233 run)",
        "Built-ins: 75.0% (15,000 passed / 20,000 run)",
        "Future-Category: 20.0% (5 passed / 25 run)",
      ].join("\n"),
    );
  });

  test("keeps runtime chart tooltip focused on duration", () => {
    const tooltip = formatTest262DurationTooltip({
      runId: 622,
      runNumber: 622,
      title: "CI",
      headSha: "1699f7b7abcdef",
      shortSha: "1699f7b7",
      runUrl: "https://example.test/runs/622",
      createdAt: "2026-06-09T12:00:00.000Z",
      updatedAt: "2026-06-09T12:21:00.000Z",
      artifactId: 262622,
      artifactCreatedAt: "2026-06-09T12:21:00.000Z",
      jsonUrl: "/api/test262/results/262622",
      summary: {
        totalDiscovered: 52_233,
        totalRun: 52_233,
        passed: 41_765,
        failed: 10_460,
        wrapperInfraFailures: 0,
        timeouts: 8,
        durationSeconds: 1_242,
        byCategory: [],
      },
    });

    expect(tooltip).toContain("Duration 20.7 min");
    expect(tooltip).toContain("10,460 failed, 8 timeouts");
  });

  test("humanizes generated top-level test262 category names", () => {
    expect(formatTest262CategoryLabel("built-ins")).toBe("Built-ins");
    expect(formatTest262CategoryLabel("intl402")).toBe("Intl402");
    expect(formatTest262CategoryLabel("future-category")).toBe(
      "Future-Category",
    );
  });
});

describe("test262 dashboard per-mode rendering", () => {
  const counts = (run: number, passed: number) => ({
    run,
    passed,
    failed: run - passed,
    wrapperInfra: 0,
    timeouts: 0,
  });
  const summary = {
    totalDiscovered: 50,
    totalRun: 100,
    passed: 90,
    failed: 10,
    wrapperInfraFailures: 0,
    timeouts: 0,
    durationSeconds: 60,
    byCategory: [
      { category: "language", ...counts(100, 90) },
    ] as Test262SuiteSummary["byCategory"],
  };
  const point = (s: Test262SuiteSummary) => ({
    runId: 1,
    runNumber: 7,
    title: "CI",
    headSha: "abc1234",
    shortSha: "abc1234",
    runUrl: "https://example.test/runs/1",
    createdAt: "2026-06-10T01:00:00.000Z",
    updatedAt: "2026-06-10T01:05:00.000Z",
    artifactId: 1,
    artifactCreatedAt: "2026-06-10T01:05:00.000Z",
    jsonUrl: "/api/test262/results/1",
    summary: s,
  });
  const html = (s: Test262SuiteSummary, results: Test262Record[] = []) => {
    const report = { summary: s, results };
    return renderToStaticMarkup(
      createElement(Test262Dashboard, {
        data: createTest262DashboardData({
          generatedAt: "2026-06-10T02:00:00.000Z",
          timeline: [point(s)],
          latestReport: report,
        }),
      }),
    );
  };

  test("formats per-mode rates, and returns null without modes", () => {
    expect(test262ModeRates(undefined)).toBeNull();
    expect(
      test262ModeRates(
        { "non-strict": counts(50, 49), strict: counts(50, 41) },
        " · ",
      ),
    ).toBe("non-strict 98.0% · strict 82.0%");
    expect(
      test262ModeRates({
        "non-strict": counts(50, 49),
        strict: counts(0, 0),
      }),
    ).toBe("non-strict 98.0%, strict n/a");
  });

  test("shows strict and non-strict side by side when the report has modes", () => {
    const markup = html({
      ...summary,
      byMode: { "non-strict": counts(50, 49), strict: counts(50, 41) },
      byCategory: [
        {
          ...summary.byCategory[0],
          byMode: { "non-strict": counts(50, 49), strict: counts(50, 41) },
        },
      ] as Test262SuiteSummary["byCategory"],
    });

    expect(markup).toContain("Strict and non-strict");
    expect(markup).toContain("98.0%");
    expect(markup).toContain("82.0%");
    expect(markup).toContain("49 passed / 50 run");
    expect(markup).toContain("non-strict 98.0% · strict 82.0%");
    expect(markup).toContain("90 / 100 runs");
    expect(markup).toContain("50 test files");
    expect(markup).not.toContain("Per-mode figures are not available");
  });

  test("renders per-mode group columns, with n/a for a mode a group never ran", () => {
    const rec = (id: string, mode: Test262Record["mode"], pass: boolean) =>
      ({ id, mode, status: pass ? "PASS" : "FAIL" }) as Test262Record;
    // language/both: 25 files in both modes; strict fails 5 of them.
    // language/strictonly: 25 onlyStrict files, so no non-strict run at all.
    const results = [
      ...Array.from({ length: 25 }, (_, i) => [
        rec(`language/both/t-${i}.js`, "non-strict", true),
        rec(`language/both/t-${i}.js`, "strict", i >= 5),
      ]).flat(),
      ...Array.from({ length: 25 }, (_, i) =>
        rec(`language/strictonly/t-${i}.js`, "strict", true),
      ),
    ];
    const markup = html(
      {
        ...summary,
        byMode: { "non-strict": counts(25, 25), strict: counts(50, 45) },
      },
      results,
    );

    expect(markup).toContain("<th>Non-strict</th>");
    expect(markup).toContain("<th>Strict</th>");
    expect(markup).toContain("<td>100.0%</td><td>80.0%</td>");
    expect(markup).toContain("<td>n/a</td><td>100.0%</td>");
  });

  test("shows modes as unavailable, not guessed, for a legacy report", () => {
    const markup = html(summary);

    expect(markup).toContain("Strict and non-strict");
    expect(markup).toContain("Per-mode figures are not available");
    expect(markup).not.toContain("non-strict 9");
    expect(markup).not.toContain(">Strict<");
    expect(markup).not.toContain("<th>Strict</th>");
    // The timeline says earlier points ran each file once.
    expect(markup).toContain("ran each test file once");
  });
});
