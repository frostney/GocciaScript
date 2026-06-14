import { describe, expect, test } from "bun:test";
import { deflateRawSync } from "node:zlib";
import {
  formatTest262CategoryLabel,
  formatTest262DurationTooltip,
  formatTest262TimelineTooltip,
} from "@/components/test262-dashboard";
import type { Test262BlobRun } from "@/lib/test262-blob-store";
import {
  extractJsonFromZip,
  normalizeTest262Report,
  rankGroupCoverage,
  readNewestValidTest262Report,
} from "@/lib/test262-dashboard";

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
