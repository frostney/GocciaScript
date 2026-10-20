import { describe, expect, test } from "bun:test";
import { TEST262_DASHBOARD_CACHE_CONTROL } from "@/lib/test262-dashboard";

describe("test262 headline endpoint", () => {
  test("returns the dashboard latest summary or 503 when unavailable", () => {
    // Keep loader fixtures in a separate process so other suites use the real loader.
    const script = `
      import { mock } from "bun:test";
      const fixtures = [
        {
          status: "ready",
          latest: { summary: { passed: 2, totalRun: 3 } },
          timeline: [{ summary: { passed: 3, totalRun: 3 } }],
        },
        { status: "ready", latest: null },
        { status: "error", latest: { summary: { passed: 3, totalRun: 3 } } },
      ];
      const dashboard = await import(${JSON.stringify(import.meta.resolve("@/lib/test262-dashboard"))});
      mock.module(${JSON.stringify(import.meta.resolve("@/lib/test262-dashboard"))}, () => ({
        ...dashboard,
        loadTest262DashboardData: async () => fixtures.shift(),
      }));
      const { GET } = await import(${JSON.stringify(import.meta.resolve("@/app/api/test262/headline/route"))});
      const responses = [];
      for (let i = 0; i < 3; i++) {
        const response = await GET();
        responses.push({
          status: response.status,
          cacheControl: response.headers.get("cache-control"),
          body: await response.json(),
        });
      }
      console.log(JSON.stringify(responses));
    `;
    const result = Bun.spawnSync([process.execPath, "--eval", script], {
      cwd: import.meta.dir,
    });
    expect(result.exitCode, result.stderr.toString()).toBe(0);
    const responses = JSON.parse(result.stdout.toString());
    expect(responses).toEqual([
      {
        status: 200,
        cacheControl: TEST262_DASHBOARD_CACHE_CONTROL,
        body: { passed: 2, totalRun: 3, percent: "66.7%" },
      },
      ...Array.from({ length: 2 }, () => ({
        status: 503,
        cacheControl: TEST262_DASHBOARD_CACHE_CONTROL,
        body: { error: "unavailable" },
      })),
    ]);
  });
});
