import {
  latestTest262Summary,
  loadTest262DashboardData,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";
import { TEST262_MODES } from "@/lib/test262-modes";
import { compatibilityPercent } from "@/lib/test262-percent";

export const dynamic = "force-dynamic";

export async function GET() {
  const summary = latestTest262Summary(await loadTest262DashboardData());
  const headers = { "cache-control": TEST262_DASHBOARD_CACHE_CONTROL };
  if (!summary) {
    return Response.json({ error: "unavailable" }, { status: 503, headers });
  }
  const { passed, totalRun, byMode } = summary;
  // `totalRun` counts runs (a test in both modes counts twice). `byMode` is
  // null for a report that predates per-mode results.
  return Response.json(
    {
      passed,
      totalRun,
      percent: compatibilityPercent(passed, totalRun),
      byMode: byMode
        ? Object.fromEntries(
            TEST262_MODES.map((mode) => [
              mode,
              {
                passed: byMode[mode].passed,
                run: byMode[mode].run,
                percent: compatibilityPercent(
                  byMode[mode].passed,
                  byMode[mode].run,
                ),
              },
            ]),
          )
        : null,
    },
    { headers },
  );
}
