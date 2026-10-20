import {
  latestTest262Summary,
  loadTest262DashboardData,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";
import { compatibilityPercent } from "@/lib/test262-percent";

export const dynamic = "force-dynamic";

export async function GET() {
  const summary = latestTest262Summary(await loadTest262DashboardData());
  return Response.json(
    {
      schemaVersion: 1,
      label: "test262",
      message: summary
        ? `${compatibilityPercent(summary.passed, summary.totalRun)} passing`
        : "see dashboard",
      color: summary ? "blue" : "lightgrey",
    },
    { headers: { "cache-control": TEST262_DASHBOARD_CACHE_CONTROL } },
  );
}
