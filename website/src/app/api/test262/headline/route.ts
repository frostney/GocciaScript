import {
  latestTest262Summary,
  loadTest262DashboardData,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";
import { compatibilityPercent } from "@/lib/test262-percent";

export const dynamic = "force-dynamic";

export async function GET() {
  const summary = latestTest262Summary(await loadTest262DashboardData());
  const headers = { "cache-control": TEST262_DASHBOARD_CACHE_CONTROL };
  if (!summary) {
    return Response.json({ error: "unavailable" }, { status: 503, headers });
  }
  const { passed, totalRun } = summary;
  return Response.json(
    { passed, totalRun, percent: compatibilityPercent(passed, totalRun) },
    { headers },
  );
}
