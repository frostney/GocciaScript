import {
  loadTest262DashboardData,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";
import { compatibilityPercent } from "@/lib/test262-percent";

export const dynamic = "force-dynamic";

export async function GET() {
  const { latest } = await loadTest262DashboardData();
  return Response.json(
    {
      schemaVersion: 1,
      label: "test262",
      message: latest
        ? `${compatibilityPercent(latest.summary.passed, latest.summary.totalRun)} passing`
        : "see dashboard",
      color: latest ? "blue" : "lightgrey",
    },
    { headers: { "cache-control": TEST262_DASHBOARD_CACHE_CONTROL } },
  );
}
