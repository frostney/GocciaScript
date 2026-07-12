import {
  loadPerformanceDashboardData,
  PERFORMANCE_DASHBOARD_CACHE_CONTROL,
} from "@/lib/performance-dashboard";

export async function GET() {
  return Response.json(await loadPerformanceDashboardData(), {
    headers: { "Cache-Control": PERFORMANCE_DASHBOARD_CACHE_CONTROL },
  });
}
