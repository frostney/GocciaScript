import {
  loadTest262DashboardData,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";

export const dynamic = "force-dynamic";

export async function GET() {
  const data = await loadTest262DashboardData();
  return Response.json(data, {
    headers: {
      "cache-control": TEST262_DASHBOARD_CACHE_CONTROL,
    },
  });
}
