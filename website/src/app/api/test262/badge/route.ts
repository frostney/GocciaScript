import {
  loadTest262Headline,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";
import { compatibilityPercent } from "@/lib/test262-percent";

export const dynamic = "force-dynamic";

export async function GET() {
  const headline = await loadTest262Headline();
  return Response.json(
    {
      schemaVersion: 1,
      label: "test262",
      message: headline
        ? `${compatibilityPercent(headline.passed, headline.totalRun)} passing`
        : "see dashboard",
      color: headline ? "blue" : "lightgrey",
    },
    { headers: { "cache-control": TEST262_DASHBOARD_CACHE_CONTROL } },
  );
}
