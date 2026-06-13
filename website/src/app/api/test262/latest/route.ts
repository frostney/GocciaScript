import {
  readLatestTest262ReportJson,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";

export const dynamic = "force-dynamic";

export async function GET() {
  const json = await readLatestTest262ReportJson();
  if (!json) {
    return Response.json(
      {
        error:
          "Latest test262 report unavailable. No Vercel Blob report is available.",
      },
      {
        headers: { "cache-control": TEST262_DASHBOARD_CACHE_CONTROL },
        status: 503,
      },
    );
  }

  return new Response(json, {
    headers: {
      "content-type": "application/json; charset=utf-8",
      "cache-control": TEST262_DASHBOARD_CACHE_CONTROL,
    },
  });
}
