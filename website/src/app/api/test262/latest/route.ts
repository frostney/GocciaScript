import { readLatestTest262ReportJson } from "@/lib/test262-dashboard";

export async function GET() {
  const json = await readLatestTest262ReportJson();
  if (!json) {
    return Response.json(
      {
        error:
          "Latest test262 report unavailable. No build-time test262 snapshot was generated.",
      },
      { status: 503 },
    );
  }

  return new Response(json, {
    headers: {
      "content-type": "application/json; charset=utf-8",
      "cache-control": "public, max-age=0, s-maxage=900",
    },
  });
}
