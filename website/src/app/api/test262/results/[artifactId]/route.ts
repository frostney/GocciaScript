import {
  readTest262ReportJsonByArtifactId,
  TEST262_DASHBOARD_CACHE_CONTROL,
} from "@/lib/test262-dashboard";

export const dynamic = "force-dynamic";

export async function GET(
  _request: Request,
  { params }: { params: Promise<{ artifactId: string }> },
) {
  const { artifactId } = await params;
  if (!/^\d+$/.test(artifactId)) {
    return Response.json({ error: "Invalid artifact id." }, { status: 400 });
  }
  const artifactIdNumber = Number(artifactId);
  if (!Number.isSafeInteger(artifactIdNumber) || artifactIdNumber <= 0) {
    return Response.json({ error: "Invalid artifact id." }, { status: 400 });
  }

  const json = await readTest262ReportJsonByArtifactId(artifactIdNumber);
  if (json) {
    return new Response(json, {
      headers: {
        "content-type": "application/json; charset=utf-8",
        "cache-control": TEST262_DASHBOARD_CACHE_CONTROL,
        "x-goccia-test262-report": artifactId,
      },
    });
  }

  return Response.json(
    {
      error:
        "test262 report unavailable. No Vercel Blob report exists for this artifact.",
    },
    {
      headers: { "cache-control": TEST262_DASHBOARD_CACHE_CONTROL },
      status: 503,
    },
  );
}
