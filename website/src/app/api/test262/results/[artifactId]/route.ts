import {
  loadTest262DashboardData,
  readTest262ReportJsonByArtifactId,
} from "@/lib/test262-dashboard";

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
        "cache-control": "public, max-age=0, s-maxage=900",
        "x-goccia-test262-report": artifactId,
      },
    });
  }

  const data = await loadTest262DashboardData();
  const point = data.timeline.find(
    (entry) => entry.artifactId === artifactIdNumber,
  );
  const blobUrl = point?.reportDownloadUrl || point?.reportUrl;
  if (blobUrl) {
    return Response.redirect(blobUrl, 302);
  }

  return Response.json(
    {
      error:
        "test262 report unavailable. No build-time test262 snapshot exists for this report.",
    },
    { status: 503 },
  );
}
