import {
  buildMcpServerCard,
  MCP_SERVER_CARD_PATH,
  versionFromReleaseTag,
} from "@/lib/agent-discovery";
import { fetchLatestRelease } from "@/lib/github";

export const dynamic = "force-dynamic";

export function buildMcpServerCardResponse(
  req: Request,
  tagName: string | null | undefined,
) {
  const origin = new URL(req.url).origin;
  const selfHref = new URL(MCP_SERVER_CARD_PATH, origin).toString();

  return Response.json(
    buildMcpServerCard(origin, versionFromReleaseTag(tagName)),
    {
      headers: {
        Link: `<${selfHref}>; rel="self"; type="application/json"`,
      },
    },
  );
}

export async function GET(req: Request) {
  const release = await fetchLatestRelease();

  return buildMcpServerCardResponse(req, release?.tagName);
}
