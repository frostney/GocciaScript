import {
  buildOAuthProtectedResourceMetadata,
  OAUTH_PROTECTED_RESOURCE_PATH,
} from "@/lib/agent-discovery";

export const dynamic = "force-dynamic";

export function GET(req: Request) {
  const origin = new URL(req.url).origin;
  const selfHref = new URL(OAUTH_PROTECTED_RESOURCE_PATH, origin).toString();

  return Response.json(buildOAuthProtectedResourceMetadata(origin), {
    headers: {
      Link: `<${selfHref}>; rel="self"; type="application/json"`,
    },
  });
}
