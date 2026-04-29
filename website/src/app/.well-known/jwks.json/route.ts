import { buildJwks, JWKS_PATH } from "@/lib/agent-discovery";

export const dynamic = "force-dynamic";

export function GET(req: Request) {
  const origin = new URL(req.url).origin;
  const selfHref = new URL(JWKS_PATH, origin).toString();

  return Response.json(buildJwks(), {
    headers: {
      Link: `<${selfHref}>; rel="self"; type="application/jwk-set+json"`,
    },
  });
}
