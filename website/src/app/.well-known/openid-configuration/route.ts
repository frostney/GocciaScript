import {
  buildOpenIdConfiguration,
  OPENID_CONFIGURATION_PATH,
} from "@/lib/agent-discovery";

export const dynamic = "force-dynamic";

export function GET(req: Request) {
  const origin = new URL(req.url).origin;
  const selfHref = new URL(OPENID_CONFIGURATION_PATH, origin).toString();

  return Response.json(buildOpenIdConfiguration(origin), {
    headers: {
      Link: `<${selfHref}>; rel="self"; type="application/json"`,
    },
  });
}
