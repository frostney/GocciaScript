import { API_CATALOG_PATH, buildApiCatalog } from "@/lib/agent-discovery";

export const dynamic = "force-dynamic";

export function GET(req: Request) {
  const origin = new URL(req.url).origin;
  const selfHref = new URL(API_CATALOG_PATH, origin).toString();
  return new Response(JSON.stringify(buildApiCatalog(origin), null, 2), {
    headers: {
      "Content-Type":
        'application/linkset+json; profile="https://www.rfc-editor.org/info/rfc9727"; charset=utf-8',
      Link: `<${selfHref}>; rel="self"; type="application/linkset+json"`,
    },
  });
}
