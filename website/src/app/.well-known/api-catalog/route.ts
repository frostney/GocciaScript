import { API_CATALOG_PATH, buildApiCatalog } from "@/lib/agent-discovery";

export const dynamic = "force-dynamic";

export function GET(req: Request) {
  const origin = new URL(req.url).origin;
  return new Response(JSON.stringify(buildApiCatalog(origin), null, 2), {
    headers: {
      "Content-Type":
        'application/linkset+json; profile="https://www.rfc-editor.org/info/rfc9727"; charset=utf-8',
      Link: `<${API_CATALOG_PATH}>; rel="self"; type="application/linkset+json"`,
    },
  });
}
