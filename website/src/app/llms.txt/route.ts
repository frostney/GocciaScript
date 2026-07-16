import { buildLlmsTxt } from "@/lib/agent-discovery";

export function GET(request: Request) {
  const origin = new URL(request.url).origin;
  return new Response(buildLlmsTxt(origin), {
    headers: {
      "cache-control": "public, max-age=0, s-maxage=3600",
      "content-type": "text/plain; charset=utf-8",
    },
  });
}
