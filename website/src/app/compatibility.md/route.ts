import { markdownResponseHeaders } from "@/lib/markdown-negotiation";
import { createSiteMarkdown } from "@/lib/site-markdown";

export const runtime = "nodejs";
export const dynamic = "force-dynamic";

export async function GET() {
  const markdown = await createSiteMarkdown(["compatibility"]);
  if (markdown === null) {
    return new Response("Compatibility summary unavailable.", { status: 503 });
  }
  return new Response(markdown, {
    headers: markdownResponseHeaders(markdown),
  });
}
