import { markdownResponseHeaders } from "@/lib/markdown-negotiation";
import { createSiteMarkdown } from "@/lib/site-markdown";

export const runtime = "nodejs";
export const dynamic = "force-dynamic";

type MarkdownContext = {
  params: Promise<{ path?: string[] }>;
};

async function buildResponse(
  request: Request,
  context: MarkdownContext,
  includeBody: boolean,
): Promise<Response> {
  const { path } = await context.params;
  const { searchParams } = new URL(request.url);
  const markdown = await createSiteMarkdown(path, searchParams);

  if (markdown === null) {
    const body = [
      "---",
      "title: Not Found",
      "description: No markdown representation is available for this route.",
      "---",
      "",
      "# Not Found",
      "",
      "No markdown representation is available for this route.",
    ].join("\n");
    return new Response(includeBody ? body : null, {
      status: 404,
      headers: markdownResponseHeaders(body),
    });
  }

  return new Response(includeBody ? markdown : null, {
    headers: markdownResponseHeaders(markdown),
  });
}

export async function GET(request: Request, context: MarkdownContext) {
  return buildResponse(request, context, true);
}

export async function HEAD(request: Request, context: MarkdownContext) {
  return buildResponse(request, context, false);
}
