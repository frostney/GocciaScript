export const MARKDOWN_CONTENT_TYPE = "text/markdown; charset=utf-8";
export const MARKDOWN_ROUTE_PREFIX = "/agent-markdown";

type AcceptEntry = {
  mediaType: string;
  quality: number;
};

function parseQuality(params: string[]): number {
  for (const param of params) {
    const [rawKey, rawValue] = param.split("=");
    if (rawKey?.trim().toLowerCase() !== "q") continue;
    const quality = Number(rawValue?.trim());
    if (!Number.isFinite(quality)) return 0;
    return Math.min(Math.max(quality, 0), 1);
  }
  return 1;
}

function parseAccept(accept: string | null | undefined): AcceptEntry[] {
  if (!accept) return [];
  return accept
    .split(",")
    .map((part) => {
      const [rawMediaType, ...params] = part.split(";");
      const mediaType = rawMediaType?.trim().toLowerCase();
      if (!mediaType) return null;
      return { mediaType, quality: parseQuality(params) };
    })
    .filter((entry): entry is AcceptEntry => entry !== null);
}

export function acceptsMarkdown(accept: string | null | undefined): boolean {
  return parseAccept(accept).some(
    (entry) => entry.mediaType === "text/markdown" && entry.quality > 0,
  );
}

export function estimateMarkdownTokens(markdown: string): number {
  const compact = markdown.trim();
  if (!compact) return 0;
  return Math.max(1, Math.ceil(compact.length / 4));
}

export function markdownResponseHeaders(markdown: string): Headers {
  return new Headers({
    "Content-Type": MARKDOWN_CONTENT_TYPE,
    Vary: "Accept",
    "x-markdown-tokens": String(estimateMarkdownTokens(markdown)),
  });
}
