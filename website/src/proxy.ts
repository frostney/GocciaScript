import type { NextRequest } from "next/server";
import { NextResponse } from "next/server";
import {
  acceptsMarkdown,
  MARKDOWN_ROUTE_PREFIX,
} from "@/lib/markdown-negotiation";

const SKIPPED_PATHS = new Set([
  "/favicon.ico",
  "/install",
  "/opengraph-image",
  "/twitter-image",
]);

function shouldSkipPath(pathname: string): boolean {
  if (pathname.startsWith("/api/")) return true;
  if (pathname.startsWith("/_next/")) return true;
  if (pathname.startsWith(`${MARKDOWN_ROUTE_PREFIX}/`)) return true;
  if (pathname === MARKDOWN_ROUTE_PREFIX) return true;
  if (SKIPPED_PATHS.has(pathname)) return true;
  return /\.[^/]+$/.test(pathname);
}

export function proxy(request: NextRequest) {
  if (request.method !== "GET" && request.method !== "HEAD") {
    return NextResponse.next();
  }
  if (!acceptsMarkdown(request.headers.get("accept"))) {
    return NextResponse.next();
  }

  const url = request.nextUrl.clone();
  if (shouldSkipPath(url.pathname)) return NextResponse.next();

  url.pathname =
    url.pathname === "/"
      ? MARKDOWN_ROUTE_PREFIX
      : `${MARKDOWN_ROUTE_PREFIX}${url.pathname}`;
  return NextResponse.rewrite(url);
}

export const config = {
  matcher: ["/:path*"],
};
