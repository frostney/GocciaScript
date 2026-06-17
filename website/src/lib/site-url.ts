export const CANONICAL_SITE_URL = "https://www.gocciascript.dev";
export const SITE_TITLE = "GocciaScript — A drop of JavaScript";
export const SITE_DESCRIPTION =
  "Sandbox-first ECMAScript runtime for embedding and AI agents, implemented in Object Pascal, intentionally not Node-compatible, with test262-tracked compatibility.";

export function getSiteUrl(): string {
  const siteUrl = (
    process.env.NEXT_PUBLIC_SITE_URL ?? CANONICAL_SITE_URL
  ).trim();

  try {
    const url = new URL(siteUrl || CANONICAL_SITE_URL);
    if (url.hostname === "gocciascript.dev") {
      url.hostname = "www.gocciascript.dev";
    }
    return url.href.replace(/\/+$/, "");
  } catch {
    return CANONICAL_SITE_URL;
  }
}

export function getSiteUrlObject(): URL {
  return new URL(getSiteUrl());
}
