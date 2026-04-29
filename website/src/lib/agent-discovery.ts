export const API_CATALOG_PATH = "/.well-known/api-catalog";

export const AGENT_DISCOVERY_LINK_HEADER = [
  `<${API_CATALOG_PATH}>; rel="api-catalog"; type="application/linkset+json"; profile="https://www.rfc-editor.org/info/rfc9727"`,
  `</docs>; rel="service-doc"; type="text/html"`,
].join(", ");

const API_ENDPOINTS = [
  { href: "/api/execute", type: "application/json" },
  { href: "/api/test", type: "application/json" },
] as const;

const SERVICE_DOCS = [
  { href: "/docs", type: "text/html" },
  { href: "/docs/testing-api", type: "text/html" },
] as const;

const DESCRIPTIONS = [{ href: "/docs/language", type: "text/html" }] as const;

function absoluteLinks(
  origin: string,
  links: readonly { href: string; type: string }[],
) {
  return links.map((link) => ({
    href: new URL(link.href, origin).toString(),
    type: link.type,
  }));
}

export function buildApiCatalog(origin: string) {
  return {
    linkset: [
      {
        anchor: new URL(API_CATALOG_PATH, origin).toString(),
        item: absoluteLinks(origin, API_ENDPOINTS),
        "service-doc": absoluteLinks(origin, SERVICE_DOCS),
        describedby: absoluteLinks(origin, DESCRIPTIONS),
      },
    ],
  };
}
