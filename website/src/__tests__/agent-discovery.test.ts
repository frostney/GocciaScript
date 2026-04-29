import { describe, expect, test } from "bun:test";
import { GET } from "@/app/.well-known/api-catalog/route";
import {
  AGENT_DISCOVERY_LINK_HEADER,
  API_CATALOG_PATH,
  buildApiCatalog,
} from "@/lib/agent-discovery";
import nextConfig from "../../next.config";

describe("agent discovery", () => {
  test("homepage advertises agent-useful Link relations", async () => {
    const headers = await nextConfig.headers?.();
    const homepage = headers?.find((entry) => entry.source === "/");
    const link = homepage?.headers.find((header) => header.key === "Link");

    expect(link?.value).toBe(AGENT_DISCOVERY_LINK_HEADER);
    expect(link?.value).toContain(`<${API_CATALOG_PATH}>; rel="api-catalog"`);
    expect(link?.value).toContain(`</docs>; rel="service-doc"`);
  });

  test("api catalog lists public API endpoints and documentation", () => {
    const catalog = buildApiCatalog("https://example.test");

    expect(catalog.linkset[0].anchor).toBe(
      "https://example.test/.well-known/api-catalog",
    );
    expect(catalog.linkset[0].item).toEqual([
      { href: "https://example.test/api/execute", type: "application/json" },
      { href: "https://example.test/api/test", type: "application/json" },
    ]);
    expect(catalog.linkset[0]["service-doc"]).toEqual([
      { href: "https://example.test/docs", type: "text/html" },
      { href: "https://example.test/docs/testing-api", type: "text/html" },
    ]);
  });

  test("api catalog route uses the Linkset media type", async () => {
    const response = await GET(
      new Request("https://example.test/.well-known/api-catalog"),
    );
    const body = await response.json();

    expect(response.headers.get("content-type")).toContain(
      "application/linkset+json",
    );
    expect(response.headers.get("link")).toContain(
      `<https://example.test${API_CATALOG_PATH}>; rel="self"`,
    );
    const itemHrefs = body.linkset[0].item.map(
      (item: { href: string }) => item.href,
    );
    expect(itemHrefs).toContain("https://example.test/api/execute");
  });
});
