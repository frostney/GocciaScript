import { describe, expect, test } from "bun:test";
import { GET as getGocciaApiSkill } from "@/app/.well-known/agent-skills/gocciascript-api/SKILL.md/route";
import { GET as getAgentSkillsIndex } from "@/app/.well-known/agent-skills/index.json/route";
import { GET } from "@/app/.well-known/api-catalog/route";
import { buildMcpServerCardResponse } from "@/app/.well-known/mcp/server-card.json/route";
import { GET as getOAuthAuthorizationServer } from "@/app/.well-known/oauth-authorization-server/route";
import { GET as getOAuthProtectedResource } from "@/app/.well-known/oauth-protected-resource/route";
import { GET as getOpenIdConfiguration } from "@/app/.well-known/openid-configuration/route";
import {
  AGENT_DISCOVERY_LINK_HEADER,
  AGENT_SKILLS_INDEX_PATH,
  API_CATALOG_PATH,
  buildAgentSkillsIndex,
  buildApiCatalog,
  buildGocciaApiSkillMd,
  buildJwks,
  buildMcpServerCard,
  buildOAuthAuthorizationServerMetadata,
  buildOAuthProtectedResourceMetadata,
  buildOpenIdConfiguration,
  GOCCIA_API_SKILL_PATH,
  MCP_SERVER_CARD_PATH,
  OAUTH_PROTECTED_RESOURCE_PATH,
  sha256Digest,
  versionFromReleaseTag,
} from "@/lib/agent-discovery";
import nextConfig from "../../next.config";

describe("agent discovery", () => {
  test("homepage advertises agent-useful Link relations", async () => {
    const headers = await nextConfig.headers?.();
    const homepage = headers?.find((entry) => entry.source === "/");
    const link = homepage?.headers.find((header) => header.key === "Link");

    expect(link?.value).toBe(AGENT_DISCOVERY_LINK_HEADER);
    expect(link?.value).toContain(`<${API_CATALOG_PATH}>; rel="api-catalog"`);
    expect(link?.value).toContain(
      `<${OAUTH_PROTECTED_RESOURCE_PATH}>; rel="oauth-protected-resource"`,
    );
    expect(link?.value).toContain(
      `<${MCP_SERVER_CARD_PATH}>; rel="mcp-server-card"`,
    );
    expect(link?.value).toContain(
      `<${AGENT_SKILLS_INDEX_PATH}>; rel="agent-skills"`,
    );
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

  test("oauth discovery metadata advertises issuer endpoints and grants", () => {
    const metadata = buildOAuthAuthorizationServerMetadata(
      "https://example.test",
    );

    expect(metadata.issuer).toBe("https://example.test");
    expect(metadata.authorization_endpoint).toBe(
      "https://example.test/oauth/authorize",
    );
    expect(metadata.token_endpoint).toBe("https://example.test/oauth/token");
    expect(metadata.jwks_uri).toBe(
      "https://example.test/.well-known/jwks.json",
    );
    expect(metadata.grant_types_supported).toContain("authorization_code");
    expect(metadata.response_types_supported).toContain("code");
  });

  test("openid configuration includes oidc-specific metadata", () => {
    const metadata = buildOpenIdConfiguration("https://example.test");

    expect(metadata.issuer).toBe("https://example.test");
    expect(metadata.subject_types_supported).toContain("public");
    expect(metadata.id_token_signing_alg_values_supported).toContain("RS256");
  });

  test("oauth protected resource metadata points agents at the issuer", () => {
    const metadata = buildOAuthProtectedResourceMetadata(
      "https://example.test",
    );

    expect(metadata.resource).toBe("https://example.test");
    expect(metadata.authorization_servers).toEqual(["https://example.test"]);
    expect(metadata.scopes_supported).toEqual([
      "goccia.execute",
      "goccia.test",
    ]);
  });

  test("jwks document is valid JSON Web Key Set metadata", () => {
    expect(buildJwks()).toEqual({ keys: [] });
  });

  test("mcp server card lists WebMCP transport and tool capabilities", () => {
    const card = buildMcpServerCard("https://example.test", "0.6.1");

    expect(card.serverInfo).toEqual({
      name: "GocciaScript",
      version: "0.6.1",
    });
    expect(card.transport.endpoint).toBe("https://example.test/");
    expect(card.capabilities.tools.map((tool) => tool.name)).toEqual([
      "goccia.execute",
      "goccia.test",
    ]);
    expect(
      card.capabilities.tools[0].inputSchema.properties.code.maxLength,
    ).toBe(8 * 1024);
    expect(
      card.capabilities.tools[0].inputSchema.properties.compatFunction
        .description,
    ).toContain("--compat-function");
  });

  test("agent skills index includes a digest for the skill artifact", () => {
    const index = buildAgentSkillsIndex("https://example.test");
    const skill = index.skills[0];

    expect(index.$schema).toBe(
      "https://schemas.agentskills.io/discovery/0.2.0/schema.json",
    );
    expect(skill).toMatchObject({
      name: "gocciascript-api",
      type: "skill-md",
      url: `https://example.test${GOCCIA_API_SKILL_PATH}`,
    });
    expect(skill.digest).toBe(
      sha256Digest(buildGocciaApiSkillMd("https://example.test")),
    );
  });

  test("release tags become MCP server versions", () => {
    expect(versionFromReleaseTag("v0.6.1")).toBe("0.6.1");
    expect(versionFromReleaseTag("0.6.1")).toBe("0.6.1");
    expect(versionFromReleaseTag(null)).toBe("nightly");
  });

  test("well-known discovery routes return expected JSON", async () => {
    const origin = "https://example.test";

    const oauth = await getOAuthAuthorizationServer(
      new Request(`${origin}/.well-known/oauth-authorization-server`),
    );
    const openid = await getOpenIdConfiguration(
      new Request(`${origin}/.well-known/openid-configuration`),
    );
    const resource = await getOAuthProtectedResource(
      new Request(`${origin}/.well-known/oauth-protected-resource`),
    );
    const skills = await getAgentSkillsIndex(
      new Request(`${origin}/.well-known/agent-skills/index.json`),
    );

    expect(await oauth.json()).toEqual(
      buildOAuthAuthorizationServerMetadata(origin),
    );
    expect(await openid.json()).toEqual(buildOpenIdConfiguration(origin));
    expect(await resource.json()).toEqual(
      buildOAuthProtectedResourceMetadata(origin),
    );
    expect(await skills.json()).toEqual(buildAgentSkillsIndex(origin));
  });

  test("agent skill artifact route serves markdown", async () => {
    const response = await getGocciaApiSkill(
      new Request(
        "https://example.test/.well-known/agent-skills/gocciascript-api/SKILL.md",
      ),
    );

    expect(response.headers.get("content-type")).toContain("text/markdown");
    const skill = await response.text();
    expect(skill).toBe(buildGocciaApiSkillMd("https://example.test"));
    expect(skill).toContain("Homepage: https://example.test/");
    expect(skill).toContain("capped at 8192 bytes (8 KiB)");
    expect(skill).toContain("`function` keyword requires `compatFunction`");
  });

  test("mcp server card route returns release-shaped server info", async () => {
    const response = buildMcpServerCardResponse(
      new Request("https://example.test/.well-known/mcp/server-card.json"),
      "v0.6.1",
    );
    const body = await response.json();

    expect(response.headers.get("link")).toContain(
      `<https://example.test${MCP_SERVER_CARD_PATH}>; rel="self"`,
    );
    expect(body.serverInfo.name).toBe("GocciaScript");
    expect(body.serverInfo.version).toBe("0.6.1");
  });
});
