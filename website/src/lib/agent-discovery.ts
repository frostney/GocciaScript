import { createHash } from "node:crypto";
import {
  gocciaRunInputSchema,
  MAX_GOCCIA_CODE_BYTES,
} from "./goccia-tool-schema";

export const API_CATALOG_PATH = "/.well-known/api-catalog";
export const OAUTH_AUTHORIZATION_SERVER_PATH =
  "/.well-known/oauth-authorization-server";
export const OAUTH_PROTECTED_RESOURCE_PATH =
  "/.well-known/oauth-protected-resource";
export const JWKS_PATH = "/.well-known/jwks.json";
export const MCP_SERVER_CARD_PATH = "/.well-known/mcp/server-card.json";
export const AGENT_SKILLS_INDEX_PATH = "/.well-known/agent-skills/index.json";
export const GOCCIA_API_SKILL_PATH =
  "/.well-known/agent-skills/gocciascript-api/SKILL.md";

export const AGENT_DISCOVERY_LINK_HEADER = [
  `<${API_CATALOG_PATH}>; rel="api-catalog"; type="application/linkset+json"; profile="https://www.rfc-editor.org/info/rfc9727"`,
  `<${OAUTH_PROTECTED_RESOURCE_PATH}>; rel="oauth-protected-resource"; type="application/json"`,
  `<${MCP_SERVER_CARD_PATH}>; rel="mcp-server-card"; type="application/json"`,
  `<${AGENT_SKILLS_INDEX_PATH}>; rel="agent-skills"; type="application/json"`,
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

export function absoluteUrl(origin: string, path: string) {
  return new URL(path, origin).toString();
}

function absoluteLinks(
  origin: string,
  links: readonly { href: string; type: string }[],
) {
  return links.map((link) => ({
    href: absoluteUrl(origin, link.href),
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

const API_SCOPES = ["goccia.execute", "goccia.test"] as const;

export function buildOAuthAuthorizationServerMetadata(origin: string) {
  return {
    issuer: origin,
    scopes_supported: API_SCOPES,
  };
}

export function buildOAuthProtectedResourceMetadata(origin: string) {
  return {
    resource: origin,
    resource_name: "GocciaScript website APIs",
    authorization_servers: [origin],
    scopes_supported: API_SCOPES,
    bearer_methods_supported: ["header"],
    resource_documentation: absoluteUrl(origin, "/docs/testing-api"),
  };
}

export function buildJwks() {
  return { keys: [] };
}

export function buildMcpServerCard(origin: string, version = "nightly") {
  return {
    serverInfo: {
      name: "GocciaScript",
      version,
    },
    transport: {
      type: "webmcp",
      endpoint: absoluteUrl(origin, "/"),
    },
    capabilities: {
      tools: [
        {
          name: "goccia.execute",
          description:
            "Run GocciaScript source through the public browser API with optional mode, ASI, and compatibility flags.",
          inputSchema: gocciaRunInputSchema(),
        },
        {
          name: "goccia.test",
          description:
            "Run a GocciaScript test file through the public browser API with optional mode, ASI, and compatibility flags.",
          inputSchema: gocciaRunInputSchema(),
        },
      ],
      resources: [
        {
          uri: absoluteUrl(origin, "/docs"),
          name: "GocciaScript documentation",
          mimeType: "text/html",
        },
      ],
      prompts: [],
    },
  };
}

export function versionFromReleaseTag(tagName: string | null | undefined) {
  if (!tagName) return "nightly";
  return tagName.replace(/^v/i, "");
}

export function buildGocciaApiSkillMd(origin: string) {
  return `# GocciaScript API Skill

Use this skill when you need to run GocciaScript code or test files through the public GocciaScript website APIs.

GocciaScript is a sandbox-first, embeddable runtime for a curated subset of ECMAScript. It is designed for predictable generated code, embedding, and AI-agent workflows rather than full Node.js or browser compatibility.

Homepage: ${absoluteUrl(origin, "/")}
Language docs: ${absoluteUrl(origin, "/docs/language")}

## Execute code

POST JSON to \`/api/execute\`:

\`\`\`json
{
  "code": "1 + 1;",
  "mode": "interpreted",
  "asi": true,
  "compatVar": false,
  "compatFunction": false
}
\`\`\`

Options:

- \`code\` — GocciaScript source code, capped at ${MAX_GOCCIA_CODE_BYTES} bytes (8 KiB).
- \`mode\` — \`interpreted\` or \`bytecode\`, matching \`--mode=bytecode\`.
- \`asi\` — enables automatic semicolon insertion, matching \`--asi\`. The website API defaults this to \`true\`.
- \`compatVar\` — enables legacy \`var\` declarations, matching \`--compat-var\`.
- \`compatFunction\` — enables the \`function\` keyword, matching \`--compat-function\`.

The response is JSON with execution status, output, value, timing, and any structured error.

## Run tests

POST JSON to \`/api/test\` with the same fields. The code should contain GocciaScript tests using the built-in \`describe\`, \`test\`, and \`expect\` API. The response includes test counts, file results, stdout, timing, and any structured error.

## Default language subset

Prefer modern JavaScript forms that GocciaScript supports by default:

- Use \`let\` and \`const\`, not \`var\` unless \`compatVar\` is enabled.
- Use arrow functions, method shorthand, async methods, and class methods; the \`function\` keyword requires \`compatFunction\`.
- Use \`===\` and \`!==\`, not \`==\` or \`!=\`.
- Use \`for...of\`, \`for await...of\`, array methods, and iterators; traditional \`for\`, \`while\`, and \`do...while\` loops are excluded.
- Do not use \`eval\`, \`Function()\`, \`arguments\`, or \`with\`.
- Use named ES module imports/exports. Default imports/exports, side-effect-only imports, and wildcard re-exports are not supported.

## Limits

Requests are rate-limited and source code is capped at 8 KiB. These APIs are intended for short examples, docs checks, and agent-assisted exploration.
`;
}

export function sha256Digest(value: string) {
  return `sha256:${createHash("sha256").update(value, "utf8").digest("hex")}`;
}

export function buildAgentSkillsIndex(origin: string) {
  return {
    $schema: "https://schemas.agentskills.io/discovery/0.2.0/schema.json",
    skills: [
      {
        name: "gocciascript-api",
        type: "skill-md",
        description:
          "Run GocciaScript snippets and tests through the public website APIs.",
        url: absoluteUrl(origin, GOCCIA_API_SKILL_PATH),
        digest: sha256Digest(buildGocciaApiSkillMd(origin)),
      },
    ],
  };
}
