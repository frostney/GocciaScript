import { fileURLToPath } from "node:url";
import { createMDX } from "fumadocs-mdx/next";
import { AGENT_DISCOVERY_LINK_HEADER } from "./src/lib/agent-discovery-header.mjs";

const withMDX = createMDX();

const nextConfig = {
  async headers() {
    return [
      {
        source: "/",
        headers: [
          {
            key: "Link",
            value: AGENT_DISCOVERY_LINK_HEADER,
          },
        ],
      },
      {
        source: "/compatibility",
        headers: [
          {
            key: "Link",
            value: '</compatibility.md>; rel="alternate"; type="text/markdown"',
          },
        ],
      },
    ];
  },
  // Docs pages that were removed keep their URL alive by pointing at the page
  // that now holds their content. docs/interpreter.md went with the tree-walk
  // interpreter in 0.15.0; its scope, error and microtask sections moved to
  // docs/architecture.md.
  async redirects() {
    return [
      {
        source: "/docs/interpreter",
        destination: "/docs/architecture",
        permanent: true,
      },
    ];
  },
  outputFileTracingIncludes: {
    "/api/execute": ["./vendor/**"],
    "/api/test": ["./vendor/**"],
  },
  // The documentation collection deliberately lives one level above the
  // Next.js package. Turbopack otherwise refuses to resolve those files.
  turbopack: {
    root: fileURLToPath(new URL("..", import.meta.url)),
  },
};

export default withMDX(nextConfig);
