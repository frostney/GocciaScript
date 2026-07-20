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
