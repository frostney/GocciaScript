import type { NextConfig } from "next";
import { AGENT_DISCOVERY_LINK_HEADER } from "./src/lib/agent-discovery";
import { CANONICAL_SITE_URL } from "./src/lib/site-url";

const nextConfig: NextConfig = {
  async redirects() {
    return [
      {
        source: "/:path*",
        has: [{ type: "host", value: "gocciascript.dev" }],
        destination: `${CANONICAL_SITE_URL}/:path*`,
        statusCode: 301,
      },
    ];
  },
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
    ];
  },
  outputFileTracingIncludes: {
    "/api/execute": ["./vendor/**"],
    "/api/test": ["./vendor/**"],
  },
};

export default nextConfig;
