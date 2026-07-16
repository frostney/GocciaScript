import type { NextConfig } from "next";
import { AGENT_DISCOVERY_LINK_HEADER } from "./src/lib/agent-discovery";

const nextConfig: NextConfig = {
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
};

export default nextConfig;
