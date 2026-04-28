import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  outputFileTracingIncludes: {
    "/api/execute": ["./vendor/**"],
    "/api/test": ["./vendor/**"],
  },
};

export default nextConfig;
