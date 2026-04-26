import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  outputFileTracingIncludes: {
    "/api/run": ["./vendor/**"],
  },
};

export default nextConfig;
