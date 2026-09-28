import type { Metadata } from "next";
import { Sandbox } from "@/components/sandbox";
import { TIMEOUT_MS } from "@/lib/engine-args";
import {
  binaryNames,
  findVersion,
  resolveAsiFlag,
  resolvePublicDefaultVersion,
} from "@/lib/vendor-manifest";
import { getVendorManifest } from "@/lib/vendor-manifest-server";

export const metadata: Metadata = {
  title: "Sandbox",
  description:
    "Run AI-agent scripts under explicit host control, with capability gates, limits, structured results, and a virtual filesystem of copied inputs.",
  alternates: { canonical: "/sandbox" },
  openGraph: {
    title: "Sandbox · GocciaScript",
    description:
      "AI-agent execution under explicit host control, with capability gates, limits, structured results, and a virtual filesystem of copied inputs.",
    url: "/sandbox",
  },
  twitter: {
    title: "Sandbox · GocciaScript",
    description:
      "AI-agent execution under explicit host control, with capability gates, limits, structured results, and a virtual filesystem of copied inputs.",
  },
};

export default function SandboxPage() {
  // The live demo posts to `/api/execute` without a `version`, so it runs the
  // default engine over stdin. Name that engine's binary and the flags the
  // API sends for the demo, rather than a command the page never runs.
  const manifest = getVendorManifest();
  const entry = findVersion(manifest, resolvePublicDefaultVersion(manifest));
  const binary = entry ? binaryNames(entry).loader : "GocciaRunner";
  const asiFlag = resolveAsiFlag(entry?.features, "loader");
  const apiCommand = [binary, asiFlag, `--timeout=${TIMEOUT_MS}`]
    .filter(Boolean)
    .join(" ");
  return <Sandbox apiCommand={apiCommand} />;
}
