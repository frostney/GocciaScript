import type { Metadata } from "next";
import { Suspense } from "react";
import { Playground } from "@/components/playground";
import { asiFlagName, listPlaygroundVersions } from "@/lib/vendor-manifest";
import { getVendorManifest } from "@/lib/vendor-manifest-server";

export const metadata: Metadata = {
  title: "Playground",
  description:
    "Run GocciaScript in the browser — pick an example or write your own. Real engine, real output, sandboxed.",
  alternates: { canonical: "/playground" },
  openGraph: {
    title: "Playground · GocciaScript",
    description:
      "Run GocciaScript in the browser — real engine, real output, sandboxed.",
    url: "/playground",
  },
  twitter: {
    title: "Playground · GocciaScript",
    description:
      "Run GocciaScript in the browser — real engine, real output, sandboxed.",
  },
};

export default async function PlaygroundPage() {
  // Versions vendored at build time by `scripts/fetch-binaries.ts`. The
  // manifest is the source of truth: it lists what's actually executable
  // server-side, in the order the dropdown should display them (newest
  // stable first, `nightly` last).
  const manifest = getVendorManifest();
  const versions = listPlaygroundVersions(manifest);
  // The ASI flag was renamed `--asi` -> `--compat-asi` after 0.7.x. Resolve the
  // name per version from each binary's probed features so the playground's
  // command banner shows the flag the selected binary actually receives.
  const asiFlags = Object.fromEntries(
    manifest.versions.map((entry) => [
      entry.tag,
      asiFlagName(entry.features, "loader"),
    ]),
  );
  return (
    <Suspense>
      <Playground
        versions={versions}
        defaultVersion={manifest.defaultVersion}
        asiFlags={asiFlags}
      />
    </Suspense>
  );
}
