import type { Metadata } from "next";
import { Suspense } from "react";
import { Playground } from "@/components/playground";
import {
  binaryNames,
  listPlaygroundVersions,
  resolveAsiFlag,
  resolvePublicDefaultVersion,
} from "@/lib/vendor-manifest";
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
  // The ASI flag was renamed `--asi` -> `--compat-asi` after 0.7.x, and the
  // loader and test runner advertise it independently. Resolve both per version
  // (null = the API omits ASI) so the playground banner matches the invocation.
  const asiFlags = Object.fromEntries(
    manifest.versions.map((entry) => [
      entry.tag,
      {
        loader: resolveAsiFlag(entry.features, "loader"),
        testRunner: resolveAsiFlag(entry.features, "testRunner"),
      },
    ]),
  );
  // The loader binary was renamed `GocciaScriptLoader` -> `GocciaRunner` in
  // 0.14.0; the banner names whichever binary each version's archive shipped.
  const runnerNames = Object.fromEntries(
    manifest.versions.map((entry) => [entry.tag, binaryNames(entry)]),
  );
  return (
    <Suspense>
      <Playground
        versions={versions}
        defaultVersion={resolvePublicDefaultVersion(manifest)}
        asiFlags={asiFlags}
        runnerNames={runnerNames}
      />
    </Suspense>
  );
}
