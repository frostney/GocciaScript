import type { Metadata } from "next";
import { Suspense } from "react";
import { Playground } from "@/components/playground";
import { fetchRecentStableTags, pickPrecedenceVersions } from "@/lib/github";

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
  // Pull a wider window of recent stable tags so the precedence picker
  // has enough history to find a major (`x.0.0`) and the previous minor
  // cycle's `x.y.0`. The picker then reduces this down to up to three
  // pins (patch / minor / major-with-fallback). The client appends
  // `nightly` after these.
  const recent = await fetchRecentStableTags(20);
  const stableTags = pickPrecedenceVersions(recent);
  return (
    <Suspense>
      <Playground stableTags={stableTags} />
    </Suspense>
  );
}
