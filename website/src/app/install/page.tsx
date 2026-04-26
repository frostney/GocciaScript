import type { Metadata } from "next";
import { headers } from "next/headers";
import { Install } from "@/components/install";
import { fetchLatestRelease } from "@/lib/github";
import { parseAcceptLanguage } from "@/lib/locale";

export const metadata: Metadata = {
  title: "Install",
  description:
    "Install GocciaScript — Homebrew, apt, prebuilt binaries, or build from source. Single self-contained runtime, no Node.js required.",
  alternates: { canonical: "/install" },
  openGraph: {
    title: "Install · GocciaScript",
    description:
      "Install GocciaScript — Homebrew, apt, prebuilt binaries, or build from source.",
    url: "/install",
  },
  twitter: {
    title: "Install · GocciaScript",
    description:
      "Install GocciaScript — Homebrew, apt, prebuilt binaries, or build from source.",
  },
};

export default async function InstallPage() {
  const [release, hdrs] = await Promise.all([fetchLatestRelease(), headers()]);
  // Resolve the user's locale from `Accept-Language` so SSR formats
  // dates the same way the browser would. Calling `headers()` opts
  // this route into per-request rendering — but the GitHub fetch
  // stays cached at its `revalidate: 1800` data-layer cache, so
  // we're not hitting the API per visitor.
  const locale = parseAcceptLanguage(hdrs.get("accept-language"));
  return <Install release={release} locale={locale} />;
}
