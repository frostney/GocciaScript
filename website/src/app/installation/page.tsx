import type { Metadata } from "next";
import { headers } from "next/headers";
import { Install } from "@/components/install";
import { fetchLatestRelease } from "@/lib/github";
import { parseAcceptLanguage } from "@/lib/locale";

export const metadata: Metadata = {
  title: "Installation",
  description:
    "Install GocciaScript — one-line installer, prebuilt binaries, or build from source. Single self-contained runtime, no Node.js required.",
  alternates: { canonical: "/installation" },
  openGraph: {
    title: "Install · GocciaScript",
    description:
      "Install GocciaScript — one-line installer, prebuilt binaries, or build from source.",
    url: "/installation",
  },
  twitter: {
    title: "Install · GocciaScript",
    description:
      "Install GocciaScript — one-line installer, prebuilt binaries, or build from source.",
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
