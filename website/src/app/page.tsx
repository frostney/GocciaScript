import type { Metadata } from "next";
import { headers } from "next/headers";
import { Landing } from "@/components/landing";
import { fetchLatestRelease } from "@/lib/github";
import { parseAcceptLanguage } from "@/lib/locale";
import { buildHomeStructuredData } from "@/lib/positioning";
import { getSiteUrl } from "@/lib/site-url";

export const metadata: Metadata = {
  alternates: { canonical: "/" },
  openGraph: { url: "/" },
};

export default async function HomePage() {
  const [release, hdrs] = await Promise.all([fetchLatestRelease(), headers()]);
  const locale = parseAcceptLanguage(hdrs.get("accept-language"));
  const structuredData = buildHomeStructuredData(getSiteUrl());
  const serializedStructuredData = JSON.stringify(structuredData).replace(
    /</g,
    "\\u003c",
  );
  return (
    <>
      <script
        type="application/ld+json"
        // biome-ignore lint/security/noDangerouslySetInnerHtml: Next.js recommends this JSON-LD pattern; the server-owned payload escapes "<" above.
        dangerouslySetInnerHTML={{ __html: serializedStructuredData }}
      />
      <Landing release={release} locale={locale} />
    </>
  );
}
