import type { Metadata } from "next";
import { headers } from "next/headers";
import { Landing } from "@/components/landing";
import { fetchLatestRelease } from "@/lib/github";
import { parseAcceptLanguage } from "@/lib/locale";

export const metadata: Metadata = {
  alternates: { canonical: "/" },
  openGraph: { url: "/" },
};

export default async function HomePage() {
  const [release, hdrs] = await Promise.all([fetchLatestRelease(), headers()]);
  const locale = parseAcceptLanguage(hdrs.get("accept-language"));
  return <Landing release={release} locale={locale} />;
}
