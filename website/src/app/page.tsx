import type { Metadata } from "next";
import { Landing } from "@/components/landing";
import { fetchLatestRelease } from "@/lib/github";

export const metadata: Metadata = {
  alternates: { canonical: "/" },
  openGraph: { url: "/" },
};

export default async function HomePage() {
  const release = await fetchLatestRelease();
  return <Landing release={release} />;
}
