import type { Metadata } from "next";
import { Install } from "@/components/install";
import { fetchLatestRelease } from "@/lib/github";

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
  const release = await fetchLatestRelease();
  return <Install release={release} />;
}
