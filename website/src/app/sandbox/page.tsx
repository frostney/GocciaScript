import type { Metadata } from "next";
import { Sandbox } from "@/components/sandbox";

export const metadata: Metadata = {
  title: "Sandbox",
  description:
    "Run untrusted GocciaScript with explicit globals, capability gates, seeded virtual filesystems, and diffable sandbox writes.",
  alternates: { canonical: "/sandbox" },
  openGraph: {
    title: "Sandbox · GocciaScript",
    description:
      "A runtime designed for code you didn't write, with explicit capabilities and a seeded virtual filesystem runner.",
    url: "/sandbox",
  },
  twitter: {
    title: "Sandbox · GocciaScript",
    description:
      "A runtime designed for code you didn't write, with explicit capabilities and a seeded virtual filesystem runner.",
  },
};

export default function SandboxPage() {
  return <Sandbox />;
}
