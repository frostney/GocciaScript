import type { Metadata } from "next";
import { Sandbox } from "@/components/sandbox";

export const metadata: Metadata = {
  title: "Sandbox",
  description:
    "Run AI-agent scripts under explicit host control, with capability gates, limits, structured results, and a virtual filesystem of copied inputs.",
  alternates: { canonical: "/sandbox" },
  openGraph: {
    title: "Sandbox · GocciaScript",
    description:
      "AI-agent execution under explicit host control, with capability gates, limits, structured results, and a virtual filesystem of copied inputs.",
    url: "/sandbox",
  },
  twitter: {
    title: "Sandbox · GocciaScript",
    description:
      "AI-agent execution under explicit host control, with capability gates, limits, structured results, and a virtual filesystem of copied inputs.",
  },
};

export default function SandboxPage() {
  return <Sandbox />;
}
