import type { Metadata } from "next";
import { Sandbox } from "@/components/sandbox";

export const metadata: Metadata = {
  title: "Sandbox",
  description:
    "GocciaScript was built to execute code produced by AI agents and language models. No eval, no ambient globals, every capability opt-in.",
  alternates: { canonical: "/sandbox" },
  openGraph: {
    title: "Sandbox · GocciaScript",
    description:
      "A runtime designed for code you didn't write — for tinkerers, embedding and AI agents.",
    url: "/sandbox",
  },
  twitter: {
    title: "Sandbox · GocciaScript",
    description:
      "A runtime designed for code you didn't write — for tinkerers, embedding and AI agents.",
  },
};

export default function SandboxPage() {
  return <Sandbox />;
}
