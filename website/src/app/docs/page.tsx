import type { Metadata } from "next";
import { Docs } from "@/components/docs";
import { readDocSource } from "@/lib/doc-source";

export const metadata: Metadata = {
  title: "Docs",
  description:
    "Reference documentation for GocciaScript — language, built-ins, architecture, bytecode VM, embedding.",
  alternates: { canonical: "/docs" },
  openGraph: {
    title: "Docs · GocciaScript",
    description:
      "Reference documentation for GocciaScript — language, built-ins, architecture, bytecode VM, embedding.",
    url: "/docs",
  },
  twitter: {
    title: "Docs · GocciaScript",
    description:
      "Reference documentation for GocciaScript — language, built-ins, architecture, bytecode VM, embedding.",
  },
};

export default async function DocsPage() {
  const source = await readDocSource("readme");
  return <Docs pageId="readme" source={source} />;
}
