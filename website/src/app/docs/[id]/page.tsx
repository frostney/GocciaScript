import type { Metadata } from "next";
import { notFound } from "next/navigation";
import { Docs } from "@/components/docs";
import { readDocSource } from "@/lib/doc-source";
import { DOC_PAGES } from "@/lib/docs-data";

export function generateStaticParams() {
  return DOC_PAGES.filter((p) => p.id !== "readme").map((p) => ({ id: p.id }));
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{ id: string }>;
}): Promise<Metadata> {
  const { id } = await params;
  const page = DOC_PAGES.find((p) => p.id === id);
  if (!page) return { title: "Docs" };
  const title = `${page.title} · Docs`;
  const description =
    page.desc ??
    "Reference documentation for GocciaScript — language, built-ins, architecture.";
  const url = `/docs/${id}`;
  return {
    title,
    description,
    alternates: { canonical: url },
    openGraph: {
      title: `${title} · GocciaScript`,
      description,
      url,
    },
    twitter: {
      title: `${title} · GocciaScript`,
      description,
    },
  };
}

export default async function DocsIdPage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id } = await params;
  if (!DOC_PAGES.some((p) => p.id === id)) notFound();
  const source = await readDocSource(id);
  return <Docs pageId={id} source={source} />;
}
