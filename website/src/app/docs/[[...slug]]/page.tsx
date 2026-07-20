import { Outline } from "@astryxdesign/core/Outline";
import type { Metadata } from "next";
import { notFound } from "next/navigation";
import { docsMdxComponents } from "@/components/docs-mdx";
import { DocsNav } from "@/components/docs-nav";
import {
  docsPageDescription,
  docsPageTitle,
  docsSource,
} from "@/lib/docs-source";

type Props = { params: Promise<{ slug?: string[] }> };

export function generateStaticParams() {
  return docsSource.generateParams();
}

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { slug } = await params;
  const page = docsSource.getPage(slug);
  if (!page) return { title: "Docs" };

  const title = docsPageTitle(page);
  const description = docsPageDescription(page);
  return {
    title: `${title} · Docs`,
    description,
    alternates: { canonical: page.url },
    openGraph: {
      title: `${title} · Docs · GocciaScript`,
      description,
      url: page.url,
    },
    twitter: { title: `${title} · Docs · GocciaScript`, description },
  };
}

export default async function DocsPage({ params }: Props) {
  const { slug } = await params;
  const page = docsSource.getPage(slug);
  if (!page) notFound();

  const Content = page.data.body;
  const pages = docsSource
    .getPages()
    .map((item) => ({
      href: item.url,
      path: item.path,
      title: docsPageTitle(item),
    }))
    .sort((a, b) => a.title.localeCompare(b.title));
  const outline = page.data.toc
    .filter((item) => typeof item.title === "string")
    .map((item) => ({
      id: item.url.replace(/^#/, ""),
      label: item.title as string,
      level: item.depth,
    }));

  return (
    <div className="container docs-route">
      <details className="docs-mobile-nav">
        <summary>Browse documentation</summary>
        <DocsNav pages={pages} activeHref={page.url} />
      </details>
      <div className="docs-shell docs-shell-astryx">
        <aside className="docs-side docs-side-astryx">
          <DocsNav pages={pages} activeHref={page.url} />
        </aside>
        <article className="docs-main docs-prose">
          <Content components={docsMdxComponents(page)} />
        </article>
        {outline.length > 0 && (
          <aside className="docs-outline">
            <Outline items={outline} density="compact" />
          </aside>
        )}
      </div>
    </div>
  );
}
