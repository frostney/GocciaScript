"use client";

import Link from "next/link";
import { useEffect, useMemo, useState } from "react";
import { AnchorH2 } from "@/components/anchor-heading";
import { Markdown } from "@/components/markdown";
import { DOC_PAGES, type DocPage } from "@/lib/docs-data";

function findPage(id: string): DocPage {
  return DOC_PAGES.find((p) => p.id === id) ?? DOC_PAGES[0];
}

export function Docs({
  pageId = "readme",
  source,
}: {
  pageId?: string;
  /** Markdown source for the active page; null when the file isn't synced. */
  source: string | null;
}) {
  const page = findPage(pageId);
  const [navOpen, setNavOpen] = useState(false);

  // Auto-collapse the mobile sidebar whenever the active page changes.
  // biome-ignore lint/correctness/useExhaustiveDependencies: closes on route change only
  useEffect(() => {
    setNavOpen(false);
  }, [pageId]);

  const groups = useMemo(() => {
    const g: Record<string, DocPage[]> = {};
    for (const p of DOC_PAGES) {
      if (!g[p.group]) g[p.group] = [];
      g[p.group].push(p);
    }
    return g;
  }, []);

  return (
    <div className="container docs-shell">
      <aside className="docs-side">
        <button
          type="button"
          className="docs-side-toggle"
          aria-expanded={navOpen}
          aria-controls="docs-side-list"
          onClick={() => setNavOpen((o) => !o)}
        >
          <span>Browse docs</span>
          <span aria-hidden="true">{navOpen ? "▴" : "▾"}</span>
        </button>
        <div id="docs-side-list" className="docs-side-list" data-open={navOpen}>
          {Object.entries(groups).map(([group, items]) => (
            <div key={group}>
              <h5>{group}</h5>
              <ul>
                {items.map((p) => (
                  <li key={p.id}>
                    <Link
                      href={p.id === "readme" ? "/docs" : `/docs/${p.id}`}
                      data-active={pageId === p.id}
                    >
                      {p.title}
                    </Link>
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </div>
      </aside>

      <div>
        {/* `null` is reserved for "file isn't synced". An empty string is a
            real (if blank) doc and must still render via `Markdown`. */}
        {source !== null ? (
          <Markdown source={source} />
        ) : (
          <div className="docs-main">
            <h1>{page.title}</h1>
            {page.desc && (
              <p className="text-[1.15rem] text-ink-2 mt-2">{page.desc}</p>
            )}

            <div className="stub-notice">
              <strong>Not synced.</strong> The markdown file for this page
              wasn&apos;t found at <code>content/docs/{page.file}</code>. Run{" "}
              <code>bun run sync-docs</code> from the website directory to pull
              the latest content from the repo&apos;s <code>docs/</code> folder.
            </div>

            <AnchorH2>Meanwhile</AnchorH2>
            <p>
              You can read the <Link href="/docs">Overview</Link> for a summary
              of the feature set, or jump into the{" "}
              <Link href="/playground">Playground</Link> to try the language.
            </p>
          </div>
        )}
      </div>
    </div>
  );
}
