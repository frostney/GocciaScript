// Server-only — relies on Node `fs`. Importing this from a client component
// will fail at bundle time (no fallback for `node:fs/promises`), which is the
// behaviour we want.
import fs from "node:fs/promises";
import path from "node:path";
import { DOC_PAGES } from "@/lib/docs-data";

const DOCS_ROOT = path.join(process.cwd(), "content", "docs");

/**
 * Read the markdown source for a doc page from the synced `content/docs/`
 * tree. Returns `null` when the file is missing — callers render a stub.
 */
export async function readDocSource(id: string): Promise<string | null> {
  const page = DOC_PAGES.find((p) => p.id === id);
  if (!page?.file) return null;
  const full = path.join(DOCS_ROOT, page.file);
  try {
    return await fs.readFile(full, "utf8");
  } catch {
    return null;
  }
}
