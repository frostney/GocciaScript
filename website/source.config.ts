import { defineConfig, defineDocs } from "fumadocs-mdx/config";
import { z } from "zod";

function titleFromMarkdown(source: string, path: string): string {
  const heading = source.match(/^#\s+(.+)$/m)?.[1]?.trim();
  if (heading) {
    return heading
      .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1")
      .replace(/[*_`]/g, "");
  }
  return (
    path
      .replace(/(?:^|\/)README\.md$/i, "")
      .replace(/\.mdx?$/i, "")
      .split("/")
      .at(-1)
      ?.replace(/[-_]+/g, " ") ?? "GocciaScript"
  );
}

export const docs = defineDocs({
  // The repository Markdown is the source of truth. Fumadocs compiles it in
  // place, so the website no longer needs a copied content tree.
  dir: "..",
  docs: {
    files: ["README.md", "docs/**/*.md"],
    // Existing docs use their first H1 as the title. Adapt that convention at
    // the collection boundary rather than adding website-only frontmatter to
    // every repository document.
    schema: ({ source, path }) =>
      z
        .object({
          title: z.string().optional(),
          description: z.string().optional(),
        })
        .transform((frontmatter) => ({
          ...frontmatter,
          title: frontmatter.title ?? titleFromMarkdown(source, path),
        })),
    lastModified: true,
    postprocess: {
      includeProcessedMarkdown: true,
    },
  },
  // There are no Fumadocs ordering files in the repository. Narrowing the
  // meta glob prevents the repo-root collection from treating unrelated JSON
  // files (including dependencies) as documentation metadata.
  meta: {
    files: ["docs/**/meta.json"],
  },
});

export default defineConfig();
