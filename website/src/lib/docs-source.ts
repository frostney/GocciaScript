import { loader } from "fumadocs-core/source";
import { docs } from "../../.source/server";

function slugsFromRepositoryPath(file: { path: string }): string[] {
  if (file.path === "README.md") return [];

  const relative = file.path.replace(/^docs\//, "").replace(/\.mdx?$/i, "");
  const segments = relative.split("/");

  // Keep the site's established flat URLs while deriving them mechanically
  // from the source tree: docs/contributing/workflow.md becomes
  // /docs/contributing-workflow and docs/adr/README.md becomes /docs/adr.
  if (segments.at(-1)?.toLowerCase() === "readme") segments.pop();
  return segments.length === 0 ? [] : [segments.join("-")];
}

export const docsSource = loader({
  baseUrl: "/docs",
  source: docs.toFumadocsSource(),
  slugs: slugsFromRepositoryPath,
});

export type DocsPage = NonNullable<ReturnType<typeof docsSource.getPage>>;

export function docsPageTitle(page: DocsPage): string {
  return page.data.title || "GocciaScript documentation";
}

export function docsPageDescription(page: DocsPage): string {
  return (
    page.data.description ||
    "Reference documentation for GocciaScript — language, built-ins, architecture, and embedding."
  );
}
