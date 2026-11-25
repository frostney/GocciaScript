import path from "node:path";
import type { Nodes } from "mdast";
import { fromMarkdown } from "mdast-util-from-markdown";
import { type DocsPage, docsSource } from "@/lib/docs-source";

const REPOSITORY_BLOB_URL =
  "https://github.com/frostney/GocciaScript/blob/main";

/** Where a link in a repository document points on the site. A link to
 *  another document becomes that page's route; a link to any other
 *  repository file goes to GitHub, since the site serves only README.md and
 *  docs/**\/*.md. */
export function resolveRepositoryHref(href: string, page: DocsPage): string {
  if (
    href.startsWith("#") ||
    href.startsWith("/") ||
    /^[a-z][a-z\d+.-]*:/i.test(href)
  ) {
    return docsSource.resolveHref(href, page);
  }

  const match = href.match(/^([^?#]*)(\?[^#]*)?(#.*)?$/);
  const pathname = match?.[1] ?? href;
  const suffix = `${match?.[2] ?? ""}${match?.[3] ?? ""}`;
  const repositoryPath = path.posix.normalize(
    path.posix.join(path.posix.dirname(page.path), pathname),
  );
  const candidates = [
    repositoryPath,
    `${repositoryPath}.md`,
    path.posix.join(repositoryPath, "README.md"),
  ];
  const target = docsSource
    .getPages()
    .find((candidate) => candidates.includes(candidate.path));

  if (target) return `${target.url}${suffix}`;
  return `${REPOSITORY_BLOB_URL}/${repositoryPath}${suffix}`;
}

/** The document's Markdown with every link and link definition resolved as
 *  the HTML page resolves it. Repository-relative targets such as
 *  `adr/0016-….md` would otherwise resolve against the page's URL to routes
 *  the site does not serve. Only the destinations change: each is replaced
 *  at the offset the parser reports, so code and the rest of the text stay
 *  byte for byte. */
export function resolveRepositoryLinks(
  markdown: string,
  page: DocsPage,
): string {
  const replacements: { start: number; end: number; url: string }[] = [];
  const visit = (node: Nodes) => {
    if (node.type === "link" || node.type === "definition") {
      const start = node.position?.start.offset;
      const end = node.position?.end.offset;
      const resolved = resolveRepositoryHref(node.url, page);
      if (start !== undefined && end !== undefined && resolved !== node.url) {
        // The destination is the last occurrence of the URL in the node's
        // source: a link's text comes before it, a definition's label too.
        const at = markdown.slice(start, end).lastIndexOf(node.url);
        if (at >= 0) {
          replacements.push({
            start: start + at,
            end: start + at + node.url.length,
            url: resolved,
          });
        }
      }
    }
    if ("children" in node) for (const child of node.children) visit(child);
  };
  visit(fromMarkdown(markdown));

  let result = markdown;
  for (const { start, end, url } of replacements.sort(
    (a, b) => b.start - a.start,
  )) {
    result = result.slice(0, start) + url + result.slice(end);
  }
  return result;
}
