import path from "node:path";
import type { Nodes, Root } from "mdast";
import { type CompileContext, fromMarkdown } from "mdast-util-from-markdown";
import { gfmFromMarkdown } from "mdast-util-gfm";
import { gfm } from "micromark-extension-gfm";
import type { Token } from "micromark-util-types";
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

type Destination = {
  start: number;
  end: number;
  node: Nodes;
};

/** Each link, image and definition destination, at its exact offsets.
 *  The parser opens the link, image or definition node before its
 *  destination, so that node is on top of the stack when the destination
 *  starts. */
function parseDestinations(markdown: string): {
  tree: Root;
  destinations: Destination[];
} {
  const destinations: Destination[] = [];
  function record(this: CompileContext, token: Token) {
    const node = this.stack.at(-1);
    if (node) {
      destinations.push({
        start: token.start.offset,
        end: token.end.offset,
        node: node as Nodes,
      });
    }
    // The default handler for both tokens.
    this.buffer();
  }
  const tree = fromMarkdown(markdown, {
    // Fumadocs parses the docs as GitHub Flavored Markdown, so footnotes,
    // tables and autolinks parse here as they do there.
    extensions: [gfm()],
    mdastExtensions: [
      gfmFromMarkdown(),
      {
        enter: {
          resourceDestinationString: record,
          definitionDestinationString: record,
        },
      },
    ],
  });
  return { tree, destinations };
}

/** Definition identifiers that links use, and those images use. */
function referenceIdentifiers(tree: Root): {
  links: Set<string>;
  images: Set<string>;
} {
  const links = new Set<string>();
  const images = new Set<string>();
  const visit = (node: Nodes) => {
    if (node.type === "linkReference") links.add(node.identifier);
    if (node.type === "imageReference") images.add(node.identifier);
    if ("children" in node) for (const child of node.children) visit(child);
  };
  visit(tree);
  return { links, images };
}

/** A resolved URL as a Markdown destination: routes and GitHub URLs need no
 *  escaping beyond the characters a bare destination cannot hold. */
function destinationText(url: string): string {
  return url.replace(/[\s()<>]/g, (character) => encodeURIComponent(character));
}

const resolvedDocuments = new Map<string, { source: string; result: string }>();

/** The document's Markdown with each link destination resolved as the HTML
 *  page resolves it. Repository-relative targets such as `adr/0016-….md`
 *  would otherwise resolve against the page's URL to routes the site does
 *  not serve. Only destinations change, at the offsets the parser reports,
 *  so titles, code and the rest of the text stay byte for byte. Images keep
 *  their sources, as on the HTML page, and so does a definition an image
 *  uses. */
export function resolveRepositoryLinks(
  markdown: string,
  page: DocsPage,
): string {
  const cached = resolvedDocuments.get(page.path);
  if (cached?.source === markdown) return cached.result;

  const { tree, destinations } = parseDestinations(markdown);
  const references = referenceIdentifiers(tree);
  const replacements: { start: number; end: number; text: string }[] = [];
  for (const { start, end, node } of destinations) {
    if (node.type === "definition") {
      if (
        !references.links.has(node.identifier) ||
        references.images.has(node.identifier)
      ) {
        continue;
      }
    } else if (node.type !== "link") {
      continue;
    }
    const resolved = resolveRepositoryHref(node.url, page);
    if (resolved !== node.url) {
      replacements.push({ start, end, text: destinationText(resolved) });
    }
  }

  let result = markdown;
  for (const { start, end, text } of replacements.sort(
    (a, b) => b.start - a.start,
  )) {
    result = result.slice(0, start) + text + result.slice(end);
  }
  resolvedDocuments.set(page.path, { source: markdown, result });
  return result;
}
