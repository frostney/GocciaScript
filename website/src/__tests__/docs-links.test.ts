import { describe, expect, test } from "bun:test";
import type { Nodes } from "mdast";
import { fromMarkdown } from "mdast-util-from-markdown";
import { gfmFromMarkdown } from "mdast-util-gfm";
import { gfm } from "micromark-extension-gfm";
import {
  resolveRepositoryHref,
  resolveRepositoryLinks,
} from "@/lib/docs-links";
import { docsSource } from "@/lib/docs-source";

/** Every link URL and table row width in a document, parsed as GFM. */
function parsed(markdown: string): { links: string[]; rowWidths: number[] } {
  const links: string[] = [];
  const rowWidths: number[] = [];
  const visit = (node: Nodes) => {
    if (node.type === "link") links.push(node.url);
    if (node.type === "tableRow") rowWidths.push(node.children.length);
    if ("children" in node) for (const child of node.children) visit(child);
  };
  visit(
    fromMarkdown(markdown, {
      extensions: [gfm()],
      mdastExtensions: [gfmFromMarkdown()],
    }),
  );
  return { links, rowWidths };
}

function page(slug: string) {
  const found = docsSource.getPage([slug]);
  if (!found) throw new Error(`no docs page ${slug}`);
  return found;
}

describe("repository links in Markdown responses", () => {
  test("resolve a document's links to the routes its HTML page uses", async () => {
    const builtIns = page("built-ins");
    // bun test does not compile MDX, so read the source the processed text
    // is made from; its links are the same.
    const markdown = await builtIns.data.getText("raw");
    const resolved = resolveRepositoryLinks(markdown, builtIns);
    expect(markdown).toContain(
      "](adr/0113-deterministic-virtual-timer-queue.md)",
    );
    expect(resolved).toContain(
      "](/docs/adr-0113-deterministic-virtual-timer-queue)",
    );
    expect(resolved).not.toMatch(/\]\((?!https?:|\/|#|mailto:)[^)]*\.md[)#]/);
  });

  test("leave code, anchors and external links as written", () => {
    const markdown = [
      "See [the ADR](adr/0016-ieee-754-number-representation.md#context), [values][v] and [math](#math).",
      "",
      "Inline `[code](adr/0016-ieee-754-number-representation.md)` stays.",
      "",
      "```md",
      "[fenced](adr/0016-ieee-754-number-representation.md)",
      "```",
      "",
      "[site](https://example.com/a.md) and [unit](../source/units/Goccia.FloatingPoint.pas).",
      "",
      "[v]: value-system.md",
    ].join("\n");
    expect(resolveRepositoryLinks(markdown, page("built-ins"))).toBe(
      [
        "See [the ADR](/docs/adr-0016-ieee-754-number-representation#context), [values][v] and [math](#math).",
        "",
        "Inline `[code](adr/0016-ieee-754-number-representation.md)` stays.",
        "",
        "```md",
        "[fenced](adr/0016-ieee-754-number-representation.md)",
        "```",
        "",
        "[site](https://example.com/a.md) and [unit](https://github.com/frostney/GocciaScript/blob/main/source/units/Goccia.FloatingPoint.pas).",
        "",
        "[v]: /docs/value-system",
      ].join("\n"),
    );
  });

  test("replace only the destination, never a title that repeats it", () => {
    const markdown = [
      '[guide](value-system.md "Read value-system.md first") and [ref][v].',
      "",
      '[v]: value-system.md "value-system.md"',
    ].join("\n");
    expect(resolveRepositoryLinks(markdown, page("built-ins"))).toBe(
      [
        '[guide](/docs/value-system "Read value-system.md first") and [ref][v].',
        "",
        '[v]: /docs/value-system "value-system.md"',
      ].join("\n"),
    );
  });

  test("resolve an escaped destination and leave an empty one empty", () => {
    expect(
      resolveRepositoryLinks(
        "[query](value-system.md\\?a=1) and [empty]().",
        page("built-ins"),
      ),
    ).toBe("[query](/docs/value-system?a=1) and [empty]().");
  });

  test("keep image sources, including a definition only an image uses", () => {
    const markdown = [
      "![logo][logo] and ![inline](./logo.png)",
      "",
      "[logo]: ./logo.png",
    ].join("\n");
    expect(resolveRepositoryLinks(markdown, page("built-ins"))).toBe(markdown);
  });

  test("resolve a link inside a footnote and keep the footnote's prose", () => {
    const markdown = [
      "Text.[^note]",
      "",
      "[^note]: Explanation",
      "    with [values](value-system.md).",
    ].join("\n");
    expect(resolveRepositoryLinks(markdown, page("built-ins"))).toBe(
      [
        "Text.[^note]",
        "",
        "[^note]: Explanation",
        "    with [values](/docs/value-system).",
      ].join("\n"),
    );
  });

  test("write each destination so it parses to the URL the HTML page uses", () => {
    const builtIns = page("built-ins");
    const markdown = [
      "| link | other |",
      "| --- | --- |",
      "| [pipe](value-system.md?x=\\|) | b |",
      "",
      "[paren](value-system.md#example\\)suffix) and [entity](value-system.md?x=\\&copy;).",
    ].join("\n");
    const before = parsed(markdown);
    const after = parsed(resolveRepositoryLinks(markdown, builtIns));
    expect(after.rowWidths).toEqual(before.rowWidths);
    expect(after.links).toEqual(
      before.links.map((url) => resolveRepositoryHref(url, builtIns)),
    );
    expect(after.links).toEqual([
      "/docs/value-system?x=|",
      "/docs/value-system#example)suffix",
      "/docs/value-system?x=&copy;",
    ]);
  });

  test("keep a docs route when the fragment holds an encoded line break", () => {
    expect(
      resolveRepositoryLinks(
        "[x](value-system.md#sample&#10;line)",
        page("built-ins"),
      ),
    ).toBe("[x](/docs/value-system#sample%0Aline)");
  });
});
