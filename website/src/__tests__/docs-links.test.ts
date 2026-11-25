import { describe, expect, test } from "bun:test";
import { resolveRepositoryLinks } from "@/lib/docs-links";
import { docsSource } from "@/lib/docs-source";

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
});
