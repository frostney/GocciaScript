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
});
