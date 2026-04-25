import { describe, expect, test } from "bun:test";
import { parseMarkdown, safeHref } from "@/components/markdown";

describe("safeHref — link scheme allowlist", () => {
  test("allows http(s) URLs", () => {
    expect(safeHref("https://example.com/x")).toBe("https://example.com/x");
    expect(safeHref("http://example.com/x")).toBe("http://example.com/x");
  });

  test("allows mailto and tel", () => {
    expect(safeHref("mailto:hi@example.com")).toBe("mailto:hi@example.com");
    expect(safeHref("tel:+15551234567")).toBe("tel:+15551234567");
  });

  test("allows in-page anchors and relative paths through unchanged", () => {
    expect(safeHref("#section")).toBe("#section");
    expect(safeHref("/docs/architecture")).toBe("/docs/architecture");
    expect(safeHref("../sibling.md")).toBe("../sibling.md");
    expect(safeHref("page.md")).toBe("page.md");
  });

  test("rejects javascript: scheme regardless of casing", () => {
    expect(safeHref("javascript:alert(1)")).toBeNull();
    expect(safeHref("JavaScript:alert(1)")).toBeNull();
    expect(safeHref("JAVASCRIPT:alert(1)")).toBeNull();
  });

  test("rejects other dangerous schemes", () => {
    expect(safeHref("data:text/html,<script>alert(1)</script>")).toBeNull();
    expect(safeHref("vbscript:msgbox")).toBeNull();
    expect(safeHref("file:///etc/passwd")).toBeNull();
  });

  test("rejects empty string", () => {
    expect(safeHref("")).toBeNull();
  });
});

describe("parseMarkdown — block segmentation", () => {
  test("breaks paragraphs at horizontal rules instead of swallowing them", () => {
    const blocks = parseMarkdown("Some paragraph text.\n---\nNext paragraph.");
    const kinds = blocks.map((b) => b.kind);
    expect(kinds).toEqual(["p", "hr", "p"]);
  });

  test("breaks paragraphs at table starts", () => {
    const src = [
      "Intro paragraph that introduces a table.",
      "| col1 | col2 |",
      "| ---- | ---- |",
      "| a    | b    |",
    ].join("\n");
    const blocks = parseMarkdown(src);
    expect(blocks.map((b) => b.kind)).toEqual(["p", "table"]);
    const tableBlock = blocks.find((b) => b.kind === "table");
    if (tableBlock?.kind !== "table") throw new Error("expected table block");
    expect(tableBlock.hdr).toEqual(["col1", "col2"]);
    expect(tableBlock.rows).toEqual([["a", "b"]]);
  });

  test("breaks paragraphs at headings, fences, blockquotes, lists (existing rule)", () => {
    const src = [
      "Paragraph before.",
      "## Heading",
      "Paragraph after heading.",
      "```js",
      "x = 1;",
      "```",
      "Paragraph after fence.",
      "> blockquote",
      "Paragraph after quote.",
      "- list item",
    ].join("\n");
    const blocks = parseMarkdown(src);
    expect(blocks.map((b) => b.kind)).toEqual([
      "p",
      "heading",
      "p",
      "code",
      "p",
      "quote",
      "p",
      "list",
    ]);
  });

  test("preserves a paragraph that legitimately contains a `-` (just not at start)", () => {
    const src = "A paragraph with a hyphen-word inside.";
    const blocks = parseMarkdown(src);
    expect(blocks).toEqual([{ kind: "p", text: src }]);
  });

  test("recognizes mixed-case fences as their canonical kind (sanity)", () => {
    const blocks = parseMarkdown("```js\nfoo\n```");
    expect(blocks).toEqual([{ kind: "code", lang: "js", code: "foo" }]);
  });

  test("table cells preserve escaped pipes (`\\|`)", () => {
    const src = [
      "| left | right |",
      "| ---- | ----- |",
      "| a \\| b | c |",
    ].join("\n");
    const blocks = parseMarkdown(src);
    if (blocks[0]?.kind !== "table") throw new Error("expected table");
    expect(blocks[0].rows[0]).toEqual(["a | b", "c"]);
  });
});
