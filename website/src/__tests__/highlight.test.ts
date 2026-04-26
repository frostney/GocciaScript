import { describe, expect, test } from "bun:test";
import {
  highlightGeneric,
  highlightGoccia,
  highlightJson,
} from "@/lib/highlight";

/** Concatenate a token stream's raw text. The highlighter must round-trip
 *  the input verbatim — this is the cheap regression check that catches
 *  the OOM-on-`@` infinite loop and any other "scanner doesn't advance"
 *  bug class. */
function joinText<T extends { text: string }>(tokens: T[]): string {
  return tokens.map((t) => t.text).join("");
}

describe("highlightGeneric — Pascal", () => {
  // Regression for the build-time OOM caused by `@` appearing in the
  // identifier-start charset but not in the continuation charset, which
  // left `j === i` and looped forever.
  test("does not loop on Pascal address-of operator (`@MyProc`)", () => {
    const src = "var p: TProc; p := @MyProc;";
    const tokens = highlightGeneric(src, "pascal");
    expect(joinText(tokens)).toBe(src);
  });

  test("recognizes `{...}` block comments in Pascal", () => {
    const src = "var x: Integer; { single-line block comment } x := 1;";
    const tokens = highlightGeneric(src, "pascal");
    const comment = tokens.find((t) => t.cls === "c");
    expect(comment?.text).toBe("{ single-line block comment }");
    expect(joinText(tokens)).toBe(src);
  });

  test("recognizes `(* … *)` block comments in Pascal", () => {
    const src = "begin (* legacy comment *) end.";
    const tokens = highlightGeneric(src, "pascal");
    const comment = tokens.find((t) => t.cls === "c");
    expect(comment?.text).toBe("(* legacy comment *)");
    expect(joinText(tokens)).toBe(src);
  });

  test("does NOT swallow `{...}` in C-family code", () => {
    // For TS, `{...}` is an object/block — must not become a comment.
    const src = "const obj = { a: 1, b: 2 };";
    const tokens = highlightGeneric(src, "ts");
    const commentTokens = tokens.filter((t) => t.cls === "c");
    expect(commentTokens).toEqual([]);
    expect(joinText(tokens)).toBe(src);
  });

  test("Pascal keywords are tokenized case-insensitively", () => {
    const src = "BEGIN END Begin End begin end";
    const tokens = highlightGeneric(src, "pascal");
    const keywordHits = tokens.filter((t) => t.cls === "k").length;
    expect(keywordHits).toBe(6);
  });

  test("string literals across multiple lines are still bounded", () => {
    // The string scanner should advance to the closing quote or EOF, not
    // run forever on an unterminated string.
    const src = '"unterminated string\nwith newline';
    const tokens = highlightGeneric(src, "ts");
    expect(joinText(tokens)).toBe(src);
  });
});

describe("highlightGoccia — JS/TS dispatcher", () => {
  test("never produces an empty token (non-advancing scanner)", () => {
    const src = "const x = 1 + 2;\n@decorator\nclass C {}\n";
    const tokens = highlightGoccia(src);
    for (const t of tokens) expect(t.text.length).toBeGreaterThan(0);
    expect(joinText(tokens)).toBe(src);
  });
});

describe("highlightJson", () => {
  test("distinguishes object keys from string values", () => {
    const src = '{ "name": "alice", "age": 30 }';
    const tokens = highlightJson(src);
    const keys = tokens.filter((t) => t.cls === "tok-key");
    const strings = tokens.filter((t) => t.cls === "tok-string");
    expect(keys.map((t) => t.text)).toEqual(['"name"', '"age"']);
    expect(strings.map((t) => t.text)).toEqual(['"alice"']);
  });

  test("classifies literals (`true`, `false`, `null`) as keywords", () => {
    const src = '{ "ok": true, "skip": false, "pad": null }';
    const tokens = highlightJson(src);
    const kws = tokens.filter((t) => t.cls === "tok-kw").map((t) => t.text);
    expect(kws).toEqual(["true", "false", "null"]);
  });

  test("numbers are tokenized correctly (incl. negatives + scientific)", () => {
    const src = "[1, -2, 3.14, 1e10, -1.5e-3]";
    const tokens = highlightJson(src);
    const nums = tokens
      .filter((t) => t.cls === "tok-number")
      .map((t) => t.text);
    expect(nums).toEqual(["1", "-2", "3.14", "1e10", "-1.5e-3"]);
  });

  test("round-trips arbitrary JSON verbatim", () => {
    const src = '[\n  { "a": 1 },\n  { "b": [true, null] }\n]';
    expect(joinText(highlightJson(src))).toBe(src);
  });
});
