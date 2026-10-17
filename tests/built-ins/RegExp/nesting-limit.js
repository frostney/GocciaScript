/*---
description: A pattern nested past the supported depth is a SyntaxError, not a crash
features: [RegExp, Goccia]
---*/

// The pattern compiler is recursive descent: each group, lookaround and
// nested class costs native stack frames, and matching recurses once per
// nested lookaround. A pattern may nest 1,023 such levels inside the pattern
// itself (1,024 with it); a deeper one is a SyntaxError. ECMA-262 sets no
// limit, and Node.js accepts somewhat deeper patterns: the error is this
// engine's limit, reported instead of exhausting the native stack, which used
// to kill the process at about 30,000 levels.
const nested = (open, close, depth, inner = "a") =>
  open.repeat(depth) + inner + close.repeat(depth);

const LIMIT_MESSAGE = "nesting exceeds the supported limit of 1024 levels";

describe("RegExp nesting limit", () => {
  test("1,023 nested capturing groups compile and match", () => {
    const re = new RegExp(nested("(", ")", 1023));
    const match = re.exec("a");
    expect(match.length).toBe(1024);
    expect(match[1023]).toBe("a");
  });

  test("1,024 nested capturing groups are a SyntaxError", () => {
    expect(() => new RegExp(nested("(", ")", 1024))).toThrow(SyntaxError);
  });

  test("the error names the limit", () => {
    let message = "";
    try {
      new RegExp(nested("(", ")", 30000));
    } catch (error) {
      expect(error instanceof SyntaxError).toBe(true);
      message = error.message;
    }
    expect(message.includes(LIMIT_MESSAGE)).toBe(true);
  });

  test("non-capturing and named groups count the same way", () => {
    expect(new RegExp(nested("(?:", ")", 1023)).test("a")).toBe(true);
    expect(() => new RegExp(nested("(?:", ")", 1024))).toThrow(SyntaxError);
    expect(new RegExp("(?<outer>" + nested("(?:", ")", 1022) + ")").exec("a").groups.outer).toBe("a");
    expect(() => new RegExp("(?<outer>" + nested("(?:", ")", 1023) + ")")).toThrow(SyntaxError);
  });

  test("nested groups that each have a name count the same way", () => {
    // Each named group records the groups open around it, so a deep nest of
    // them must be refused before that record is built.
    const named = (depth) =>
      Array.from({ length: depth }, (_, i) => `(?<g${i}>`).join("") + "a" + ")".repeat(depth);
    expect(new RegExp(named(1023)).exec("a").groups.g1022).toBe("a");
    expect(() => new RegExp(named(1024))).toThrow(SyntaxError);
    expect(() => new RegExp(named(30000))).toThrow(SyntaxError);
  });

  test("nested lookaheads and lookbehinds match up to the limit", () => {
    expect(new RegExp(nested("(?=", ")", 1023)).test("a")).toBe(true);
    expect(new RegExp(nested("(?<=", ")", 1023)).test("a")).toBe(true);
    // An odd number of nested negative lookaheads around a failing "b".
    expect(new RegExp(nested("(?!", ")", 1023, "b")).test("a")).toBe(true);
    expect(new RegExp(nested("(?!", ")", 1022, "b")).test("a")).toBe(false);
    expect(() => new RegExp(nested("(?=", ")", 1024))).toThrow(SyntaxError);
    expect(() => new RegExp(nested("(?<!", ")", 30000))).toThrow(SyntaxError);
  });

  test("nested character classes under the v flag count too", () => {
    expect(new RegExp(nested("[", "]", 1023), "v").test("a")).toBe(true);
    expect(() => new RegExp(nested("[", "]", 1024), "v")).toThrow(SyntaxError);
    expect(() => new RegExp(nested("[", "]", 30000), "v")).toThrow(SyntaxError);
  });

  test("groups and classes share one count", () => {
    expect(new RegExp(nested("(?:", ")", 511, nested("[", "]", 512)), "v").test("a")).toBe(true);
    expect(() => new RegExp(nested("(?:", ")", 512, nested("[", "]", 512)), "v")).toThrow(SyntaxError);
  });

  test("width, alternatives and repetition do not count", () => {
    expect(new RegExp("a".repeat(30000)).test("a".repeat(30000))).toBe(true);
    expect(new RegExp("a|".repeat(30000) + "b").test("b")).toBe(true);
    expect(new RegExp("(?:a)".repeat(30000)).test("a".repeat(30000))).toBe(true);
    expect(new RegExp("[a]".repeat(30000), "v").test("a".repeat(30000))).toBe(true);
  });

  test("the engine keeps working after the error", () => {
    expect(() => new RegExp(nested("(", ")", 30000))).toThrow(SyntaxError);
    expect(() => new RegExp(nested("(", ")", 30000))).toThrow(SyntaxError);
    expect(/(a)(b)/.exec("ab")[2]).toBe("b");
  });
});
