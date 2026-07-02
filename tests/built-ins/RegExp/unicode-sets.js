/*---
description: RegExp v flag (unicodeSets) semantics
features: [RegExp, UnicodeSets]
---*/

// --- Flag acceptance and properties ---

test("v flag accepted in regex literal", () => {
  const re = /abc/v;
  expect(re.unicodeSets).toBe(true);
  expect(re.flags).toBe("v");
});

test("v flag accepted in RegExp constructor", () => {
  const re = new RegExp("abc", "v");
  expect(re.unicodeSets).toBe(true);
  expect(re.flags).toBe("v");
});

test("v flag implies unicode semantics", () => {
  expect(new RegExp("abc", "v").unicode).toBe(false);
  expect(new RegExp("abc", "v").unicodeSets).toBe(true);
});

test("v and u flags are mutually exclusive", () => {
  expect(() => new RegExp("abc", "uv")).toThrow(SyntaxError);
});

test("v flag combined with other flags", () => {
  const re = new RegExp("hello", "giv");
  expect(re.global).toBe(true);
  expect(re.ignoreCase).toBe(true);
  expect(re.unicodeSets).toBe(true);
  expect(re.flags).toBe("giv");
});

test("d and v flags together", () => {
  const re = /abc/dv;
  expect(re.hasIndices).toBe(true);
  expect(re.unicodeSets).toBe(true);
  expect(re.flags).toBe("dv");
});

// --- Basic matching with v flag ---

test("basic character matching with v flag", () => {
  expect(/hello/v.test("hello")).toBe(true);
  expect(/hello/v.test("HELLO")).toBe(false);
});

test("case-insensitive matching with v flag", () => {
  expect(/hello/iv.test("HELLO")).toBe(true);
});

test("character class matching with v flag", () => {
  expect(/^[a-z]+$/v.test("hello")).toBe(true);
  expect(/^[a-z]+$/v.test("HELLO")).toBe(false);
});

test("unicode property escape with v flag", () => {
  expect(new RegExp("^\\p{ASCII}+$", "v").test("hello123")).toBe(true);
  expect(new RegExp("^\\p{Letter}+$", "v").test("hello")).toBe(true);
  expect(new RegExp("\\p{Number}", "v").test("5")).toBe(true);
});

// --- Set intersection (&&) ---

test("intersection of ASCII and Letter matches ASCII letters only", () => {
  const re = new RegExp("[\\p{ASCII}&&\\p{Letter}]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("Z")).toBe(true);
  expect(re.test("1")).toBe(false);
  expect(re.test(" ")).toBe(false);
});

test("intersection of ranges", () => {
  const re = new RegExp("[a-z&&a-f]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("f")).toBe(true);
  expect(re.test("g")).toBe(false);
  expect(re.test("z")).toBe(false);
});

test("intersection of digit ranges", () => {
  const re = new RegExp("[0-9&&[0-5]]", "v");
  expect(re.test("3")).toBe(true);
  expect(re.test("5")).toBe(true);
  expect(re.test("6")).toBe(false);
  expect(re.test("9")).toBe(false);
});

test("intersection with character class shorthand", () => {
  const re = new RegExp("[\\w&&[a-z]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("z")).toBe(true);
  expect(re.test("A")).toBe(false);
  expect(re.test("0")).toBe(false);
  expect(re.test("_")).toBe(false);
});

// --- Set subtraction (--) ---

test("subtraction removes vowels from lowercase", () => {
  const re = new RegExp("[a-z--[aeiou]]", "v");
  expect(re.test("b")).toBe(true);
  expect(re.test("c")).toBe(true);
  expect(re.test("z")).toBe(true);
  expect(re.test("a")).toBe(false);
  expect(re.test("e")).toBe(false);
  expect(re.test("i")).toBe(false);
  expect(re.test("o")).toBe(false);
  expect(re.test("u")).toBe(false);
});

test("subtraction of digit ranges", () => {
  const re = new RegExp("[0-9--[0-4]]", "v");
  expect(re.test("5")).toBe(true);
  expect(re.test("9")).toBe(true);
  expect(re.test("0")).toBe(false);
  expect(re.test("4")).toBe(false);
});

test("subtraction with property escapes", () => {
  const re = new RegExp("[\\p{ASCII}--\\p{Number}]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("!")).toBe(true);
  expect(re.test("5")).toBe(false);
});

// --- Nested character classes ---

test("nested class union", () => {
  const re = new RegExp("[[a-z][A-Z]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("A")).toBe(true);
  expect(re.test("1")).toBe(false);
});

test("nested class with multiple groups", () => {
  const re = new RegExp("[[a-f][0-3]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("f")).toBe(true);
  expect(re.test("0")).toBe(true);
  expect(re.test("3")).toBe(true);
  expect(re.test("g")).toBe(false);
  expect(re.test("4")).toBe(false);
});

test("nested class inside intersection", () => {
  const re = new RegExp("[[a-z]&&[a-f0-9]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("f")).toBe(true);
  expect(re.test("g")).toBe(false);
  expect(re.test("5")).toBe(false);
});

test("nested class inside subtraction", () => {
  const re = new RegExp("[[a-z]--[aeiou]]", "v");
  expect(re.test("b")).toBe(true);
  expect(re.test("a")).toBe(false);
});

// --- String disjunction \q{} ---

test("\\q{} matches multi-codepoint strings", () => {
  const re = new RegExp("[\\q{abc|def}]", "v");
  expect(re.test("abc")).toBe(true);
  expect(re.test("def")).toBe(true);
  expect(re.test("ab")).toBe(false);
  expect(re.test("a")).toBe(false);
});

test("\\q{} with single-codepoint alternatives", () => {
  const re = new RegExp("[\\q{a|b|c}]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("b")).toBe(true);
  expect(re.test("c")).toBe(true);
  expect(re.test("d")).toBe(false);
});

test("\\q{} mixed single and multi-codepoint", () => {
  const re = new RegExp("[\\q{abc|x|yz}]", "v");
  expect(re.test("abc")).toBe(true);
  expect(re.test("x")).toBe(true);
  expect(re.test("yz")).toBe(true);
  expect(re.test("a")).toBe(false);
  expect(re.test("y")).toBe(false);
});

test("large \\q{} string sets backtrack from prefix alternatives", () => {
  const extras = Array.from({ length: 130 }, (_, i) => "z" + i);

  const re = new RegExp("^[\\q{ab|a|" + extras.join("|") + "}]b$", "v");

  expect(re.test("ab")).toBe(true);
});

// --- Properties of strings ---

test("\\p{Emoji_Keycap_Sequence} matches keycap sequences", () => {
  const re = new RegExp("[\\p{Emoji_Keycap_Sequence}]", "v");
  expect(re.test("1️⃣")).toBe(true);
  expect(re.test("1")).toBe(false);
});

test("\\p{Basic_Emoji} matches emoji code points", () => {
  const re = new RegExp("[\\p{Basic_Emoji}]", "v");
  expect(re.test("❤️")).toBe(true);
  expect(re.test("⌚")).toBe(true);
  expect(re.test("a")).toBe(false);
});

test("\\p{RGI_Emoji} matches string and code point emoji", () => {
  const re = new RegExp("[\\p{RGI_Emoji}]", "v");
  expect(re.test("1️⃣")).toBe(true);
  expect(re.test("⌚")).toBe(true);
  expect(re.test("a")).toBe(false);
});

test("\\P{} rejected for properties of strings", () => {
  expect(() => new RegExp("[\\P{Basic_Emoji}]", "v")).toThrow(SyntaxError);
  expect(() => new RegExp("[\\P{Emoji_Keycap_Sequence}]", "v")).toThrow(SyntaxError);
});

// --- Negated v-mode classes ---

test("negated v-mode class inverts code point set", () => {
  const re = new RegExp("[^a-z]", "v");
  expect(re.test("A")).toBe(true);
  expect(re.test("1")).toBe(true);
  expect(re.test("a")).toBe(false);
  expect(re.test("z")).toBe(false);
});

test("negated class with strings throws", () => {
  expect(() => new RegExp("[^\\q{abc}]", "v")).toThrow(SyntaxError);
});

// --- \\P{} (negated property) inside v-mode class ---

test("\\P{Letter} matches non-letters in v-mode class", () => {
  const re = new RegExp("[\\P{Letter}]", "v");
  expect(re.test("1")).toBe(true);
  expect(re.test("!")).toBe(true);
  expect(re.test("a")).toBe(false);
  expect(re.test("Z")).toBe(false);
});

// --- v-mode syntax restrictions ---

test("unescaped special chars in v-mode class throw", () => {
  expect(() => new RegExp("[(]", "v")).toThrow(SyntaxError);
  expect(() => new RegExp("[)]", "v")).toThrow(SyntaxError);
  expect(() => new RegExp("[/]", "v")).toThrow(SyntaxError);
  expect(() => new RegExp("[|]", "v")).toThrow(SyntaxError);
});

test("escaped special chars in v-mode class are accepted", () => {
  expect(new RegExp("[\\(]", "v").test("(")).toBe(true);
  expect(new RegExp("[\\)]", "v").test(")")).toBe(true);
  expect(new RegExp("[\\|]", "v").test("|")).toBe(true);
});

// --- v flag with exec and match ---

test("v flag with exec returns match array", () => {
  const re = new RegExp("[\\p{ASCII}&&\\p{Letter}]+", "v");
  const result = re.exec("abc123");
  expect(result[0]).toBe("abc");
  expect(result.index).toBe(0);
});

test("v flag with String.match", () => {
  const re = new RegExp("[\\p{ASCII}&&\\p{Letter}]+", "gv");
  const matches = "abc 123 def".match(re);
  expect(matches).toEqual(["abc", "def"]);
});

test("v flag with String.replace", () => {
  const re = new RegExp("[a-z--[aeiou]]", "gv");
  const result = "hello".replace(re, "*");
  expect(result).toBe("*e**o");
});

// --- Edge cases ---

test("empty intersection yields no matches", () => {
  const re = new RegExp("[a-z&&[0-9]]", "v");
  expect(re.test("a")).toBe(false);
  expect(re.test("5")).toBe(false);
});

test("subtracting everything yields no matches", () => {
  const re = new RegExp("[a-z--[a-z]]", "v");
  expect(re.test("a")).toBe(false);
  expect(re.test("m")).toBe(false);
});

test("intersection with same set is identity", () => {
  const re = new RegExp("[a-z&&[a-z]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("z")).toBe(true);
  expect(re.test("A")).toBe(false);
});

test("subtraction of empty set is identity", () => {
  const re = new RegExp("[a-z--[]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("z")).toBe(true);
});

test("\\q{} with empty alternative", () => {
  const re = new RegExp("[\\q{|abc}]", "v");
  expect(re.test("abc")).toBe(true);
});

// --- Chained operators ---

test("chained intersection narrows progressively", () => {
  const re = new RegExp("[a-z&&[a-m]&&[a-f]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("f")).toBe(true);
  expect(re.test("g")).toBe(false);
  expect(re.test("m")).toBe(false);
  expect(re.test("z")).toBe(false);
});

test("chained subtraction removes multiple sets", () => {
  const re = new RegExp("[a-z--[aeiou]--[xyz]]", "v");
  expect(re.test("b")).toBe(true);
  expect(re.test("w")).toBe(true);
  expect(re.test("a")).toBe(false);
  expect(re.test("e")).toBe(false);
  expect(re.test("x")).toBe(false);
  expect(re.test("z")).toBe(false);
});

test("mixing && and -- at the same level throws", () => {
  expect(() => new RegExp("[a-z&&[a-f]--[abc]]", "v")).toThrow(SyntaxError);
  expect(() => new RegExp("[a-z--[abc]&&[a-f]]", "v")).toThrow(SyntaxError);
});

// --- Nested negated classes ---

test("nested negated class computes complement", () => {
  const re = new RegExp("[[^0-9]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("!")).toBe(true);
  expect(re.test("5")).toBe(false);
});

test("nested negated class in intersection", () => {
  const re = new RegExp("[[^0-9]&&[a-z]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("z")).toBe(true);
  expect(re.test("5")).toBe(false);
  expect(re.test("A")).toBe(false);
});

test("nested negated class in subtraction", () => {
  const re = new RegExp("[a-z--[^b-y]]", "v");
  expect(re.test("b")).toBe(true);
  expect(re.test("y")).toBe(true);
  expect(re.test("a")).toBe(false);
  expect(re.test("z")).toBe(false);
});

// --- \\q{} in set operations ---

test("\\q{} string intersection retains common strings", () => {
  const re = new RegExp("[\\q{abc|def|ghi}&&\\q{abc|ghi|xyz}]", "v");
  expect(re.test("abc")).toBe(true);
  expect(re.test("ghi")).toBe(true);
  expect(re.test("def")).toBe(false);
  expect(re.test("xyz")).toBe(false);
});

test("\\q{} string subtraction removes matched strings", () => {
  const re = new RegExp("[\\q{abc|def|ghi}--\\q{def}]", "v");
  expect(re.test("abc")).toBe(true);
  expect(re.test("ghi")).toBe(true);
  expect(re.test("def")).toBe(false);
});

// --- Bytecode mode ---

test("v flag works in bytecode mode", () => {
  const re = new RegExp("[\\p{ASCII}&&\\p{Letter}]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("1")).toBe(false);
});

test("set subtraction works in bytecode mode", () => {
  const re = new RegExp("[a-z--[aeiou]]", "v");
  expect(re.test("b")).toBe(true);
  expect(re.test("a")).toBe(false);
});

test("\\q{} works in bytecode mode", () => {
  const re = new RegExp("[\\q{abc|def}]", "v");
  expect(re.test("abc")).toBe(true);
  expect(re.test("a")).toBe(false);
});

// --- Nested set operators inside nested classes ---

test("nested class with set operators", () => {
  const re = new RegExp("[[a-z&&[a-f]]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("f")).toBe(true);
  expect(re.test("g")).toBe(false);
});

test("nested class with subtraction operator", () => {
  const re = new RegExp("[[a-z--[aeiou]]]", "v");
  expect(re.test("b")).toBe(true);
  expect(re.test("a")).toBe(false);
});

test("deeply nested set operators", () => {
  const re = new RegExp("[[[a-z&&[a-m]]&&[a-f]]]", "v");
  expect(re.test("a")).toBe(true);
  expect(re.test("f")).toBe(true);
  expect(re.test("g")).toBe(false);
});

// --- Character class escapes rejected as range endpoints ---

test("\\d as range start throws in v-mode", () => {
  expect(() => new RegExp("[\\d-a]", "v")).toThrow(SyntaxError);
});

test("\\w as range start throws in v-mode", () => {
  expect(() => new RegExp("[\\w-z]", "v")).toThrow(SyntaxError);
});

test("\\p{} as range endpoint throws in v-mode", () => {
  expect(() => new RegExp("[a-\\p{Letter}]", "v")).toThrow(SyntaxError);
});

// --- Case-insensitive string matching ---

test("\\q{} respects case-insensitive flag", () => {
  const re = new RegExp("[\\q{ABC|def}]", "iv");
  expect(re.test("abc")).toBe(true);
  expect(re.test("ABC")).toBe(true);
  expect(re.test("DEF")).toBe(true);
  expect(re.test("def")).toBe(true);
  expect(re.test("xyz")).toBe(false);
});
