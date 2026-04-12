/*---
description: RegExp Modifiers (?ims-ims:...)
features: [RegExp Modifiers]
---*/

// --- Enable modifiers ---

test("(?i:...) enables case-insensitive matching within the group", () => {
  const re = new RegExp("(?i:abc)def");
  expect(re.test("ABCdef")).toBe(true);
  expect(re.test("abcdef")).toBe(true);
  expect(re.test("ABCDEF")).toBe(false);
});

test("(?m:...) enables multiline matching within the group", () => {
  const re = new RegExp("(?m:^abc)");
  expect(re.test("abc")).toBe(true);
  expect(re.test("xyz\nabc")).toBe(true);
});

test("(?s:...) enables dotAll matching within the group", () => {
  const re = new RegExp("(?s:a.b)");
  expect(re.test("axb")).toBe(true);
  expect(re.test("a\nb")).toBe(true);
});

test("(?s:...) dotAll is scoped to the group", () => {
  const re = new RegExp("(?s:a.b)c.d");
  expect(re.test("a\nbc\nd")).toBe(false);
  expect(re.test("a\nbcxd")).toBe(true);
});

// --- Disable modifiers ---

test("(?-i:...) disables case-insensitive matching within the group", () => {
  const re = new RegExp("(?-i:abc)def", "i");
  expect(re.test("abcDEF")).toBe(true);
  expect(re.test("abcdef")).toBe(true);
  expect(re.test("ABCdef")).toBe(false);
});

test("(?-m:...) disables multiline within the group", () => {
  const re = new RegExp("(?-m:^abc)", "m");
  expect(re.test("abc")).toBe(true);
  expect(re.test("xyz\nabc")).toBe(false);
});

test("(?-s:...) disables dotAll within the group", () => {
  const re = new RegExp("(?-s:a.b)", "s");
  expect(re.test("axb")).toBe(true);
  expect(re.test("a\nb")).toBe(false);
});

// --- Combined enable/disable ---

test("(?im:...) enables both ignoreCase and multiline", () => {
  const re = new RegExp("(?im:^abc)");
  expect(re.test("xyz\nABC")).toBe(true);
  expect(re.test("ABC")).toBe(true);
});

test("(?ims:...) enables all three modifiers", () => {
  const re = new RegExp("(?ims:^a.b)");
  expect(re.test("xyz\nA\nB")).toBe(true);
});

test("(?i-s:...) enables ignoreCase and disables dotAll", () => {
  const re = new RegExp("(?i-s:a.b)", "s");
  expect(re.test("AXB")).toBe(true);
  expect(re.test("A\nB")).toBe(false);
});

test("(?-ims:...) disables all three modifiers", () => {
  const re = new RegExp("(?-ims:^a.b$)", "ims");
  expect(re.test("axb")).toBe(true);
  expect(re.test("AXB")).toBe(false);
  expect(re.test("a\nb")).toBe(false);
});

test("(?is-m:...) enables ignoreCase and dotAll, disables multiline", () => {
  const re = new RegExp("(?is-m:^A.b)", "m");
  expect(re.test("a\nb")).toBe(true);
  expect(re.test("xyz\na\nb")).toBe(false);
});

// --- Nested modifier groups ---

test("nested modifier groups override outer modifiers", () => {
  const re = new RegExp("(?i:a(?-i:b)c)");
  expect(re.test("AbC")).toBe(true);
  expect(re.test("abc")).toBe(true);
  expect(re.test("ABC")).toBe(false);
  expect(re.test("aBc")).toBe(false);
});

test("deeply nested modifier groups", () => {
  const re = new RegExp("(?i:a(?-i:b(?i:c)))");
  expect(re.test("abC")).toBe(true);
  expect(re.test("abc")).toBe(true);
  expect(re.test("AbC")).toBe(true);
  expect(re.test("aBC")).toBe(false);
  expect(re.test("aBc")).toBe(false);
});

// --- Modifier groups with captures ---

test("modifier group can contain a capturing group", () => {
  const re = new RegExp("(?i:(abc))def");
  const match = re.exec("ABCdef");
  expect(match[1]).toBe("ABC");
});

test("modifier group with named capture", () => {
  const re = new RegExp("(?i:(?<word>[a-z]+))");
  const match = re.exec("HELLO");
  expect(match.groups.word).toBe("HELLO");
});

test("modifier group does not create a capture group itself", () => {
  const re = new RegExp("(?i:abc)(def)");
  const match = re.exec("ABCdef");
  expect(match[0]).toBe("ABCdef");
  expect(match[1]).toBe("def");
});

// --- Modifier groups with alternation ---

test("modifier group with alternation", () => {
  const re = new RegExp("(?i:abc|def)");
  expect(re.test("ABC")).toBe(true);
  expect(re.test("DEF")).toBe(true);
});

// --- Edge cases: patterns that should NOT trigger modifier validation ---

test("(?:...) non-capturing group is not affected", () => {
  const re = new RegExp("(?:abc)");
  expect(re.test("abc")).toBe(true);
  expect(re.test("ABC")).toBe(false);
});

test("modifier-like syntax inside character class is not validated", () => {
  const re = new RegExp("[(?i:]");
  expect(re.test("(")).toBe(true);
  expect(re.test("i")).toBe(true);
});

test("escaped parenthesis does not trigger modifier validation", () => {
  const re = new RegExp("\\(?i:abc\\)");
  expect(re.test("(?i:abc)")).toBe(true);
});

// --- source and flags ---

test("source preserves modifier group syntax", () => {
  const re = new RegExp("(?i:abc)");
  expect(re.source).toBe("(?i:abc)");
});

test("toString includes modifier group", () => {
  const re = new RegExp("(?i:abc)", "g");
  expect(re.toString()).toBe("/(?i:abc)/g");
});

// --- Error cases: invalid modifier flags ---

test("(?g:...) throws SyntaxError", () => {
  expect(() => { new RegExp("(?g:abc)"); }).toThrow(SyntaxError);
});

test("(?u:...) throws SyntaxError", () => {
  expect(() => { new RegExp("(?u:abc)"); }).toThrow(SyntaxError);
});

test("(?v:...) throws SyntaxError", () => {
  expect(() => { new RegExp("(?v:abc)"); }).toThrow(SyntaxError);
});

test("(?y:...) throws SyntaxError", () => {
  expect(() => { new RegExp("(?y:abc)"); }).toThrow(SyntaxError);
});

test("(?d:...) throws SyntaxError", () => {
  expect(() => { new RegExp("(?d:abc)"); }).toThrow(SyntaxError);
});

// --- Error cases: bare modifier without colon ---

test("(?i) bare modifier without colon throws SyntaxError", () => {
  expect(() => { new RegExp("(?i)"); }).toThrow(SyntaxError);
});

test("(?ims) bare modifier without colon throws SyntaxError", () => {
  expect(() => { new RegExp("(?ims)"); }).toThrow(SyntaxError);
});

test("(?-i) bare disable modifier without colon throws SyntaxError", () => {
  expect(() => { new RegExp("(?-i)"); }).toThrow(SyntaxError);
});

// --- Error cases: duplicate flags ---

test("(?ii:...) duplicate enable flag throws SyntaxError", () => {
  expect(() => { new RegExp("(?ii:abc)"); }).toThrow(SyntaxError);
});

test("(?-ss:...) duplicate disable flag throws SyntaxError", () => {
  expect(() => { new RegExp("(?-ss:abc)"); }).toThrow(SyntaxError);
});

// --- Error cases: flag in both enable and disable ---

test("(?i-i:...) flag in both enable and disable throws SyntaxError", () => {
  expect(() => { new RegExp("(?i-i:abc)"); }).toThrow(SyntaxError);
});

test("(?im-ms:...) overlapping flag throws SyntaxError", () => {
  expect(() => { new RegExp("(?im-ms:abc)"); }).toThrow(SyntaxError);
});

// --- Error cases: empty modifier lists ---

test("(?-:...) empty add and remove throws SyntaxError", () => {
  expect(() => { new RegExp("(?-:abc)"); }).toThrow(SyntaxError);
});

// --- Error cases: double dash ---

test("(?i--s:...) double dash throws SyntaxError", () => {
  expect(() => { new RegExp("(?i--s:abc)"); }).toThrow(SyntaxError);
});
