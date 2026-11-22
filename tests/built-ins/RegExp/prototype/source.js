/*---
description: RegExp.prototype.source escapes a pattern so it reads back as a literal
features: [RegExp, UnicodeSets]
---*/

// Each pattern, its expected source, and strings the pattern must match or
// reject; the expected sources match V8 and JavaScriptCore.
const cases = [
  { re: /<\/a>/y, source: "<\\/a>", matches: ["</a>"], rejects: ["<\\/a>"] },
  { re: /a\/b/, source: "a\\/b", matches: ["a/b"], rejects: ["a\\/b"] },
  { re: /[/]/, source: "[/]", matches: ["/"], rejects: ["\\"] },
  { re: /\\\//, source: "\\\\\\/", matches: ["\\/"], rejects: ["/", "\\"] },
  { re: /\\/, source: "\\\\", matches: ["\\"], rejects: ["/"] },
  { re: new RegExp("/"), source: "\\/", matches: ["/"], rejects: ["\\"] },
  { re: new RegExp("[/]"), source: "[/]", matches: ["/"], rejects: ["\\"] },
  { re: new RegExp("\\\\/"), source: "\\\\\\/", matches: ["\\/"], rejects: ["/"] },
  { re: new RegExp("\\[/]"), source: "\\[\\/]", matches: ["[/]"], rejects: ["/"] },
  { re: new RegExp("[\\]/]"), source: "[\\]/]", matches: ["]", "/"], rejects: ["\\"] },
  { re: new RegExp("[a/]", "u"), source: "[a/]", matches: ["a", "/"], rejects: ["b"] },
  { re: new RegExp("[[a]\\/]", "v"), source: "[[a]\\/]", matches: ["a", "/"], rejects: ["\\"] },
  { re: new RegExp("\n"), source: "\\n", matches: ["\n"], rejects: ["n"] },
  { re: new RegExp("\\\n"), source: "\\n", matches: ["\n"], rejects: ["n"] },
  { re: new RegExp("[\n]"), source: "[\\n]", matches: ["\n"], rejects: ["n"] },
  { re: new RegExp("\r"), source: "\\r", matches: ["\r"], rejects: ["r"] },
  { re: new RegExp("\u2028"), source: "\\u2028", matches: ["\u2028"], rejects: ["u2028"] },
  { re: new RegExp("\\\u2029"), source: "\\u2029", matches: ["\u2029"], rejects: ["u2029"] },
];

describe("RegExp.prototype.source", () => {
  cases.forEach(({ re, source, matches, rejects }) => {
    test(`source is ${JSON.stringify(source)}`, () => {
      expect(re.source).toBe(source);
    });

    test(`new RegExp(source, flags) for ${JSON.stringify(source)} matches the same strings`, () => {
      const copy = new RegExp(re.source, re.flags);
      expect(copy.source).toBe(re.source);
      matches.forEach((input) => {
        re.lastIndex = 0;
        copy.lastIndex = 0;
        expect(re.test(input)).toBe(true);
        expect(copy.test(input)).toBe(true);
      });
      rejects.forEach((input) => {
        re.lastIndex = 0;
        copy.lastIndex = 0;
        expect(re.test(input)).toBe(false);
        expect(copy.test(input)).toBe(false);
      });
    });
  });
});
