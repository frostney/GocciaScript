/*---
description: Regular expression literals
features: [RegExp]
---*/

test("regex literals create RegExp objects", () => {
  const regex = /ab/gi;

  expect(regex.source).toBe("ab");
  expect(regex.flags).toBe("gi");
  expect(regex.test("zABz")).toBe(true);
});

test("regex literals create a fresh object on each evaluation", () => {
  const create = () => /ab/g;
  const first = create();
  const second = create();

  first.lastIndex = 2;

  expect(first === second).toBe(false);
  expect(second.lastIndex).toBe(0);
});

test("regex literals coexist with division expressions", () => {
  expect(/ab/.test("zabz")).toBe(true);
  expect(8 / 2).toBe(4);
});

test("regex literals are recognized as the first token inside parentheses", () => {
  expect((/ab+c/.test("abbbc"))).toBe(true);
  expect((/x/).source).toBe("x");
});

test("regex literals are recognized after a comma inside parentheses", () => {
  expect((0, /x/.source)).toBe("x");
});

test("division is recognized inside and after parenthesized groups", () => {
  const a = 10;
  const b = 2;
  const c = 5;

  expect((a / b / c)).toBe(1);
  expect((a) / b).toBe(5);
  expect(((a - 0)) / b).toBe(5);
  expect(({ v: a / b }).v).toBe(5);
});

test("regex literals work as arrow default parameter values", () => {
  const sourceOf = (re = /\d+/) => re.source;

  expect(sourceOf()).toBe("\\d+");
  expect(sourceOf(/ab/).length).toBe(2);
});

test("arrow bodies can return division expressions", () => {
  const half = (x) => x / 2;
  const ratio = (x, y) => x / y;

  expect(half(10)).toBe(5);
  expect(ratio(20, 4)).toBe(5);
});

test("deeply nested parenthesized arrows and groups disambiguate", () => {
  const inc = ((((q) => q + 1)));

  expect(inc(41)).toBe(42);
  expect(((((7))))).toBe(7);
});

test("callback chains with division bodies parse and run", () => {
  const out = [1, 2, 3, 4].map((n) => n / 2).filter((n) => n > 1);

  expect(out.join(",")).toBe("1.5,2");
});

test("regex literals throw SyntaxError for duplicate flags", () => {
  expect(() => {
    new RegExp("a", "gg");
  }).toThrow(SyntaxError);
});

test("regex literals are allowed after condition parentheses", () => {
  let matched = false;

  if (true) matched = /ab/.test("zabz");

  expect(matched).toBe(true);
});

test("unicode regex literals treat astral symbols as code points", () => {
  expect(/𝌆{2}/u.test("𝌆𝌆")).toBe(true);
  expect(/^.$/u.test("𝌆")).toBe(true);
  expect(/^[𝌆]$/u.test("𝌆")).toBe(true);
  expect(/[💩-💫]/u.test("💩")).toBe(true);
  expect(/[💩-💫]/u.test("💨")).toBe(false);
});

test("unicode regex literals handle escapes and forward named references", () => {
  expect(/\u{3f}/u.test("?")).toBe(true);
  expect(/\0/u.test(String.fromCharCode(0))).toBe(true);
  expect(/\u212a/iu.test("k")).toBe(true);
  expect(/\u212a/u.test("k")).toBe(false);
  expect(/\k<a>(?<a>x)/.test("x")).toBe(true);
});
