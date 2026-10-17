/*---
description: Operator, member and call chains taller than the supported tree height are a catchable SyntaxError
features: [ShadowRealm, Goccia]
---*/

// The parser reads `a + b + c` and `a.b().c` in a loop, but the tree it
// builds is as tall as the chain is long, and the compiler and the code that
// frees the tree walk it recursively. The syntax tree of a
// statement may be 4,096 levels tall, counting each level of nesting and each
// link of a chain. A 30,000-term chain used to crash the development build.
const LIMIT_MESSAGE = "exceeds the supported limit of 4096 levels";
const DEEP = 30000;
const realm = new ShadowRealm();

const expectHeightError = (source) => {
  let error;
  try {
    realm.evaluate(source);
  } catch (caught) {
    error = caught;
  }
  expect(error instanceof SyntaxError).toBe(true);
  expect(error.message.includes(LIMIT_MESSAGE)).toBe(true);
};

const chains = {
  addition: [(d) => `1${" + 1".repeat(d)}`, (d) => d + 1],
  "string concatenation": [(d) => `const s = "x"; (s${" + s".repeat(d)}).length`, (d) => d + 1],
  "logical and": [(d) => `true${" && true".repeat(d)}`, () => true],
  "nullish coalescing": [(d) => `const n = 1; n${" ?? n".repeat(d)}`, () => 1],
  comparison: [(d) => `1${" < 2".repeat(d)}`, () => true],
  "member access": [(d) => `const o = {}; o.o = o; typeof o${".o".repeat(d)}`, () => "object"],
  calls: [(d) => `const f = () => f; typeof f${"()".repeat(d)}`, () => "function"],
  "optional calls": [(d) => `const f = () => f; typeof f${"?.()".repeat(d)}`, () => "function"],
  "tagged templates": [(d) => `const tag = () => tag; typeof tag${"`x`".repeat(d)}`, () => "function"],
  "tagged member access": [(d) => `const o = () => o; o.o = o; typeof o${".o".repeat(d)}\`x\``, () => "function"],
};

describe("chain length limit", () => {
  for (const [name, [source, expected]] of Object.entries(chains)) {
    test(`a ${name} chain of 4,000 links evaluates`, () => {
      expect(realm.evaluate(source(4000))).toBe(expected(4000));
    });

    test(`a ${name} chain of ${DEEP} links is a SyntaxError`, () => {
      expectHeightError(source(DEEP));
    });
  }

  test("a member chain of 300,000 links used as a template tag is a SyntaxError", () => {
    // The parser checks a template tag for an optional chain by walking the
    // chain recursively; a chain this long used to exhaust the native stack
    // there, before the height was checked.
    expectHeightError(`const o = () => o; o.o = o; typeof o${".o".repeat(300000)}\`x\``);
  });

  test("a chain of 2,000 method calls evaluates", () => {
    // The shape of regenerate's Unicode tables, which Babel bundles: a call
    // is two links, the member access and the call.
    const ranges = `const set = { n: 0, addRange(a, b) { this.n += b - a; return this; } }; ` +
      `set${".addRange(1, 2)".repeat(2000)}.n`;
    expect(realm.evaluate(ranges)).toBe(2000);
  });

  test("an argument or computed key does not nest inside its chain", () => {
    const calls = `const id = (v) => v; ${"id(".repeat(1000)}1${")".repeat(1000)}`;
    expect(realm.evaluate(calls)).toBe(1);
    const keys = `const a = [0]; ${"a[".repeat(1000)}0${"]".repeat(1000)}`;
    expect(realm.evaluate(keys)).toBe(0);
  });

  test("a chain and the nesting inside it add up", () => {
    const nestedAfter = (links, nesting) =>
      `1${" + 1".repeat(links)} + ${"[".repeat(nesting)}${"]".repeat(nesting)}.length`;
    expect(realm.evaluate(nestedAfter(2500, 1000))).toBe(2502);
    expectHeightError(nestedAfter(3500, 1000));
  });

  test("links that follow a nested chain count toward its height", () => {
    // Each level is a call whose argument is the level below, followed by
    // links: the tree is as tall as all the levels' links together, though
    // no single chain is long.
    const levels = (count, links) =>
      `const o = {}; o.p = o; const g = (v) => v; ${"g(".repeat(count)}o${`)${".p".repeat(links)}`.repeat(count)} === o`;
    expect(realm.evaluate(levels(60, 60))).toBe(true);
    expectHeightError(levels(100, 100));
  });

  test("lists do not count: sequences, arguments, elements and statements", () => {
    expect(realm.evaluate(`let c = 0; (c${", c".repeat(DEEP)})`)).toBe(0);
    expect(realm.evaluate(`[${"1, ".repeat(DEEP)}1].length`)).toBe(DEEP + 1);
    expect(realm.evaluate(`const f = (...a) => a.length; f(${"1, ".repeat(DEEP)}1)`)).toBe(DEEP + 1);
    expect(realm.evaluate(`let n = 0;${" n += 1;".repeat(DEEP)} n`)).toBe(DEEP);
  });

  test("the engine keeps working after the error", () => {
    expectHeightError(chains.addition[0](DEEP));
    expect(realm.evaluate(chains.addition[0](10))).toBe(11);
  });
});
