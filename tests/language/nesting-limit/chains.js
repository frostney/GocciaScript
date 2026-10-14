/*---
description: Operator, member and call chains longer than the supported depth are a catchable SyntaxError
features: [ShadowRealm, Goccia]
---*/

// The parser reads `a + b + c` and `a.b().c` in a loop, but the tree it
// builds is as deep as the chain is long, and the compiler walks that tree
// recursively. Each link of such a chain counts as a level of nesting, so a
// chain may have about 1,024 links. A 30,000-term chain used to crash the
// development build, and the interpreter in the production build.
const LIMIT_MESSAGE = "Nesting exceeds the supported limit of 1024 levels";
const DEEP = 30000;
const realm = new ShadowRealm();

const expectNestingError = (source) => {
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
};

describe("chain length limit", () => {
  for (const [name, [source, expected]] of Object.entries(chains)) {
    test(`a ${name} chain of 1,000 links evaluates`, () => {
      expect(realm.evaluate(source(1000))).toBe(expected(1000));
    });

    test(`a ${name} chain of ${DEEP} links is a SyntaxError`, () => {
      expectNestingError(source(DEEP));
    });
  }

  test("an argument or computed key does not nest inside its chain", () => {
    // Nested calls cost one level each, not two.
    const calls = `const id = (v) => v; ${"id(".repeat(1000)}1${")".repeat(1000)}`;
    expect(realm.evaluate(calls)).toBe(1);
    const keys = `const a = [0]; ${"a[".repeat(1000)}0${"]".repeat(1000)}`;
    expect(realm.evaluate(keys)).toBe(0);
  });

  test("nesting inside a chain counts the links before it", () => {
    const nestedAfter = (links, nesting) =>
      `1${" + 1".repeat(links)} + ${"[".repeat(nesting)}${"]".repeat(nesting)}.length`;
    expect(realm.evaluate(nestedAfter(500, 500))).toBe(502);
    expectNestingError(nestedAfter(600, 600));
  });

  test("lists do not count: sequences, arguments, elements and statements", () => {
    expect(realm.evaluate(`let c = 0; (c${", c".repeat(DEEP)})`)).toBe(0);
    expect(realm.evaluate(`[${"1, ".repeat(DEEP)}1].length`)).toBe(DEEP + 1);
    expect(realm.evaluate(`const f = (...a) => a.length; f(${"1, ".repeat(DEEP)}1)`)).toBe(DEEP + 1);
    expect(realm.evaluate(`let n = 0;${" n += 1;".repeat(DEEP)} n`)).toBe(DEEP);
  });
});
