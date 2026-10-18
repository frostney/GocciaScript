/*---
description: Expressions nested past the supported depth are a catchable SyntaxError, not a crash
features: [ShadowRealm, Goccia]
---*/

// The parser and the compiler are recursive descent, so each level of nesting
// costs native stack. Source may nest 1,024 levels; past that the parser
// reports a SyntaxError where a deep enough program used to crash the
// process (from about 5,000 levels for arrays, objects, calls and templates,
// and 30,000 for unary and conditional operators). Each level counts once: an
// array element, a property value, a call argument, a template substitution,
// a parenthesized expression, an arrow body, a conditional branch, an
// assignment's right-hand side, a prefix operator, a `new` or an `**` operand.
// The source is evaluated in a ShadowRealm, so the error reaches the test as
// a SyntaxError it can catch.
const LIMIT_MESSAGE = "Nesting exceeds the supported limit of 1024 levels";
const DEEP = 30000;
const realm = new ShadowRealm();

const errorOf = (source) => {
  try {
    realm.evaluate(source);
  } catch (error) {
    return error;
  }
  return undefined;
};

const expectNestingError = (source) => {
  const error = errorOf(source);
  expect(error instanceof SyntaxError).toBe(true);
  expect(error.message.includes(LIMIT_MESSAGE)).toBe(true);
};

const shapes = {
  arrays: (d) => `${"[".repeat(d)}${"]".repeat(d)}.length`,
  objects: (d) => `typeof (${"{ o: ".repeat(d)}1${" }".repeat(d)})`,
  calls: (d) => `const id = (v) => v; ${"id(".repeat(d)}1${")".repeat(d)}`,
  templates: (d) => `${"`${".repeat(d)}1${"}`".repeat(d)}`,
  parentheses: (d) => `${"(".repeat(d)}1${")".repeat(d)}`,
  arrows: (d) => `typeof (${"() => ".repeat(d)}1)`,
  "unparenthesized arrows": (d) => `typeof (${"x => ".repeat(d)}1)`,
  "prefix operators": (d) => `${"!".repeat(d)}true`,
  "typeof operators": (d) => `${"typeof ".repeat(d)}1`,
  conditionals: (d) => `${"true ? ".repeat(d)}1${" : 0".repeat(d)}`,
  exponentiation: (d) => `${"1 ** ".repeat(d)}1`,
  assignments: (d) => `let a = 0; ${"a = ".repeat(d)}1`,
  "new operators": (d) => `class C { constructor() { return C; } } typeof ${"new ".repeat(d)}C`,
  "class heritage": (d) => `typeof ${"class extends ".repeat(d)}Object${" {}".repeat(d)}`,
  spreads: (d) => `[${"...[".repeat(d)}1${"]".repeat(d)}].length`,
};

describe("expression nesting limit", () => {
  for (const [name, source] of Object.entries(shapes)) {
    test(`${name} nested 500 deep evaluate`, () => {
      expect(() => realm.evaluate(source(500))).not.toThrow();
    });

    test(`${name} nested ${DEEP} deep are a SyntaxError`, () => {
      expectNestingError(source(DEEP));
    });
  }

  test("nested values evaluate to the right result below the limit", () => {
    expect(realm.evaluate(shapes.arrays(1000))).toBe(1);
    expect(realm.evaluate(shapes.calls(1000))).toBe(1);
    expect(realm.evaluate(shapes.templates(1000))).toBe("1");
    expect(realm.evaluate(shapes.parentheses(1000))).toBe(1);
    expect(realm.evaluate(shapes["prefix operators"](1000))).toBe(true);
    expect(realm.evaluate(shapes.conditionals(1000))).toBe(1);
  });

  test("1,024 levels parse and 1,025 do not", () => {
    // The expression statement and its expression are two levels, and each
    // parenthesized expression one more.
    expect(realm.evaluate(shapes.parentheses(1022))).toBe(1);
    expectNestingError(shapes.parentheses(1023));
  });

  test("the limit is the same for every kind of nesting", () => {
    const mixed = (d) => `${"[(".repeat(d)}1${")]".repeat(d)}.length`;
    expect(realm.evaluate(mixed(511))).toBe(1);
    expectNestingError(mixed(512));
  });

  test("a regular expression literal nested too deep is a SyntaxError", () => {
    // The literal's pattern compiles when it is evaluated.
    const source = `(() => { try { return String(/${"(".repeat(DEEP)}a${")".repeat(DEEP)}/.test("a")); } ` +
      `catch (error) { return error.constructor.name; } })()`;
    expect(realm.evaluate(source)).toBe("SyntaxError");
  });

  test("the engine keeps working after the error", () => {
    expectNestingError(shapes.arrays(DEEP));
    expectNestingError(shapes.arrays(DEEP));
    expect(realm.evaluate("[[1, 2], [3]].flat().length")).toBe(3);
  });
});
