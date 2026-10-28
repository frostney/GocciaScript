/*---
description: Lexical names are unique and do not overlap var or parameter names in the same declaration scope
features: [compat-function, compat-var, compat-non-strict-mode]
---*/

const bodies = [
  ["function", (body) => `function f() { ${body} }`],
  ["arrow", (body) => `const f = () => { ${body} };`],
  ["async arrow", (body) => `const f = async () => { ${body} };`],
  ["generator", (body) => `function* f() { ${body} }`],
  ["async function", (body) => `async function f() { ${body} }`],
  ["async generator", (body) => `async function* f() { ${body} }`],
  ["object method", (body) => `const o = { m() { ${body} } };`],
  ["getter", (body) => `const o = { get m() { ${body} } };`],
  ["setter", (body) => `const o = { set m(value) { ${body} } };`],
  ["class method", (body) => `class C { m() { ${body} } }`],
  ["constructor", (body) => `class C { constructor() { ${body} } }`],
  ["static block", (body) => `class C { static { ${body} } }`],
  ["block", (body) => `{ ${body} }`],
  ["switch", (body) => `switch (0) { case 0: ${body} }`],
];
const conflicts = [
  "let a; let a;",
  "let a; const a = 1;",
  "const a = 1; let a;",
  "let a; class a {}",
  "class a {} let a;",
  "let a; function a() {}",
  "function a() {} let a;",
  "let a; async function a() {}",
  "let a; function* a() {}",
  "let a; var a;",
  "var a; let a;",
  "let a; if (false) { var a; }",
  "if (false) { var a; } let a;",
  "let {a, b: a} = {};",
  "const [a, a] = [];",
  "let a, a;",
];

for (const [name, wrap] of bodies) {
  describe(`${name} lexical declarations`, () => {
    for (const source of conflicts) {
      test(`rejects ${source}`, () => {
        expect(() => new Function(wrap(source))).toThrow(SyntaxError);
      });
    }
    test("allows lexical shadowing in separate blocks and nested functions", () => {
      const source = "let a; { let a; } { const a = 1; } const f = () => { var a; };";
      expect(() => new Function(wrap(source))).not.toThrow();
    });
    test("allows a var outside a nested lexical scope", () => {
      expect(() => new Function(wrap("var a; { let a; }"))).not.toThrow();
    });
  });
}

describe("parameter and body lexical names", () => {
  for (const source of [
    "function f(a) { let a; }",
    "function f({a}) { const a = 1; }",
    "function f([a]) { class a {} }",
    "const f = a => { let a; };",
    "const f = async a => { let a; };",
    "const f = (a) => { let a; };",
    "const f = async (a) => { let a; };",
    "function* f(a) { let a; }",
    "async function f(a) { let a; }",
    "async function* f(a) { let a; }",
    "const o = { m(a) { let a; } };",
    "const o = { set x(a) { let a; } };",
    "class C { m(a) { let a; } }",
    "class C { constructor(a) { let a; } }",
  ]) {
    test(`rejects ${source}`, () => {
      expect(() => new Function(source)).toThrow(SyntaxError);
    });
  }
  test("allows parameter/var and parameter/function overlap and nested lexical shadowing", () => {
    const f = new Function("a", "var a; { let a; } function a() { return 7; } return a();");
    expect(f(3)).toBe(7);
  });
});

describe("scope boundaries", () => {
  for (const source of [
    "switch (0) { case 0: let a; break; default: let a; }",
    "try {} catch (a) { let a; }",
    "try {} catch ({a}) { var a; }",
    "try {} catch ({a, b: a}) {}",
    "for (let [a, a] of []) {}",
    "for (const {a, b: a} in {}) {}",
    "for (let a of []) { var a; }",
    "for (let a in {}) { var a; }",
    "for (let a = 0; false;) { var a; }",
    '"use strict"; { function a() {} function a() {} }',
    "{ async function a() {} async function a() {} }",
    "{ function* a() {} function* a() {} }",
  ]) {
    test(`rejects ${source}`, () => {
      expect(() => new Function(source)).toThrow(SyntaxError);
    });
  }
  for (const source of [
    "var a; var a;",
    "var [a, a] = [1, 2];",
    "function a() {} function a() {}",
    "{ function a() {} function a() {} }",
    "let a; { function a() {} }",
    "class C { static { function a() {} function a() {} } }",
    "try {} catch (a) { var a; }",
    "try {} catch ({a}) { { let a; } }",
    "for (let a of []) { let a; }",
    "for (let a = 0; false;) { let a; }",
    "for (var [a, a] of []) {}",
  ]) {
    test(`accepts ${source}`, () => {
      expect(() => new Function(source)).not.toThrow();
    });
  }
});


describe("labelled function declaration scope", () => {
  for (const wrap of [
    (body) => `{ ${body} }`,
    (body) => `switch (0) { case 0: ${body} }`,
  ]) {
    for (const source of [
      "var a; label: function a() {}",
      "label: function a() {} var a;",
      "let a; label: function a() {}",
      "label: function a() {} let a;",
    ]) {
      test(`rejects ${wrap(source)}`, () => {
        expect(() => new Function(wrap(source))).toThrow(SyntaxError);
      });
    }
    for (const source of [
      "function a() {} label: function a() {}",
      "label: function a() {} function a() {}",
      "first: function a() {} second: function a() {}",
    ]) {
      test(`accepts ${wrap(source)}`, () => {
        expect(() => new Function(wrap(source))).not.toThrow();
      });
    }
  }
  for (const body of [
    "label: function a() {}",
    "var a; label: function a() {}",
    "label: function a() {} var a;",
  ]) {
    test(`rejects catch parameter overlap with ${body}`, () => {
      expect(() => new Function(`try {} catch (a) { ${body} }`)).toThrow(SyntaxError);
    });
  }
});
