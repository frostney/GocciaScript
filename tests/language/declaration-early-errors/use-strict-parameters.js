/*---
description: A function with non-simple parameters cannot contain its own use strict directive
features: [compat-function, compat-non-strict-mode]
---*/

const forms = [
  ["function", (params, body) => `function f(${params}) { ${body} }`],
  ["function expression", (params, body) => `const f = function(${params}) { ${body} };`],
  ["arrow", (params, body) => `const f = (${params}) => { ${body} };`],
  ["async arrow", (params, body) => `const f = async (${params}) => { ${body} };`],
  ["generator", (params, body) => `function* f(${params}) { ${body} }`],
  ["async function", (params, body) => `async function f(${params}) { ${body} }`],
  ["async generator", (params, body) => `async function* f(${params}) { ${body} }`],
  ["method", (params, body) => `const o = { m(${params}) { ${body} } };`],
  ["generator method", (params, body) => `const o = { *m(${params}) { ${body} } };`],
  ["async method", (params, body) => `const o = { async m(${params}) { ${body} } };`],
  ["async generator method", (params, body) => `const o = { async *m(${params}) { ${body} } };`],
  ["class method", (params, body) => `class C { m(${params}) { ${body} } }`],
  ["private method", (params, body) => `class C { #m(${params}) { ${body} } }`],
  ["static method", (params, body) => `class C { static m(${params}) { ${body} } }`],
  ["constructor", (params, body) => `class C { constructor(${params}) { ${body} } }`],
  ["setter", (params, body) => `const o = { set m(${params}) { ${body} } };`],
];

for (const [name, form] of forms) {
  describe(`${name} strict directives`, () => {
    for (const params of ["a = 1", "{a}", "[a]", "...a"]) {
      if (name === "setter" && params === "...a") continue;
      test(`rejects an own directive with ${params}`, () => {
        expect(() => new Function(form(params, '"use strict";'))).toThrow(SyntaxError);
      });
      test(`allows inherited strictness with ${params}`, () => {
        expect(() => new Function('"use strict"; ' + form(params, ""))).not.toThrow();
      });
      test(`accepts ${params} without an own directive`, () => {
        expect(() => new Function(form(params, ""))).not.toThrow();
      });
    }
    test("allows a directive with simple parameters", () => {
      expect(() => new Function(form("a", '"use strict";'))).not.toThrow();
    });
    for (const body of [
      '("use strict");',
      '"use\\x20strict";',
      '; "use strict";',
      '{ "use strict"; }',
      'const nested = () => { "use strict"; };',
    ]) {
      test(`does not treat ${body} as an own strict directive`, () => {
        expect(() => new Function(form("a = 1", body))).not.toThrow();
      });
    }
    test("recognizes a directive after another prologue string", () => {
      expect(() => new Function(form("a = 1", '\'other\'; \'use strict\';'))).toThrow(SyntaxError);
    });
  });
}

describe("dynamic function parameter lists", () => {
  test("rejects the Function constructor's own non-simple list and directive", () => {
    expect(() => new Function("a = 1", '"use strict";')).toThrow(SyntaxError);
  });
  test("preserves default parameter evaluation", () => {
    expect(new Function("a = 7", "return a;")()).toBe(7);
  });
});
