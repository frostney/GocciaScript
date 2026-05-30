/*---
description: Labeled contextual let identifiers follow ASI in non-strict mode
features: [compat-label, automatic-semicolon-insertion, compat-non-strict-mode]
---*/

describe("labeled contextual let", () => {
  test("let followed by a newline parses as a labeled expression statement", () => {
    if (false) {
      label: let
      x = 1
    }

    expect(true).toBe(true);
  });

  test("same-line lexical let declarations remain invalid labeled declarations", () => {
    expect(() => new Function("label: let x = 1;")).toThrow(SyntaxError);
    expect(() => new Function("label: let [x] = [1];")).toThrow(SyntaxError);
  });

  test("let followed by newline and bracket remains a lexical declaration", () => {
    expect(() => new Function("if (false) { label: let\n[x] = [1]; }")).toThrow(SyntaxError);
  });

  test("strict directive rejects let identifier expressions", () => {
    expect(() => new Function('"use strict"; let + 1;')).toThrow(SyntaxError);
    expect(() => new Function('"use strict"; if (false) { label: let\nx = 1; }')).toThrow(SyntaxError);
    expect(() => new Function('(function f() { "use strict"; let + 1; });')).toThrow(SyntaxError);
    expect(() => new Function('(() => { "use strict"; let + 1; });')).toThrow(SyntaxError);
  });

  test("class element code rejects let identifier expressions", () => {
    expect(() => new Function("class C { m(){ let + 1; } }")).toThrow(SyntaxError);
    expect(() => new Function("class C { static m(){ let + 1; } }")).toThrow(SyntaxError);
    expect(() => new Function("class C { get x(){ let + 1; } }")).toThrow(SyntaxError);
    expect(() => new Function("class C { set x(v){ let + 1; } }")).toThrow(SyntaxError);
    expect(() => new Function("class C { static { let + 1; } }")).toThrow(SyntaxError);
    expect(() => new Function("class C { x = let + 1; }")).toThrow(SyntaxError);
  });

  test("object methods do not force strict mode", () => {
    new Function("({ m(){ let + 1; } });");
    expect(true).toBe(true);
  });
});
