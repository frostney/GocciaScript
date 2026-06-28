/*---
description: Regex literals inside ${...} template interpolation expressions
features: [template-literals, interpolation-boundaries, regexp]
---*/

describe("template interpolation regex literal scanning", () => {
  test("regex with single quote inside interpolation", () => {
    const x = `${"a'b".replace(/'/g, "-")}`;
    expect(x).toBe("a-b");
  });

  test("regex with single quote and escaped-quote replacement string", () => {
    const x = `${"a'b".replace(/'/g, "\\'")}`;
    expect(x).toBe("a\\'b");
  });

  test("regex with double quote inside interpolation", () => {
    const x = `${'a"b'.replace(/"/g, "-")}`;
    expect(x).toBe("a-b");
  });

  test("regex containing closing brace inside interpolation", () => {
    const x = `${/x}/.test("x}")}`;
    expect(x).toBe("true");
  });

  test("regex character class with quote and brace", () => {
    const x = `${/['}]/.test("'")}`;
    expect(x).toBe("true");
  });

  test("regex character class with unescaped slash", () => {
    const x = `${/[/]/.test("/")}`;
    expect(x).toBe("true");
  });

  test("regex at interpolation expression start", () => {
    const x = `${/'/.source}`;
    expect(x).toBe("'");
  });

  test("regex after ternary question mark", () => {
    const x = `${true ? /q'/.test("q'") : false}`;
    expect(x).toBe("true");
  });

  test("regex inside nested template interpolation", () => {
    const x = `${`inner ${/n'/.test("n'")}`}`;
    expect(x).toBe("inner true");
  });

  test("division after numeric literal is not a regex", () => {
    const x = `${8 / 2}`;
    expect(x).toBe("4");
  });

  test("division after parenthesized expression is not a regex", () => {
    const x = `${(8) / 2}`;
    expect(x).toBe("4");
  });

  test("division after array element access is not a regex", () => {
    const x = `${[8][0] / 2}`;
    expect(x).toBe("4");
  });

  test("division after object literal closing brace is not a regex", () => {
    const x = `${{} / 2}`;
    expect(x).toBe("NaN");
  });

  test("division after keyword property access is not a regex", () => {
    const obj = { if: 8 };
    const x = `${obj.if / 2}`;
    expect(x).toBe("4");
  });

  test("division after identifier is not a regex", () => {
    const n = 8;
    const x = `${n / 2}`;
    expect(x).toBe("4");
  });

  test("chained division stays division", () => {
    const x = `${8 / 2 / 2}`;
    expect(x).toBe("2");
  });

  test("regex with flags followed by method call", () => {
    const x = `${"AbAb".replace(/ab/gi, "-")}`;
    expect(x).toBe("--");
  });

  test("typeof keyword allows a regex operand", () => {
    const x = `${typeof /k'/}`;
    expect(x).toBe("object");
  });

  test("string then division then regex in one interpolation", () => {
    const x = `${"a'a".split(/'/).length / 2}`;
    expect(x).toBe("1");
  });

  test("division after trailing-dot numeric literal", () => {
    const x = `${1. / 2}`;
    expect(x).toBe("0.5");
  });

  test("division after trailing-dot numeric literal without spaces", () => {
    const x = `${1./2}`;
    expect(x).toBe("0.5");
  });

  test("chained division after trailing-dot numeric literal", () => {
    const x = `${1./2/2}`;
    expect(x).toBe("0.25");
  });

  test("regex after while-head close paren inside arrow body", () => {
    const s = "x}";
    const x = `${(() => { while (false) /[}]/.test(s); return 7; })()}`;
    expect(x).toBe("7");
  });

  test("division after ordinary close paren still divides", () => {
    const x = `${(() => 8)() / 2}`;
    expect(x).toBe("4");
  });

  test("slash without same-line closing slash stays division", () => {
    const total = 10;
    const x = `${total / 2}`;
    expect(x).toBe("5");
  });

  test("template interpolation inside a parenthesized expression", () => {
    const x = 7;

    expect((`v=${x + 1}`)).toBe("v=8");
    expect((`a${1 + 2}b${3 + 4}c`)).toBe("a3b7c");
  });

  test("template interpolation inside a nested parenthesized group", () => {
    const x = 7;

    expect((((`n=${x / 2}`)))).toBe("n=3.5");
  });

  test("regex and template interpolation together inside parentheses", () => {
    const x = 2;

    expect((/ab/.test("ab") ? `y=${x / 2}` : "n")).toBe("y=1");
  });
});
