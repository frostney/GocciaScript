/*---
description: Template literals with substitutions in arrow parameter default values
features: [arrow-function, template-literals, template-interpolation, default-parameters]
---*/

describe("template literal defaults in arrow parameters", () => {
  test("arrow default with arithmetic substitution", () => {
    const h = (t = `a${1 + 2}b`) => t;
    expect(h()).toBe("a3b");
    expect(h("x")).toBe("x");
  });

  test("arrow default with division inside substitution", () => {
    const f = (label = `d=${6 / 2}`) => label;
    expect(f()).toBe("d=3");
  });

  test("arrow default with regex literal inside substitution", () => {
    const g = (x = `m=${/a.c/.test("abc")}`) => x;
    expect(g()).toBe("m=true");
  });

  test("arrow default with division after a parenthesized operand", () => {
    const u = (v = `q=${(10) / 2}`) => v;
    expect(u()).toBe("q=5");
  });

  test("nested template inside an arrow default", () => {
    const a = (x = `a${`b${1 + 2}c`}d`) => x;
    expect(a()).toBe("ab3cd");
  });

  test("multiple substitutions then a following parameter", () => {
    const b = (p = `${1}-${2}`, q = 9) => p + ":" + q;
    expect(b()).toBe("1-2:9");
    expect(b("z", 3)).toBe("z:3");
  });

  test("balanced braces inside the substitution", () => {
    const c = (y = `len=${[1, 2, 3].length}`) => y;
    expect(c()).toBe("len=3");
  });

  test("tagged template as a default value", () => {
    const tag = (strings, ...vals) => strings.join("|") + "/" + vals.join(",");
    const f = (w = tag`a${1 + 1}b`) => w;
    expect(f()).toBe("a|b/2");
  });

  test("plain division and regex defaults still parse without templates", () => {
    const d = (m = 4 / 2) => m;
    const e = (n = /xy/.source) => n;
    expect(d()).toBe(2);
    expect(e()).toBe("xy");
  });
});

describe("template literal defaults in destructured arrow parameters", () => {
  test("object destructuring default with substitution", () => {
    const g = ({ a = `x${1}y` }) => a;
    expect(g({})).toBe("x1y");
    expect(g({ a: "z" })).toBe("z");
  });

  test("object destructuring default with division inside substitution", () => {
    const e = ({ z = `r=${8 / 4}` }) => z;
    expect(e({})).toBe("r=2");
  });

  test("array destructuring default with substitution", () => {
    const h = ([first = `n${2 * 3}`] = []) => first;
    expect(h()).toBe("n6");
    expect(h(["set"])).toBe("set");
  });
});
