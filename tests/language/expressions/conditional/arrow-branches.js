/*---
description: Arrow functions in both branches of a conditional parse, including a consequent arrow whose body is a parenthesized group followed by the conditional's ':' and an alternate arrow
features: [conditional, arrow-function]
---*/

// Regression for #1533. Inside a conditional's consequent, `(x) : y => z` can
// read as an arrow with return type `y` or as the parenthesized expression
// `(x)`, the conditional's ':' and an alternate arrow `y => z`. The arrow probe
// used to take the first reading, so `u ? (l) => ({ a: 1 }) : (l) => 2` failed
// with "Expected destructuring pattern" and minified bundles such as prettier
// 3.9 did not parse. The ':' belongs to the conditional (ECMA-262 §13.14), so
// a return-typed arrow is only accepted there when its body is followed by the
// ':' that closes the consequent.

describe("arrow functions in conditional branches", () => {
  test("object-literal body in the consequent, arrow alternate", () => {
    const pick = (u) => (u ? (l) => ({ a: 1 }) : (l) => 2);

    expect(pick(1)(0)).toEqual({ a: 1 });
    expect(pick(0)(0)).toBe(2);
  });

  test("computed-key object body whose literal is also a valid pattern", () => {
    const pick = (u) => (u ? (l, m) => ({ [l]: m }) : (l, m, f) => 0);

    expect(pick(1)("k", 2)).toEqual({ k: 2 });
    expect(pick(0)("k", 2, 3)).toBe(0);
  });

  test("parenthesized consequent with an arrow alternate", () => {
    const b = 7;
    const pickParen = (a) => (a ? (b) : (c) => c + 1);
    const pickBare = (a) => (a ? (b) : c => c + 2);

    expect(pickParen(1)).toBe(7);
    expect(pickParen(0)(1)).toBe(2);
    expect(pickBare(1)).toBe(7);
    expect(pickBare(0)(1)).toBe(3);
  });

  test("parenthesized expression bodies in both branches", () => {
    const pick = (a) => (a ? (x) => (x + 1) : (y) => (y - 1));

    expect(pick(1)(1)).toBe(2);
    expect(pick(0)(1)).toBe(0);
  });

  test("block bodies returning objects", () => {
    const pick = (a) => (a ? (l) => { return { a: 1 }; } : (l) => 2);

    expect(pick(1)()).toEqual({ a: 1 });
    expect(pick(0)()).toBe(2);
  });

  test("multi-line formatting with the ':' on its own line", () => {
    const pick = (a) =>
      a
        ? (l) => ({
            a: 1,
          })
        : (l) => 2;

    expect(pick(1)()).toEqual({ a: 1 });
    expect(pick(0)()).toBe(2);
  });

  test("async arrows in both branches", async () => {
    const pick = (a) => (a ? async (x) => ({ v: x }) : async (y) => 0);

    expect(await pick(1)(5)).toEqual({ v: 5 });
    expect(await pick(0)(5)).toBe(0);
  });

  test("default parameters and destructuring", () => {
    const pick = (a) =>
      a ? ({ x = 1, y: [z] }, w = 2) => ({ s: x + z + w }) : ([p]) => p;

    expect(pick(1)({ y: [3] })).toEqual({ s: 6 });
    expect(pick(0)([9])).toBe(9);
  });

  test("curried arrows in the consequent", () => {
    const pick = (a) => (a ? (x) => (y) => ({ n: x + y }) : (z) => 0);

    expect(pick(1)(1)(2)).toEqual({ n: 3 });
  });

  test("a conditional inside a consequent arrow's body", () => {
    const pick = (a, b) => (a ? (x) => b ? ({ k: 1 }) : 2 : (y) => 3);

    expect(pick(1, 1)()).toEqual({ k: 1 });
    expect(pick(1, 0)()).toBe(2);
    expect(pick(0, 1)()).toBe(3);
  });

  test("nested conditionals with arrows in every branch", () => {
    const inner = (a, b) =>
      a ? (b ? (x) => ({ q: 1 }) : (y) => ({ q: 2 })) : (z) => ({ q: 3 });
    const chained = (a, b) =>
      a ? (x) => ({ q: 1 }) : b ? (y) => ({ q: 2 }) : (z) => ({ q: 3 });

    expect(inner(1, 1)()).toEqual({ q: 1 });
    expect(inner(1, 0)()).toEqual({ q: 2 });
    expect(inner(0, 0)()).toEqual({ q: 3 });
    expect(chained(1, 0)()).toEqual({ q: 1 });
    expect(chained(0, 1)()).toEqual({ q: 2 });
    expect(chained(0, 0)()).toEqual({ q: 3 });
  });

  test("nested consequent whose parenthesized group meets two ':'", () => {
    const x = 7;
    const pick = (a, b) => (a ? b ? (x) : (c) => 8 : 9);

    expect(pick(1, 1)).toBe(7);
    expect(pick(1, 0)()).toBe(8);
    expect(pick(0, 1)).toBe(9);
  });

  test("minified shapes", () => {
    const e = 5;
    const f = 6;
    const bare = (a) => (a?b=>3:d=>4);
    const parens = (a) => (a?(b)=>3:(d)=>4);
    const nested = (a, b) => (a?b?c=>({a:1}):d=>e:f);

    expect(bare(1)()).toBe(3);
    expect(bare(0)()).toBe(4);
    expect(parens(1)()).toBe(3);
    expect(parens(0)()).toBe(4);
    expect(nested(1, 1)()).toEqual({ a: 1 });
    expect(nested(1, 0)()).toBe(5);
    expect(nested(0, 0)).toBe(6);
  });

  test("nested consequent from the prettier 3.9 standalone bundle", () => {
    const make = (u) =>
      u?Array.isArray(u)?(l,m)=>u.includes(l)?{[l]:m}:void 0:(l,m)=>({[l]:m}):(l,m,f)=>f;

    expect(make(["k"])("k", 1)).toEqual({ k: 1 });
    expect(make(["k"])("j", 1)).toBe(undefined);
    expect(make(true)("k", 2)).toEqual({ k: 2 });
    expect(make(false)("k", 2, 3)).toBe(3);
  });

  test("assignment and array element contexts", () => {
    let assigned;
    const a = 1;
    a ? assigned = (l) => ({ a: 1 }) : (l) => 2;
    const first = [a ? (l) => ({ b: 1 }) : (l) => 2][0];

    expect(assigned()).toEqual({ a: 1 });
    expect(first()).toEqual({ b: 1 });
  });

  test("regular expression and template bodies", () => {
    const pick = (a) => (a ? (l) => (/a:b/).test("a:b") : (l) => 2);
    const tpl = (a) => (a ? (l) => `${({ a: 1 }).a}` : (l) => 2);

    expect(pick(1)()).toBe(true);
    expect(tpl(1)()).toBe("1");
    expect(tpl(0)()).toBe(2);
  });
});
