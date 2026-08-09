/*---
description: A ternary whose consequent is parenthesized is not mistaken for an arrow function parameter list, even when an arrow appears later in the file
features: [conditional, ternary]
---*/

// Regression: `cond ? (x) : y` reaches the speculative arrow-function probe as
// "parenthesized group followed by ':'", the same shape as an arrow with a
// return type. The probe used to scan for a '=>' with no bound, find an
// unrelated arrow later in the file, and parse the ternary as an arrow — the
// conditional then failed with "Expected ':' in conditional expression". Every
// case here therefore needs at least one arrow somewhere after it, which the
// surrounding test callbacks already provide.

describe("parenthesized ternary consequent", () => {
  test("parenthesized consequent with an identifier alternate", () => {
    const c = true;
    const a = 2;
    const b = 3;

    expect(c ? (a) : b).toBe(2);
    expect(!c ? (a) : b).toBe(3);
  });

  test("parenthesized consequent inside a declaration", () => {
    const flag = false;
    const x = flag ? (1) : 2;

    expect(x).toBe(2);
  });

  test("parenthesized consequent followed by a statement boundary", () => {
    const flag = true;
    const values = [];
    values.push(flag ? (10) : 20);

    expect(values[0]).toBe(10);
  });

  test("both branches parenthesized", () => {
    const flag = false;

    expect(flag ? (1) : (2)).toBe(2);
  });

  test("parenthesized consequent with a call alternate", () => {
    const flag = false;
    const fallback = () => 42;

    expect(flag ? (1) : fallback()).toBe(42);
  });

  test("empty-paren-like shapes still parse", () => {
    const flag = true;
    const obj = { v: 5 };

    expect(flag ? (obj).v : 0).toBe(5);
    expect(flag ? (obj.v) : 0).toBe(5);
  });

  test("nested parenthesized consequents", () => {
    const outer = true;
    const inner = false;

    expect(outer ? (inner ? (1) : 2) : 3).toBe(2);
  });

  test("arrow functions with return type annotations still parse", () => {
    const typed = (v: number): number => v * 2;
    const noParams = (): string => "ok";

    expect(typed(4)).toBe(8);
    expect(noParams()).toBe("ok");
  });

  test("arrow with a structured return type still parses", () => {
    const make = (): { a: number } => ({ a: 1 });
    const union = (flag: boolean): string | null => (flag ? "y" : null);

    expect(make().a).toBe(1);
    expect(union(true)).toBe("y");
  });
});
