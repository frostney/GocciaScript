/*---
description: Arrow functions with return type annotations still parse inside conditional branches once the conditional's ':' disambiguates them from a parenthesized consequent
features: [types-as-comments, conditional, arrow-function]
---*/

// #1533 made a `(params) : Type =>` head inside a conditional's consequent an
// arrow only when its body is followed by the conditional's ':', which is the
// rule TypeScript's parser uses (allowReturnTypeInArrowFunction). These shapes
// take the arrow reading and must keep working.

describe("return-typed arrows in conditional branches", () => {
  test("return-typed arrow consequent with an arrow alternate", () => {
    const pick = (c: number) => (c ? (x: number): number => x : (y: number) => -y);

    expect(pick(1)(4)).toBe(4);
    expect(pick(0)(4)).toBe(-4);
  });

  test("return types in both branches", () => {
    const pick = (c: number) =>
      c ? (x: number): number => x : (y: number): string => "s" + y;

    expect(pick(1)(4)).toBe(4);
    expect(pick(0)(4)).toBe("s4");
  });

  test("object return type with a parenthesized object body", () => {
    const pick = (c: number) =>
      c ? (x: number): { k: number } => ({ k: x }) : (y: number) => null;

    expect(pick(1)(2)).toEqual({ k: 2 });
    expect(pick(0)(2)).toBe(null);
  });

  test("block body with a return type", () => {
    const pick = (c: number) =>
      c ? (x: number): number => { return x; } : (y: number) => y;

    expect(pick(1)(8)).toBe(8);
  });

  test("async arrow with a return type", async () => {
    const pick = (c: number) => (c ? async (x: number): Promise<number> => x : 0);

    expect(await pick(1)(3)).toBe(3);
    expect(pick(0)).toBe(0);
  });

  test("default and destructured parameters with a return type", () => {
    const pick = (c: number) =>
      c ? (x = 1, { y }: { y: number } = { y: 2 }): number => x + y : 0;

    expect(pick(1)()).toBe(3);
  });

  test("generic arrows in both branches", () => {
    const identity = <T,>(x: T) => x;
    const pick = (c: number) => (c ? <T,>(x: T): T => x : <U,>(y: U) => y);

    expect(identity(1)).toBe(1);
    expect(pick(1)(2)).toBe(2);
    expect(pick(0)("u")).toBe("u");
  });

  test("return-typed arrow in a nested consequent", () => {
    const pick = (a: number, b: number) =>
      a ? b ? (x: number): number => x : (y: number) => 0 : (z: number) => 1;

    expect(pick(1, 1)(6)).toBe(6);
    expect(pick(1, 0)(6)).toBe(0);
    expect(pick(0, 1)(6)).toBe(1);
  });

  test("return-typed arrows nested in arguments, elements and templates", () => {
    const apply = (cb: (v: number) => number): number => cb(5);
    const c = 1;

    expect(c ? apply((x: number): number => x) : 0).toBe(5);
    expect(c ? [(y: number): number => y][0](2) : 0).toBe(2);
    expect(c ? `${((z: number): number => z)(3)}` : "").toBe("3");
  });

  test("return-typed arrow in the alternate", () => {
    const pick = (c: number) => (c ? 0 : (x: number): number => x);

    expect(pick(0)(5)).toBe(5);
  });
});
