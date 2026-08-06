/*---
description: Angle-bracket type syntax in .ts sources, where '<' is never JSX
features: [types-as-comments]
---*/

describe("generic function type annotations", () => {

test("generic function type annotation on a const", () => {
  const identity: <T>(x: T) => T = (x) => x;

  expect(identity("hi")).toBe("hi");
  expect(identity(1)).toBe(1);
});

test("multi-parameter generic function type", () => {
  const first: <T, U>(a: T, b: U) => T = (a) => a;

  expect(first("a", 1)).toBe("a");
});

test("constrained generic function type", () => {
  const pluck: <T extends { id: number }>(v: T) => number = (v) => v.id;

  expect(pluck({ id: 4 })).toBe(4);
});

test("generic function type inside a union", () => {
  const maybe: (<T>(x: T) => T) | null = (x) => x;

  expect(maybe("kept")).toBe("kept");
});

test("bare type parameter on a generic arrow expression", () => {
  const wrap = <T>(v: T): T[] => [v];

  expect(wrap("x")[0]).toBe("x");
});

test("constrained type parameter on a generic arrow expression", () => {
  const idOf = <T extends { id: number }>(v: T): number => v.id;

  expect(idOf({ id: 9 })).toBe(9);
});

test("default type parameter on a generic arrow expression", () => {
  const withDefault = <T = string>(v: T): T => v;

  expect(withDefault("hi")).toBe("hi");
});

test("relational chains are unchanged", () => {
  const a = 1;
  const b = 2;
  const c = 3;

  expect(a < b > c).toBe(false);
  expect(a < b).toBe(true);
});

});
