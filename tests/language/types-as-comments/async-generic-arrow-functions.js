/*---
description: Generic async arrow functions parse and run, and `async <` without an arrow stays a comparison
features: [types-as-comments, async-await]
---*/

describe("generic async arrow functions", () => {
  test("trailing-comma type parameter list", async () => {
    const identity = async <T,>(value: T): Promise<T> => value;

    expect(await identity("hi")).toBe("hi");
  });

  test("several type parameters and a block body", async () => {
    const pair = async <A, B>(a: A, b: B) => {
      await null;
      return [a, b];
    };

    expect(await pair("x", 1)).toEqual(["x", 1]);
  });

  test("a constrained type parameter and await in the body", async () => {
    const idOf = async <T extends { id: number },>(load: () => Promise<T>) =>
      (await load()).id;

    expect(await idOf(async () => ({ id: 7 }))).toBe(7);
  });

  test("the arrow is async", () => {
    const run = async <T,>(value: T) => value;

    expect(run(1) instanceof Promise).toBe(true);
  });

  test("async compared with a value stays a comparison", () => {
    const async = 3;
    const limit = 4;

    expect(async < limit).toBe(true);
    expect(async < limit > false).toBe(true);
  });
});
