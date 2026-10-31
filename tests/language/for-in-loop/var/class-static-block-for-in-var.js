/*---
description: A for-in var head in a class static block is bound for the whole block (#1329)
features: [class-static-block, compat-for-in-loop, compat-var]
---*/

describe("for-in var head in a class static block", () => {
  test("stays bound after the loop", () => {
    let seen;
    class C {
      static {
        for (var key in { first: 1, last: 2 }) {}
        const laterName = 3;
        seen = [key, typeof laterName];
      }
    }
    expect(seen).toEqual(["last", "number"]);
  });

  test("is undefined before the loop", () => {
    let before;
    class C {
      static {
        before = key;
        for (var key in { only: 1 }) {}
      }
    }
    expect(before).toBeUndefined();
  });
});
