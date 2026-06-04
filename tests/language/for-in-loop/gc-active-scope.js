/*---
description: for-in loop bindings survive explicit Goccia.gc calls in the body
features: [compat-for-in-loop, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("for-in explicit GC", () => {
  test("preserves the current iteration binding across Goccia.gc", () => {
    const obj = { a: 1, b: 2, c: 3 };
    const keys = [];

    for (let key in obj) {
      Goccia.gc();
      keys.push(key);
    }

    expect(keys).toEqual(["a", "b", "c"]);
  });

  test("handles deletion of unvisited properties after Goccia.gc", () => {
    const obj = { p: 1, r: 3, s: 4, t: 5 };
    const keys = [];

    for (let key in obj) {
      Goccia.gc();
      keys.push(key);
      delete obj.t;
      delete obj.s;
    }

    expect(keys).toEqual(["p", "r"]);
  });
});
