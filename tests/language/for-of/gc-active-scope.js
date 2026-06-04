/*---
description: for-of loop bindings survive explicit Goccia.gc calls in the body
features: [for-of, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("for-of explicit GC", () => {
  test("preserves the current iteration binding across Goccia.gc", () => {
    const values = [];

    for (let value of ["a", "b", "c"]) {
      Goccia.gc();
      values.push(value);
    }

    expect(values).toEqual(["a", "b", "c"]);
  });
});
