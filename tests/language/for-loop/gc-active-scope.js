/*---
description: traditional for-loop lexical bindings survive explicit Goccia.gc calls in the body
features: [compat-traditional-for-loop, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("traditional for explicit GC", () => {
  test("preserves the current lexical iteration binding across Goccia.gc", () => {
    const values = [];

    for (let i = 0; i < 3; i++) {
      Goccia.gc();
      values.push(i);
    }

    expect(values).toEqual([0, 1, 2]);
  });
});
