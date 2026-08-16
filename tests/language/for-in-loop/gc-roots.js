/*---
description: for-in iteration heads stay reachable during explicit GC
features: [compat-for-in-loop, Goccia.gc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
    total += scratch.a + scratch.b[0];
  }
  return total;
};

describe.runIf(hasGoccia)("for-in iteration head GC roots", () => {
  test("a key survives a collecting assignment target", () => {
    // The key string is constructed for this iteration and held only by the
    // evaluator's locals until the assignment target lands it somewhere.
    const source = { alpha: 1, beta: 2 };
    const sink = {};
    const seen = [];

    for (sink[(churn(), "slot")] in source) seen.push(sink.slot);

    expect(seen).toEqual(["alpha", "beta"]);
  });
});
