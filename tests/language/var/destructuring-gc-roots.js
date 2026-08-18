/*---
description: var destructuring sources stay reachable during explicit GC
features: [compat-var, Goccia.gc]
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

describe.runIf(hasGoccia)("var destructuring GC roots", () => {
  test("a source produced by a call survives a collecting default", () => {
    const makeSource = () => ({ missing: undefined, kept: "second" });

    var { missing: fallback = churn(), kept: keptVar } = makeSource();

    expect(typeof fallback).toBe("number");
    expect(keptVar).toBe("second");
  });

  test("a rest target survives a getter that collects", () => {
    const makeSource = () => ({
      taken: 0,
      get spilled() {
        churn();
        return "s";
      },
      other: "o",
    });

    var { taken, ...rest } = makeSource();

    expect(taken).toBe(0);
    expect(rest.spilled).toBe("s");
    expect(rest.other).toBe("o");
  });
});
