/*---
description: Destructuring sources, computed keys and rest targets stay reachable during explicit GC
features: [Goccia.gc, Symbol.iterator]
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

describe.runIf(hasGoccia)("destructuring GC roots", () => {
  test("a source produced by a call survives a collecting default", () => {
    // The source is read once per pattern property. Nothing refers to it after
    // the call returns, so a collection inside the first default frees it
    // before the second property is read.
    const makeSource = () => ({ missing: undefined, kept: "second" });

    const { missing = churn(), kept } = makeSource();

    expect(typeof missing).toBe("number");
    expect(kept).toBe("second");
  });

  test("a source survives a getter that collects", () => {
    const makeSource = () => ({
      get first() {
        churn();
        return 1;
      },
      second: 2,
    });

    const { first, second } = makeSource();

    expect(first).toBe(1);
    expect(second).toBe(2);
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

    const { taken, ...rest } = makeSource();

    expect(taken).toBe(0);
    expect(rest.spilled).toBe("s");
    expect(rest.other).toBe("o");
  });

  test("a computed key survives the property read behind it", () => {
    const makeSource = () => ({
      get computed() {
        churn();
        return "found";
      },
      kept: "still-here",
    });

    const {
      [(churn(), "comp" + "uted")]: picked,
      kept,
    } = makeSource();

    expect(picked).toBe("found");
    expect(kept).toBe("still-here");
  });

  test("an array pattern source survives a collecting default", () => {
    const makeSource = () => [undefined, "tail"];

    const [head = churn(), tail] = makeSource();

    expect(typeof head).toBe("number");
    expect(tail).toBe("tail");
  });
});
