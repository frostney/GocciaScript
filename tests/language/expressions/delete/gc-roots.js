/*---
description: A delete target's base stays reachable across the key expression and its conversion
features: [Goccia.gc, Symbol.toPrimitive]
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

describe.runIf(hasGoccia)("delete GC roots", () => {
  test("a base produced by a call survives a collecting key expression", () => {
    // §13.5.1.2 resolves the base before the key is converted, so the base sits
    // in a native local across the key expression. A base that came from a call
    // is referenced from nowhere else.
    const make = () => ({ slot: 1, other: 2 });

    expect(delete make()[(churn(), "slot")]).toBe(true);
  });

  test("a base survives a collecting Symbol.toPrimitive key", () => {
    const make = () => ({ slot: 1 });
    const key = {
      [Symbol.toPrimitive]() {
        churn();
        return "slot";
      },
    };

    expect(delete make()[key]).toBe(true);
  });

  test("a base survives a collecting toString key", () => {
    const make = () => ({ slot: 1 });
    const key = {
      toString() {
        churn();
        return "slot";
      },
    };

    expect(delete make()[key]).toBe(true);
  });

  test("the non-configurable TypeError still names a collected-through base", () => {
    // The throw path formats the base into the message, so it dereferences the
    // base after the collection the key expression triggered.
    const make = () => {
      const target = {};
      Object.defineProperty(target, "fixed", {
        value: 1,
        configurable: false,
      });
      return target;
    };

    expect(() => delete make()[(churn(), "fixed")]).toThrow(TypeError);
  });
});
