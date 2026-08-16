/*---
description: Object literal keys, spread sources and spread key lists stay reachable during explicit GC
features: [Goccia.gc, Symbol.toPrimitive]
---*/

const hasGoccia = typeof Goccia !== "undefined";

// A bare gc() usually leaves a freed slot readable; the allocation churn after
// it is what makes a collected temporary observable.
const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
    total += scratch.a + scratch.b[0];
  }
  return total;
};

describe.runIf(hasGoccia)("object literal GC roots", () => {
  test("a converted computed key survives a collecting value expression", () => {
    // §13.2.5.5 converts the key before the value runs. The key string is
    // produced inside the conversion, so nothing but the evaluator's own local
    // refers to it while the value expression collects.
    const target = {
      [{
        [Symbol.toPrimitive]() {
          return "conv" + "erted";
        },
      }]: (churn(), "value"),
    };

    expect(target.converted).toBe("value");
  });

  test("a computed method key survives building the method", () => {
    const target = {
      [{
        toString() {
          churn();
          return "meth" + "od";
        },
      }]: () => "called",
    };

    expect(target.method()).toBe("called");
  });

  test("a spread key list survives a getter that collects", () => {
    // The spread's key list is built up front as fresh string values held only
    // by a native array. The first getter collects, and the keys the copy has
    // not reached yet are reachable from nowhere at that moment.
    const source = {
      get first() {
        churn();
        return 1;
      },
      second: 2,
      third: 3,
    };

    const copy = { ...source };

    expect(copy.first).toBe(1);
    expect(copy.second).toBe(2);
    expect(copy.third).toBe(3);
  });

  test("a spread source produced by a call survives its own getter", () => {
    const makeSource = () => ({
      get head() {
        churn();
        return "h";
      },
      tail: "t",
    });

    // The source exists only in the evaluator's local: the arrow's scope is
    // gone by the time the copy starts.
    const copy = { ...makeSource() };

    expect(copy.head).toBe("h");
    expect(copy.tail).toBe("t");
  });

  test("the literal under construction survives a collecting property value", () => {
    const built = {
      first: (churn(), 1),
      second: (churn(), 2),
    };

    expect(built.first).toBe(1);
    expect(built.second).toBe(2);
  });
});
