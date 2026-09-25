/*---
description: Assignment and update expressions keep their temporaries reachable during explicit GC
features: [Goccia.gc, Symbol.toPrimitive]
---*/

const hasGoccia = typeof Goccia !== "undefined";

// The base, the key and the assigned value of an assignment live only in the
// evaluator's own locals while the expression is being evaluated, so a
// collection that runs from inside the key conversion, the right-hand side or
// the setter can free them. The allocation churn after gc() is what makes a
// freed slot observable; a bare gc() usually leaves it readable.
const churn = () => {
  Goccia.gc();
  let total = 0;
  for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
    total += scratch.a + scratch.b[0];
  }
  return total;
};

describe.runIf(hasGoccia)("assignment expression GC roots", () => {
  test("a computed store survives a collecting right-hand side", () => {
    const target = {};
    target[{ toString: () => "computed" }] = (() => {
      churn();
      return "assigned";
    })();

    expect(target.computed).toBe("assigned");
  });

  test("a transient base survives a collecting key expression", () => {
    let observed = null;
    const makeTarget = () => ({
      set slot(value) {
        observed = value;
      },
    });

    makeTarget()[(churn(), "slot")] = "through-setter";

    expect(observed).toBe("through-setter");
  });

  test("a key converted by Symbol.toPrimitive does not lose the value", () => {
    let stored = null;
    const target = {
      set converted(value) {
        stored = value;
      },
    };

    // The value is evaluated before the key is converted, so it is live across
    // the guest [Symbol.toPrimitive] call and everything that collects inside it.
    target[{
      [Symbol.toPrimitive]() {
        churn();
        return "converted";
      },
    }] = "via-toPrimitive";

    expect(stored).toBe("via-toPrimitive");
  });

  test("a key that is also the assigned value survives the store", () => {
    let storedTag = null;
    const target = {
      set alias(value) {
        churn();
        storedTag = value.tag;
      },
    };
    // After the pop, the same object is the key AND the value, and nothing but
    // the evaluator refers to it.
    const holder = [{ toString: () => "alias", tag: "same-object" }];

    target[holder[0]] = holder.pop();

    expect(storedTag).toBe("same-object");
    expect(holder.length).toBe(0);
  });

  test("compound assignment survives a collecting base, key and right-hand side", () => {
    const makeCounter = () => ({ count: 40 });

    const counter = makeCounter();
    counter[{
      toString() {
        churn();
        return "count";
      },
    }] += (() => {
      churn();
      return 2;
    })();

    expect(counter.count).toBe(42);
  });

  test("compound assignment survives a right-hand side that rebinds the target", () => {
    // §13.15.2 step 3 reads the left operand before the right-hand side runs,
    // and the right-hand side then drops the binding's only other reference to
    // it: the old string is reachable from the executor's own temporaries
    // alone while gc() runs. The replacement is empty, so a value that was not
    // rooted across the collection cannot produce the expected result.
    let text = "a".repeat(20);
    text += ((text = ""), churn(), "tail");

    expect(text).toBe("a".repeat(20) + "tail");
  });

  test("an update expression survives a collecting base, key and getter", () => {
    let written = null;
    const makeBox = () => ({
      get counter() {
        churn();
        return 41;
      },
      set counter(next) {
        written = next;
      },
    });

    const previous = makeBox()[(churn(), "counter")]++;

    expect(previous).toBe(41);
    expect(written).toBe(42);
  });

  test("an update expression survives a collecting valueOf", () => {
    const box = {
      amount: {
        valueOf() {
          churn();
          return 7;
        },
      },
    };

    box.amount++;

    expect(box.amount).toBe(8);
  });
});
