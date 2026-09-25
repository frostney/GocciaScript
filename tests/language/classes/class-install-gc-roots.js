/*---
description: A class under construction and its private stores stay reachable during explicit GC
features: [Goccia.gc, class-fields-private, class-static-block]
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

describe.runIf(hasGoccia)("class installation GC roots", () => {
  test("an anonymous class survives a collecting static field initializer", () => {
    // A class declaration is bound into the scope while it installs; a class
    // expression is not, so the class object is reachable from nothing at all
    // while the initializer runs.
    const Anonymous = class {
      static first = (churn(), "one");
      static second = (churn(), "two");
    };

    expect(Anonymous.first).toBe("one");
    expect(Anonymous.second).toBe("two");
  });

  test("an anonymous class survives a collecting static block", () => {
    const Anonymous = class {
      static tag = "before";
      static {
        churn();
        this.tag = "after";
      }
    };

    expect(Anonymous.tag).toBe("after");
  });

  test("a computed static field key survives its own conversion", () => {
    // The key is resolved while the class is being defined, so the class
    // itself is what is live across the guest toString.
    const Keyed = class {
      static [{
        toString() {
          churn();
          return "com" + "puted";
        },
      }] = "held" + "-value";
    };

    expect(Keyed.computed).toBe("held-value");
  });

  test("a computed static field key survives an earlier field's collection", () => {
    // Every computed element key is resolved up front, during class
    // definition, and held until the element that owns it is installed. A
    // field declared ahead of it runs its initializer in between, so the
    // already-resolved key is live across that guest code.
    const Keyed = class {
      static early = (churn(), "first");
      static [{
        toString() {
          return "com" + "puted";
        },
      }] = "held" + "-value";
    };

    expect(Keyed.early).toBe("first");
    expect(Keyed.computed).toBe("held-value");
  });

  test("a computed static field key survives a collecting static block", () => {
    const Keyed = class {
      static {
        churn();
      }
      static [{
        toString() {
          return "blocked" + "-key";
        },
      }] = "block-value";
    };

    expect(Keyed["blocked-key"]).toBe("block-value");
  });

  test("an anonymous class survives collecting instance field initializers", () => {
    const Holder = class {
      field = (churn(), "instance");
    };

    expect(new Holder().field).toBe("instance");
  });

  test("a private store keeps a transient receiver alive across its value", () => {
    class Box {
      #slot = "empty";
      static make() {
        return new Box();
      }
      static fill() {
        // The receiver is a call result held only by the evaluator while the
        // right-hand side runs.
        Box.make().#slot = (churn(), "filled");
      }
      static fillAndRead() {
        const box = Box.make();
        box.#slot = (churn(), "read-back");
        return box.#slot;
      }
    }

    Box.fill();
    expect(Box.fillAndRead()).toBe("read-back");
  });

  test("a private compound assignment survives a collecting right-hand side", () => {
    class Counter {
      #count = 40;
      static make() {
        return new Counter();
      }
      static bump() {
        const counter = Counter.make();
        counter.#count += (churn(), 2);
        return counter.#count;
      }
      static bumpTransientReceiver() {
        return (Counter.make().#count += (churn(), 2));
      }
    }

    expect(Counter.bump()).toBe(42);
    expect(Counter.bumpTransientReceiver()).toBe(42);
  });

  test("a private nullish assignment survives a collecting right-hand side", () => {
    class Slot {
      #value = undefined;
      static make() {
        return new Slot();
      }
      static fill() {
        const slot = Slot.make();
        slot.#value ??= (churn(), "defaulted");
        return slot.#value;
      }
    }

    expect(Slot.fill()).toBe("defaulted");
  });
});
