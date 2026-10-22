/*---
description: When the value of an assignment or compound assignment throws, the binding keeps its previous value
features: [assignment, compound-assignment-operators, accessor-properties, Proxy]
---*/

const throwing = {
  get p() {
    throw new Error("read");
  },
};

describe("a throwing value leaves the binding as it was", () => {
  test("x = y.p and x = x.p", () => {
    const run = () => {
      const source = throwing;
      let x = 1;
      try {
        x = source.p;
      } catch (error) {
        expect(error.message).toBe("read");
      }
      const before = x;
      let base = throwing;
      try {
        base = base.p;
      } catch {
        // base keeps the object it held
      }
      return [before === 1, base === throwing];
    };
    expect(run()).toEqual([true, true]);
  });

  test("x = a[i] through a throwing Proxy and x = a + b with a throwing valueOf", () => {
    const run = () => {
      const trap = new Proxy([], {
        get() {
          throw new Error("trap");
        },
      });
      const bad = { valueOf() { throw new Error("valueOf"); } };
      let x = "kept";
      let y = 2;
      try { x = trap[0]; } catch { /* x unchanged */ }
      try { y = y + bad; } catch { /* y unchanged */ }
      try { y += bad; } catch { /* y unchanged */ }
      return [x, y];
    };
    expect(run()).toEqual(["kept", 2]);
  });

  test("this.x op= v keeps the property when the value throws", () => {
    class Counter {
      constructor() {
        this.count = 5;
      }
      bump(source) {
        this.count += source.p;
      }
    }
    const counter = new Counter();
    expect(() => counter.bump(throwing)).toThrow(Error);
    expect(counter.count).toBe(5);
  });
});
