/*---
description: Object.fromEntries
features: [Object.fromEntries]
---*/

describe("Object.fromEntries", () => {
  test("basic key-value pairs", () => {
    const entries = [["a", 1], ["b", 2], ["c", 3]];
    const obj = Object.fromEntries(entries);
    expect(obj.a).toBe(1);
    expect(obj.b).toBe(2);
    expect(obj.c).toBe(3);
  });

  test("roundtrip with Object.entries", () => {
    const original = { x: 10, y: 20 };
    const roundtripped = Object.fromEntries(Object.entries(original));
    expect(roundtripped.x).toBe(10);
    expect(roundtripped.y).toBe(20);
  });

  test("empty entries array produces empty object", () => {
    const obj = Object.fromEntries([]);
    expect(Object.keys(obj).length).toBe(0);
    expect(obj instanceof Object).toBe(true);
  });

  test("duplicate keys use last value", () => {
    const entries = [["a", 1], ["a", 2]];
    const obj = Object.fromEntries(entries);
    expect(obj.a).toBe(2);
  });

  test("uses define semantics for result properties", () => {
    let setterCalled = false;
    Object.defineProperty(Object.prototype, "property", {
      configurable: true,
      get() {
        throw new Error("getter should not run");
      },
      set(value) {
        setterCalled = true;
        throw new Error("setter should not run");
      },
    });

    try {
      const obj = Object.fromEntries([["property", "value"]]);
      expect(obj.property).toBe("value");
      expect(Object.hasOwn(obj, "property")).toBe(true);
      expect(setterCalled).toBe(false);
    } finally {
      delete Object.prototype.property;
    }
  });

  test("uses ToPropertyKey for symbol keys", () => {
    const key = Symbol("entry");
    const obj = Object.fromEntries([[key, 42]]);
    expect(obj[key]).toBe(42);
    expect(Object.getOwnPropertySymbols(obj)[0]).toBe(key);
  });
});

// Both entry reads are accessor calls and ToPropertyKey coerces the key, so
// three separate GC safe points run while the entry, the key and the value are
// held only in native locals.
describe.runIf(typeof Goccia !== "undefined")("Object.fromEntries under explicit GC", () => {
  // A bare gc() usually leaves the freed slot readable; the allocation churn
  // afterwards is what makes a collected value observable.
  const gcChurn = () => {
    Goccia.gc();
    let total = 0;
    for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
      const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
      total += scratch.a + scratch.b[0];
    }
    return total;
  };

  const freshEntries = (makeEntry) => ({
    [Symbol.iterator]() {
      let index = 0;
      return {
        next() {
          index++;
          if (index > 2) {
            return { done: true };
          }
          return { value: makeEntry(index), done: false };
        },
      };
    },
  });

  test("keeps the key alive when the value getter collects", () => {
    const obj = Object.fromEntries(
      freshEntries((index) => {
        const label = "k" + index;
        return {
          get 0() {
            return {
              [Symbol.toPrimitive]() {
                return label;
              },
            };
          },
          get 1() {
            gcChurn();
            return "v" + index;
          },
        };
      }),
    );

    expect(obj.k1).toBe("v1");
    expect(obj.k2).toBe("v2");
  });

  test("keeps the value alive when the key's toPrimitive collects", () => {
    const obj = Object.fromEntries(
      freshEntries((index) => {
        const label = "k" + index;
        return {
          get 0() {
            return {
              [Symbol.toPrimitive]() {
                gcChurn();
                return label;
              },
            };
          },
          get 1() {
            return { s: "v" + index, pad: [1, 2, 3] };
          },
        };
      }),
    );

    expect(obj.k1.s).toBe("v1");
    expect(obj.k2.s).toBe("v2");
  });
});
