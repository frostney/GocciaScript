/*---
description: instanceof keeps the instance alive across a Symbol.hasInstance getter
features: [Goccia, Symbol.hasInstance]
---*/

// `value instanceof RHS` boxes a primitive left operand into a fresh number
// object, then looks up RHS[Symbol.hasInstance]. When that lookup runs a user
// getter (or proxy trap), a collection forced from it can sweep the still-
// unrooted instance before it is handed to the handler, so the handler sees a
// freed block instead of the original value.

const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

const churn = () => {
  const junk = Array.from({ length: 256 }, (_, i) => ({ i, s: `${i}` }));
  return junk.length;
};

describe("instanceof instance under collection", () => {
  test("primitive instance survives a collecting hasInstance getter", () => {
    const RHS = {};
    Object.defineProperty(RHS, Symbol.hasInstance, {
      get() {
        churn();
        collect();
        return (v) => v === 42.5;
      },
      configurable: true,
    });

    expect(42.5 instanceof RHS).toBe(true);
    expect(41.5 instanceof RHS).toBe(false);
    expect(123456.75 instanceof RHS).toBe(false);
  });

  test("proxy hasInstance trap keeps the instance alive", () => {
    const RHS = new Proxy(
      { [Symbol.hasInstance]: (v) => v === 7.5 },
      {
        get(target, key, receiver) {
          churn();
          collect();
          return Reflect.get(target, key, receiver);
        },
      },
    );

    expect(7.5 instanceof RHS).toBe(true);
    expect(8.5 instanceof RHS).toBe(false);
  });
});
