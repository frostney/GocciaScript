/*---
description: with-statement binding assignments keep the value alive across the has trap
features: [Goccia, Proxy]
---*/

// Assigning to a name resolved through a `with` object materializes the value
// operand and then probes the binding object with HasProperty. For a Proxy that
// runs a `has` trap, a collection forced from the trap can sweep the still-
// unrooted value before the store, so the property ends up holding garbage.

const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

const churn = () => {
  const junk = Array.from({ length: 256 }, (_, i) => ({ i, s: `${i}` }));
  return junk.length;
};

describe("with-binding value under collection", () => {
  test("value survives a collecting proxy has trap", () => {
    const target = { x: 0 };
    const bindings = new Proxy(target, {
      has(t, k) {
        churn();
        collect();
        return k in t;
      },
    });

    with (bindings) {
      x = 1.5;
    }
    expect(target.x).toBe(1.5);
  });

  test("successive assignments keep distinct values", () => {
    const target = { a: 0, b: 0 };
    const bindings = new Proxy(target, {
      has(t, k) {
        churn();
        collect();
        return k in t;
      },
    });

    with (bindings) {
      a = 12345.75;
      b = 67890.25;
    }
    expect(target.a).toBe(12345.75);
    expect(target.b).toBe(67890.25);
  });
});
