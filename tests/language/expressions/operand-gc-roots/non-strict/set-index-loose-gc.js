/*---
description: non-strict computed stores keep the value alive across key coercion
features: [Goccia]
---*/

// In non-strict code `o[keyObj] = 1.5` compiles to the loose set-index opcode,
// which materializes the value operand and then coerces the object key through
// SetIndexValueLoose. That coercion re-enters guest code; a collection forced
// from the key's hook can sweep the still-unrooted value before the store, so
// the property ends up holding garbage.

const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

const churn = () => {
  const junk = Array.from({ length: 256 }, (_, i) => ({ i, s: `${i}` }));
  return junk.length;
};

const keyBox = (name) => ({
  toString() {
    churn();
    collect();
    return name;
  },
});

describe("non-strict computed store operands under collection", () => {
  test("object target keeps the value alive across key coercion", () => {
    const o = {};
    o[keyBox("a")] = 1.5;
    expect(o.a).toBe(1.5);

    const o2 = {};
    o2[keyBox("b")] = 2.5;
    o2[keyBox("c")] = 3.5;
    expect(o2.b).toBe(2.5);
    expect(o2.c).toBe(3.5);
  });

  test("large-int value survives too", () => {
    const o = {};
    o[keyBox("n")] = 123456.75;
    expect(o.n).toBe(123456.75);
  });

  test("primitive target does not crash while the key coerces", () => {
    // Assigning to a property of a primitive is a silent no-op in non-strict
    // code, but the boxed target and value must still survive the key coercion
    // rather than faulting mid-store.
    const x = 2.5;
    expect(() => {
      x[keyBox("ignored")] = 9.5;
    }).not.toThrow();
  });
});
