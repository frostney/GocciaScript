/*---
description: computed property loads on a primitive receiver keep the receiver alive across key coercion
features: [Goccia]
---*/

// `(2.5)[keyObj]` boxes the primitive receiver into a fresh number object, then
// resolves the object key through ToPrimitive — re-entering guest code. A
// collection forced from the key's hook can sweep the still-unrooted receiver
// before the boxed [[Get]] runs, so an accessor that reads `this` sees a freed
// block rather than the original number.

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

// A getter on Number.prototype that reads its receiver, so a corrupted receiver
// produces a wrong (or faulting) result rather than the exact expected value.
Object.defineProperty(Number.prototype, "gcProbe", {
  get() {
    return this * 10;
  },
  configurable: true,
});

describe("computed load receiver under collection", () => {
  test("primitive number receiver survives key coercion", () => {
    expect((2.5)[keyBox("gcProbe")]).toBe(25);
    expect((6.5)[keyBox("gcProbe")]).toBe(65);
    expect((123456.75)[keyBox("gcProbe")]).toBe(1234567.5);
  });

  test("chained loads keep each receiver alive", () => {
    const a = (2.5)[keyBox("gcProbe")];
    const b = (4.5)[keyBox("gcProbe")];
    expect(a).toBe(25);
    expect(b).toBe(45);
  });
});
