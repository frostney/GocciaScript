/*---
description: strict computed property stores keep the stored value alive across key coercion
features: [Goccia, Symbol]
---*/

// A strict computed store `o[keyObj] = 1.5` materializes the value operand into
// a fresh, native-only number object, then runs ClassifyPropertyKey on the key.
// When the key is an object, classification coerces it through ToPrimitive and
// re-enters guest code (toString/valueOf). A collection forced from there can
// sweep the still-unrooted value and hand the freed block back to the hook, so
// the property ends up storing garbage rather than the intended number.
//
// Every case asserts an exact, distinctive value and collects twice inside the
// key's coercion hook, with a churn allocation so the freed block is likely to
// be reused before the store completes.

const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

const churn = () => {
  const junk = Array.from({ length: 256 }, (_, i) => ({ i, s: `${i}` }));
  return junk.length;
};

// A key object whose string coercion churns the heap and collects twice.
const keyBox = (name) => ({
  toString() {
    churn();
    collect();
    return name;
  },
});

describe("strict computed store operands under collection", () => {
  test("plain object store keeps the value alive across key coercion", () => {
    const o = {};
    o[keyBox("a")] = 1.5;
    expect(o.a).toBe(1.5);

    const o2 = {};
    o2[keyBox("b")] = 2.5;
    o2[keyBox("c")] = 3.5;
    expect(o2.b).toBe(2.5);
    expect(o2.c).toBe(3.5);
  });

  test("large-int values survive too (non-singleton materialization)", () => {
    const o = {};
    o[keyBox("n")] = 123456.75;
    expect(o.n).toBe(123456.75);
  });

  test("array target keeps a non-index value alive across key coercion", () => {
    const arr = [];
    arr[keyBox("tag")] = 9.5;
    expect(arr.tag).toBe(9.5);
  });

  test("class static computed store keeps the value alive", () => {
    class C {}
    C[keyBox("s")] = 4.5;
    expect(C.s).toBe(4.5);
  });

  test("compound computed store goes through the same opcode", () => {
    const o = { k: 10 };
    o[keyBox("k")] += 2.5;
    expect(o.k).toBe(12.5);
  });
});
