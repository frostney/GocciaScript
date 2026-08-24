/*---
description: computed super get/set keep this and value alive across key coercion
features: [Goccia]
---*/

// A computed super reference `super[keyObj]` resolves the super base, then
// coerces the object key through ToPropertyKey — re-entering guest code. For a
// super GET the receiver (`this`) is used after that coercion to run the
// resolved accessor; for a super SET the assigned value is used after it. When
// `this` is a boxed primitive or the value is a fresh number, a collection
// forced from the key's hook can sweep the still-unrooted operand and corrupt
// the accessor receiver or the stored value.

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

describe("computed super operands under collection", () => {
  test("super get keeps a primitive this alive across key coercion", () => {
    class Base {
      get gcProbe() {
        return this * 10;
      }
    }
    class Derived extends Base {
      read(key) {
        return super[key];
      }
    }
    // `this` is the primitive 2.5, which the getter reads; a swept receiver
    // would answer something other than 25.
    expect(Derived.prototype.read.call(2.5, keyBox("gcProbe"))).toBe(25);
    expect(Derived.prototype.read.call(6.5, keyBox("gcProbe"))).toBe(65);
  });

  test("super get with an object this reads the inherited value", () => {
    class Base {
      get gcValue() {
        return 33.5;
      }
    }
    class Derived extends Base {
      read(key) {
        return super[key];
      }
    }
    expect(new Derived().read(keyBox("gcValue"))).toBe(33.5);
  });

  test("super set keeps the assigned value alive across key coercion", () => {
    class Base {}
    class Derived extends Base {
      write(key) {
        super[key] = 1.5;
      }
    }
    const d = new Derived();
    d.write(keyBox("x"));
    expect(d.x).toBe(1.5);
  });

  test("super set stores distinct values across successive keys", () => {
    class Base {}
    class Derived extends Base {
      writeTwo(k1, k2) {
        super[k1] = 12345.75;
        super[k2] = 67890.25;
      }
    }
    const d = new Derived();
    d.writeTwo(keyBox("a"), keyBox("b"));
    expect(d.a).toBe(12345.75);
    expect(d.b).toBe(67890.25);
  });
});
