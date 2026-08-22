/*---
description: computed data-property and field definitions define the value correctly under collection
features: [Goccia]
---*/

// Regression guard for the property-definition paths. Unlike computed *stores*,
// the define opcodes (OP_DEFINE_DATA_PROP / OP_DEFINE_METHOD_PROP and the
// class-field DYNAMIC opcodes) receive an already-coerced key: the compiler
// emits OP_TO_PROPERTY_KEY (object literals) or captures the coerced key as an
// upvalue/local (class fields) *before* the value operand is materialized, so
// the define op's own ClassifyPropertyKey never re-enters guest code and cannot
// collect while the value is unrooted. Method values are register-rooted
// functions. These cases therefore do not leak; this file collects hard from the
// key hook to keep that invariant honest.

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

describe("computed definition operands under collection", () => {
  test("object literal computed data property keeps its value", () => {
    const obj = { [keyBox("k")]: 2.5 };
    expect(obj.k).toBe(2.5);

    const multi = { [keyBox("a")]: 3.5, [keyBox("b")]: 4.5 };
    expect(multi.a).toBe(3.5);
    expect(multi.b).toBe(4.5);
  });

  test("class instance field with a computed key keeps its value", () => {
    class C {
      [keyBox("f")] = 5.5;
    }
    const c = new C();
    expect(c.f).toBe(5.5);
  });

  test("class static field with a computed key keeps its value", () => {
    class C {
      static [keyBox("s")] = 6.5;
    }
    expect(C.s).toBe(6.5);
  });

  test("mixed static and instance computed fields keep their values", () => {
    class C {
      static [keyBox("s")] = 7.5;
      [keyBox("f")] = 8.5;
    }
    expect(C.s).toBe(7.5);
    expect(new C().f).toBe(8.5);
  });

  test("class method with a computed key still resolves correctly", () => {
    class C {
      [keyBox("m")]() {
        return 42;
      }
    }
    expect(new C().m()).toBe(42);
  });
});
