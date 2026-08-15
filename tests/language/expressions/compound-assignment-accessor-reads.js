/*---
description: Compound assignment reads its target exactly once, before the right-hand side
features: [compound-assignment-operators, Symbol, accessor-properties]
---*/

// ES2026 §13.15.2 evaluates `target op= value` by taking GetValue(target) once
// (step 3) and only then evaluating the right-hand side, so an accessor target
// runs its getter once per compound assignment and its setter once per store.

const makeAccessor = (initial) => {
  const state = {
    reads: 0,
    writes: 0,
    stored: initial,
  };

  state.target = {
    get value() {
      state.reads += 1;
      return state.stored;
    },
    set value(next) {
      state.writes += 1;
      state.stored = next;
    },
  };

  return state;
};

describe("compound assignment on accessor properties", () => {
  test("a named target runs the getter and the setter once", () => {
    const state = makeAccessor(1);

    state.target.value += 1;

    expect(state.reads).toBe(1);
    expect(state.writes).toBe(1);
    expect(state.stored).toBe(2);
  });

  test("a named target reports the stored value as the expression result", () => {
    const state = makeAccessor(10);

    expect((state.target.value -= 4)).toBe(6);
    expect(state.reads).toBe(1);
    expect(state.writes).toBe(1);
  });

  test("a computed target runs the getter and the setter once", () => {
    const state = makeAccessor("a");
    const key = "value";

    state.target[key] += "b";

    expect(state.reads).toBe(1);
    expect(state.writes).toBe(1);
    expect(state.stored).toBe("ab");
  });

  test("an inherited accessor is read once through the receiver", () => {
    const state = makeAccessor(3);
    const child = Object.create(state.target);

    child.value *= 5;

    expect(state.reads).toBe(1);
    expect(state.writes).toBe(1);
    expect(state.stored).toBe(15);
  });

  test("each compound assignment reads its own target once", () => {
    const left = makeAccessor(2);
    const right = makeAccessor(7);

    left.target.value += right.target.value;

    expect(left.reads).toBe(1);
    expect(left.writes).toBe(1);
    expect(right.reads).toBe(1);
    expect(right.writes).toBe(0);
    expect(left.stored).toBe(9);
  });
});

describe("compound assignment on symbol-keyed accessor properties", () => {
  const key = Symbol("value");

  const makeSymbolAccessor = (initial) => {
    const state = {
      reads: 0,
      writes: 0,
      stored: initial,
    };

    state.target = {};
    Object.defineProperty(state.target, key, {
      get() {
        state.reads += 1;
        return state.stored;
      },
      set(next) {
        state.writes += 1;
        state.stored = next;
      },
      configurable: true,
    });

    return state;
  };

  test("a symbol-keyed target runs the getter and the setter once", () => {
    const state = makeSymbolAccessor(4);

    state.target[key] += 6;

    expect(state.reads).toBe(1);
    expect(state.writes).toBe(1);
    expect(state.stored).toBe(10);
  });

  test("a short-circuiting symbol-keyed target is read once and never stored", () => {
    const state = makeSymbolAccessor(4);

    state.target[key] ??= 99;

    expect(state.reads).toBe(1);
    expect(state.writes).toBe(0);
    expect(state.stored).toBe(4);
  });
});

describe("short-circuiting compound assignment on accessor properties", () => {
  test("??= reads once and stores only when the target is nullish", () => {
    const nullish = makeAccessor(undefined);
    nullish.target.value ??= "filled";
    expect(nullish.reads).toBe(1);
    expect(nullish.writes).toBe(1);
    expect(nullish.stored).toBe("filled");

    const present = makeAccessor("kept");
    present.target.value ??= "filled";
    expect(present.reads).toBe(1);
    expect(present.writes).toBe(0);
    expect(present.stored).toBe("kept");
  });

  test("||= reads once and stores only when the target is falsy", () => {
    const falsy = makeAccessor(0);
    falsy.target.value ||= 5;
    expect(falsy.reads).toBe(1);
    expect(falsy.writes).toBe(1);
    expect(falsy.stored).toBe(5);

    const truthy = makeAccessor(1);
    truthy.target.value ||= 5;
    expect(truthy.reads).toBe(1);
    expect(truthy.writes).toBe(0);
    expect(truthy.stored).toBe(1);
  });

  test("&&= reads once and stores only when the target is truthy", () => {
    const truthy = makeAccessor(1);
    truthy.target.value &&= 5;
    expect(truthy.reads).toBe(1);
    expect(truthy.writes).toBe(1);
    expect(truthy.stored).toBe(5);

    const falsy = makeAccessor(0);
    falsy.target.value &&= 5;
    expect(falsy.reads).toBe(1);
    expect(falsy.writes).toBe(0);
    expect(falsy.stored).toBe(0);
  });

  test("a computed short-circuiting target is read once", () => {
    const state = makeAccessor(null);
    const key = "value";

    state.target[key] ??= "filled";

    expect(state.reads).toBe(1);
    expect(state.writes).toBe(1);
    expect(state.stored).toBe("filled");
  });
});

describe("compound assignment on data properties", () => {
  test("named, computed and element targets keep their values", () => {
    const object = { count: 1, label: "a" };

    object.count += 2;
    object["label"] += "b";

    expect(object.count).toBe(3);
    expect(object.label).toBe("ab");

    const values = [1, 2, 3];
    values[1] *= 10;
    expect(values[1]).toBe(20);
  });

  test("a missing target compounds against undefined", () => {
    const object = {};

    object.missing += 1;
    object["alsoMissing"] ??= "default";

    expect(object.missing).toBeNaN();
    expect(object.alsoMissing).toBe("default");
  });

  test("a symbol-keyed data target keeps its value", () => {
    const key = Symbol("count");
    const object = { [key]: 5 };

    object[key] -= 2;

    expect(object[key]).toBe(3);
  });

  test("a primitive base is read through its wrapper", () => {
    // §6.2.5.5 GetValue step 3 boxes the primitive, so the short-circuit read
    // sees the real property value; the store then throws because a primitive
    // base accepts no property write.
    const text = "abc";

    expect(() => {
      text.length &&= 0;
    }).toThrow(TypeError);
    expect(text.length).toBe(3);
  });

  test("a nullish base throws before the right-hand side runs", () => {
    // §13.15.2 step 3 performs the read before step 4 evaluates the RHS, so
    // the TypeError fires with the side effect never executed.
    let sideEffects = 0;
    const base = { missing: null };

    expect(() => {
      base.missing.x += (sideEffects += 1);
    }).toThrow(TypeError);
    expect(sideEffects).toBe(0);
  });
});
