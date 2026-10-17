/*---
description: >
  Reading a global identifier finds the binding when deciding that it exists
  runs guest code that grows the VM's stacks.
features: [Proxy]
---*/

// Whether a global binding exists can be decided by a Proxy `has` trap on the
// global object's prototype chain. The trap recurses, so the bytecode VM's
// register stack is reallocated while the check runs. The check's result must
// still reach the register the read tests, or the read throws a ReferenceError.
// A global that a shim creates on first use, such as Date, runs guest code at
// the same point.

const recurse = (n) => (n === 0 ? 0 : 1 + recurse(n - 1));

const withGlobalPrototype = (prototype, body) => {
  const original = Object.getPrototypeOf(globalThis);
  Object.setPrototypeOf(globalThis, prototype);
  try {
    return body();
  } finally {
    Object.setPrototypeOf(globalThis, original);
  }
};

const growingHasTrap = (name, value, depth) =>
  new Proxy(
    {},
    {
      has(target, key) {
        if (key !== name) return false;
        recurse(depth);
        return true;
      },
      get(target, key) {
        return key === name ? value : undefined;
      },
    },
  );

describe("global identifier read while guest code grows the stacks", () => {
  test("finds a binding whose existence a recursing trap reports", () => {
    const read = () => trapBackedGlobal;
    const result = withGlobalPrototype(
      growingHasTrap("trapBackedGlobal", 42, 500),
      read,
    );
    expect(result).toBe(42);
  });

  test("finds it at every depth of the recursion", () => {
    const read = () => trapBackedGlobalAtDepth;
    for (const depth of [1, 10, 100, 1000]) {
      const result = withGlobalPrototype(
        growingHasTrap("trapBackedGlobalAtDepth", depth, depth),
        read,
      );
      expect(result).toBe(depth);
    }
  });
});
