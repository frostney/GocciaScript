/*---
description: >
  `delete` of a global identifier stores its result when deciding whether the
  binding exists runs guest code that grows the VM's stacks.
features: [Proxy]
---*/

// Deleting an unqualified global name asks the global object whether it has
// the property. A Proxy `has` trap on the global object's prototype chain
// answers, and it recurses, so the bytecode VM's register stack is reallocated
// during the delete. The delete's result must still reach its register.

const recurse = (n) => (n === 0 ? 0 : 1 + recurse(n - 1));

describe("global delete while guest code grows the stacks", () => {
  test("stores true for a binding the trap reports", () => {
    let trapCalls = 0;
    const original = Object.getPrototypeOf(globalThis);
    Object.setPrototypeOf(
      globalThis,
      new Proxy(
        {},
        {
          has(target, key) {
            if (key !== "trapBackedGlobal") return false;
            trapCalls++;
            recurse(500);
            return true;
          },
        },
      ),
    );
    let result;
    try {
      result = delete trapBackedGlobal;
    } finally {
      Object.setPrototypeOf(globalThis, original);
    }
    expect(trapCalls).toBeGreaterThan(0);
    expect(result).toBe(true);
  });
});
