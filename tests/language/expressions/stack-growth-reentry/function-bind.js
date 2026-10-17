/*---
description: >
  Function.prototype.bind stores the bound function when reading the target's
  name or length runs guest code that grows the VM's stacks.
---*/

// bind reads the target's `length` and `name` (ES2026 §20.2.3.2 steps 4-10),
// which can be getters. These recurse deeper each time, so the bytecode VM's
// register stack is reallocated while bind runs, and the bound function must
// still reach the register that holds bind's result.

const recurse = (n) => (n === 0 ? 0 : 1 + recurse(n - 1));

let depth = 8;
const withGrowingName = (fn, name) => {
  Object.defineProperty(fn, "name", {
    get() {
      depth *= 4;
      recurse(depth);
      return name;
    },
    configurable: true,
  });
  return fn;
};

describe("bind while guest code grows the stacks", () => {
  test("with a this value only", () => {
    const target = withGrowingName(
      {
        readValue() {
          return this.value;
        },
      }.readValue,
      "readValue",
    );
    const bound = target.bind({ value: 7 });
    expect(typeof bound).toBe("function");
    expect(bound.name).toBe("bound readValue");
    expect(bound()).toBe(7);
  });

  test("with one bound argument", () => {
    const target = withGrowingName((a, b) => a * 10 + b, "combine");
    const bound = target.bind(null, 4);
    expect(bound.name).toBe("bound combine");
    expect(bound(2)).toBe(42);
  });

  test("with no arguments", () => {
    const target = withGrowingName(() => "called", "noArgs");
    const bound = target.bind();
    expect(bound.name).toBe("bound noArgs");
    expect(bound()).toBe("called");
  });
});
