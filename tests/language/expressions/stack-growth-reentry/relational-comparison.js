/*---
description: >
  A relational comparison stores its result when converting an operand runs
  guest code that grows the VM's stacks.
---*/

// `<`, `>`, `<=` and `>=` on objects call valueOf through ToPrimitive. Here
// valueOf recurses deeper each time, so the bytecode VM's register stack is
// reallocated during every comparison, and the comparison's result must still
// reach its register.

const recurse = (n) => (n === 0 ? 0 : 1 + recurse(n - 1));

let depth = 4;
const growing = (value) => ({
  valueOf() {
    depth *= 4;
    recurse(depth);
    return value;
  },
});

describe("relational comparison while guest code grows the stacks", () => {
  test("< and > store their results", () => {
    const less = growing(1) < 2;
    const greater = growing(1) > 2;
    expect(less).toBe(true);
    expect(greater).toBe(false);
  });

  test("<= and >= store their results", () => {
    const lessOrEqual = growing(3) <= 2;
    const greaterOrEqual = growing(3) >= 2;
    expect(lessOrEqual).toBe(false);
    expect(greaterOrEqual).toBe(true);
  });
});
