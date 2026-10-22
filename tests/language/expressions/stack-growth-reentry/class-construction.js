/*---
description: >
  `new` stores the constructed instance when the constructor runs guest code
  that grows the VM's stacks.
---*/

// The constructor recurses deeper each time, so the bytecode VM's register
// stack is reallocated while it runs, and the instance must still reach the
// register that holds the `new` expression's result.

const recurse = (n) => (n === 0 ? 0 : 1 + recurse(n - 1));

let depth = 8;
class Grower {
  constructor(...values) {
    depth *= 4;
    this.depth = recurse(depth);
    this.values = values;
  }
}

describe("class construction while guest code grows the stacks", () => {
  test("new with arguments", () => {
    const instance = new Grower(1, 2);
    expect(instance instanceof Grower).toBe(true);
    expect(instance.depth).toBe(depth);
    expect(instance.values).toEqual([1, 2]);
  });

  test("new with spread arguments", () => {
    const args = [3, 4, 5];
    const instance = new Grower(...args);
    expect(instance instanceof Grower).toBe(true);
    expect(instance.depth).toBe(depth);
    expect(instance.values).toEqual([3, 4, 5]);
  });
});
