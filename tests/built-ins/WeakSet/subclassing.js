/*---
description: WeakSet supports subclass construction
features: [WeakSet, class]
---*/

test("WeakSet can be subclassed", () => {
  class ChildWeakSet extends WeakSet {}
  const value = {};
  const set = new ChildWeakSet([value]);
  expect(set instanceof ChildWeakSet).toBe(true);
  expect(set instanceof WeakSet).toBe(true);
  expect(set.has(value)).toBe(true);
});

test("WeakSet subclass can add instance fields", () => {
  class ChildWeakSet extends WeakSet {
    constructor(values) {
      super(values);
      this.name = "child";
    }
  }
  const value = {};
  const set = new ChildWeakSet([value]);
  expect(set.name).toBe("child");
  expect(set.has(value)).toBe(true);
});
