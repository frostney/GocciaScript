/*---
description: WeakMap supports subclass construction
features: [WeakMap, class]
---*/

test("WeakMap can be subclassed", () => {
  class ChildWeakMap extends WeakMap {}
  const key = {};
  const map = new ChildWeakMap([[key, 42]]);
  expect(map instanceof ChildWeakMap).toBe(true);
  expect(map instanceof WeakMap).toBe(true);
  expect(map.get(key)).toBe(42);
});

test("WeakMap subclass can add instance fields", () => {
  class ChildWeakMap extends WeakMap {
    constructor(entries) {
      super(entries);
      this.name = "child";
    }
  }
  const key = {};
  const map = new ChildWeakMap([[key, "value"]]);
  expect(map.name).toBe("child");
  expect(map.get(key)).toBe("value");
});
