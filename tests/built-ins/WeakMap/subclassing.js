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

// ES2026 §24.3.1.1 WeakMap creates the map from NewTarget before it reads its
// `set` adder, so a subclass's super(iterable) adds through the subclass's own
// `set`. Expected values from Node.js v24.
test("WeakMap subclass super(iterable) adds through the subclass's set", () => {
  const log = [];
  const key = {};
  class LoggingWeakMap extends WeakMap {
    constructor() {
      super([[key, 2]]);
    }
    set(k, v) {
      log.push("set");
      return super.set(k, v);
    }
  }
  const map = new LoggingWeakMap();
  expect(log).toEqual(["set"]);
  expect(map.get(key)).toBe(2);
});
