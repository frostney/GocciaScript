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

// ES2026 §24.4.1.1 WeakSet creates the set from NewTarget before it reads its
// `add` adder, so a subclass's super(iterable) adds through the subclass's own
// `add`. Expected values from Node.js v24.
test("WeakSet subclass super(iterable) adds through the subclass's add", () => {
  const log = [];
  const value = {};
  class LoggingWeakSet extends WeakSet {
    constructor() {
      super([value]);
    }
    add(v) {
      log.push("add");
      return super.add(v);
    }
  }
  const set = new LoggingWeakSet();
  expect(log).toEqual(["add"]);
  expect(set.has(value)).toBe(true);
});
