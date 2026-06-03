/*---
description: WeakRef supports subclass construction
features: [WeakRef, class]
---*/

test("WeakRef can be subclassed", () => {
  class ChildWeakRef extends WeakRef {}
  const target = {};
  const ref = new ChildWeakRef(target);
  expect(ref instanceof ChildWeakRef).toBe(true);
  expect(ref instanceof WeakRef).toBe(true);
  expect(ref.deref()).toBe(target);
});

test("WeakRef subclass can add instance fields", () => {
  class ChildWeakRef extends WeakRef {
    constructor(target) {
      super(target);
      this.name = "child";
    }
  }
  const target = {};
  const ref = new ChildWeakRef(target);
  expect(ref.name).toBe("child");
  expect(ref.deref()).toBe(target);
});
