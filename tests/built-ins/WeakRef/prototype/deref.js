/*---
description: WeakRef.prototype.deref returns a live target or undefined
features: [WeakRef, Symbol]
---*/

test("deref returns the target while it is live", () => {
  const target = { marker: 1 };
  const ref = new WeakRef(target);
  expect(ref.deref()).toBe(target);
  expect(ref.deref().marker).toBe(1);
});

test("deref works for non-registered symbols", () => {
  const target = Symbol("weak");
  const ref = new WeakRef(target);
  expect(ref.deref()).toBe(target);
});

test("deref throws TypeError when receiver is not a WeakRef", () => {
  const deref = WeakRef.prototype.deref;
  expect(() => deref.call({})).toThrow(TypeError);
  expect(() => deref.call(WeakRef.prototype)).toThrow(TypeError);
});

test("deref is not constructable", () => {
  expect(() =>
    Reflect.construct(class {}, [], WeakRef.prototype.deref)
  ).toThrow(TypeError);
});
