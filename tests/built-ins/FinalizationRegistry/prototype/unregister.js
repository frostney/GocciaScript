/*---
description: FinalizationRegistry.prototype.unregister removes matching cells
features: [FinalizationRegistry, Symbol]
---*/

test("unregister returns false when no cells match", () => {
  const registry = new FinalizationRegistry(() => {});
  expect(registry.unregister({})).toBe(false);
});

test("unregister returns true after removing an object-token registration", () => {
  const registry = new FinalizationRegistry(() => {});
  const target = {};
  const token = {};
  registry.register(target, "held", token);

  expect(registry.unregister(token)).toBe(true);
  expect(registry.unregister(token)).toBe(false);
});

test("unregister removes all cells with the same token", () => {
  const registry = new FinalizationRegistry(() => {});
  const token = {};

  registry.register({}, "first", token);
  registry.register({}, "second", token);

  expect(registry.unregister(token)).toBe(true);
  expect(registry.unregister(token)).toBe(false);
});

test("unregister supports non-registered symbol tokens", () => {
  const registry = new FinalizationRegistry(() => {});
  const target = {};
  const token = Symbol("token");

  registry.register(target, "held", token);
  expect(registry.unregister(token)).toBe(true);
});

test("unregister rejects primitive and registered symbol tokens", () => {
  const registry = new FinalizationRegistry(() => {});

  expect(() => registry.unregister()).toThrow(TypeError);
  expect(() => registry.unregister(undefined)).toThrow(TypeError);
  expect(() => registry.unregister(null)).toThrow(TypeError);
  expect(() => registry.unregister(1)).toThrow(TypeError);
  expect(() => registry.unregister("token")).toThrow(TypeError);
  expect(() => registry.unregister(true)).toThrow(TypeError);
  expect(() => registry.unregister(Symbol.for("registered"))).toThrow(TypeError);
});

test("unregister throws TypeError when receiver is not a FinalizationRegistry", () => {
  const unregister = FinalizationRegistry.prototype.unregister;
  expect(() => unregister.call({}, {})).toThrow(TypeError);
  expect(() => unregister.call(FinalizationRegistry.prototype, {})).toThrow(TypeError);
});

test("unregister is not constructable", () => {
  expect(() =>
    Reflect.construct(class {}, [], FinalizationRegistry.prototype.unregister)
  ).toThrow(TypeError);
});
