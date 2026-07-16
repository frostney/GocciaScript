/*---
description: FinalizationRegistry.prototype.register validates weak targets and unregister tokens
features: [FinalizationRegistry, Symbol]
---*/

test("register returns undefined for object targets", () => {
  const registry = new FinalizationRegistry(() => {});
  const target = {};
  expect(registry.register(target, "held")).toBe(undefined);
});

test("register accepts non-registered symbol targets and tokens", () => {
  const registry = new FinalizationRegistry(() => {});
  const target = Symbol("target");
  const token = Symbol("token");
  expect(registry.register(target, "held", token)).toBe(undefined);
  expect(registry.unregister(token)).toBe(true);
});

test("register accepts object unregister tokens", () => {
  const registry = new FinalizationRegistry(() => {});
  const target = {};
  const token = {};
  registry.register(target, "held", token);
  expect(registry.unregister(token)).toBe(true);
});

test("register rejects primitive targets and registered symbol targets", () => {
  const registry = new FinalizationRegistry(() => {});
  expect(() => registry.register()).toThrow(TypeError);
  expect(() => registry.register(undefined, "held")).toThrow(TypeError);
  expect(() => registry.register(null, "held")).toThrow(TypeError);
  expect(() => registry.register(1, "held")).toThrow(TypeError);
  expect(() => registry.register("target", "held")).toThrow(TypeError);
  expect(() => registry.register(true, "held")).toThrow(TypeError);
  expect(() => registry.register(Symbol.for("registered"), "held")).toThrow(TypeError);
});

test("register rejects a held value that is SameValue to the target", () => {
  const registry = new FinalizationRegistry(() => {});
  const objectTarget = {};
  const symbolTarget = Symbol("target");

  expect(() => registry.register(objectTarget, objectTarget)).toThrow(TypeError);
  expect(() => registry.register(symbolTarget, symbolTarget)).toThrow(TypeError);
});

test("register rejects primitive and registered symbol unregister tokens", () => {
  const registry = new FinalizationRegistry(() => {});
  const target = {};

  expect(() => registry.register(target, "held", null)).toThrow(TypeError);
  expect(() => registry.register(target, "held", 1)).toThrow(TypeError);
  expect(() => registry.register(target, "held", "token")).toThrow(TypeError);
  expect(() => registry.register(target, "held", true)).toThrow(TypeError);
  expect(() => registry.register(target, "held", Symbol.for("registered"))).toThrow(TypeError);
});

test("register treats undefined unregister token as absent", () => {
  const registry = new FinalizationRegistry(() => {});
  expect(registry.register({}, "held", undefined)).toBe(undefined);
});

test("register throws TypeError when receiver is not a FinalizationRegistry", () => {
  const register = FinalizationRegistry.prototype.register;
  expect(() => register.call({}, {}, "held")).toThrow(TypeError);
  expect(() => register.call(FinalizationRegistry.prototype, {}, "held")).toThrow(TypeError);
});

test("register is not constructable", () => {
  expect(() =>
    Reflect.construct(class {}, [], FinalizationRegistry.prototype.register)
  ).toThrow(TypeError);
});
