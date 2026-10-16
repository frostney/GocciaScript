/*---
description: Defining a class's own prototype property as a static element or through defineProperty throws a TypeError
features: [computed-property-names, class-static-fields-public, static-methods, decorators, auto-accessor]
---*/

const key = "prototype";

test("a computed static field named prototype throws a TypeError", () => {
  expect(() => {
    class C {
      static [key] = 1;
    }
    return C;
  }).toThrow(TypeError);
});

test("a computed static method named prototype throws a TypeError", () => {
  expect(() => {
    class C {
      static [key]() {}
    }
    return C;
  }).toThrow(TypeError);
});

test("a computed static auto-accessor named prototype throws a TypeError", () => {
  expect(() => {
    class C {
      static accessor [key] = 1;
    }
    return C;
  }).toThrow(TypeError);
});

test("Object.defineProperty cannot change a class's prototype value", () => {
  class C {}
  const original = C.prototype;
  expect(() => Object.defineProperty(C, "prototype", { value: {} })).toThrow(
    TypeError,
  );
  expect(C.prototype).toBe(original);
});
