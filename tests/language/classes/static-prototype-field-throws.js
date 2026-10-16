/*---
description: Defining a class's own prototype property as a static field or through defineProperty throws a TypeError
features: [computed-property-names, class-static-fields-public]
---*/

test("a computed static field named prototype throws a TypeError", () => {
  const key = "prototype";
  expect(() => {
    class C {
      static [key] = 1;
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
