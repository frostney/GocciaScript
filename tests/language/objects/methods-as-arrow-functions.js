/*---
description: Arrow functions as object properties follow ECMAScript arrow semantics
features: [Object, arrow-functions, property-access]
---*/

test("arrow functions as object properties work without this", () => {
  const obj = {
    method: (x) => {
      return "Method called with: " + x;
    },
    complexMethod: (a, b, c) => {
      return {
        params: [a, b, c],
        total: a + b + c,
        type: typeof a,
      };
    },
  };

  // Arrow function methods work when they don't reference 'this'
  expect(obj.method("first")).toBe("Method called with: first");
  expect(obj.method("second")).toBe("Method called with: second");
  expect(obj.method(123)).toBe("Method called with: 123");

  const result = obj.complexMethod(10, 20, 30);
  expect(result.params).toEqual([10, 20, 30]);
  expect(result.total).toBe(60);
  expect(result.type).toBe("number");
});

test("arrow functions inherit this from lexical scope, not call-site", () => {
  // Arrow functions defined at script top-level inherit `this` from the
  // global script environment, whose [[ThisValue]] is the global object
  // (ES2026 §9.1.2.5 / §9.4.3 / §13.2.1.1).
  const obj = {
    value: 42,
    arrow: () => this,
    arrowType: () => typeof this,
    shorthand() {
      return this.value;
    },
  };

  // Arrow function does NOT get 'this' bound to the object — it inherits
  // the surrounding script-level `this` (globalThis).
  expect(obj.arrow()).toBe(globalThis);
  expect(obj.arrowType()).toBe("object");

  // Shorthand method DOES get 'this' bound to the object
  expect(obj.shorthand()).toBe(42);
});

test("arrow functions inside methods inherit method this", () => {
  const obj = {
    value: 10,
    getDoubled() {
      // Arrow function inside a method inherits the method's 'this'
      const double = () => this.value * 2;
      return double();
    },
  };

  expect(obj.getDoubled()).toBe(20);
});
