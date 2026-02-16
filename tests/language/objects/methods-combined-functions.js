/*---
description: Object methods - shorthand vs arrow function this binding
features: [Object, method-calls, arrow-functions]
---*/

test("shorthand methods bind this, arrow functions do not", () => {
  const obj = {
    name: "TestObject",
    value: 42,

    // Arrow function — does not bind 'this' to the object
    arrowMethod: (x) => {
      return "Arrow method called with: " + x;
    },

    // Shorthand method — binds 'this' like a regular function
    regularMethod(x) {
      return "Regular method called with: " + x + " on " + this.name;
    },

    // Shorthand method that calls other methods
    combinedMethod(x) {
      const arrowResult = this.arrowMethod(x);
      const regularResult = this.regularMethod(x);
      return {
        arrow: arrowResult,
        regular: regularResult,
        value: this.value,
      };
    },
  };

  // Arrow function works (doesn't use this)
  expect(obj.arrowMethod("test1")).toBe("Arrow method called with: test1");

  // Shorthand method uses this correctly
  expect(obj.regularMethod("test2")).toBe(
    "Regular method called with: test2 on TestObject"
  );

  // Combined method can call both
  const combined = obj.combinedMethod("test3");
  expect(combined.arrow).toBe("Arrow method called with: test3");
  expect(combined.regular).toBe(
    "Regular method called with: test3 on TestObject"
  );
  expect(combined.value).toBe(42);
});

test("arrow functions defined inside methods inherit method this", () => {
  const obj = {
    items: [1, 2, 3],
    multiplier: 10,

    getScaled() {
      // Arrow function callback inherits this from the method
      return this.items.map((x) => x * this.multiplier);
    },
  };

  expect(obj.getScaled()).toEqual([10, 20, 30]);
});
