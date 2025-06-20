test("object method calls with arrow functions and context", () => {
  const obj = {
    name: "TestObject",
    value: 42,

    // Arrow function - doesn't bind 'this'
    arrowMethod: (x) => {
      return "Arrow method called with: " + x;
    },

    // Regular function - binds 'this'
    regularMethod(x) {
      return "Regular method called with: " + x + " on " + this.name;
    },

    // Method that calls other methods
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

  expect(obj.arrowMethod("test1")).toBe("Arrow method called with: test1");
  expect(obj.regularMethod("test2")).toBe(
    "Regular method called with: test2 on TestObject"
  );

  const combined = obj.combinedMethod("test3");
  expect(combined.arrow).toBe("Arrow method called with: test3");
  expect(combined.regular).toBe(
    "Regular method called with: test3 on TestObject"
  );
  expect(combined.value).toBe(42);
});
