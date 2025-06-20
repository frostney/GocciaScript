/*---
description: Spread syntax for objects with method preservation
features: [object-method-preservation]
---*/

test("spread with object method preservation", () => {
  const source = {
    name: "test",
    greet() {
      return `Hello, ${this.name}`;
    },
    arrow: () => "arrow function",
  };

  const spread = { ...source };
  expect(spread.name).toBe("test");
  expect(spread.greet()).toBe("Hello, test");
  expect(spread.arrow()).toBe("arrow function");
});

test("object spread with getters and computed values", () => {
  let getterCallCount = 0;
  const source = {
    get dynamic() {
      getterCallCount++;
      return `computed-${getterCallCount}`;
    },
    static: "fixed",
  };

  // Getter should be called during spread
  const spread1 = { ...source };
  expect(spread1.dynamic).toBe("computed-1");
  expect(spread1.static).toBe("fixed");
  expect(getterCallCount).toBe(1);

  // Multiple spreads call getter multiple times
  const spread2 = { ...source, ...source };
  expect(spread2.dynamic).toBe("computed-3"); // Called twice more
  expect(getterCallCount).toBe(3);
});
