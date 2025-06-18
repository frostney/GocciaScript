/*---
description: Methods with default parameters
features: [methods-default-parameters]
---*/

test("methods with default parameters", () => {
  class Calculator {
    // Instance method with default parameters
    add(a, b = 0) {
      return a + b;
    }
  }

  const calc = new Calculator();
  expect(calc.add(5)).toBe(5); // 5 (using default b = 0)
  expect(calc.add(5, 3)).toBe(8); // 8 (using provided values)
});

test("static method with default parameters", () => {
  class Calculator {
    // Static method with multiple defaults
    static multiply(x = 1, y = 1) {
      return x * y;
    }
  }

  expect(Calculator.multiply()).toBe(1); // 1 (using both defaults)
  expect(Calculator.multiply(3)).toBe(3); // 3 (using y default)
  expect(Calculator.multiply(3, 4)).toBe(12); // 12 (using provided values)
});
