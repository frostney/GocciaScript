/*---
description: Basic static methods and static properties on classes
features: [static-methods, static-properties]
---*/

test("static methods are available on the constructor only", () => {
  class MathUtils {
    static add(a, b) {
      return a + b;
    }

    static multiply(a, b) {
      return a * b;
    }

    static PI = 3.14159;
  }

  expect(MathUtils.add(2, 3)).toBe(5);
  expect(MathUtils.multiply(4, 5)).toBe(20);
  expect(MathUtils.PI).toBe(3.14159);

  const instance = new MathUtils();
  expect(instance.add).toBeUndefined();
});
