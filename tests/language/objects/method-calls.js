/*---
description: Object shorthand methods accept parameters and return values
features: [Object, method-calls, property-access]
---*/

test("object method calls with parameters", () => {
  const obj = {
    method(x) {
      return "Method called with: " + x;
    },
    complexMethod(a, b, c) {
      return {
        params: [a, b, c],
        total: a + b + c,
        type: typeof a,
      };
    },
  };

  expect(obj.method("first")).toBe("Method called with: first");
  expect(obj.method("second")).toBe("Method called with: second");
  expect(obj.method(123)).toBe("Method called with: 123");

  const result = obj.complexMethod(10, 20, 30);
  expect(result.params).toEqual([10, 20, 30]);
  expect(result.total).toBe(60);
  expect(result.type).toBe("number");
});

test("object method calls preserve receiver and fixed arguments", () => {
  const obj = {
    base: 10,
    zero() {
      return this.base;
    },
    one(a) {
      return this.base + a;
    },
    two(a, b) {
      return this.base + a + b;
    },
    three(a, b, c) {
      return this.base + a + b + c;
    },
  };

  expect(obj.zero()).toBe(10);
  expect(obj.one(1)).toBe(11);
  expect(obj.two(1, 2)).toBe(13);
  expect(obj.three(1, 2, 3)).toBe(16);
});
