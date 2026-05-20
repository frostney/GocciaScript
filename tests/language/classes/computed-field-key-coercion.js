/*---
description: Computed public class field keys are coerced during class definition
features: [computed-property-names, class-fields-public, class-static-fields-public]
---*/

test("static computed field coerces key before initializer", () => {
  const order = [];
  const key = {
    toString() {
      order.push("key");
      return "value";
    },
  };

  class C {
    static [key] = (order.push("init"), 1);
  }

  expect(order).toEqual(["key", "init"]);
  expect(C.value).toBe(1);
});

test("instance computed field coerces key once at class definition", () => {
  const order = [];
  const key = {
    toString() {
      order.push("key");
      return "value";
    },
  };

  class C {
    [key] = (order.push("init"), 1);
  }

  expect(order).toEqual(["key"]);

  const first = new C();
  const second = new C();

  expect(order).toEqual(["key", "init", "init"]);
  expect(first.value).toBe(1);
  expect(second.value).toBe(1);
});
