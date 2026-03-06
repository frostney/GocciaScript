/*---
description: Field initializers run in source order
features: [classes, private-fields]
---*/

test("field initializers run in source order", () => {
  const order = [];

  class Foo {
    a = order.push("a");
    #b = order.push("b");
    c = order.push("c");
  }

  new Foo();
  expect(order.length).toBe(3);
  expect(order[0]).toBe("a");
  expect(order[1]).toBe("b");
  expect(order[2]).toBe("c");
});
