/*---
description: Decorator evaluation and application order follows the spec
features: [decorators]
---*/

describe("decorator ordering", () => {
  test("decorators applied bottom-to-top", () => {
    const order = [];

    const first = (value, context) => { order.push("first-call"); };
    const second = (value, context) => { order.push("second-call"); };

    class C {
      @first
      @second
      foo() {}
    }

    expect(order[0]).toBe("second-call");
    expect(order[1]).toBe("first-call");
  });

  test("element decorators called before class decorator", () => {
    const order = [];

    const classDecorator = (cls, context) => {
      order.push("class");
    };

    const methodDecorator = (method, context) => {
      order.push("method");
    };

    @classDecorator
    class C {
      @methodDecorator
      foo() {}
    }

    expect(order[0]).toBe("method");
    expect(order[1]).toBe("class");
  });
});
