/*---
description: Decorator error cases throw appropriate errors
features: [decorators]
---*/

describe("decorator errors", () => {
  test("non-callable decorator throws", () => {
    expect(() => {
      const notAFunction = 42;

      @notAFunction
      class C {}
    }).toThrow(TypeError);
  });
});
