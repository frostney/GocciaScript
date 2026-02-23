/*---
description: Decorator error cases throw appropriate errors
features: [decorators]
---*/

describe("decorator errors", () => {
  test("non-callable decorator throws", () => {
    let threw = false;
    try {
      const notAFunction = 42;

      @notAFunction
      class C {}
    } catch (e) {
      threw = true;
    }
    expect(threw).toBe(true);
  });
});
