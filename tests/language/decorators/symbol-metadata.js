/*---
description: Symbol.metadata is available as a well-known symbol
features: [decorators, decorator-metadata]
---*/

describe("Symbol.metadata", () => {
  test("Symbol.metadata is a symbol", () => {
    expect(typeof Symbol.metadata).toBe("symbol");
  });

  test("Symbol.metadata is always the same instance", () => {
    expect(Symbol.metadata === Symbol.metadata).toBe(true);
  });

  test("Symbol.metadata description", () => {
    expect(Symbol.metadata.toString()).toBe("Symbol(Symbol.metadata)");
  });
});
