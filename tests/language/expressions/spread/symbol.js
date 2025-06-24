/*---
description: Spread syntax for Symbol
features: [symbol-spread]
---*/

test.skip("object spread with symbol properties", () => {
  const sym1 = Symbol("key1");
  const sym2 = Symbol("key2");

  const source = {
    regular: "value",
    [sym1]: "symbol1",
    [sym2]: "symbol2",
  };

  const spread = { ...source };
  expect(spread.regular).toBe("value");
  expect(spread[sym1]).toBe("symbol1");
  expect(spread[sym2]).toBe("symbol2");
});
