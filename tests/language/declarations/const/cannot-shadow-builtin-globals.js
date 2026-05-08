/*---
description: const declarations cannot shadow built-in globals at the same scope level
features: [const-declaration]
---*/

test("const NaN at top level throws SyntaxError", () => {
  expect(() => {
    eval("const NaN = 42");
  }).toThrow();
});

test("const Array at top level throws SyntaxError", () => {
  expect(() => {
    eval("const Array = []");
  }).toThrow();
});

test("const in a nested block does not conflict with built-in globals", () => {
  {
    const NaN = 42;
    expect(NaN).toBe(42);
  }
  expect(NaN).toBeNaN();
});
