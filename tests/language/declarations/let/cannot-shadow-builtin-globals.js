/*---
description: let declarations cannot shadow built-in globals at the same scope level
features: [let-declaration]
---*/

test("let NaN at top level throws SyntaxError", () => {
  expect(() => {
    eval("let NaN = 42");
  }).toThrow();
});

test("let Infinity at top level throws SyntaxError", () => {
  expect(() => {
    eval("let Infinity = 42");
  }).toThrow();
});

test("let undefined at top level throws SyntaxError", () => {
  expect(() => {
    eval("let undefined = 42");
  }).toThrow();
});

test("let Array at top level throws SyntaxError", () => {
  expect(() => {
    eval("let Array = 42");
  }).toThrow();
});

test("let in a nested block does not conflict with built-in globals", () => {
  {
    let NaN = 42;
    expect(NaN).toBe(42);
  }
  expect(NaN).toBeNaN();
});
