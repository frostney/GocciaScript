/*---
description: Structured return type annotations on function-keyword forms are parsed and ignored at runtime
features: [types-as-comments, compat-function]
---*/

test("function declaration with object return type", () => {
  function makeEntry(): { label: string } {
    return { label: "ready" };
  }

  expect(makeEntry().label).toBe("ready");
});

test("function expression with nested structured return type", () => {
  const makeCollection = function (): { items: { label: string }[] } {
    return { items: [{ label: "ready" }] };
  };

  expect(makeCollection().items[0].label).toBe("ready");
});
