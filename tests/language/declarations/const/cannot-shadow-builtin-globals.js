/*---
description: const cannot shadow built-in globals at the same scope level but can in nested blocks
features: [const-declaration]
---*/

test("const in a nested block shadows built-in NaN locally", () => {
  {
    const NaN = 42;
    expect(NaN).toBe(42);
  }
  expect(NaN).toBeNaN();
});

test("const in a nested block shadows built-in Array locally", () => {
  {
    const Array = "not-an-array";
    expect(Array).toBe("not-an-array");
  }
  expect(typeof Array).toBe("function");
});
