/*---
description: let cannot shadow built-in globals at the same scope level but can in nested blocks
features: [let-declaration]
---*/

test("let in a nested block shadows built-in NaN locally", () => {
  {
    let NaN = 42;
    expect(NaN).toBe(42);
  }
  expect(NaN).toBeNaN();
});

test("let in a nested block shadows built-in Infinity locally", () => {
  {
    let Infinity = "finite";
    expect(Infinity).toBe("finite");
  }
  expect(Infinity).toBe(1 / 0);
});

test("let in a nested block shadows built-in Array locally", () => {
  {
    let Array = "not-an-array";
    expect(Array).toBe("not-an-array");
  }
  expect(typeof Array).toBe("function");
});
