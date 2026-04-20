/*---
description: var declarations are function-scoped, escaping block scopes
features: [compat-var]
---*/

test("var escapes if-block scope", () => {
  const fn = () => {
    if (true) {
      var x = 42;
    }
    return x;
  };
  expect(fn()).toBe(42);
});

test("var escapes for-of block scope", () => {
  const fn = () => {
    for (const item of [1, 2, 3]) {
      var last = item;
    }
    return last;
  };
  expect(fn()).toBe(3);
});

test("var in nested blocks accessible in function", () => {
  const fn = () => {
    if (true) {
      if (true) {
        var deep = "found";
      }
    }
    return deep;
  };
  expect(fn()).toBe("found");
});
