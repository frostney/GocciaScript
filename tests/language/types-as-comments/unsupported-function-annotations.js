/*---
description: typed annotations on unsupported function syntax still follow GocciaScript's graceful function fallback
features: [types-as-comments, parser-warnings, unsupported-features]
---*/

test("typed function declaration is skipped", () => {
  let x = 1;

  function foo<T>(value: T): T {
    x = 99;
    return value;
  }

  expect(x).toBe(1);
  expect(typeof foo).toBe("undefined");
});

test("typed function expression evaluates to undefined", () => {
  const fn = function <T>(value: T): T {
    return value;
  };

  expect(fn).toBe(undefined);
});
