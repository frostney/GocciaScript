/*---
description: The implicit arguments object requires --compat-non-strict-mode
features: [compat-function]
---*/

test("ordinary functions do not create arguments by default", () => {
  function count() {
    return typeof arguments;
  }

  expect(count(1, 2)).toBe("undefined");
});

test("arguments remains an ordinary identifier", () => {
  function echo(arguments) {
    return arguments;
  }

  expect(echo("param")).toBe("param");
});
