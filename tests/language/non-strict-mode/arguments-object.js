/*---
description: Non-strict compatibility does not implicitly enable arguments objects
features: [compat-function, compat-non-strict-mode]
---*/

test("non-strict compatibility alone leaves arguments disabled", () => {
  function capture() {
    return [typeof arguments, this === globalThis];
  }

  expect(capture("value")).toEqual(["undefined", true]);
});
