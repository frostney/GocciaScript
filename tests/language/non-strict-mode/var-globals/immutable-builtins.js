/*---
description: Non-strict global var initializers for immutable built-ins are silent
features: [compat-non-strict-mode, compat-var]
---*/

const originalInfinity = Infinity;

var Infinity = 1;
Infinity = "overwritten";

var NaN = 1;
NaN = 0;

var undefined = 1;

test("global var initializers do not overwrite immutable built-ins", () => {
  expect(Infinity).toBe(originalInfinity);
  expect(Number.isNaN(NaN)).toBe(true);
  expect(undefined).toBeUndefined();
});
