/*---
description: RegExp literals preserve NUL code units in their pattern payload
features: [RegExp]
---*/

test("regex literal with NUL escape is not split at the token separator", () => {
  const nul = String.fromCharCode(0);
  const regex = Function("return /\\" + nul + "/;")();

  expect(regex.source).toBe("\\" + nul);
  expect(regex.test(nul)).toBe(true);
});
