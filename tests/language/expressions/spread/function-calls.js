/*---
description: Spread syntax for function calls
features: [function-spread]
---*/

test("spread syntax for function calls", () => {
  const numbers = [42, 17, 89, 3, 56];

  const max = Math.max(...numbers);
  const min = Math.min(...numbers);

  expect(max).toBe(89);
  expect(min).toBe(3);
});
