/*---
description: Unary operators work correctly
features: [unary-operators]
---*/

test("unary operators", () => {
  expect(typeof +1).toBe("number");
  expect(typeof -1).toBe("number");
  expect(typeof !true).toBe("boolean");
  expect(typeof !false).toBe("boolean");
});
