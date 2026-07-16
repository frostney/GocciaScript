/*---
description: Date.now
features: [Date]
---*/

test("returns a current epoch millisecond number", () => {
  const before = new Date().getTime();
  const now = Date.now();
  const after = new Date().getTime();

  expect(typeof now).toBe("number");
  expect(now).toBeGreaterThanOrEqual(before);
  expect(now).toBeLessThanOrEqual(after);
});
