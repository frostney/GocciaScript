/*---
description: Promise.prototype.constructor
features: [Promise]
---*/

test("links Promise.prototype and Promise instances back to Promise", () => {
  const promise = new Promise((resolve) => resolve(1));
  expect(Promise.prototype.constructor).toBe(Promise);
  expect(promise.constructor).toBe(Promise);
});
