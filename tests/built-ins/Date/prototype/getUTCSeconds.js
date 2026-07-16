/*---
description: Date.prototype.getUTCSeconds
features: [Date]
---*/

test("returns the UTC second", () => {
  expect(new Date(1718451045123).getUTCSeconds()).toBe(45);
});
