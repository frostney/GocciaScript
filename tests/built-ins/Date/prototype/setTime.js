/*---
description: Date.prototype.setTime
features: [Date]
---*/

test("updates the time value and returns it", () => {
  const value = new Date(0);
  expect(value.setTime(1718451045123)).toBe(1718451045123);
  expect(value.getTime()).toBe(1718451045123);
});

test("TimeClip canonicalizes negative zero", () => {
  const value = new Date(1);
  expect(Object.is(value.setTime(-0), 0)).toBe(true);
  expect(Object.is(value.getTime(), 0)).toBe(true);
  expect(Object.is(value.setTime(-1.23e-15), 0)).toBe(true);
});
