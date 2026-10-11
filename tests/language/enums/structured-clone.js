/*---
description: structuredClone copies an enum object's members into a plain object
features: [enum-declaration, structuredClone]
---*/

// The enum proposal makes an enum object an ordinary object, so it takes the
// structured clone property walk.
test("structuredClone copies an enum's members", () => {
  enum Color {
    Red = 1,
    Green = 2,
  }
  const clone = structuredClone(Color);
  expect(clone).toEqual({ Red: 1, Green: 2 });
  expect(Object.getPrototypeOf(clone)).toBe(Object.prototype);
});
