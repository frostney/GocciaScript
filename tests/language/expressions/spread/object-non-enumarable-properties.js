/*---
description: Spread syntax for objects with non-enumerable properties
features: [object-non-enumerable-properties]
---*/

test("object spread with non-enumerable properties", () => {
  const obj = {};
  Object.defineProperty(obj, "hidden", {
    value: "secret",
    enumerable: false,
    writable: true,
    configurable: true,
  });
  Object.defineProperty(obj, "visible", {
    value: "public",
    enumerable: true,
    writable: true,
    configurable: true,
  });

  const spread = { ...obj };
  expect(spread.visible).toBe("public");
  expect(spread.hidden).toBeUndefined();
  expect("hidden" in spread).toBe(false);
});
