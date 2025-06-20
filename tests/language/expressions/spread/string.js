/*---
description: Spread syntax for strings
features: [string-spread]
---*/

test("spread to convert string to array", () => {
  const str = "hello";
  const spread = [...str];
  expect(spread).toEqual(["h", "e", "l", "l", "o"]);
});
