/*---
description: RegExp.prototype[Symbol.split]
features: [RegExp.prototype[Symbol.split]]
---*/

test("Symbol.split coerces limit with ToUint32", () => {
  expect(/,/[Symbol.split]("a,b,c", NaN)).toEqual([]);
  expect(/,/[Symbol.split]("a,b,c", Infinity)).toEqual([]);
  expect(/,/[Symbol.split]("a,b,c", -Infinity)).toEqual([]);
  expect(/,/[Symbol.split]("a,b,c", 4294967297)).toEqual(["a"]);
  expect(/,/[Symbol.split]("a,b,c", -4294967295)).toEqual(["a"]);
});
