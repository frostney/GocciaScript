/*---
description: Spread syntax for type coercion
features: [type-coercion]
---*/

test("spread with primitive type coercion for objects", () => {
  // Number spreading (no enumerable properties)
  const num = { ...42 };
  expect(num).toEqual({});

  // Boolean spreading (no enumerable properties)
  const bool = { ...true };
  expect(bool).toEqual({});

  // String spreading (has indexed properties)
  const str = { ..."abc" };
  expect(str).toEqual({ 0: "a", 1: "b", 2: "c" });
});

test("spread with primitive type coercion for arrays causes a type error", () => {
  expect(() => [...42]).toThrow(TypeError);
  expect(() => [...true]).toThrow(TypeError);
  expect(() => [...null]).toThrow(TypeError);
  expect(() => [...undefined]).toThrow(TypeError);
});

test("spread with null and undefined in objects are being ignored", () => {
  const obj = { ...null, ...undefined };
  expect(obj).toEqual({});

  const obj2 = { ...null, ...{ a: 1 }, ...undefined };
  expect(obj2).toEqual({ a: 1 });
});
