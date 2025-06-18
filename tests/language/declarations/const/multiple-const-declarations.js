/*---
description: Multiple const declarations
features: [const]
---*/

test("multiple const declarations", () => {
  const a = 1,
    b = 2,
    c = 3;
  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(c).toBe(3);
});

test("multiple const declarations with mix of types", () => {
  const num = 42,
    str = "hello",
    bool = true,
    obj = { key: "value" };
  expect(num).toBe(42);
  expect(str).toBe("hello");
  expect(bool).toBe(true);
  expect(obj).toEqual({ key: "value" });
});
