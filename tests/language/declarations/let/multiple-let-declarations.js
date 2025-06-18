/*---
description: Multiple let declarations
features: [let]
---*/

test("multiple let declarations without initializers", () => {
  let a, b, c;
  expect(a).toBeUndefined();
  expect(b).toBeUndefined();
  expect(c).toBeUndefined();
});

test("multiple let declarations with some initializers", () => {
  let x = 10,
    y,
    z = 20;
  expect(x).toBe(10);
  expect(y).toBeUndefined();
  expect(z).toBe(20);
});

test("multiple let declarations with mix of types", () => {
  let num = 42,
    str = "hello",
    bool = true,
    obj = { key: "value" };
  expect(num).toBe(42);
  expect(str).toBe("hello");
  expect(bool).toBe(true);
  expect(obj).toEqual({ key: "value" });
});
