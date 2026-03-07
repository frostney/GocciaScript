/*---
description: Variables without type annotations allow any type reassignment (inference hints only, not strict)
features: [types-as-comments]
---*/

test("untyped variable allows any reassignment", () => {
  let x = 5;
  expect(x).toBe(5);
  x = "hello";
  expect(x).toBe("hello");
  x = true;
  expect(x).toBe(true);
  x = null;
  expect(x).toBe(null);
  x = undefined;
  expect(x).toBe(undefined);
  x = [1, 2];
  expect(x.length).toBe(2);
  x = { a: 1 };
  expect(x.a).toBe(1);
});

test("union type annotation does not enforce", () => {
  let value: string | number = "hello";
  expect(value).toBe("hello");
  value = 42;
  expect(value).toBe(42);
  value = true;
  expect(value).toBe(true);
});

test("any type annotation does not enforce", () => {
  let value: any = 1;
  expect(value).toBe(1);
  value = "text";
  expect(value).toBe("text");
});

test("unknown type annotation does not enforce", () => {
  let value: unknown = 1;
  expect(value).toBe(1);
  value = "text";
  expect(value).toBe("text");
});

test("let without initializer with type allows first assignment", () => {
  let x: number;
  expect(x).toBe(undefined);
  x = 42;
  expect(x).toBe(42);
});
