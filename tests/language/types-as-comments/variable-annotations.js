/*---
description: Type annotations on variable declarations are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("simple variable type annotations", () => {
  let x: number = 42;
  expect(x).toBe(42);

  let name: string = "hello";
  expect(name).toBe("hello");

  let flag: boolean = true;
  expect(flag).toBe(true);
});

test("const with type annotation", () => {
  const pi: number = 3.14;
  expect(pi).toBe(3.14);

  const greeting: string = "world";
  expect(greeting).toBe("world");
});

test("complex type annotations", () => {
  let arr: Array<number> = [1, 2, 3];
  expect(arr.length).toBe(3);

  let obj: { name: string, age: number } = { name: "test", age: 25 };
  expect(obj.name).toBe("test");
});

test("union type annotations", () => {
  let value: string | number = "hello";
  expect(value).toBe("hello");

  value = 42;
  expect(value).toBe(42);
});

test("multiple variable declarations with types", () => {
  let a: number = 1, b: string = "two", c: boolean = true;
  expect(a).toBe(1);
  expect(b).toBe("two");
  expect(c).toBe(true);
});

test("let without initializer with type", () => {
  let x: number;
  expect(x).toBe(undefined);
});

test("destructuring with type annotation", () => {
  const { x, y }: { x: number, y: number } = { x: 1, y: 2 };
  expect(x).toBe(1);
  expect(y).toBe(2);
});

test("array destructuring with type annotation", () => {
  const [a, b]: [number, string] = [1, "two"];
  expect(a).toBe(1);
  expect(b).toBe("two");
});
