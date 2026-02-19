/*---
description: Type annotations on function parameters are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("simple parameter type annotations", () => {
  const add = (a: number, b: number) => a + b;
  expect(add(1, 2)).toBe(3);
});

test("optional parameter marker", () => {
  const greet = (name?: string) => name === undefined ? "hello" : "hello " + name;
  expect(greet()).toBe("hello");
  expect(greet("world")).toBe("hello world");
});

test("parameter with default value and type", () => {
  const greet = (name: string = "world") => "hello " + name;
  expect(greet()).toBe("hello world");
  expect(greet("test")).toBe("hello test");
});

test("rest parameter with type annotation", () => {
  const sum = (...nums: number[]) => nums.reduce((acc, n) => acc + n, 0);
  expect(sum(1, 2, 3)).toBe(6);
});

test("destructuring parameter with type annotation", () => {
  const getName = ({ name, age }: { name: string, age: number }) => name;
  expect(getName({ name: "Alice", age: 30 })).toBe("Alice");
});

test("array destructuring parameter with type", () => {
  const first = ([a, b]: [number, number]) => a;
  expect(first([10, 20])).toBe(10);
});

test("complex generic parameter types", () => {
  const identity = (x: Map<string, Array<number>>) => x;
  const m = new Map();
  m.set("key", [1, 2]);
  expect(identity(m).get("key")[0]).toBe(1);
});
