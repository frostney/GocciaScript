/*---
description: Type annotations on function parameters enforce type constraints at runtime (bytecode mode)
features: [types-as-comments, strict-type-enforcement]
---*/

describe.skipIf(!GocciaScript.strictTypes)("parameter type enforcement", () => {

test("typed parameter rejects wrong type", () => {
  const add = (a: number, b: number) => a + b;
  expect(() => add("hello", "world")).toThrow(TypeError);
});

test("typed parameter accepts correct type", () => {
  const add = (a: number, b: number) => a + b;
  expect(add(3, 4)).toBe(7);
});

test("string parameter rejects number", () => {
  const greet = (name: string) => "hello " + name;
  expect(() => greet(42)).toThrow(TypeError);
});

test("string parameter accepts string", () => {
  const greet = (name: string) => "hello " + name;
  expect(greet("world")).toBe("hello world");
});

test("boolean parameter rejects number", () => {
  const toggle = (flag: boolean) => !flag;
  expect(() => toggle(0)).toThrow(TypeError);
});

test("boolean parameter accepts boolean", () => {
  const toggle = (flag: boolean) => !flag;
  expect(toggle(true)).toBe(false);
});

test("optional parameter skips enforcement", () => {
  const greet = (name?: string) => name === undefined ? "hello" : "hello " + name;
  expect(greet()).toBe("hello");
  expect(greet("world")).toBe("hello world");
  expect(greet(42)).toBe("hello 42");
});

test("parameter with default value skips enforcement", () => {
  const greet = (name: string = "world") => "hello " + name;
  expect(greet()).toBe("hello world");
  expect(greet("test")).toBe("hello test");
  expect(greet(42)).toBe("hello 42");
});

}); // describe
