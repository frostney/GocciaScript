/*---
description: Type annotations on function parameters enforce type constraints at runtime (bytecode mode)
features: [types-as-comments, strict-type-enforcement]
---*/

describe("parameter type enforcement", () => {

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

test("parameter with default value checks the argument once the default applies", () => {
  const greet = (name: string = "world") => "hello " + name;
  expect(greet()).toBe("hello world");
  expect(greet(undefined)).toBe("hello world");
  expect(greet("test")).toBe("hello test");
  expect(() => greet(42)).toThrow(TypeError);
});

test("parameter with default value checks the default", () => {
  const count = (n: number = "none") => n;
  expect(count(3)).toBe(3);
  expect(() => count()).toThrow(TypeError);
});

test("parameter with default value checks later stores", () => {
  const update = (n: number = 1, next) => {
    n = next;
    return n;
  };
  expect(update(undefined, 2)).toBe(2);
  expect(() => update(undefined, "two")).toThrow(TypeError);
});

test("parameter with default value is checked before a closure in a later default sees it", () => {
  const read = (n: number = 1, get = () => n) => get();
  expect(read(5)).toBe(5);
  expect(read()).toBe(1);
  expect(() => read("five")).toThrow(TypeError);
});

test("parameter with default value reports the parameter's type", () => {
  const greet = (name: string = "world") => name;
  expect(() => greet(42)).toThrow("Type 'number' is not assignable to type 'string'");
});

test("optional parameter skips later store enforcement", () => {
  const reset = (count?: number) => {
    count = "none";
    return count;
  };
  expect(reset()).toBe("none");
});

}); // describe
