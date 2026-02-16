/*---
description: Arrow function edge cases
features: [arrow-function]
---*/

describe("arrow function edge cases", () => {
  test("arrow function cannot be used with new", () => {
    const Foo = () => {};
    expect(() => {
      new Foo();
    }).toThrow();
  });

  test("arrow function expression body returns value", () => {
    const add = (a, b) => a + b;
    expect(add(2, 3)).toBe(5);
  });

  test("arrow function block body requires explicit return", () => {
    const noReturn = (x) => {
      x + 1;
    };
    expect(noReturn(5)).toBe(undefined);
  });

  test("arrow function with no parameters", () => {
    const greet = () => "hello";
    expect(greet()).toBe("hello");
  });

  test("arrow function with rest parameters", () => {
    const sum = (...nums) => nums.reduce((a, b) => a + b, 0);
    expect(sum(1, 2, 3)).toBe(6);
  });

  test("arrow function with default parameters", () => {
    const greet = (name = "world") => `hello ${name}`;
    expect(greet()).toBe("hello world");
    expect(greet("Alice")).toBe("hello Alice");
  });

  test("immediately invoked arrow function", () => {
    const result = ((x) => x * x)(5);
    expect(result).toBe(25);
  });

  test("arrow function returning an object literal", () => {
    const makeObj = (x) => ({ value: x });
    expect(makeObj(10).value).toBe(10);
  });

  test("nested arrow functions (currying)", () => {
    const add = (a) => (b) => a + b;
    expect(add(3)(4)).toBe(7);
  });

  test("arrow function as callback", () => {
    const numbers = [1, 2, 3, 4, 5];
    const doubled = numbers.map((n) => n * 2);
    expect(doubled).toEqual([2, 4, 6, 8, 10]);
  });

  test("arrow function closure captures variables", () => {
    const makeCounter = () => {
      let count = 0;
      return () => {
        count = count + 1;
        return count;
      };
    };
    const counter = makeCounter();
    expect(counter()).toBe(1);
    expect(counter()).toBe(2);
    expect(counter()).toBe(3);
  });

  test("arrow function with destructuring parameter", () => {
    const getX = ({ x }) => x;
    expect(getX({ x: 42, y: 10 })).toBe(42);
  });

  test("arrow function with array destructuring parameter", () => {
    const first = ([a]) => a;
    expect(first([10, 20, 30])).toBe(10);
  });
});
