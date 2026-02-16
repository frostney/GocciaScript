/*---
description: Chained function and method calls
features: [arrow-functions, chained-calls]
---*/

describe("chained function calls", () => {
  test("curried function call", () => {
    const add = (a) => (b) => a + b;
    expect(add(1)(2)).toBe(3);
  });

  test("triple-curried function call", () => {
    const f = (a) => (b) => (c) => a + b + c;
    expect(f(1)(2)(3)).toBe(6);
  });

  test("factory returning function", () => {
    const multiplier = (factor) => (x) => x * factor;
    expect(multiplier(3)(10)).toBe(30);
    expect(multiplier(0)(5)).toBe(0);
  });

  test("chained call with intermediate variable", () => {
    const outer = (x) => (y) => x * y;
    const inner = outer(5);
    expect(inner(3)).toBe(15);
    expect(outer(5)(3)).toBe(15);
  });
});

describe("chained method calls", () => {
  test("array method chaining", () => {
    const result = [1, 2, 3, 4, 5]
      .filter((x) => x > 2)
      .map((x) => x * 10);
    expect(result).toEqual([30, 40, 50]);
  });

  test("array triple chain", () => {
    const result = [1, 2, 3, 4, 5, 6]
      .filter((x) => x % 2 === 0)
      .map((x) => x * x)
      .reduce((sum, x) => sum + x, 0);
    expect(result).toBe(56);
  });

  test("string method chaining", () => {
    const result = "  Hello World  ".trim().toLowerCase();
    expect(result).toBe("hello world");
  });

  test("method call on function result", () => {
    const getArray = () => [3, 1, 4, 1, 5];
    expect(getArray().length).toBe(5);
    expect(getArray().map((x) => x * 2)).toEqual([6, 2, 8, 2, 10]);
  });

  test("method call on object literal method result", () => {
    const obj = {
      getItems() { return [10, 20, 30]; }
    };
    expect(obj.getItems().length).toBe(3);
    expect(obj.getItems().map((x) => x + 1)).toEqual([11, 21, 31]);
  });
});

describe("chained property access on call results", () => {
  test("property access on function return", () => {
    const makeObj = () => ({ x: 1, y: 2 });
    expect(makeObj().x).toBe(1);
    expect(makeObj().y).toBe(2);
  });

  test("nested property access on function return", () => {
    const getData = () => ({ user: { name: "Alice" } });
    expect(getData().user.name).toBe("Alice");
  });

  test("computed property on function return", () => {
    const getMap = () => ({ a: 10, b: 20 });
    const key = "b";
    expect(getMap()[key]).toBe(20);
  });
});
