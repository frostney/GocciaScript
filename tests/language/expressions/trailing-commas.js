/*---
description: Trailing commas in function calls, parameters, constructors, arrays, and objects
features: [trailing-commas]
---*/

describe("trailing commas", () => {
  describe("function call arguments", () => {
    test("single argument with trailing comma", () => {
      const f = (x) => x * 2;
      expect(f(3,)).toBe(6);
    });

    test("multiple arguments with trailing comma", () => {
      const f = (a, b, c) => a + b + c;
      expect(f(1, 2, 3,)).toBe(6);
    });

    test("method call with trailing comma", () => {
      const arr = [1, 2, 3];
      expect(arr.indexOf(2,)).toBe(1);
    });

    test("nested calls with trailing commas", () => {
      const add = (a, b) => a + b;
      expect(add(add(1, 2,), 3,)).toBe(6);
    });

    test("spread argument with trailing comma", () => {
      const f = (...args) => args.length;
      expect(f(...[1, 2, 3],)).toBe(3);
    });

    test("multiline argument list with trailing comma", () => {
      const f = (a, b) => a + b;
      expect(f(
        1,
        2,
      )).toBe(3);
    });
  });

  describe("function parameter definitions", () => {
    test("single parameter with trailing comma", () => {
      const f = (x,) => x;
      expect(f(42)).toBe(42);
    });

    test("multiple parameters with trailing comma", () => {
      const f = (a, b, c,) => a + b + c;
      expect(f(1, 2, 3)).toBe(6);
    });

    test("destructuring parameter with trailing comma", () => {
      const f = ({ x, y },) => x + y;
      expect(f({ x: 1, y: 2 })).toBe(3);
    });

    test("array destructuring parameter with trailing comma", () => {
      const f = ([a, b],) => a + b;
      expect(f([10, 20])).toBe(30);
    });

    test("default parameter with trailing comma", () => {
      const f = (a, b = 5,) => a + b;
      expect(f(1)).toBe(6);
    });
  });

  describe("constructor calls", () => {
    test("new expression with trailing comma", () => {
      const m = new Map([["a", 1],]);
      expect(m.get("a")).toBe(1);
    });

    test("new expression multiple args with trailing comma", () => {
      class Point {
        constructor(x, y) {
          this.x = x;
          this.y = y;
        }
      }
      const p = new Point(1, 2,);
      expect(p.x).toBe(1);
      expect(p.y).toBe(2);
    });
  });

  describe("array literals", () => {
    test("single element with trailing comma", () => {
      const arr = [1,];
      expect(arr.length).toBe(1);
      expect(arr[0]).toBe(1);
    });

    test("multiple elements with trailing comma", () => {
      const arr = [1, 2, 3,];
      expect(arr.length).toBe(3);
    });

    test("nested arrays with trailing commas", () => {
      const arr = [[1, 2,], [3, 4,],];
      expect(arr.length).toBe(2);
      expect(arr[0]).toEqual([1, 2]);
    });
  });

  describe("object literals", () => {
    test("single property with trailing comma", () => {
      const obj = { x: 1, };
      expect(obj.x).toBe(1);
    });

    test("multiple properties with trailing comma", () => {
      const obj = { a: 1, b: 2, c: 3, };
      expect(obj.a).toBe(1);
      expect(obj.b).toBe(2);
      expect(obj.c).toBe(3);
    });

    test("shorthand properties with trailing comma", () => {
      const x = 1;
      const y = 2;
      const obj = { x, y, };
      expect(obj.x).toBe(1);
      expect(obj.y).toBe(2);
    });

    test("computed properties with trailing comma", () => {
      const key = "name";
      const obj = { [key]: "Alice", };
      expect(obj.name).toBe("Alice");
    });

    test("method shorthand with trailing comma", () => {
      const obj = {
        greet() { return "hello"; },
      };
      expect(obj.greet()).toBe("hello");
    });

    test("nested objects with trailing commas", () => {
      const obj = {
        inner: {
          a: 1,
          b: 2,
        },
      };
      expect(obj.inner.a).toBe(1);
    });

    test("getter/setter with trailing comma", () => {
      const obj = {
        get value() { return 42; },
      };
      expect(obj.value).toBe(42);
    });
  });
});
