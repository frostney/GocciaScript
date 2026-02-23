/*---
description: Destructuring in function and callback parameters
features: [destructuring, arrow-functions]
---*/

describe("destructuring parameters", () => {
  describe("array destructuring in parameters", () => {
    test("basic array parameter", () => {
      const sum = ([a, b]) => a + b;
      expect(sum([1, 2])).toBe(3);
    });

    test("with rest element", () => {
      const headAndTail = ([head, ...tail]) => ({ head, tail });
      const result = headAndTail([1, 2, 3, 4]);
      expect(result.head).toBe(1);
      expect(result.tail).toEqual([2, 3, 4]);
    });

    test("with default values", () => {
      const f = ([a, b = 10, c = 20]) => a + b + c;
      expect(f([1])).toBe(31);
      expect(f([1, 2])).toBe(23);
      expect(f([1, 2, 3])).toBe(6);
    });

    test("skipping elements", () => {
      const third = ([, , c]) => c;
      expect(third([1, 2, 3])).toBe(3);
    });

    test("nested array destructuring", () => {
      const f = ([[a, b], [c, d]]) => a + b + c + d;
      expect(f([[1, 2], [3, 4]])).toBe(10);
    });

    test("multiple destructured params", () => {
      const f = ([a, b], [c, d]) => a + b + c + d;
      expect(f([1, 2], [3, 4])).toBe(10);
    });
  });

  describe("object destructuring in parameters", () => {
    test("basic object parameter", () => {
      const greet = ({ name }) => "hello " + name;
      expect(greet({ name: "Alice" })).toBe("hello Alice");
    });

    test("multiple properties", () => {
      const area = ({ width, height }) => width * height;
      expect(area({ width: 3, height: 4 })).toBe(12);
    });

    test("with defaults", () => {
      const f = ({ x = 0, y = 0 }) => x + y;
      expect(f({})).toBe(0);
      expect(f({ x: 5 })).toBe(5);
      expect(f({ x: 3, y: 7 })).toBe(10);
    });

    test("with renaming", () => {
      const f = ({ firstName: first, lastName: last }) => first + " " + last;
      expect(f({ firstName: "Alice", lastName: "Smith" })).toBe("Alice Smith");
    });

    test("nested object destructuring", () => {
      const city = ({ address: { city } }) => city;
      expect(city({ address: { city: "NYC" } })).toBe("NYC");
    });

    test("with rest properties", () => {
      const f = ({ a, ...rest }) => Object.keys(rest).length;
      expect(f({ a: 1, b: 2, c: 3 })).toBe(2);
    });
  });

  describe("callback parameters with destructuring", () => {
    test("forEach with array destructuring", () => {
      const data = [["a", 1], ["b", 2], ["c", 3]];
      let result = "";
      data.forEach(([key, val]) => {
        result = result + key + val;
      });
      expect(result).toBe("a1b2c3");
    });

    test("map with array destructuring", () => {
      const pairs = [[1, 2], [3, 4], [5, 6]];
      const sums = pairs.map(([a, b]) => a + b);
      expect(sums).toEqual([3, 7, 11]);
    });

    test("map with object destructuring", () => {
      const items = [{ name: "a", value: 1 }, { name: "b", value: 2 }];
      const names = items.map(({ name }) => name);
      expect(names).toEqual(["a", "b"]);
    });

    test("filter with destructuring", () => {
      const entries = [["x", 10], ["y", 5], ["z", 20]];
      const big = entries.filter(([, val]) => val > 8);
      expect(big.length).toBe(2);
    });

    test("reduce with destructuring", () => {
      const entries = [["a", 1], ["b", 2], ["c", 3]];
      const sum = entries.reduce((acc, [, val]) => acc + val, 0);
      expect(sum).toBe(6);
    });

    test("nested destructuring in callback", () => {
      const data = [{ user: { name: "Alice" } }, { user: { name: "Bob" } }];
      const names = data.map(({ user: { name } }) => name);
      expect(names).toEqual(["Alice", "Bob"]);
    });

    test("rest in array destructuring callback", () => {
      const rows = [[1, 2, 3, 4], [5, 6, 7, 8]];
      const firsts = rows.map(([first, ...rest]) => first);
      expect(firsts).toEqual([1, 5]);
    });

    test("default values in destructuring callback", () => {
      const items = [{ x: 1 }, { x: 2, y: 10 }];
      const ys = items.map(({ y = 0 }) => y);
      expect(ys).toEqual([0, 10]);
    });

    test("mixed destructuring and regular params in callback", () => {
      const data = [[10, 20], [30, 40]];
      const result = data.map(([a, b], index) => a + b + index);
      expect(result).toEqual([30, 71]);
    });
  });
});
