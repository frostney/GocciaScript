/*---
description: Destructuring assignment into member expression targets
features: [destructuring]
---*/

test("array destructuring into this.x properties", () => {
  class Point {
    constructor(x, y) {
      this.x = x;
      this.y = y;
    }
    swap() {
      [this.x, this.y] = [this.y, this.x];
    }
  }

  const p = new Point(1, 2);
  expect(p.x).toBe(1);
  expect(p.y).toBe(2);
  p.swap();
  expect(p.x).toBe(2);
  expect(p.y).toBe(1);
});

test("array destructuring into obj.prop targets", () => {
  const obj = { a: 0, b: 0 };
  [obj.a, obj.b] = [10, 20];
  expect(obj.a).toBe(10);
  expect(obj.b).toBe(20);
});

test("array destructuring into arr[i] targets", () => {
  const arr = [0, 0, 0];
  [arr[0], arr[1], arr[2]] = [7, 8, 9];
  expect(arr[0]).toBe(7);
  expect(arr[1]).toBe(8);
  expect(arr[2]).toBe(9);
});

test("array destructuring into computed obj[key] targets", () => {
  const obj = {};
  const k1 = "x";
  const k2 = "y";
  [obj[k1], obj[k2]] = [100, 200];
  expect(obj.x).toBe(100);
  expect(obj.y).toBe(200);
});

test("object destructuring into member expression targets", () => {
  const target = {};
  const source = { a: 1, b: 2, c: 3 };
  ({ a: target.a, b: target.b, c: target.c } = source);
  expect(target.a).toBe(1);
  expect(target.b).toBe(2);
  expect(target.c).toBe(3);
});

test("mixed member expressions and identifiers in array pattern", () => {
  const obj = {};
  let z;
  [obj.x, z, obj.y] = [1, 2, 3];
  expect(obj.x).toBe(1);
  expect(z).toBe(2);
  expect(obj.y).toBe(3);
});

test("mixed member expressions and identifiers in object pattern", () => {
  const target = {};
  let local;
  ({ a: target.prop, b: local } = { a: 42, b: 99 });
  expect(target.prop).toBe(42);
  expect(local).toBe(99);
});

test("nested destructuring with member expression leaf targets", () => {
  const obj = {};
  [obj.a, [obj.b, obj.c]] = [1, [2, 3]];
  expect(obj.a).toBe(1);
  expect(obj.b).toBe(2);
  expect(obj.c).toBe(3);
});

test("rest element with member expression target", () => {
  const obj = {};
  [obj.first, ...obj.rest] = [1, 2, 3, 4];
  expect(obj.first).toBe(1);
  expect(obj.rest).toEqual([2, 3, 4]);
});

test("fibonacci iterator using this.a/this.b swap", () => {
  class FibIterator {
    constructor() {
      this.a = 0;
      this.b = 1;
    }
    next() {
      const value = this.a;
      [this.a, this.b] = [this.b, this.a + this.b];
      return { value, done: false };
    }
    [Symbol.iterator]() {
      return this;
    }
  }

  const fib = new FibIterator();
  const results = Array.from({ length: 8 }, () => fib.next().value);
  expect(results).toEqual([0, 1, 1, 2, 3, 5, 8, 13]);
});

test("evaluation order: RHS evaluated before targets assigned", () => {
  const obj = { x: 1, y: 2 };
  [obj.x, obj.y] = [obj.y, obj.x];
  expect(obj.x).toBe(2);
  expect(obj.y).toBe(1);
});

test("computed property with expression evaluated once per target", () => {
  const obj = {};
  let counter = 0;
  const key = () => {
    counter++;
    return "k" + counter;
  };
  [obj[key()], obj[key()]] = ["a", "b"];
  expect(obj.k1).toBe("a");
  expect(obj.k2).toBe("b");
  expect(counter).toBe(2);
});

test("member expression targets with default values", () => {
  const obj = {};
  [obj.a = 10, obj.b = 20] = [1, undefined];
  expect(obj.a).toBe(1);
  expect(obj.b).toBe(20);
});
