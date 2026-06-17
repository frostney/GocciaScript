/*---
description: Decrement operator works correctly
features: [decrement-operator]
---*/

test("pre-decrement", () => {
  let a = 5;
  expect(--a).toBe(4);
  expect(a).toBe(4);
});

test("post-decrement", () => {
  let a = 5;
  expect(a--).toBe(5);
  expect(a).toBe(4);
});

test("decrement preserves fractional part", () => {
  let x = 2.5;
  x--;
  expect(x).toBe(1.5);

  let y = 0.5;
  --y;
  expect(y).toBe(-0.5);
});

test("decrement coerces strings to numbers", () => {
  let value = "3";
  expect(value--).toBe(3);
  expect(value).toBe(2);

  const obj = { count: "10" };
  expect(--obj.count).toBe(9);
  expect(obj.count).toBe(9);
});

test("post-decrement on property access", () => {
  const obj = { count: 10 };
  expect(obj.count--).toBe(10);
  expect(obj.count).toBe(9);
});

test("pre-decrement on property access", () => {
  const obj = { count: 10 };
  expect(--obj.count).toBe(9);
  expect(obj.count).toBe(9);
});

test("post-decrement on computed member (array index)", () => {
  const arr = [5, 10, 15];
  expect(arr[0]--).toBe(5);
  expect(arr[0]).toBe(4);
  expect(arr[2]--).toBe(15);
  expect(arr[2]).toBe(14);
});

test("pre-decrement on computed member (array index)", () => {
  const arr = [5, 10, 15];
  expect(--arr[0]).toBe(4);
  expect(arr[0]).toBe(4);
  expect(--arr[1]).toBe(9);
  expect(arr[1]).toBe(9);
});

test("decrement on computed member with variable key", () => {
  const arr = [10, 20, 30];
  let i = 2;
  arr[i]--;
  expect(arr[2]).toBe(29);
});

test("decrement on computed member with string key", () => {
  const obj = { x: 100, y: 200 };
  const key = "x";
  obj[key]--;
  expect(obj.x).toBe(99);
  --obj["y"];
  expect(obj.y).toBe(199);
});

test("decrement on computed member evaluates object key once", () => {
  let keyEvaluated = false;
  const obj = {};
  const key = {
    toString() {
      if (keyEvaluated) {
        throw new Error("key evaluated twice");
      }
      keyEvaluated = true;
      return "count";
    },
  };

  obj[key]--;
  expect(keyEvaluated).toBe(true);
  expect(Number.isNaN(obj.count)).toBe(true);
});

test("decrement on nullish computed member checks base before property key coercion", () => {
  const key = {
    toString() {
      throw new Error("key coerced");
    },
  };

  expect(() => {
    const base = null;
    base[key]--;
  }).toThrow(TypeError);

  expect(() => {
    const base = undefined;
    --base[key];
  }).toThrow(TypeError);
});

test("decrement on nullish computed member still evaluates property expression", () => {
  expect(() => {
    const base = null;
    const key = () => {
      throw new RangeError("property expression evaluated");
    };
    base[key()]--;
  }).toThrow(RangeError);
});

test("decrement on computed member with symbol key", () => {
  const sym = Symbol("counter");
  const obj = { [sym]: 10 };
  obj[sym]--;
  expect(obj[sym]).toBe(9);
  --obj[sym];
  expect(obj[sym]).toBe(8);
});

test("post-decrement on non-writable property throws", () => {
  const obj = {};
  Object.defineProperty(obj, "x", { value: 5, writable: false });
  expect(() => obj.x--).toThrow(TypeError);
  expect(obj.x).toBe(5);
});

test("decrement on unresolved identifier throws ReferenceError", () => {
  expect(() => missingPostDecrement--).toThrow(ReferenceError);
  expect(() => --missingPreDecrement).toThrow(ReferenceError);
});
