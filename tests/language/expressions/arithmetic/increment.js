/*---
description: Increment operator works correctly
features: [increment-operators]
---*/

test("pre-increment", () => {
  let a = 5;
  expect(a).toBe(5);
  expect(++a).toBe(6);
  expect(a).toBe(6);
});

test("post-increment", () => {
  let a = 5;
  expect(a).toBe(5);
  expect(a++).toBe(5);
  expect(a).toBe(6);
});

test("increment preserves fractional part", () => {
  let x = 1.5;
  x++;
  expect(x).toBe(2.5);

  let y = -0.5;
  ++y;
  expect(y).toBe(0.5);
});

test("increment coerces strings to numbers", () => {
  let value = "1";
  expect(value++).toBe(1);
  expect(value).toBe(2);

  const obj = { count: "10" };
  expect(++obj.count).toBe(11);
  expect(obj.count).toBe(11);
});

test("post-increment on property access", () => {
  const obj = { count: 10 };
  expect(obj.count++).toBe(10);
  expect(obj.count).toBe(11);
});

test("pre-increment on property access", () => {
  const obj = { count: 10 };
  expect(++obj.count).toBe(11);
  expect(obj.count).toBe(11);
});

test("post-increment on computed member (array index)", () => {
  const arr = [1, 2, 3];
  expect(arr[0]++).toBe(1);
  expect(arr[0]).toBe(2);
  expect(arr[1]++).toBe(2);
  expect(arr[1]).toBe(3);
});

test("pre-increment on computed member (array index)", () => {
  const arr = [1, 2, 3];
  expect(++arr[0]).toBe(2);
  expect(arr[0]).toBe(2);
  expect(++arr[2]).toBe(4);
  expect(arr[2]).toBe(4);
});

test("increment on computed member with variable key", () => {
  const arr = [10, 20, 30];
  let i = 1;
  arr[i]++;
  expect(arr[1]).toBe(21);
});

test("increment on computed member with string key", () => {
  const obj = { a: 5, b: 10 };
  const key = "a";
  obj[key]++;
  expect(obj.a).toBe(6);
  ++obj["b"];
  expect(obj.b).toBe(11);
});

test("increment on computed member with symbol key", () => {
  const sym = Symbol("counter");
  const obj = { [sym]: 5 };
  obj[sym]++;
  expect(obj[sym]).toBe(6);
  ++obj[sym];
  expect(obj[sym]).toBe(7);
});

test("post-increment on non-writable property throws", () => {
  const obj = {};
  Object.defineProperty(obj, "x", { value: 1, writable: false });
  expect(() => obj.x++).toThrow(TypeError);
  expect(obj.x).toBe(1);
});

test("increment cannot mutate captured const binding", () => {
  expect(() => {
    const value = 1;
    const mutate = () => {
      value++;
    };
    mutate();
  }).toThrow(TypeError);
});

test("post-increment on captured local syncs upvalue cell", () => {
  const f = () => {
    let i = 0;
    const get = () => i;
    i++;
    i++;
    return [get(), i];
  };
  expect(f()).toEqual([2, 2]);
});

test("pre-increment on captured local syncs upvalue cell", () => {
  const f = () => {
    let i = 0;
    const get = () => i;
    ++i;
    ++i;
    return [get(), i];
  };
  expect(f()).toEqual([2, 2]);
});

test("post-decrement on captured local syncs upvalue cell", () => {
  const f = () => {
    let i = 5;
    const get = () => i;
    i--;
    i--;
    return [get(), i];
  };
  expect(f()).toEqual([3, 3]);
});

test("pre-decrement on captured local syncs upvalue cell", () => {
  const f = () => {
    let i = 5;
    const get = () => i;
    --i;
    --i;
    return [get(), i];
  };
  expect(f()).toEqual([3, 3]);
});

test("post-increment on captured local returns old value", () => {
  const f = () => {
    let i = 10;
    const get = () => i;
    const a = i++;
    const b = i++;
    return [a, b, get()];
  };
  expect(f()).toEqual([10, 11, 12]);
});

test("pre-increment on captured local returns new value", () => {
  const f = () => {
    let i = 10;
    const get = () => i;
    const a = ++i;
    const b = ++i;
    return [a, b, get()];
  };
  expect(f()).toEqual([11, 12, 12]);
});

test("increment on captured local coerces string and syncs", () => {
  const f = () => {
    let i = "5";
    const get = () => i;
    i++;
    return [get(), i, typeof i];
  };
  expect(f()).toEqual([6, 6, "number"]);
});

test("increment on captured local coerces boolean and syncs", () => {
  const f = () => {
    let i = true;
    const get = () => i;
    i++;
    return [get(), i];
  };
  expect(f()).toEqual([2, 2]);
});

test("increment on captured local coerces null and syncs", () => {
  const f = () => {
    let i = null;
    const get = () => i;
    i++;
    return [get(), i];
  };
  expect(f()).toEqual([1, 1]);
});
