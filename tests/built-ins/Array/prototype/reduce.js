/*---
description: Array.prototype.reduce reduces an array to a single value
features: [Array.prototype.reduce]
---*/

test("Array.prototype.reduce reduces an array to a single value", () => {
  const arr = [1, 2, 3];
  const reduced = arr.reduce((acc, x) => acc + x, 0);
  expect(reduced).toBe(6);
});

test("Array.prototype.reduce with index and array parameters", () => {
  const arr = [1, 2, 3];
  const reduced = arr.reduce((acc, x, index, array) => {
    expect(array).toBe(arr);

    if (index === 0) {
      expect(acc).toBe(0);
      expect(x).toBe(1);
      expect(index).toBe(0);
    }

    if (index === 1) {
      expect(acc).toBe(1);
      expect(x).toBe(2);
      expect(index).toBe(1);
    }

    if (index === 2) {
      expect(acc).toBe(3);
      expect(x).toBe(3);
      expect(index).toBe(2);
    }

    return acc + x;
  }, 0);

  expect(reduced).toBe(6);
});

test("Array.prototype.reduce to create object out of array", () => {
  const arr = [
    { name: "John", age: 20 },
    { name: "Jane", age: 21 },
    { name: "Jim", age: 22 },
  ];
  const reduced = arr.reduce((acc, x) => {
    acc[x.name] = x.age;
    return acc;
  }, {});

  expect(reduced).toEqual({ John: 20, Jane: 21, Jim: 22 });
  const keys = Object.keys(reduced);
  expect(keys).toContain("John");
  expect(keys).toContain("Jane");
  expect(keys).toContain("Jim");
  expect(reduced.John).toBe(20);
  expect(reduced.Jane).toBe(21);
  expect(reduced.Jim).toBe(22);
});

test("Array.prototype.reduce with empty array", () => {
  const arr = [];
  const reduced = arr.reduce((acc, x) => acc + x, 0);
  expect(reduced).toBe(0);
});

test("reduce without initial value uses first element as accumulator", () => {
  const arr = [1, 2, 3, 4];
  const result = arr.reduce((acc, x) => acc + x);
  expect(result).toBe(10);
});

test("reduce without initial value starts callback at index 1", () => {
  const indices = [];
  [10, 20, 30].reduce((acc, val, idx) => {
    indices.push(idx);
    return acc + val;
  });
  expect(indices).toEqual([1, 2]);
});

test("reduce with single element and no initial value returns that element", () => {
  const result = [42].reduce((acc, x) => acc + x);
  expect(result).toBe(42);
});

test("reduce on empty array without initial value throws TypeError", () => {
  expect(() => {
    [].reduce((acc, x) => acc + x);
  }).toThrow(TypeError);
});

test("reduce with initial value on single-element array", () => {
  const result = [5].reduce((acc, x) => acc + x, 10);
  expect(result).toBe(15);
});

test("reduce passes correct arguments to callback", () => {
  const calls = [];
  [1, 2, 3].reduce((acc, val, idx, arr) => {
    calls.push({ acc, val, idx, len: arr.length });
    return acc + val;
  }, 0);
  expect(calls.length).toBe(3);
  expect(calls[0].acc).toBe(0);
  expect(calls[0].val).toBe(1);
  expect(calls[0].idx).toBe(0);
  expect(calls[0].len).toBe(3);
  expect(calls[2].acc).toBe(3);
  expect(calls[2].val).toBe(3);
  expect(calls[2].idx).toBe(2);
});
