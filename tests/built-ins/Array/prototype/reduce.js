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

test("Array.protoype.reduce to create object out of array", () => {
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
  expect(Object.keys(reduced)).toEqual(["John", "Jane", "Jim"]);
  expect(reduced.John).toBe(20);
  expect(reduced.Jane).toBe(21);
  expect(reduced.Jim).toBe(22);
});

test("Array.prototype.reduce with empty array", () => {
  const arr = [];
  const reduced = arr.reduce((acc, x) => acc + x, 0);
  expect(reduced).toBe(0);
});
