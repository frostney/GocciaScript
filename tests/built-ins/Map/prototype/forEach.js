/*---
description: Map.prototype.forEach iterates over entries in insertion order
features: [Map.prototype.forEach]
---*/

test("forEach iterates in insertion order", () => {
  const map = new Map([["c", 3], ["a", 1], ["b", 2]]);
  const keys = [];
  const values = [];
  map.forEach((value, key) => {
    keys.push(key);
    values.push(value);
  });
  expect(keys).toEqual(["c", "a", "b"]);
  expect(values).toEqual([3, 1, 2]);
});

test("forEach callback receives value, key, and map", () => {
  const map = new Map([["x", 10]]);
  map.forEach((value, key, m) => {
    expect(value).toBe(10);
    expect(key).toBe("x");
    expect(m).toBe(map);
  });
});

test("forEach on empty Map", () => {
  const map = new Map();
  let called = false;
  map.forEach(() => {
    called = true;
  });
  expect(called).toBe(false);
});

test("forEach visits each entry exactly once", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  let count = 0;
  map.forEach(() => {
    count = count + 1;
  });
  expect(count).toBe(3);
});
