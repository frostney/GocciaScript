/*---
description: Set.prototype.forEach iterates over values in insertion order
features: [Set.prototype.forEach]
---*/

test("forEach iterates in insertion order", () => {
  const set = new Set([3, 1, 2]);
  const values = [];
  set.forEach((value) => {
    values.push(value);
  });
  expect(values).toEqual([3, 1, 2]);
});

test("forEach callback receives value, value, and set", () => {
  const set = new Set(["a"]);
  set.forEach((value1, value2, s) => {
    expect(value1).toBe("a");
    expect(value2).toBe("a");
    expect(s).toBe(set);
  });
});

test("forEach on empty Set", () => {
  const set = new Set();
  let called = false;
  set.forEach(() => {
    called = true;
  });
  expect(called).toBe(false);
});

test("forEach visits each value exactly once", () => {
  const set = new Set([10, 20, 30]);
  let count = 0;
  set.forEach(() => {
    count = count + 1;
  });
  expect(count).toBe(3);
});
