/*---
description: Traditional for-loop counts up/down with let
features: [compat-traditional-for-loop]
---*/

test("counts up", () => {
  const result = [];
  for (let i = 0; i < 5; i++) result.push(i);
  expect(result).toEqual([0, 1, 2, 3, 4]);
});

test("counts down", () => {
  const result = [];
  for (let i = 5; i > 0; i--) result.push(i);
  expect(result).toEqual([5, 4, 3, 2, 1]);
});

test("step by 2", () => {
  const result = [];
  for (let i = 0; i < 10; i += 2) result.push(i);
  expect(result).toEqual([0, 2, 4, 6, 8]);
});

test("comma-separated init bindings", () => {
  const result = [];
  for (let i = 0, j = 10; i < 3; i++, j--) result.push([i, j]);
  expect(result).toEqual([[0, 10], [1, 9], [2, 8]]);
});

test("zero-iteration loop", () => {
  const result = [];
  for (let i = 0; i < 0; i++) result.push(i);
  expect(result).toEqual([]);
});

test("division in initializer remains division", () => {
  const result = [];
  for (let i = 4 / 2; i < 5; i++) result.push(i);
  expect(result).toEqual([2, 3, 4]);
});

test("division does not hide the header semicolon during lookahead", () => {
  let elapsed = 2;
  let scale = 2;
  let result = 0;
  for (result = elapsed / scale; result < elapsed / scale + 1; result++);
  expect(result).toBe(2);
});

test("regex in initializer remains a regex literal", () => {
  let matched = false;
  for (let pattern = /\)/; !matched; matched = true) {
    expect(pattern.test(")")).toBe(true);
  }
});

test("nested function bodies restore the in operator in initializers", () => {
  let arrow;
  let holder;

  for (arrow = () => { return "x" in { x: true }; }; false;) {}
  for (holder = { check() { return "x" in { x: true }; } }; false;) {}

  expect(arrow()).toBe(true);
  expect(holder.check()).toBe(true);
});

test("nested function parameters restore the in operator in initializers", () => {
  let arrow;
  let holder;

  for (arrow = (value = "x" in { x: true }) => value; false;) {}
  for (holder = {
    check(value = "x" in { x: true }) {
      return value;
    },
  }; false;) {}

  expect(arrow()).toBe(true);
  expect(holder.check()).toBe(true);
});
