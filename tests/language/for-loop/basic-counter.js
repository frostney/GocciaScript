/*---
description: Traditional for-loop counts up/down with let
features: [compat-traditional-for-loop]
---*/

test("counts up", () => {
  const result = [];
  for (let i = 0; i < 5; i++) result.push(i);
  expect(result).toEqual([0, 1, 2, 3, 4]);
});

test("counts up with assign-add", () => {
  const result = [];
  for (let i = 0; i < 5; i = i + 1) result.push(i);
  expect(result).toEqual([0, 1, 2, 3, 4]);
});

test("counts up with plus-assign", () => {
  const result = [];
  for (let i = 0; i < 5; i += 1) result.push(i);
  expect(result).toEqual([0, 1, 2, 3, 4]);
});

test("counts down", () => {
  const result = [];
  for (let i = 5; i > 0; i--) result.push(i);
  expect(result).toEqual([5, 4, 3, 2, 1]);
});

test("counts down with assign-subtract", () => {
  const result = [];
  for (let i = 5; i > 0; i = i - 1) result.push(i);
  expect(result).toEqual([5, 4, 3, 2, 1]);
});

test("counts down with minus-assign", () => {
  const result = [];
  for (let i = 5; i > 0; i -= 1) result.push(i);
  expect(result).toEqual([5, 4, 3, 2, 1]);
});

test("assign-add matches increment with identifier limit", () => {
  const n = 5;
  const incremented = [];
  for (let i = 0; i < n; i++) incremented.push(i);
  const assigned = [];
  for (let i = 0; i < n; i = i + 1) assigned.push(i);
  expect(assigned).toEqual(incremented);
  expect(assigned).toEqual([0, 1, 2, 3, 4]);
});

test("assign-add with parameter limit", () => {
  const run = (n) => {
    const result = [];
    for (let i = 0; i < n; i = i + 1) result.push(i);
    return result;
  };
  expect(run(4)).toEqual([0, 1, 2, 3]);
});

test("body write to counter matches increment without integer step", () => {
  const incremented = [];
  for (let i = 0; i < 5; i++) {
    incremented.push(i);
    if (i === 2) i = 10;
  }
  const assigned = [];
  for (let i = 0; i < 5; i = i + 1) {
    assigned.push(i);
    if (i === 2) i = 10;
  }
  expect(assigned).toEqual(incremented);
  expect(assigned).toEqual([0, 1, 2]);
});

test("body string write uses addition not integer step", () => {
  const result = [];
  for (let i = 0; i < 5; i = i + 1) {
    result.push(i);
    if (i === 1) i = "x";
  }
  expect(result).toEqual([0, 1]);
});

test("mutating the limit in the body is visible", () => {
  let n = 5;
  const result = [];
  for (let i = 0; i < n; i = i + 1) {
    result.push(i);
    if (i === 1) n = 2;
  }
  expect(result).toEqual([0, 1]);
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

test("nested [+In] grammar productions restore the in operator in initializers", () => {
  const scope = { marker: true };
  let value;

  for (value = true ? "marker" in scope : false; false;) {}
  expect(value).toBe(true);

  for (value = ["marker" in scope][0]; false;) {}
  expect(value).toBe(true);

  for (value = { result: "marker" in scope }.result; false;) {}
  expect(value).toBe(true);

  for (value = `${"marker" in scope}`; false;) {}
  expect(value).toBe("true");

  for (value = { get ["marker" in scope]() { return "object"; } }[true]; false;) {}
  expect(value).toBe("object");

  for (value = new (class {
    get ["marker" in scope]() { return "instance"; }
  })()[true]; false;) {}
  expect(value).toBe("instance");

  for (value = class {
    static get ["marker" in scope]() { return "static"; }
  }[true]; false;) {}
  expect(value).toBe("static");

  for (value = false && import("unused", "marker" in scope); false;) {}
  expect(value).toBe(false);
});
