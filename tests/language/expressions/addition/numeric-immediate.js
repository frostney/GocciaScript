/*---
description: Proven numeric locals add an Int16 immediate
features: [addition-operator]
---*/

test("adds an Int16 immediate to an integer local", () => {
  let n = 4;
  expect(n + 3).toBe(7);
  expect(3 + n).toBe(7);
  expect(n + -3).toBe(1);
  expect(-3 + n).toBe(1);
});

test("adds an Int16 immediate to a float local", () => {
  let n = 4.5;
  expect(n + 3).toBe(7.5);
  expect(3 + n).toBe(7.5);
  expect(n + -3).toBe(1.5);
  expect(-3 + n).toBe(1.5);
});

// Each local below is reassigned only numeric expressions, so the compiler
// proves it a Number, but the value it ends up holding is not a scalar
// register: NaN, the infinities and -0 stay boxed number values.
test("adds an Int16 immediate to a local that holds NaN", () => {
  let n = 0;
  for (const _ of [1, 2]) {
    n = n + 0 / 0;
  }
  expect(n + 3).toBe(NaN);
  expect(n - 3).toBe(NaN);
});

test("adds an Int16 immediate to a local that holds an infinity", () => {
  let up = 0;
  let down = 0;
  for (const _ of [1, 2]) {
    up = up + 1 / 0;
    down = down - 1 / 0;
  }
  expect(up + 3).toBe(Infinity);
  expect(up - 3).toBe(Infinity);
  expect(down + 3).toBe(-Infinity);
  expect(down - 3).toBe(-Infinity);
});

test("adds an Int16 immediate to a local that holds -0", () => {
  let n = 0;
  for (const _ of [1]) {
    n = n * -0;
  }
  expect(Object.is(n, -0)).toBe(true);
  expect(n + 3).toBe(3);
  expect(n - 3).toBe(-3);
});

test("compares a local that holds NaN or an infinity with an Int16 immediate", () => {
  let nan = 0;
  let up = 0;
  let down = 0;
  for (const _ of [1, 2]) {
    nan = nan + 0 / 0;
    up = up + 1 / 0;
    down = down - 1 / 0;
  }
  expect(nan <= 5 ? "lte" : "gt").toBe("gt");
  expect(up <= 5 ? "lte" : "gt").toBe("gt");
  expect(down <= 5 ? "lte" : "gt").toBe("lte");
});
