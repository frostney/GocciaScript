/*---
description: let/const in for-init binds per iteration (closure capture)
features: [compat-traditional-for-loop]
---*/

test("let captures per-iteration binding", () => {
  const fns = [];
  for (let i = 0; i < 3; i++) fns.push(() => i);
  expect(fns.map(f => f())).toEqual([0, 1, 2]);
});

test("multiple let bindings each pinned per iteration", () => {
  const fns = [];
  for (let i = 0, j = 100; i < 3; i++, j -= 10) fns.push(() => [i, j]);
  expect(fns.map(f => f())).toEqual([[0, 100], [1, 90], [2, 80]]);
});

test("body sees current iteration's binding", () => {
  const result = [];
  for (let i = 0; i < 3; i++) result.push(i);
  expect(result).toEqual([0, 1, 2]);
});

test("continue after capture preserves per-iteration binding in general path", () => {
  const fns = [];
  const limit = 3;
  for (let i = 0; i < limit; i++) {
    fns.push(() => i);
    continue;
  }
  expect(fns.map(fn => fn())).toEqual([0, 1, 2]);
});

test("closures in update capture the post-body per-iteration binding", () => {
  const fns = [];
  for (let i = 0; i < 3; i++, fns.push(() => i)) {}
  expect(fns.map(fn => fn())).toEqual([1, 2, 3]);
});

test("body and update closures capture distinct per-iteration bindings", () => {
  const bodyFns = [];
  const updateFns = [];
  for (let i = 0; i < 3; i++, updateFns.push(() => i)) {
    bodyFns.push(() => i);
  }
  expect(bodyFns.map(fn => fn())).toEqual([0, 1, 2]);
  expect(updateFns.map(fn => fn())).toEqual([1, 2, 3]);
});

test("continue evaluates update closures in the next per-iteration binding", () => {
  const fns = [];
  for (let i = 0; i < 3; i++, fns.push(() => i)) {
    continue;
  }
  expect(fns.map(fn => fn())).toEqual([1, 2, 3]);
});

test("closures in the test expression capture the body per-iteration binding", () => {
  const fns = [];
  for (let i = 0; i < 3 && (fns.push(() => i), true); i++) {}
  expect(fns.map(fn => fn())).toEqual([0, 1, 2]);
});
