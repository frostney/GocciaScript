/*---
description: Traditional for-loop per-iteration bindings preserve strict type metadata
features: [compat-traditional-for-loop, types-as-comments, strict-type-enforcement]
---*/

test("condition bindings keep annotations", () => {
  expect(() => {
    for (let i: number = 0; (i = "bad", i < 1); i++) {}
  }).toThrow(TypeError);
});

test("body bindings keep annotations", () => {
  expect(() => {
    for (let i: number = 0; i < 1; i++) {
      i = "bad";
    }
  }).toThrow(TypeError);
});

test("update bindings keep annotations", () => {
  expect(() => {
    for (let i: number = 0; i < 1; i = "bad") {}
  }).toThrow(TypeError);
});
