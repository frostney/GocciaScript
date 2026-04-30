/*---
description: The number annotation accepts both integer and float values
features: [types-as-comments, strict-type-enforcement]
---*/

test("number annotation accepts integer", () => {
  let x: number = 5;
  expect(x).toBe(5);
});

test("number annotation accepts float", () => {
  let x: number = 3.14;
  expect(x).toBe(3.14);
});

describe("number coercion with enforcement", () => {

test("number annotation accepts integer then float", () => {
  let x: number = 5;
  x = 3.14;
  expect(x).toBe(3.14);
});

test("number annotation accepts float then integer", () => {
  let x: number = 3.14;
  x = 42;
  expect(x).toBe(42);
});

test("number annotation accepts negative numbers", () => {
  let x: number = -10;
  expect(x).toBe(-10);
  x = -0.5;
  expect(x).toBe(-0.5);
});

test("number annotation accepts zero", () => {
  let x: number = 0;
  expect(x).toBe(0);
});

test("number annotation accepts Infinity", () => {
  let x: number = 1;
  x = 1 / 0;
  expect(x).toBe(Infinity);
});

test("number annotation accepts NaN", () => {
  let x: number = 1;
  x = Number.NaN;
  expect(Number.isNaN(x)).toBe(true);
});

}); // describe
