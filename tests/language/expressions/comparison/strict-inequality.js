/*---
description: Strict inequality operator (!==)
features: [strict-inequality]
---*/

test("strict inequality operator (!==)", () => {
  expect(5 !== "5").toBeTruthy();
  expect(true !== 1).toBeTruthy();
  expect(null !== undefined).toBeTruthy();
});

test("strict inequality operator (!==) with NaN", () => {
  expect(NaN !== NaN).toBeTruthy();
});

test("strict inequality operator (!==) with Infinity", () => {
  expect(Infinity !== Infinity).toBeFalsy();
  expect(Infinity !== -Infinity).toBeTruthy();
});

test("strict inequality operator (!==) with -Infinity", () => {
  expect(-Infinity !== -Infinity).toBeFalsy();
  expect(-Infinity !== Infinity).toBeTruthy();
});

test("strict inequality operator (!==) with null", () => {
  expect(null !== null).toBeFalsy();
  expect(null !== undefined).toBeTruthy();
});

test("strict inequality operator (!==) with undefined", () => {
  expect(undefined !== undefined).toBeFalsy();
  expect(undefined !== null).toBeTruthy();
});

test("strict inequality operator (!==) with object", () => {
  expect(new Object() !== new Object()).toBeTruthy();
});

test("strict inequality operator (!==) with array", () => {
  expect([1, 2, 3] !== [1, 2, 3]).toBeTruthy();
});

test("strict inequality operator (!==) with string", () => {
  expect("hello" !== "hello").toBeFalsy();
  expect("hello" !== "world").toBeTruthy();
});

test("strict inequality operator (!==) with number", () => {
  expect(1 !== 1).toBeFalsy();
  expect(1 !== 2).toBeTruthy();
});
