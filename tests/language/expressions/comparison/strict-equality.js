/*---
description: Strict equality operator (===) works correctly
features: [strict-equality-operator]
---*/

test("strict equality operator (===) with NaN", () => {
  expect(NaN === NaN).toBeFalsy();
});

test("strict equality operator (===) with Infinity", () => {
  expect(Infinity === Infinity).toBeTruthy();
  expect(Infinity === -Infinity).toBeFalsy();
});

test("strict equality operator (===) with -Infinity", () => {
  expect(-Infinity === -Infinity).toBeTruthy();
  expect(-Infinity === Infinity).toBeFalsy();
});

test("strict equality operator (===) with null", () => {
  expect(null === null).toBeTruthy();
  expect(null === undefined).toBeFalsy();
});

test("strict equality operator (===) with undefined", () => {
  expect(undefined === undefined).toBeTruthy();
  expect(undefined === null).toBeFalsy();
});

test("strict equality operator (===) with object", () => {
  expect(new Object() === new Object()).toBeFalsy();
});

test("strict equality operator (===) with array", () => {
  // @ts-ignore: This is a test for strict equality, not a test for array equality
  expect([1, 2, 3] === [1, 2, 3]).toBeFalsy();
});

test("strict equality operator (===) with string", () => {
  expect("hello" === "hello").toBeTruthy();
  expect("hello" === "world").toBeFalsy();
});

test("strict equality operator (===) with number", () => {
  expect(1 === 1).toBeTruthy();
  expect(1 === 2).toBeFalsy();
});
