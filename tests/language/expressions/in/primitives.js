/*---
description: |
  The 'in' operator requires an object on the right-hand side.
  Using it with primitives (null, undefined, number, boolean, string)
  must throw a TypeError per ECMAScript spec.
---*/

test("in operator throws TypeError for null", () => {
  expect(() => {
    "property" in null;
  }).toThrow(TypeError);
});

test("in operator throws TypeError for undefined", () => {
  expect(() => {
    "property" in undefined;
  }).toThrow(TypeError);
});

test("in operator throws TypeError for number", () => {
  expect(() => {
    "property" in 123;
  }).toThrow(TypeError);
});

test("in operator throws TypeError for boolean", () => {
  expect(() => {
    "property" in true;
  }).toThrow(TypeError);
});

test("in operator throws TypeError for string", () => {
  expect(() => {
    0 in "hello";
  }).toThrow(TypeError);

  expect(() => {
    "length" in "hello";
  }).toThrow(TypeError);
});
