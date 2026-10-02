/*---
description: |
  The 'in' operator requires an object on the right-hand side.
  Using it with primitives (null, undefined, number, boolean, string, bigint, symbol)
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

test("in operator throws TypeError for bigint", () => {
  expect(() => {
    "property" in 10n;
  }).toThrow(TypeError);

  expect(() => {
    0 in 0n;
  }).toThrow(TypeError);
});

test("in operator throws TypeError for symbol", () => {
  const symbol = Symbol("description");

  expect(() => {
    "property" in symbol;
  }).toThrow(TypeError);

  expect(() => {
    Symbol.iterator in symbol;
  }).toThrow(TypeError);
});

test("in operator still answers for wrapper objects of those primitives", () => {
  expect("toString" in Object(10n)).toBe(true);
  expect("description" in Object(Symbol("description"))).toBe(true);
  expect("missing" in Object(10n)).toBe(false);
});
