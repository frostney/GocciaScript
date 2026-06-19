/*---
description: Anonymous function expressions
features: [compat-function]
---*/

test("anonymous function expression assigned to variable", () => {
  const multiply = function(a, b) {
    return a * b;
  };
  expect(multiply(3, 4)).toBe(12);
});

test("anonymous function expression infers name from assignment target", () => {
  let fn;

  fn = function() {};

  expect(fn.name).toBe("fn");
});

test("anonymous function expression does not infer name from member assignment", () => {
  const obj = {};

  obj.attr = function() {};

  expect(obj.attr.name).toBe("");
});

test("destructuring default infers anonymous function expression name", () => {
  let { missing = function() {} } = {};

  expect(missing.name).toBe("missing");
});

test("parenthesized function defaults infer names but sequence defaults do not", () => {
  let xCover;
  let cover;

  [xCover = (0, function() {}), cover = (function() {})] = [];

  expect(xCover.name).not.toBe("xCover");
  expect(cover.name).toBe("cover");
});

test("var initializer infers names only for anonymous function definitions", () => {
  var xCover = (0, function() {});
  var cover = (function() {});

  expect(xCover.name).not.toBe("xCover");
  expect(cover.name).toBe("cover");
});

test("parenthesized assignment target does not infer function name", () => {
  let fn;

  (fn) = function() {};

  expect(fn.name).toBe("");
});

test("function expression as callback", () => {
  const arr = [1, 2, 3];
  const doubled = arr.map(function(x) {
    return x * 2;
  });
  expect(doubled).toEqual([2, 4, 6]);
});

test("immediately invoked function expression", () => {
  const result = (function() {
    return 42;
  })();
  expect(result).toBe(42);
});

test("named async function expression source text starts at async keyword", () => {
  const source = (async function namedAsync() {
    return 1;
  }).toString();

  expect(source.startsWith("async function namedAsync")).toBe(true);
});
