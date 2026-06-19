/*---
description: var supports destructuring patterns
features: [compat-var]
---*/

var __gocciaGlobalDestructuringVar;
({ __gocciaGlobalDestructuringVar } = { __gocciaGlobalDestructuringVar: 73 });
var [__gocciaGlobalArrayDestructuringVar] = [91];
var { value: __gocciaGlobalObjectDestructuringVar } = { value: 92 };

test("var with object destructuring", () => {
  var { a, b } = { a: 1, b: 2 };
  expect(a).toBe(1);
  expect(b).toBe(2);
});

test("var with array destructuring", () => {
  var [x, y] = [10, 20];
  expect(x).toBe(10);
  expect(y).toBe(20);
});

test("top-level destructuring assignment updates global-backed var", () => {
  expect(__gocciaGlobalDestructuringVar).toBe(73);
  expect(globalThis.__gocciaGlobalDestructuringVar).toBe(73);
});

test("top-level var destructuring creates global-backed bindings", () => {
  expect(__gocciaGlobalArrayDestructuringVar).toBe(91);
  expect(globalThis.__gocciaGlobalArrayDestructuringVar).toBe(91);
  expect(__gocciaGlobalObjectDestructuringVar).toBe(92);
  expect(globalThis.__gocciaGlobalObjectDestructuringVar).toBe(92);
});

test("object rest var binding copies enumerable data through getters", () => {
  var count = 0;
  var { ...rest } = {
    get value() {
      count++;
      return 2;
    },
  };

  expect(count).toBe(1);
  expect(rest.value).toBe(2);
  expect(Object.getOwnPropertyDescriptor(rest, "value").enumerable).toBe(true);
});

test("object rest var binding excludes extracted and non-enumerable properties", () => {
  var source = { x: 1, y: 2, a: 3 };
  Object.defineProperty(source, "hidden", { value: 4, enumerable: false });

  var { a, ...rest } = source;

  expect(a).toBe(3);
  expect(rest.a).toBeUndefined();
  expect(rest.hidden).toBeUndefined();
  expect(rest.x).toBe(1);
  expect(rest.y).toBe(2);
});
