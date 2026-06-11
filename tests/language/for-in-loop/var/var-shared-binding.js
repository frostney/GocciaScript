/*---
description: var in for-in heads hoists out of the loop
features: [compat-for-in-loop, compat-var]
---*/

for (var __gocciaForInEmptyGlobal in {}) {}
for (var [__gocciaForInEmptyDestructured] in {}) {}

var __gocciaForInGlobalKeys = [];
var __gocciaForInGlobalObject = {};
Object.defineProperty(__gocciaForInGlobalObject, "property", {
  enumerable: true,
});
for (var __gocciaForInGlobalKey in __gocciaForInGlobalObject) {
  __gocciaForInGlobalKeys.push(__gocciaForInGlobalKey);
}

var __gocciaForInGlobalDestructuredKeys = [];
for (var [__gocciaForInGlobalDestructuredKey] in { xy: 1 }) {
  __gocciaForInGlobalDestructuredKeys.push(
    __gocciaForInGlobalDestructuredKey
  );
}

test("var in for-in is visible after the loop", () => {
  for (var key in { a: 1, b: 2 }) {}
  expect(key).toBe("b");
});

test("top-level var in non-empty for-in receives enumerable keys", () => {
  expect(__gocciaForInGlobalKeys).toEqual(["property"]);
  expect(__gocciaForInGlobalKey).toBe("property");
});

test("top-level var destructuring in non-empty for-in receives enumerable keys", () => {
  expect(__gocciaForInGlobalDestructuredKeys).toEqual(["x"]);
  expect(__gocciaForInGlobalDestructuredKey).toBe("x");
});

test("top-level var in empty for-in creates global property", () => {
  const desc = Object.getOwnPropertyDescriptor(
    globalThis,
    "__gocciaForInEmptyGlobal"
  );
  expect(typeof desc).toBe("object");
  expect(desc.configurable).toBe(false);
  expect(globalThis.__gocciaForInEmptyGlobal).toBeUndefined();
});

test("top-level var destructuring in empty for-in creates global property", () => {
  const desc = Object.getOwnPropertyDescriptor(
    globalThis,
    "__gocciaForInEmptyDestructured"
  );
  expect(typeof desc).toBe("object");
  expect(desc.configurable).toBe(false);
  expect(globalThis.__gocciaForInEmptyDestructured).toBeUndefined();
});

test("var in for-in hoists into enclosing function", () => {
  const f = () => {
    for (var name in { first: 1 }) {}
    return name;
  };
  expect(f()).toBe("first");
});

test("var captures a shared binding across iterations", () => {
  const fns = [];
  for (var key in { a: 1, b: 2 }) {
    fns.push(() => key);
  }
  expect(fns[0]()).toBe("b");
  expect(fns[1]()).toBe("b");
});

test("var array destructuring in for-in assigns hoisted bindings", () => {
  const values = [];
  for (var [x] in { xy: 1 }) {
    values.push(x);
  }
  expect(values).toEqual(["x"]);
  expect(x).toBe("x");
});

test("var array destructuring in for-in is hoisted before the loop", () => {
  expect(x).toBe(undefined);
  for (var [x] in { xy: 1 }) {}
  expect(x).toBe("x");
});

test("var object destructuring in for-in assigns hoisted bindings", () => {
  const values = [];
  for (var { length: len, [0]: first } in { ab: 1, c: 2 }) {
    values.push([len, first]);
  }
  expect(values).toEqual([[2, "a"], [1, "c"]]);
  expect(len).toBe(1);
  expect(first).toBe("c");
});

test("var object destructuring in for-in supports computed names and defaults", () => {
  const values = [];
  var count = 0;
  for (var { length: len, [len - 1 + count]: value = "fallback" } in "foo") {
    values.push([len, value]);
    count++;
  }
  expect(values).toEqual([[1, "0"], [1, "fallback"], [1, "fallback"]]);
  expect(len).toBe(1);
  expect(value).toBe("fallback");
});

test("var destructuring captures a shared binding across iterations", () => {
  const fns = [];
  for (var [key] in { a: 1, b: 2 }) {
    fns.push(() => key);
  }
  expect(fns[0]()).toBe("b");
  expect(fns[1]()).toBe("b");
});
