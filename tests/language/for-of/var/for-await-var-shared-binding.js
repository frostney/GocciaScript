/*---
description: var in for-await-of heads hoists out of the loop
features: [for-of, async-await, top-level-await, compat-var]
---*/

var __gocciaForAwaitGlobalBeforeLoop = __gocciaForAwaitGlobalValue;
var __gocciaForAwaitGlobalValues = [];
for await (var __gocciaForAwaitGlobalValue of [Promise.resolve(3), Promise.resolve(4)]) {
  __gocciaForAwaitGlobalValues.push(__gocciaForAwaitGlobalValue);
}

for await (var [__gocciaForAwaitEmptyDestructured] of []) {}

const asyncRange = (limit) => ({
  [Symbol.asyncIterator]() {
    let i = 0;
    return {
      next() {
        i = i + 1;
        if (i <= limit) {
          return Promise.resolve({ value: i, done: false });
        }
        return Promise.resolve({ value: undefined, done: true });
      },
    };
  },
});

test("var in for-await-of is visible after the loop", async () => {
  for await (var item of asyncRange(2)) {}
  expect(item).toBe(2);
});

test("var in for-await-of over a sync iterable is visible after the loop", async () => {
  for await (var item of [Promise.resolve(1), 2]) {}
  expect(item).toBe(2);
});

test("var in for-await-of is hoisted before the loop", async () => {
  expect(item).toBe(undefined);
  for await (var item of asyncRange(2)) {}
  expect(item).toBe(2);
});

test("var in for-await-of that runs zero times is undefined", async () => {
  for await (var item of asyncRange(0)) {}
  expect(item).toBe(undefined);
});

test("var in for-await-of keeps its value across a later await", async () => {
  for await (var item of asyncRange(3)) {
    await Promise.resolve();
  }
  await Promise.resolve();
  expect(item).toBe(3);
});

test("var in for-await-of captures a shared binding across iterations", async () => {
  const fns = [];
  for await (var item of asyncRange(2)) {
    fns.push(() => item);
  }
  expect(fns[0]()).toBe(2);
  expect(fns[1]()).toBe(2);
});

test("var array destructuring in for-await-of assigns hoisted bindings", async () => {
  expect(a).toBe(undefined);
  for await (var [a, b] of [Promise.resolve([1, 2]), [3, 4]]) {}
  expect(a).toBe(3);
  expect(b).toBe(4);
});

test("var object destructuring in for-await-of assigns hoisted bindings", async () => {
  for await (var { a, b = "fallback" } of [{ a: 1, b: 2 }, { a: 3 }]) {}
  expect(a).toBe(3);
  expect(b).toBe("fallback");
});

test("var destructuring in for-await-of that runs zero times is undefined", async () => {
  for await (var [a, { b }] of asyncRange(0)) {}
  expect(a).toBe(undefined);
  expect(b).toBe(undefined);
});

test("top-level var in for-await-of is visible after the loop", () => {
  expect(__gocciaForAwaitGlobalValues).toEqual([3, 4]);
  expect(__gocciaForAwaitGlobalValue).toBe(4);
  expect(globalThis.__gocciaForAwaitGlobalValue).toBe(4);
});

test("top-level var in for-await-of is hoisted before the loop", () => {
  expect(__gocciaForAwaitGlobalBeforeLoop).toBe(undefined);
});

test("top-level var destructuring in empty for-await-of creates global property", () => {
  const desc = Object.getOwnPropertyDescriptor(
    globalThis,
    "__gocciaForAwaitEmptyDestructured"
  );
  expect(typeof desc).toBe("object");
  expect(desc.configurable).toBe(false);
  expect(globalThis.__gocciaForAwaitEmptyDestructured).toBeUndefined();
});
