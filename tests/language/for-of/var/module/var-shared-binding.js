/*---
description: var in for-of heads hoists to module scope
features: [for-of, async-await, top-level-await, compat-var, types-as-comments, modules]
---*/

var moduleBeforeLoop = moduleValue;
var moduleValues = [];
for (var moduleValue of [3, 4]) {
  moduleValues.push(moduleValue);
}

const moduleItems: number[] = [5, 6];
for (var moduleCounted of moduleItems) {}

var moduleReaders = [];
for (var [moduleFirst, moduleSecond] of [[1, 2], [3, 4]]) {
  moduleReaders.push(() => moduleFirst + moduleSecond);
}

for (var moduleEmpty of []) {}
for (var { moduleEmptyDestructured } of []) {}

for await (var moduleAwaited of [Promise.resolve(7), Promise.resolve(8)]) {}

test("module-scope var in for-of is visible after the loop", () => {
  expect(moduleValues).toEqual([3, 4]);
  expect(moduleValue).toBe(4);
});

test("module-scope var in for-of over a typed const array is visible after the loop", () => {
  expect(moduleCounted).toBe(6);
});

test("module-scope var in for-of is hoisted before the loop", () => {
  expect(moduleBeforeLoop).toBe(undefined);
});

test("module-scope var destructuring in for-of shares one binding", () => {
  expect(moduleFirst).toBe(3);
  expect(moduleSecond).toBe(4);
  expect(moduleReaders[0]()).toBe(7);
  expect(moduleReaders[1]()).toBe(7);
});

test("module-scope var in for-of that runs zero times is undefined", () => {
  expect(moduleEmpty).toBe(undefined);
  expect(moduleEmptyDestructured).toBe(undefined);
});

test("module-scope var in for-await-of is visible after the loop", () => {
  expect(moduleAwaited).toBe(8);
});

test("module-scope var in for-of does not create a global property", () => {
  expect(Object.hasOwn(globalThis, "moduleValue")).toBe(false);
  expect(Object.hasOwn(globalThis, "moduleEmpty")).toBe(false);
});
