/*---
description: var in for-of heads hoists out of the loop
features: [for-of, compat-var, types-as-comments]
---*/

var __gocciaForOfGlobalBeforeLoop = __gocciaForOfGlobalValue;
var __gocciaForOfGlobalValues = [];
for (var __gocciaForOfGlobalValue of [3, 4]) {
  __gocciaForOfGlobalValues.push(__gocciaForOfGlobalValue);
}

var __gocciaForOfGlobalPairs = [];
for (var [__gocciaForOfGlobalFirst, __gocciaForOfGlobalSecond] of [[1, 2], [3, 4]]) {
  __gocciaForOfGlobalPairs.push(
    __gocciaForOfGlobalFirst + __gocciaForOfGlobalSecond
  );
}

const __gocciaForOfGlobalItems: number[] = [5, 6];
for (var __gocciaForOfGlobalCounted of __gocciaForOfGlobalItems) {}

for (var __gocciaForOfEmptyGlobal of []) {}
for (var [__gocciaForOfEmptyDestructured] of []) {}

test("var in for-of is visible after the loop", () => {
  for (var item of [1, 2]) {}
  expect(item).toBe(2);
});

test("var in for-of hoists into enclosing function", () => {
  const collect = () => {
    for (var item of [1, 2]) {}
    return item;
  };
  expect(collect()).toBe(2);
});

test("var in for-of over a typed const array is visible after the loop", () => {
  const items: number[] = [1, 2, 3];
  const seen = [];
  for (var item of items) {
    seen.push(item);
  }
  expect(seen).toEqual([1, 2, 3]);
  expect(item).toBe(3);
});

test("var in for-of over a non-array iterable is visible after the loop", () => {
  for (var item of new Set(["a", "b"])) {}
  expect(item).toBe("b");
});

test("var in for-of is hoisted before the loop", () => {
  expect(item).toBe(undefined);
  for (var item of [1, 2]) {}
  expect(item).toBe(2);
});

test("var in for-of that runs zero times is undefined", () => {
  for (var item of []) {}
  expect(item).toBe(undefined);

  const items: number[] = [];
  for (var other of items) {}
  expect(other).toBe(undefined);
});

test("var in for-of that runs zero times keeps an earlier value", () => {
  var item = "before";
  for (var item of []) {}
  expect(item).toBe("before");
});

test("var in for-of keeps the value from the iteration that breaks", () => {
  for (var item of [1, 2, 3]) {
    if (item === 2) {
      break;
    }
  }
  expect(item).toBe(2);
});

test("var in a nested for-of hoists into the enclosing function", () => {
  const collect = (rows) => {
    if (rows.length > 0) {
      for (var row of rows) {
        for (var cell of row) {}
      }
    }
    return [row, cell];
  };
  expect(collect([[1, 2], [3, 4]])).toEqual([[3, 4], 4]);
  expect(collect([])).toEqual([undefined, undefined]);
});

test("var captures a shared binding across iterations", () => {
  const fns = [];
  for (var item of [1, 2]) {
    fns.push(() => item);
  }
  expect(fns[0]()).toBe(2);
  expect(fns[1]()).toBe(2);
});

test("var over a typed const array captures a shared binding across iterations", () => {
  const items: number[] = [1, 2];
  const fns = [];
  for (var item of items) {
    fns.push(() => item);
  }
  expect(fns[0]()).toBe(2);
  expect(fns[1]()).toBe(2);
});

test("closure created before the loop observes the for-of var", () => {
  var item = 0;
  const read = () => item;
  const write = (value) => {
    item = value;
  };
  for (var item of [1, 2]) {}
  expect(read()).toBe(2);
  write(5);
  expect(item).toBe(5);
});

test("var array destructuring in for-of assigns hoisted bindings", () => {
  const sums = [];
  for (var [a, b] of [[1, 2], [3, 4]]) {
    sums.push(a + b);
  }
  expect(sums).toEqual([3, 7]);
  expect(a).toBe(3);
  expect(b).toBe(4);
});

test("var array destructuring in for-of is hoisted before the loop", () => {
  expect(a).toBe(undefined);
  expect(rest).toBe(undefined);
  for (var [a, ...rest] of [[1, 2, 3]]) {}
  expect(a).toBe(1);
  expect(rest).toEqual([2, 3]);
});

test("var array destructuring over a typed const array assigns hoisted bindings", () => {
  const pairs: number[][] = [[1, 2], [3, 4]];
  for (var [a, b] of pairs) {}
  expect(a).toBe(3);
  expect(b).toBe(4);
});

test("var object destructuring in for-of assigns hoisted bindings", () => {
  const seen = [];
  for (var { a, b: renamed = "fallback" } of [{ a: 1, b: 2 }, { a: 3 }]) {
    seen.push([a, renamed]);
  }
  expect(seen).toEqual([[1, 2], [3, "fallback"]]);
  expect(a).toBe(3);
  expect(renamed).toBe("fallback");
});

test("var destructuring in for-of that runs zero times is undefined", () => {
  for (var [a, b] of []) {}
  for (var { c } of []) {}
  expect(a).toBe(undefined);
  expect(b).toBe(undefined);
  expect(c).toBe(undefined);
});

test("var destructuring captures a shared binding across iterations", () => {
  const fns = [];
  for (var [a] of [[1], [2]]) {
    fns.push(() => a);
  }
  expect(fns[0]()).toBe(2);
  expect(fns[1]()).toBe(2);
});

test("top-level var in for-of is visible after the loop", () => {
  expect(__gocciaForOfGlobalValues).toEqual([3, 4]);
  expect(__gocciaForOfGlobalValue).toBe(4);
  expect(globalThis.__gocciaForOfGlobalValue).toBe(4);
});

test("top-level var in for-of over a typed const array is visible after the loop", () => {
  expect(__gocciaForOfGlobalCounted).toBe(6);
  expect(globalThis.__gocciaForOfGlobalCounted).toBe(6);
});

test("top-level var in for-of is hoisted before the loop", () => {
  expect(__gocciaForOfGlobalBeforeLoop).toBe(undefined);
});

test("top-level var destructuring in for-of is visible after the loop", () => {
  expect(__gocciaForOfGlobalPairs).toEqual([3, 7]);
  expect(__gocciaForOfGlobalFirst).toBe(3);
  expect(__gocciaForOfGlobalSecond).toBe(4);
  expect(globalThis.__gocciaForOfGlobalSecond).toBe(4);
});

test("top-level var in empty for-of creates global property", () => {
  const desc = Object.getOwnPropertyDescriptor(
    globalThis,
    "__gocciaForOfEmptyGlobal"
  );
  expect(typeof desc).toBe("object");
  expect(desc.configurable).toBe(false);
  expect(globalThis.__gocciaForOfEmptyGlobal).toBeUndefined();
});

test("top-level var destructuring in empty for-of creates global property", () => {
  const desc = Object.getOwnPropertyDescriptor(
    globalThis,
    "__gocciaForOfEmptyDestructured"
  );
  expect(typeof desc).toBe("object");
  expect(desc.configurable).toBe(false);
  expect(globalThis.__gocciaForOfEmptyDestructured).toBeUndefined();
});
