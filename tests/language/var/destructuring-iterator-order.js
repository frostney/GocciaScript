/*---
description: |
  var array binding patterns run each element's default and nested pattern
  during its own iterator step and call return() once, after the pattern.
  ES2026 §8.6.2 BindingInitialization and §8.6.3 IteratorBindingInitialization.
features: [compat-var, destructuring, iterators]
---*/

const logged = (log, values) => {
  let index = 0;
  return {
    [Symbol.iterator]() { return this; },
    next() {
      log.push("next");
      if (index < values.length) {
        return { value: values[index++], done: false };
      }
      return { value: undefined, done: true };
    },
    return() {
      log.push("return");
      return {};
    },
  };
};

const fallback = (log, value) => {
  log.push("default");
  return value;
};

test("var runs each default before stepping the next element", () => {
  const log = [];
  var [a = fallback(log, 1), { x } = { x: fallback(log, 2) }] =
    logged(log, [undefined, undefined, 3]);
  expect([a, x]).toEqual([1, 2]);
  expect(log).toEqual(["next", "default", "next", "default", "return"]);
});

test("var closes the iterator after a throwing default", () => {
  const log = [];
  expect(() => {
    var [a = (log.push("default-throws"), null).missing] = logged(log, [undefined, 2]);
  }).toThrow(TypeError);
  expect(log).toEqual(["next", "default-throws", "return"]);
});

test("a for-of var head runs the default before closing the iterator", () => {
  const log = [];
  const seen = [];
  for (var [a = fallback(log, "d"), b] of [logged(log, [undefined, 2, 3])]) {
    seen.push(a, b);
  }
  expect(seen).toEqual(["d", 2]);
  expect(log).toEqual(["next", "default", "next", "return"]);
});
